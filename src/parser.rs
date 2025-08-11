use std::fmt;
use std::mem::replace;
use std::ops::Range;
use std::result::Result as StdResult;

use memchr::{memchr, memchr_iter, memchr2, memchr3};

use crate::{
    Document, DocumentBuilder, NameData,
    attributes::AttributeData,
    error::{ErrorKind, Result},
    namespaces::NamespaceData,
    nodes::{ElementData, NodeData, NodeId},
    strings::{StringBuf, StringsBuilder},
    tokenizer::{Reference, Tokenizer},
};

impl<'input> Document<'input> {
    pub fn parse(text: &'input str) -> Result<Self> {
        Self::parse_with_opts(text, Default::default())
    }

    pub fn parse_with_opts(text: &'input str, opts: Options<'input>) -> Result<Self> {
        let mut parser = Parser::new(text, opts)?;

        let mut tokenizer = Tokenizer::new(text);
        tokenizer.parse(&mut parser)?;

        if parser.parent_namespaces.len() > 1 {
            return ErrorKind::UnclosedRootElement.into();
        }

        let doc = parser.doc.build();

        if !doc.root().children().any(|child| child.is_element()) {
            return ErrorKind::MissingRootElement.into();
        }

        Ok(doc)
    }
}

#[derive(Default)]
pub struct Options<'input> {
    pub entity_resolver: Option<&'input mut EntityResolver<'input>>,
}

pub type EntityResolver<'input> =
    dyn FnMut(Option<&str>, &str) -> StdResult<Option<&'input str>, String>;

impl fmt::Debug for Options<'_> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.debug_struct("Options")
            .field(
                "entity_resolver",
                &if self.entity_resolver.is_some() {
                    "Some(..)"
                } else {
                    "None"
                },
            )
            .finish()
    }
}

pub(crate) struct Parser<'input> {
    doc: DocumentBuilder<'input>,
    element: Option<CurrElement<'input>>,
    parent: NodeId,
    subtree: Vec<NodeId>,
    attributes: Vec<CurrAttribute<'input>>,
    parent_namespaces: Vec<Range<u32>>,
    namespaces_offset: u32,
    entities: Vec<Entity<'input>>,
    entity_depth: u8,
    entity_breadth: u8,
    pub(crate) opts: Options<'input>,
}

#[derive(Clone, Copy)]
struct CurrElement<'input> {
    prefix: Option<&'input str>,
    local: &'input str,
}

struct CurrAttribute<'input> {
    prefix: Option<&'input str>,
    local: &'input str,
    value: NodeId,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct Entity<'input> {
    name: &'input str,
    value: &'input str,
}

impl<'input> Parser<'input> {
    fn new(text: &'input str, opts: Options<'input>) -> Result<Self> {
        let nodes = memchr_iter(b'<', text.as_bytes()).count();
        let attributes = memchr_iter(b'=', text.as_bytes()).count();

        let mut doc = DocumentBuilder {
            nodes: Vec::with_capacity(nodes),
            elements: Vec::with_capacity(nodes / 2),
            attributes: Vec::with_capacity(attributes),
            strings: StringsBuilder::new(text, nodes / 2)?,
            namespaces: Default::default(),
        };

        doc.nodes.push(NodeData {
            element: None,
            text: None,
            parent: None,
            prev_sibling: None,
            next_subtree: None,
            last_child: None,
        });

        doc.namespaces.push(
            NamespaceData {
                name: Some("xml"),
                uri: doc.strings.owned("http://www.w3.org/XML/1998/namespace")?,
            },
            &doc.strings,
        )?;

        let namespaces_offset = doc.namespaces.len();

        let default_namespaces = 0..namespaces_offset;

        Ok(Self {
            doc,
            element: None,
            parent: NodeId::new(0).unwrap(),
            subtree: Vec::new(),
            attributes: Vec::new(),
            parent_namespaces: vec![default_namespaces],
            namespaces_offset,
            entities: Vec::new(),
            entity_depth: 0,
            entity_breadth: 0,
            opts,
        })
    }

    pub(crate) fn open_element(
        &mut self,
        prefix: Option<&'input str>,
        local: &'input str,
    ) -> Result {
        self.element = Some(CurrElement { prefix, local });

        Ok(())
    }

    pub(crate) fn push_attribute(
        &mut self,
        tokenizer: &mut Tokenizer<'input>,
        prefix: Option<&'input str>,
        local: &'input str,
        value: &'input str,
    ) -> Result {
        let value = self.normalize_attribute_value(tokenizer, value)?;

        if prefix == Some("xmlns") {
            let inserted = self.doc.namespaces.push(
                NamespaceData {
                    name: Some(local),
                    uri: value,
                },
                &self.doc.strings,
            )?;

            if !inserted {
                self.doc.strings.pop(value);
            }
        } else if prefix.is_none() && local == "xmlns" {
            let inserted = self.doc.namespaces.push(
                NamespaceData {
                    name: None,
                    uri: value,
                },
                &self.doc.strings,
            )?;

            if !inserted {
                self.doc.strings.pop(value);
            }
        } else {
            self.attributes.push(CurrAttribute {
                prefix,
                local,
                value,
            });
        }

        Ok(())
    }

    pub(crate) fn close_empty_element(&mut self) -> Result {
        let namespaces = self.resolve_namespaces()?;
        let attributes = self.resolve_attributes(&namespaces)?;

        let Some(element) = self.element.take() else {
            return ErrorKind::UnexpectedCloseElement.into();
        };

        let namespace = self.doc.namespaces.find(&namespaces, element.prefix)?;

        let id = self.append_element_node(ElementData {
            name: NameData {
                namespace,
                local: element.local,
            },
            attributes_start: attributes.start,
            attributes_end: attributes.end,
        })?;

        self.subtree.push(id);

        Ok(())
    }

    pub(crate) fn close_element(
        &mut self,
        prefix: Option<&'input str>,
        local: &'input str,
    ) -> Result {
        let namespaces = self.resolve_namespaces()?;

        self.element = None;

        let parent = &self.doc.nodes[self.parent.get()];

        if let Some(element) = parent.element {
            let namespace = self.doc.namespaces.find(&namespaces, prefix)?;

            let name = &self.doc.elements[element.get()].name;
            let name_namespace = name.namespace;
            let name_local = name.local;

            if namespace != name_namespace || local != name_local {
                return ErrorKind::UnexpectedCloseElement.into();
            }
        }

        self.subtree.push(self.parent);

        if let Some(ancestor) = parent.parent {
            self.parent = ancestor;
            self.parent_namespaces.pop();
        } else {
            return ErrorKind::UnexpectedCloseElement.into();
        }

        Ok(())
    }

    pub(crate) fn close_open_element(&mut self) -> Result {
        let namespaces = self.resolve_namespaces()?;
        let attributes = self.resolve_attributes(&namespaces)?;

        let Some(element) = self.element.take() else {
            return ErrorKind::UnexpectedCloseElement.into();
        };

        let namespace = self.doc.namespaces.find(&namespaces, element.prefix)?;

        let id = self.append_element_node(ElementData {
            name: NameData {
                namespace,
                local: element.local,
            },
            attributes_start: attributes.start,
            attributes_end: attributes.end,
        })?;

        self.parent = id;
        self.parent_namespaces.push(namespaces);

        Ok(())
    }

    fn append_node(&mut self, element: Option<NodeId>, text: Option<NodeId>) -> Result<NodeId> {
        let new_id = NodeId::new(self.doc.nodes.len())?;

        let prev_sibling = self.doc.nodes[self.parent.get()].last_child.replace(new_id);

        self.doc.nodes.push(NodeData {
            element,
            text,
            parent: Some(self.parent),
            prev_sibling,
            next_subtree: None,
            last_child: None,
        });

        for id in self.subtree.drain(..) {
            self.doc.nodes[id.get()].next_subtree = Some(new_id);
        }

        Ok(new_id)
    }

    fn append_element_node(&mut self, element: ElementData<'input>) -> Result<NodeId> {
        let id = NodeId::new(self.doc.elements.len())?;

        self.doc.elements.push(element);

        self.append_node(Some(id), None)
    }

    fn append_text_node(&mut self, text: NodeId) -> Result<()> {
        let id = self.append_node(None, Some(text))?;

        self.subtree.push(id);

        Ok(())
    }

    pub(crate) fn append_text(
        &mut self,
        tokenizer: &mut Tokenizer<'input>,
        text: &'input str,
    ) -> Result {
        let pos = memchr2(b'&', b'\r', text.as_bytes());

        if pos.is_none() {
            let text = self.doc.strings.borrowed(text)?;

            self.append_text_node(text)?;
            return Ok(());
        }

        self.append_text_impl(tokenizer, text, pos)
    }

    #[inline(never)]
    fn append_text_impl(
        &mut self,
        tokenizer: &mut Tokenizer<'input>,
        mut text: &'input str,
        mut pos: Option<usize>,
    ) -> Result {
        let mut strings = self.doc.strings.take();
        let mut buf = StringBuf::new(&mut strings, text.len());

        let mut was_cr = false;

        while let Some(pos1) = pos {
            let (before, after) = text.split_at(pos1);
            buf.push_str(before);
            text = after;

            match text.as_bytes() {
                [b'\r', b'\n', ..] => {
                    buf.push('\n');
                    text = &text[2..];
                }
                [b'\r', ..] => {
                    buf.push('\n');
                    text = &text[1..];
                }
                _ => {
                    text = &text[1..];

                    let ref_ =
                        tokenizer.with_text(&mut text, |tokenizer| tokenizer.parse_reference())?;

                    match ref_ {
                        Reference::Char(char_) => {
                            let is_entity = self.entity_depth != 0;

                            match char_ {
                                '\r' if is_entity => {
                                    buf.push('\n');
                                    was_cr = true;
                                }
                                '\n' if is_entity => {
                                    if !was_cr {
                                        buf.push('\n');
                                    }
                                    was_cr = false;
                                }
                                char_ => {
                                    buf.push(char_);
                                    was_cr = false;
                                }
                            }
                        }
                        Reference::Entity(name) => {
                            let mut value = self.find_entity(name)?;

                            if !buf.is_empty() {
                                let text = buf.finish()?;

                                self.append_text_node(text)?;
                            }

                            self.doc.strings = strings;

                            self.open_entity()?;
                            let element = self.element.take();

                            tokenizer
                                .with_text(&mut value, |tokenizer| tokenizer.parse_content(self))?;

                            self.element = element;
                            self.close_entity();

                            strings = self.doc.strings.take();
                            buf = StringBuf::new(&mut strings, 0);
                        }
                    }
                }
            }

            pos = memchr2(b'&', b'\r', text.as_bytes());
        }

        buf.push_str(text);

        if !buf.is_empty() {
            let text = buf.finish()?;

            self.append_text_node(text)?;
        }

        self.doc.strings = strings;

        Ok(())
    }

    pub(crate) fn append_cdata(&mut self, cdata: &'input str) -> Result {
        let pos = memchr(b'\r', cdata.as_bytes());

        if pos.is_none() {
            let text = self.doc.strings.borrowed(cdata)?;

            self.append_text_node(text)?;
            return Ok(());
        }

        self.append_cdata_impl(cdata, pos)
    }

    #[inline(never)]
    fn append_cdata_impl(&mut self, mut cdata: &'input str, mut pos: Option<usize>) -> Result {
        let mut buf = StringBuf::new(&mut self.doc.strings, cdata.len());

        while let Some(pos1) = pos {
            let (line, rest) = cdata.split_at(pos1);

            buf.push_str(line);
            buf.push('\n');

            cdata = match rest.as_bytes().get(1) {
                Some(&b'\n') => &rest[2..],
                _ => &rest[1..],
            };

            pos = memchr(b'\r', cdata.as_bytes());
        }

        buf.push_str(cdata);

        let text = buf.finish()?;

        self.append_text_node(text)?;
        Ok(())
    }

    fn normalize_attribute_value(
        &mut self,
        tokenizer: &mut Tokenizer<'input>,
        value: &'input str,
    ) -> Result<NodeId> {
        let pos_entity = memchr(b'&', value.as_bytes());
        let pos_space = memchr3(b'\t', b'\r', b'\n', value.as_bytes());

        if pos_entity.is_none() && pos_space.is_none() {
            return self.doc.strings.borrowed(value);
        }

        let mut strings = self.doc.strings.take();
        let mut buf = StringBuf::new(&mut strings, value.len());

        self.normalize_attribute_value_impl(tokenizer, value, pos_entity, pos_space, &mut buf)?;

        let value = buf.finish()?;
        self.doc.strings = strings;

        Ok(value)
    }

    #[inline(never)]
    fn normalize_attribute_value_impl(
        &mut self,
        tokenizer: &mut Tokenizer<'input>,
        mut value: &'input str,
        mut pos_entity: Option<usize>,
        mut pos_space: Option<usize>,
        buf: &mut StringBuf<'_, 'input>,
    ) -> Result {
        while let Some(mut pos) = pos_entity {
            while let Some(pos1) = pos_space {
                if pos1 >= pos {
                    break;
                }

                let (before, after) = value.split_at(pos1);

                buf.push_str(before);
                buf.push(' ');

                value = match after.as_bytes().get(1) {
                    Some(b'\n') => {
                        pos -= pos1 + 2;
                        &after[2..]
                    }
                    _ => {
                        pos -= pos1 + 1;
                        &after[1..]
                    }
                };

                pos_space = memchr3(b'\t', b'\r', b'\n', value.as_bytes());
            }

            let (before, after) = value.split_at(pos);
            buf.push_str(before);
            value = &after[1..];

            let ref_ = tokenizer.with_text(&mut value, |tokenizer| tokenizer.parse_reference())?;

            match ref_ {
                Reference::Char(char_) => {
                    let is_entity = self.entity_depth != 0;

                    let char_ = match char_ {
                        '\t' | '\r' | '\n' if is_entity => ' ',
                        char_ => char_,
                    };

                    buf.push(char_);
                }
                Reference::Entity(name) => {
                    let value = self.find_entity(name)?;

                    let pos_entity = memchr(b'&', value.as_bytes());
                    let pos_space = memchr3(b'\t', b'\r', b'\n', value.as_bytes());

                    if pos_entity.is_none() && pos_space.is_none() {
                        buf.push_str(value);
                    } else {
                        self.open_entity()?;

                        self.normalize_attribute_value_impl(
                            tokenizer, value, pos_entity, pos_space, buf,
                        )?;

                        self.close_entity();
                    }
                }
            }

            pos_entity = memchr(b'&', value.as_bytes());
            pos_space = memchr3(b'\t', b'\r', b'\n', value.as_bytes());
        }

        while let Some(pos) = pos_space {
            let (before, after) = value.split_at(pos);

            buf.push_str(before);
            buf.push(' ');

            value = match after.as_bytes().get(1) {
                Some(b'\n') => &after[2..],
                _ => &after[1..],
            };

            pos_space = memchr3(b'\t', b'\r', b'\n', value.as_bytes());
        }

        buf.push_str(value);

        Ok(())
    }

    fn resolve_namespaces(&mut self) -> Result<Range<u32>> {
        if let Some(parent_namespaces) = self.parent_namespaces.last() {
            let parent_namespaces = parent_namespaces.clone();

            let self_namespaces = self.namespaces_offset..self.doc.namespaces.len();

            if self_namespaces.is_empty() {
                return Ok(parent_namespaces);
            }

            for idx in parent_namespaces {
                self.doc.namespaces.push_ref(&self_namespaces, idx)?;
            }
        }

        let new_len = self.doc.namespaces.len();
        let old_len = replace(&mut self.namespaces_offset, new_len);

        Ok(old_len..new_len)
    }

    fn resolve_attributes(&mut self, namespaces: &Range<u32>) -> Result<Range<u32>> {
        let old_len = self.doc.attributes.len();
        let new_len = old_len + self.attributes.len();

        if new_len > u32::MAX as usize {
            return ErrorKind::TooManyAttributes.into();
        }

        for attribute in self.attributes.drain(..) {
            let namespace = if attribute.prefix.is_none() {
                None
            } else {
                self.doc.namespaces.find(namespaces, attribute.prefix)?
            };

            self.doc.attributes.push(AttributeData {
                name: NameData {
                    namespace,
                    local: attribute.local,
                },
                value: attribute.value,
            });
        }

        Ok(old_len as u32..new_len as u32)
    }

    pub(crate) fn push_entity(&mut self, name: &'input str, value: &'input str) {
        if let Err(idx) = self
            .entities
            .binary_search_by_key(&name, |entity| entity.name)
        {
            self.entities.insert(idx, Entity { name, value });
        }
    }

    fn find_entity(&self, name: &'input str) -> Result<&'input str> {
        match self
            .entities
            .binary_search_by_key(&name, |entity| entity.name)
        {
            Ok(idx) => Ok(self.entities[idx].value),
            Err(_idx) => ErrorKind::UnknownEntity(name.to_owned()).into(),
        }
    }

    fn open_entity(&mut self) -> Result {
        if self.entity_depth != 0 {
            if self.entity_breadth == 100 {
                return ErrorKind::TooManyEntityReferences.into();
            }

            self.entity_breadth += 1;
        }

        if self.entity_depth == 10 {
            return ErrorKind::TooManyEntityReferences.into();
        }

        self.entity_depth += 1;

        Ok(())
    }

    fn close_entity(&mut self) {
        if self.entity_depth != 0 {
            self.entity_depth -= 1;
        }

        if self.entity_depth == 0 {
            self.entity_breadth = 0;
        }
    }
}
