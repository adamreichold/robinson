use std::ops::Range;

use crate::{
    Document, DocumentBuilder, NameData,
    attributes::AttributeData,
    error::{ErrorKind, Result},
    memchr::{memchr, memchr2, memchr2_count, memchr4},
    nodes::{ElementData, NodeData, NodeId},
    strings::{StringBuf, StringsBuilder, cmp_names, cmp_opt_names},
    tokenizer::{Reference, Tokenizer},
};

impl<'input> Document<'input> {
    pub fn parse(text: &'input str) -> Result<Self> {
        let mut parser = Parser::new(text)?;

        let mut tokenizer = Tokenizer::new(text);
        tokenizer.parse(&mut parser)?;

        let doc = parser.doc.build();

        if !doc.root().children().any(|child| child.is_element()) {
            return ErrorKind::MissingRootElement.into();
        }

        Ok(doc)
    }
}

pub(crate) struct Parser<'input> {
    doc: DocumentBuilder<'input>,
    element: Option<CurrElement<'input>>,
    parent: NodeId,
    subtree: Vec<NodeId>,
    attributes: Vec<CurrAttribute<'input>>,
    entities: Vec<Entity<'input>>,
    entity_depth: u8,
    entity_breadth: u8,
}

#[derive(Clone, Copy)]
struct CurrElement<'input> {
    prefix: Option<&'input str>,
    local: &'input str,
}

#[derive(Clone, Copy)]
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
    fn new(text: &'input str) -> Result<Self> {
        let (nodes, attributes) = memchr2_count(b'<', b'=', text.as_bytes());

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

        let xml_uri = doc.strings.owned("http://www.w3.org/XML/1998/namespace")?;

        doc.namespaces
            .push(&mut doc.strings, 0, Some("xml"), xml_uri)?;

        Ok(Self {
            doc,
            element: None,
            parent: NodeId::ROOT,
            subtree: Vec::new(),
            attributes: Vec::new(),
            entities: Vec::new(),
            entity_depth: 0,
            entity_breadth: 0,
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

        if cmp_opt_names(prefix, Some("xmlns")) {
            self.doc.namespaces.push(
                &mut self.doc.strings,
                tokenizer.element_depth(),
                Some(local),
                value,
            )?;
        } else if prefix.is_none() && cmp_names(local, "xmlns") {
            self.doc.namespaces.push(
                &mut self.doc.strings,
                tokenizer.element_depth(),
                None,
                value,
            )?;
        } else {
            self.attributes.push(CurrAttribute {
                prefix,
                local,
                value,
            });
        }

        Ok(())
    }

    pub(crate) fn close_empty_element(&mut self, tokenizer: &Tokenizer<'input>) -> Result {
        let attributes = self.resolve_attributes()?;

        let Some(element) = self.element.take() else {
            return ErrorKind::UnexpectedCloseElement.into();
        };

        let namespace = self.doc.namespaces.find(element.prefix)?;

        let id = self.append_element_node(ElementData {
            name: NameData {
                namespace,
                local: element.local,
            },
            attributes_start: attributes.start,
            attributes_end: attributes.end,
        })?;

        self.subtree.push(id);

        self.doc.namespaces.pop(tokenizer.element_depth());

        Ok(())
    }

    pub(crate) fn close_element(
        &mut self,
        tokenizer: &mut Tokenizer<'input>,
        prefix: Option<&'input str>,
        local: &'input str,
    ) -> Result {
        self.element = None;

        let parent = &self.doc.nodes[self.parent.get()];

        if let Some(element) = parent.element {
            let namespace = self.doc.namespaces.find(prefix)?;

            let name = &self.doc.elements[element.get()].name;
            let name_namespace = name.namespace;
            let name_local = name.local;

            if namespace != name_namespace || !cmp_names(local, name_local) {
                return ErrorKind::UnexpectedCloseElement.into();
            }
        }

        self.subtree.push(self.parent);

        if let Some(ancestor) = parent.parent {
            self.parent = ancestor;
        } else {
            return ErrorKind::UnexpectedCloseElement.into();
        }

        self.doc.namespaces.pop(tokenizer.element_depth());

        Ok(())
    }

    pub(crate) fn close_open_element(&mut self) -> Result {
        let attributes = self.resolve_attributes()?;

        let Some(element) = self.element.take() else {
            return ErrorKind::UnexpectedCloseElement.into();
        };

        let namespace = self.doc.namespaces.find(element.prefix)?;

        let id = self.append_element_node(ElementData {
            name: NameData {
                namespace,
                local: element.local,
            },
            attributes_start: attributes.start,
            attributes_end: attributes.end,
        })?;

        self.parent = id;

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

        for &id in &self.subtree {
            self.doc.nodes[id.get()].next_subtree = Some(new_id);
        }

        self.subtree.clear();

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

    #[cold]
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

    #[cold]
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
        let pos = memchr4(b'&', b'\t', b'\r', b'\n', value.as_bytes());

        if pos.is_none() {
            return self.doc.strings.borrowed(value);
        }

        let mut strings = self.doc.strings.take();
        let mut buf = StringBuf::new(&mut strings, value.len());

        self.normalize_attribute_value_impl_recursive(tokenizer, value, pos, &mut buf)?;

        let value = buf.finish()?;
        self.doc.strings = strings;

        Ok(value)
    }

    #[cold]
    #[inline(never)]
    fn normalize_attribute_value_impl_recursive(
        &mut self,
        tokenizer: &mut Tokenizer<'input>,
        mut value: &'input str,
        mut pos: Option<usize>,
        buf: &mut StringBuf<'_, 'input>,
    ) -> Result {
        while let Some(pos1) = pos {
            let (before, after) = value.split_at(pos1);
            buf.push_str(before);
            value = after;

            match value.as_bytes() {
                [b'\r', b'\n', ..] => {
                    buf.push(' ');
                    value = &value[2..];
                }
                [b'\t' | b'\r' | b'\n', ..] => {
                    buf.push(' ');
                    value = &value[1..];
                }
                _ => {
                    value = &value[1..];

                    let ref_ =
                        tokenizer.with_text(&mut value, |tokenizer| tokenizer.parse_reference())?;

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

                            let pos = memchr4(b'&', b'\t', b'\r', b'\n', value.as_bytes());

                            if pos.is_none() {
                                buf.push_str(value);
                            } else {
                                self.open_entity()?;

                                self.normalize_attribute_value_impl_recursive(
                                    tokenizer, value, pos, buf,
                                )?;

                                self.close_entity();
                            }
                        }
                    }
                }
            }

            pos = memchr4(b'&', b'\t', b'\r', b'\n', value.as_bytes());
        }

        buf.push_str(value);

        Ok(())
    }

    fn resolve_attributes(&mut self) -> Result<Range<u32>> {
        let old_len = self.doc.attributes.len();
        let new_len = old_len + self.attributes.len();

        if new_len > u32::MAX as usize {
            return ErrorKind::TooManyAttributes.into();
        }

        for attribute in &self.attributes {
            let namespace = if attribute.prefix.is_none() {
                None
            } else {
                self.doc.namespaces.find(attribute.prefix)?
            };

            self.doc.attributes.push(AttributeData {
                name: NameData {
                    namespace,
                    local: attribute.local,
                },
                value: attribute.value,
            });
        }

        self.attributes.clear();

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
