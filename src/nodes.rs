use std::borrow::Cow;
use std::cmp::Ordering;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::iter::{Enumerate, from_fn};
use std::num::NonZeroU32;
use std::slice::Iter;

use crate::{
    Document, Name, NameData,
    error::{ErrorKind, Result},
};

impl<'input> Document<'input> {
    pub fn root<'doc>(&'doc self) -> Node<'doc, 'input> {
        self.node(NodeId::ROOT).unwrap()
    }

    pub fn root_element<'doc>(&'doc self) -> Node<'doc, 'input> {
        self.root().first_child_element().unwrap()
    }

    pub fn node<'doc>(&'doc self, id: NodeId) -> Option<Node<'doc, 'input>> {
        self.nodes.get(id.get()).map(|data| Node {
            id,
            data,
            doc: self,
        })
    }
}

#[derive(Clone, Copy)]
pub struct Node<'doc, 'input> {
    pub(crate) id: NodeId,
    pub(crate) data: &'doc NodeData,
    pub(crate) doc: &'doc Document<'input>,
}

impl PartialEq for Node<'_, '_> {
    fn eq(&self, other: &Self) -> bool {
        let doc = self.doc as *const Document;
        let other_doc = other.doc as *const Document;

        (self.id, doc) == (other.id, other_doc)
    }
}

impl Eq for Node<'_, '_> {}

impl PartialOrd for Node<'_, '_> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Node<'_, '_> {
    fn cmp(&self, other: &Self) -> Ordering {
        let doc = self.doc as *const Document;
        let other_doc = other.doc as *const Document;

        (self.id, doc).cmp(&(other.id, other_doc))
    }
}

impl Hash for Node<'_, '_> {
    fn hash<H>(&self, hasher: &mut H)
    where
        H: Hasher,
    {
        let doc = self.doc as *const Document;

        (self.id, doc).hash(hasher);
    }
}

impl<'doc, 'input> Node<'doc, 'input> {
    pub fn document(self) -> &'doc Document<'input> {
        self.doc
    }

    pub fn id(self) -> NodeId {
        self.id
    }

    pub fn is_root(&self) -> bool {
        self.id == NodeId::ROOT
    }

    pub fn is_element(&self) -> bool {
        self.data.element.is_some()
    }

    pub(crate) fn element_data(self) -> Option<&'doc ElementData<'input>> {
        self.data
            .element
            .map(|element| &self.doc.elements[element.get()])
    }

    pub fn is_text(&self) -> bool {
        self.data.text.is_some()
    }

    fn other(self, id: NodeId) -> Self {
        self.doc.node(id).unwrap()
    }

    fn iter<F>(self, f: F) -> impl Iterator<Item = Self> + Clone
    where
        F: Fn(Self) -> Option<Self> + Clone,
    {
        let mut next = Some(self);

        from_fn(move || match next {
            Some(next1) => {
                next = f(next1);

                Some(next1)
            }
            None => None,
        })
    }

    pub fn parent(self) -> Option<Self> {
        self.data.parent.map(|id| self.other(id))
    }

    pub fn ancestors(self) -> impl Iterator<Item = Self> + Clone {
        self.iter(Self::parent)
    }

    pub fn prev_sibling(self) -> Option<Self> {
        self.data.prev_sibling.map(|id| self.other(id))
    }

    pub fn prev_siblings(self) -> impl Iterator<Item = Self> + Clone {
        self.iter(Self::prev_sibling)
    }

    pub fn prev_sibling_element(self) -> Option<Self> {
        self.prev_siblings().find(Self::is_element)
    }

    pub fn next_sibling(self) -> Option<Self> {
        self.data
            .next_subtree
            .filter(|id| self.doc.nodes[id.get()].prev_sibling.unwrap() == self.id)
            .map(|id| self.other(id))
    }

    pub fn next_siblings(self) -> impl Iterator<Item = Self> + Clone {
        self.iter(Self::next_sibling)
    }

    pub fn next_sibling_element(self) -> Option<Self> {
        self.next_siblings().find(Self::is_element)
    }

    pub fn has_children(self) -> bool {
        self.data.last_child.is_some()
    }

    pub fn first_child(self) -> Option<Self> {
        if self.has_children() {
            Some(self.other(self.id.next()))
        } else {
            None
        }
    }

    pub fn first_child_element(self) -> Option<Self> {
        self.child_elements().next()
    }

    pub fn last_child(self) -> Option<Self> {
        self.data.last_child.map(|id| self.other(id))
    }

    pub fn last_child_element(self) -> Option<Self> {
        self.child_elements().next_back()
    }

    pub fn children(self) -> Children<'doc, 'input> {
        Children {
            front: self.first_child(),
            back: self.last_child(),
        }
    }

    pub fn child_elements(self) -> impl DoubleEndedIterator<Item = Self> + Clone {
        self.children().filter(Self::is_element)
    }

    /// ```
    /// # use robinson::{Document, Name};
    /// let doc = Document::parse(r#"<root><parent><child/></parent></root>"#).unwrap();
    ///
    /// let mut nodes = doc.root_element().descendants();
    ///
    /// let node = nodes.next().unwrap();
    /// assert_eq!(node.name(), Some(Name { namespace: None, local: "root" }));
    ///
    /// let node = nodes.next().unwrap();
    /// assert_eq!(node.name(), Some(Name { namespace: None, local: "parent" }));
    ///
    /// let node = nodes.next().unwrap();
    /// assert_eq!(node.name(), Some(Name { namespace: None, local: "child" }));
    ///
    /// assert_eq!(nodes.next(), None);
    pub fn descendants(self) -> Descendants<'doc, 'input> {
        let from = self.id.get();

        let until = self
            .data
            .next_subtree
            .map_or(self.doc.nodes.len(), |id| id.get());

        let nodes = self.doc.nodes[from..until].iter().enumerate();

        Descendants {
            from,
            nodes,
            doc: self.doc,
        }
    }

    pub fn name(self) -> Option<Name<'doc, 'input>> {
        self.element_data()
            .map(|element| element.name.get(self.doc))
    }

    pub fn has_name<N>(self, name: N) -> bool
    where
        Name<'doc, 'input>: PartialEq<N>,
    {
        self.name().is_some_and(|name1| name1 == name)
    }

    pub fn text(self) -> Option<&'doc str> {
        self.data.text.map(|text| self.doc.strings.get(text))
    }

    pub fn child_texts(self) -> impl Iterator<Item = &'doc str> + Clone {
        self.children().filter_map(Self::text)
    }

    /// ```
    /// # use std::borrow::Cow;
    /// # use robinson::Document;
    /// let doc = Document::parse(r#"<root>foo<child>bar</child>baz</root>"#).unwrap();
    ///
    /// let text = doc.root_element().child_text();
    ///
    /// assert_eq!(text, Some(Cow::Owned("foobaz".to_owned())));
    pub fn child_text(self) -> Option<Cow<'doc, str>> {
        collect_text(self.child_texts())
    }

    pub fn descedant_texts(self) -> impl Iterator<Item = &'doc str> + Clone {
        self.descendants().filter_map(Self::text)
    }

    /// ```
    /// # use std::borrow::Cow;
    /// # use robinson::Document;
    /// let doc = Document::parse(r#"<root>foo<child>bar</child>baz</root>"#).unwrap();
    ///
    /// let text = doc.root_element().descedant_text();
    ///
    /// assert_eq!(text, Some(Cow::Owned("foobarbaz".to_owned())));
    pub fn descedant_text(self) -> Option<Cow<'doc, str>> {
        collect_text(self.descedant_texts())
    }
}

fn collect_text<'doc>(mut iter: impl Iterator<Item = &'doc str> + Clone) -> Option<Cow<'doc, str>> {
    let mut cnt = 0;
    let mut len = 0;

    for text in iter.clone() {
        cnt += 1;
        len += text.len();
    }

    if cnt == 0 {
        return None;
    } else if cnt == 1 {
        let text = iter.next().unwrap();

        return Some(Cow::Borrowed(text));
    }

    let mut buf = String::with_capacity(len);

    for text in iter {
        buf.push_str(text);
    }

    Some(Cow::Owned(buf))
}

pub(crate) struct NodeData {
    pub(crate) element: Option<NodeId>,
    pub(crate) text: Option<NodeId>,
    pub(crate) parent: Option<NodeId>,
    pub(crate) prev_sibling: Option<NodeId>,
    pub(crate) next_subtree: Option<NodeId>,
    pub(crate) last_child: Option<NodeId>,
}

const _SIZE_OF_NODE_DATA: () = assert!(size_of::<NodeData>() == 3 * size_of::<usize>());

#[repr(Rust, packed)]
pub(crate) struct ElementData<'input> {
    pub(crate) name: NameData<'input>,
    pub(crate) attributes_start: u32,
    pub(crate) attributes_end: u32,
}

const _SIZE_OF_ELEMENT_DATA: () = assert!(
    size_of::<ElementData<'static>>()
        == size_of::<u16>() + 2 * size_of::<usize>() + 2 * size_of::<u32>()
);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NodeId(NonZeroU32);

impl NodeId {
    pub(crate) const ROOT: Self = Self(NonZeroU32::new(1).unwrap());

    pub(crate) fn new(id: usize) -> Result<Self> {
        if id >= u32::MAX as usize {
            return ErrorKind::TooManyNodes.into();
        }

        Ok(Self(NonZeroU32::new(id as u32 + 1).unwrap()))
    }

    pub(crate) fn get(self) -> usize {
        self.0.get() as usize - 1
    }

    pub(crate) fn next(self) -> Self {
        Self(self.0.checked_add(1).unwrap())
    }
}

#[derive(Clone)]
pub struct Children<'doc, 'input> {
    front: Option<Node<'doc, 'input>>,
    back: Option<Node<'doc, 'input>>,
}

impl<'doc, 'input> Iterator for Children<'doc, 'input> {
    type Item = Node<'doc, 'input>;

    fn next(&mut self) -> Option<Self::Item> {
        let is_last = self.front == self.back;

        let node = self.front.take();

        if is_last {
            self.back = None;
        } else {
            self.front = node.and_then(Node::next_sibling);
        }

        node
    }
}

impl DoubleEndedIterator for Children<'_, '_> {
    fn next_back(&mut self) -> Option<Self::Item> {
        let is_last = self.front == self.back;

        let node = self.back.take();

        if is_last {
            self.front = None;
        } else {
            self.back = node.and_then(Node::prev_sibling);
        }

        node
    }
}

#[derive(Clone)]
pub struct Descendants<'doc, 'input> {
    from: usize,
    nodes: Enumerate<Iter<'doc, NodeData>>,
    doc: &'doc Document<'input>,
}

impl<'doc, 'input> Iterator for Descendants<'doc, 'input> {
    type Item = Node<'doc, 'input>;

    fn next(&mut self) -> Option<Self::Item> {
        self.nodes.next().map(|(idx, data)| Node {
            id: NodeId::new(self.from + idx).unwrap(),
            data,
            doc: self.doc,
        })
    }

    fn nth(&mut self, n: usize) -> Option<Self::Item> {
        self.nodes.nth(n).map(|(idx, data)| Node {
            id: NodeId::new(self.from + idx).unwrap(),
            data,
            doc: self.doc,
        })
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.nodes.size_hint()
    }
}

impl ExactSizeIterator for Descendants<'_, '_> {}

impl DoubleEndedIterator for Descendants<'_, '_> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.nodes.next_back().map(|(idx, data)| Node {
            id: NodeId::new(self.from + idx).unwrap(),
            data,
            doc: self.doc,
        })
    }
}

impl fmt::Debug for Node<'_, '_> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut fmt = fmt.debug_struct("Node");

        if let Some(name) = self.name() {
            fmt.field("name", &name);
        }

        if let Some(text) = self.text() {
            fmt.field("text", &text);
        }

        if self.has_attributes() {
            fmt.field("attributes", &self.attributes());
        }

        if self.has_children() {
            fmt.field("children", &self.children());
        }

        fmt.finish()
    }
}

impl fmt::Debug for Children<'_, '_> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.debug_list().entries(self.clone()).finish()
    }
}

impl fmt::Debug for Descendants<'_, '_> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.debug_list().entries(self.clone()).finish()
    }
}
