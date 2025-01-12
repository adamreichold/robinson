use std::fmt;
use std::iter::{Enumerate, from_fn};
use std::num::NonZeroU32;
use std::ops::Range;
use std::ptr::eq;
use std::slice::Iter;

use crate::{
    Document, Name, NameData, StringData,
    error::{ErrorKind, Result},
};

impl<'input> Document<'input> {
    pub fn root<'doc>(&'doc self) -> Node<'doc, 'input> {
        self.node(NodeId::new(0).unwrap()).unwrap()
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
    pub(crate) data: &'doc NodeData<'input>,
    pub(crate) doc: &'doc Document<'input>,
}

impl PartialEq for Node<'_, '_> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id && eq(self.doc, other.doc)
    }
}

impl Eq for Node<'_, '_> {}

impl<'doc, 'input> Node<'doc, 'input> {
    pub fn id(self) -> NodeId {
        self.id
    }

    pub fn is_root(self) -> bool {
        matches!(self.data.kind, NodeKind::Root)
    }

    pub fn is_element(self) -> bool {
        matches!(self.data.kind, NodeKind::Element { .. })
    }

    pub fn is_text(self) -> bool {
        matches!(self.data.kind, NodeKind::Text { .. })
    }

    fn other(self, id: NodeId) -> Self {
        self.doc.node(id).unwrap()
    }

    fn iter<F>(self, f: F) -> impl Iterator<Item = Self>
    where
        F: Fn(Self) -> Option<Self>,
    {
        let mut current = Some(self);

        from_fn(move || {
            let next = f(current?);
            current = next;
            next
        })
    }

    pub fn parent(self) -> Option<Self> {
        self.data.parent.map(|id| self.other(id))
    }

    pub fn ancestors(self) -> impl Iterator<Item = Self> {
        self.iter(Node::parent)
    }

    pub fn prev_sibling(self) -> Option<Self> {
        self.data.prev_sibling.map(|id| self.other(id))
    }

    pub fn prev_siblings(self) -> impl Iterator<Item = Self> {
        self.iter(Node::prev_sibling)
    }

    pub fn prev_sibling_element(self) -> Option<Self> {
        self.prev_siblings().find(|node| node.is_element())
    }

    pub fn next_sibling(self) -> Option<Self> {
        self.data
            .next_subtree
            .filter(|id| self.doc.nodes[id.get()].prev_sibling.unwrap() == self.id)
            .map(|id| self.other(id))
    }

    pub fn next_siblings(self) -> impl Iterator<Item = Self> {
        self.iter(Node::next_sibling)
    }

    pub fn next_sibling_element(self) -> Option<Self> {
        self.next_siblings().find(|node| node.is_element())
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

    pub fn child_elements(self) -> impl DoubleEndedIterator<Item = Self> {
        self.children().filter(|node| node.is_element())
    }

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
        match &self.data.kind {
            NodeKind::Element { name, .. } => Some(name.get(self.doc)),
            _ => None,
        }
    }

    pub fn text(self) -> Option<&'doc str> {
        match &self.data.kind {
            NodeKind::Text(text) => Some(text.as_ref()),
            _ => None,
        }
    }

    pub fn child_texts(self) -> impl Iterator<Item = &'doc str> {
        self.children().filter_map(|node| node.text())
    }

    pub fn descedant_texts(self) -> impl Iterator<Item = &'doc str> {
        self.descendants().filter_map(|node| node.text())
    }

    pub fn preceeding_texts(self) -> impl Iterator<Item = &'doc str> {
        self.prev_siblings().filter_map(|node| node.text())
    }

    pub fn following_texts(self) -> impl Iterator<Item = &'doc str> {
        self.next_siblings().filter_map(|node| node.text())
    }
}

#[derive(Debug, Clone)]
pub struct NodeData<'input> {
    pub kind: NodeKind<'input>,
    pub parent: Option<NodeId>,
    pub prev_sibling: Option<NodeId>,
    pub next_subtree: Option<NodeId>,
    pub last_child: Option<NodeId>,
}

#[derive(Debug, Clone)]
pub enum NodeKind<'input> {
    Root,
    Element {
        name: NameData<'input>,
        attributes: Range<u32>,
    },
    Text(StringData<'input>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NodeId(NonZeroU32);

impl NodeId {
    pub fn new(id: usize) -> Result<Self> {
        if id >= u32::MAX as usize {
            return ErrorKind::TooManyNodes.into();
        }

        Ok(Self(NonZeroU32::new(id as u32 + 1).unwrap()))
    }

    pub fn get(self) -> usize {
        self.0.get() as usize - 1
    }

    pub fn next(self) -> Self {
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
    nodes: Enumerate<Iter<'doc, NodeData<'input>>>,
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
