#![deny(
    unsafe_code,
    missing_debug_implementations,
    missing_copy_implementations
)]

mod attributes;
mod error;
mod namespaces;
mod nodes;
mod parser;
#[cfg(feature = "serde")]
pub mod serde;
mod strings;
mod tokenizer;

use std::fmt;
use std::num::NonZeroUsize;

use attributes::AttributeData;
use namespaces::{Namespace, Namespaces, NamespacesBuilder};
use nodes::{ElementData, NodeData};
use strings::StringData;

pub use attributes::{Attribute, Attributes};
pub use error::{Error, ErrorKind};
pub use nodes::{Children, Descendants, Node, NodeId};

#[derive(Clone)]
pub struct Document<'input> {
    nodes: Box<[NodeData]>,
    elements: Box<[ElementData<'input>]>,
    texts: Box<[StringData<'input>]>,
    attributes: Box<[AttributeData<'input>]>,
    namespaces: Namespaces<'input>,
}

impl Document<'_> {
    pub fn len(&self) -> NonZeroUsize {
        NonZeroUsize::new(self.nodes.len()).unwrap()
    }
}

struct DocumentBuilder<'input> {
    nodes: Vec<NodeData>,
    elements: Vec<ElementData<'input>>,
    texts: Vec<StringData<'input>>,
    attributes: Vec<AttributeData<'input>>,
    namespaces: NamespacesBuilder<'input>,
}

impl<'input> DocumentBuilder<'input> {
    fn build(self) -> Document<'input> {
        Document {
            nodes: self.nodes.into_boxed_slice(),
            elements: self.elements.into_boxed_slice(),
            texts: self.texts.into_boxed_slice(),
            attributes: self.attributes.into_boxed_slice(),
            namespaces: self.namespaces.build(),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Name<'doc, 'input> {
    pub namespace: Option<&'doc str>,
    pub local: &'input str,
}

/// ```
/// # use robinson::Document;
/// let doc = Document::parse(r#"<foo xmlns="http://bar"/>"#).unwrap();
///
/// let root_name = doc.root_element().name().unwrap();
///
/// assert_eq!(root_name.namespace, Some("http://bar"));
/// assert_eq!(root_name.local, "foo");
///
/// assert_eq!(root_name, "foo");
/// ```
impl PartialEq<&str> for Name<'_, '_> {
    fn eq(&self, other: &&str) -> bool {
        self.local == *other
    }
}

#[derive(Debug, Clone, Copy)]
struct NameData<'input> {
    namespace: Option<Namespace>,
    local: &'input str,
}

const _SIZE_OF_NAME_DATA: () =
    assert!(size_of::<NameData<'static>>() == (1 + 2) * size_of::<usize>());

impl<'input> NameData<'input> {
    fn get<'doc>(self, doc: &'doc Document) -> Name<'doc, 'input> {
        let namespace = self
            .namespace
            .map(|namespace| doc.namespaces.get(namespace));

        Name {
            namespace,
            local: self.local,
        }
    }
}

impl fmt::Debug for Document<'_> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.debug_struct("Document")
            .field("root", &self.root())
            .finish()
    }
}

impl fmt::Debug for Name<'_, '_> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.namespace {
            Some(namespace) => write!(fmt, "\"{{{}}}{}\"", namespace, self.local),
            None => write!(fmt, "\"{}\"", self.local),
        }
    }
}
