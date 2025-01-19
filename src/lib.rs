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
mod strings;
mod tokenizer;

use std::fmt;

use attributes::AttributeData;
use namespaces::{Namespace, Namespaces};
use nodes::NodeData;

pub use attributes::{Attribute, Attributes};
pub use error::{Error, ErrorKind};
pub use nodes::{Children, Descendants, Node, NodeId};

#[derive(Clone)]
pub struct Document<'input> {
    nodes: Vec<NodeData<'input>>,
    attributes: Vec<AttributeData<'input>>,
    namespaces: Namespaces<'input>,
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
