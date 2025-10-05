#![deny(
    unsafe_code,
    missing_debug_implementations,
    missing_copy_implementations,
    unreachable_pub
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
use strings::{Strings, StringsBuilder, cmp_names, cmp_opt_uris};

pub use attributes::{Attribute, Attributes};
pub use error::{Error, ErrorKind};
pub use nodes::{Children, Descendants, Node, NodeId};

pub struct Document<'input> {
    nodes: Box<[NodeData]>,
    elements: Box<[ElementData<'input>]>,
    attributes: Box<[AttributeData<'input>]>,
    strings: Strings<'input>,
    namespaces: Namespaces,
}

const fn _is_send_and_sync<T>()
where
    T: Send + Sync,
{
}

const _DOCUMENT_IS_SEND_AND_SYNC: () = _is_send_and_sync::<Document>();

impl Document<'_> {
    pub fn len(&self) -> NonZeroUsize {
        NonZeroUsize::new(self.nodes.len()).unwrap()
    }
}

struct DocumentBuilder<'input> {
    nodes: Vec<NodeData>,
    elements: Vec<ElementData<'input>>,
    attributes: Vec<AttributeData<'input>>,
    strings: StringsBuilder<'input>,
    namespaces: NamespacesBuilder<'input>,
}

impl<'input> DocumentBuilder<'input> {
    fn build(self) -> Document<'input> {
        Document {
            nodes: self.nodes.into_boxed_slice(),
            elements: self.elements.into_boxed_slice(),
            attributes: self.attributes.into_boxed_slice(),
            strings: self.strings.build(),
            namespaces: self.namespaces.build(),
        }
    }
}

#[derive(Clone, Copy, Eq, PartialOrd, Ord, Hash)]
pub struct Name<'doc, 'input> {
    pub namespace: Option<&'doc str>,
    pub local: &'input str,
}

impl PartialEq for Name<'_, '_> {
    fn eq(&self, other: &Self) -> bool {
        cmp_opt_uris(self.namespace, other.namespace) && cmp_names(self.local, other.local)
    }
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
        cmp_names(self.local, other)
    }
}

#[derive(Clone, Copy)]
#[repr(Rust, packed)]
struct NameData<'input> {
    namespace: Option<Namespace>,
    local: &'input str,
}

const _SIZE_OF_NAME_DATA: () =
    assert!(size_of::<NameData<'static>>() == size_of::<u16>() + 2 * size_of::<usize>());

impl<'input> NameData<'input> {
    fn get<'doc>(self, doc: &'doc Document) -> Name<'doc, 'input> {
        let namespace = self
            .namespace
            .map(|namespace| doc.namespaces.get(namespace, &doc.strings));

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
