#![allow(unsafe_code)]

use std::cell::Cell;
use std::fmt;
use std::marker::PhantomData;
use std::mem::transmute;
use std::ops::Deref;
use std::ptr;

use serde::de;

use crate::{
    Node,
    serde::{Deserializer, Source},
};

/// ```
/// use serde::Deserialize;
/// use robinson::{serde::{from_doc, RawNode}, Document};
///
/// #[derive(Deserialize)]
/// struct Record<'a> {
///     #[serde(borrow)]
///     subtree: RawNode<'a>,
/// }
///
/// let document = Document::parse(r#"<document><subtree><field attribute="bar">foo</field></subtree></document>"#).unwrap();
///
/// let record = from_doc::<Record>(&document).unwrap();
///
/// assert_eq!(record.subtree.name().unwrap(), "subtree");
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RawNode<'a>(pub Node<'a, 'a>);

impl<'a> Deref for RawNode<'a> {
    type Target = Node<'a, 'a>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'de, 'a> de::Deserialize<'de> for RawNode<'a>
where
    'de: 'a,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: de::Deserializer<'de>,
    {
        struct Visitor<'a>(PhantomData<&'a ()>);

        impl<'de, 'a> de::Visitor<'de> for Visitor<'a>
        where
            'de: 'a,
        {
            type Value = RawNode<'a>;

            fn expecting(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
                fmt.write_str("struct RawNode")
            }

            fn visit_map<M>(self, _map: M) -> Result<Self::Value, M::Error>
            where
                M: de::MapAccess<'de>,
            {
                match CURR_NODE.get() {
                    // SAFETY: This is set only while `deserialize_struct` is active.
                    Some(curr_node) => Ok(RawNode(unsafe {
                        transmute::<Node<'static, 'static>, Node<'a, 'a>>(curr_node)
                    })),
                    None => Err(de::Error::custom("no current node")),
                }
            }
        }

        deserializer.deserialize_struct(RAW_NODE_NAME, &[], Visitor(PhantomData))
    }
}

pub(super) fn deserialize_struct<'de, 'input, 'temp, O, F, R>(
    this: Deserializer<'de, 'input, 'temp, O>,
    name: &'static str,
    f: F,
) -> R
where
    F: FnOnce(Deserializer<'de, 'input, 'temp, O>) -> R,
{
    let _reset_curr_node = match &this.source {
        Source::Node(node) if ptr::eq(name, RAW_NODE_NAME) => {
            let reset_curr_node = ResetCurrNode(CURR_NODE.get());

            // SAFETY: The guard will reset this before `deserialize_struct` returns.
            CURR_NODE.set(Some(unsafe {
                transmute::<Node<'de, 'input>, Node<'static, 'static>>(*node)
            }));

            Some(reset_curr_node)
        }
        _ => None,
    };

    f(this)
}

static RAW_NODE_NAME: &str = "RawNode";

thread_local! {
    static CURR_NODE: Cell<Option<Node<'static, 'static>>> = const { Cell::new(None) };
}

struct ResetCurrNode(Option<Node<'static, 'static>>);

impl Drop for ResetCurrNode {
    fn drop(&mut self) {
        CURR_NODE.set(self.0.take());
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use serde::Deserialize;

    use crate::{Document, serde::from_doc};

    #[test]
    fn raw_node_captures_subtree() {
        #[derive(Debug, Deserialize)]
        struct Root<'a> {
            #[serde(borrow)]
            foo: RawNode<'a>,
        }

        let doc = Document::parse(r#"<root><foo><bar qux="42">23</bar>baz</foo></root>"#).unwrap();
        let val = from_doc::<Root>(&doc).unwrap();

        assert!(val.foo.0.is_element());
        assert_eq!(val.foo.0.name().unwrap(), "foo");

        let children = val.foo.0.children().collect::<Vec<_>>();
        assert_eq!(children.len(), 2);
        assert!(children[0].is_element());
        assert_eq!(children[0].name().unwrap(), "bar");
        assert_eq!(children[0].attribute("qux").unwrap(), "42");
        assert!(children[1].is_text());
        assert_eq!(children[1].text().unwrap(), "baz");
    }
}
