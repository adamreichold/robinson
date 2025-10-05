use std::num::NonZeroU16;

use crate::{
    error::{ErrorKind, Result},
    nodes::NodeId,
    strings::{Strings, StringsBuilder, cmp_opt_names, cmp_uris},
};

pub(crate) struct Namespaces {
    uris: Box<[NodeId]>,
}

impl Namespaces {
    pub(crate) fn get<'doc>(&self, namespace: Namespace, strings: &'doc Strings) -> &'doc str {
        strings.get(self.uris[namespace.get()])
    }
}

#[derive(Default)]
pub(crate) struct NamespacesBuilder<'input> {
    uris: Vec<NodeId>,
    data: Vec<NamespaceData<'input>>,
}

impl<'input> NamespacesBuilder<'input> {
    pub(crate) fn push(
        &mut self,
        strings: &mut StringsBuilder,
        element_depth: u16,
        prefix: Option<&'input str>,
        uri: NodeId,
    ) -> Result {
        debug_assert_ne!(prefix, Some(""));

        let idx = {
            let uri = strings.get(uri);

            self.uris
                .iter()
                .position(|uri1| cmp_uris(strings.get(*uri1), uri))
        };

        let namespace = match idx {
            Some(idx) => {
                strings.pop(uri);

                Namespace::new(idx)?
            }
            None => {
                let namespace = Namespace::new(self.uris.len())?;

                self.uris.push(uri);

                namespace
            }
        };

        self.data.push(NamespaceData {
            element_depth,
            prefix,
            namespace,
        });

        Ok(())
    }

    pub(crate) fn pop(&mut self, element_depth: u16) {
        let len = self
            .data
            .iter()
            .rposition(|data| data.element_depth < element_depth)
            .map_or(0, |idx| idx + 1);

        self.data.truncate(len);
    }

    pub(crate) fn find(&self, prefix: Option<&str>) -> Result<Option<Namespace>> {
        let namespace = self
            .data
            .iter()
            .rfind(|data| cmp_opt_names(data.prefix, prefix))
            .map(|data| data.namespace);

        match namespace {
            Some(namespace) => Ok(Some(namespace)),
            None => match prefix {
                None => Ok(None),
                Some(prefix) => ErrorKind::UnknownNamespace(prefix.to_owned()).into(),
            },
        }
    }

    pub(crate) fn build(self) -> Namespaces {
        Namespaces {
            uris: self.uris.into_boxed_slice(),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct Namespace(NonZeroU16);

impl Namespace {
    fn new(id: usize) -> Result<Self> {
        if id >= u16::MAX as usize {
            return ErrorKind::TooManyNamespaces.into();
        }

        Ok(Self(NonZeroU16::new(id as u16 + 1).unwrap()))
    }

    fn get(self) -> usize {
        self.0.get() as usize - 1
    }
}

#[derive(Clone, Copy)]
struct NamespaceData<'input> {
    element_depth: u16,
    prefix: Option<&'input str>,
    namespace: Namespace,
}

const _SIZE_OF_NAMESPACE_DATA: () =
    assert!(size_of::<NamespaceData<'static>>() == (2 + 1) * size_of::<usize>());
