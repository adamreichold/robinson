use std::ops::Range;

use crate::{
    error::{ErrorKind, Result},
    strings::StringData,
};

pub(crate) struct Namespaces<'input> {
    uris: Box<[StringData<'input>]>,
}

impl Namespaces<'_> {
    pub(crate) fn get(&self, namespace: Namespace) -> &str {
        self.uris[namespace.0 as usize].as_ref()
    }
}

#[derive(Default)]
pub(crate) struct NamespacesBuilder<'input> {
    data: Vec<NamespaceData<'input>>,
    sorted: Vec<Namespace>,
    parsed: Vec<Namespace>,
}

impl<'input> NamespacesBuilder<'input> {
    pub(crate) fn build(self) -> Namespaces<'input> {
        Namespaces {
            uris: self.data.into_iter().map(|data| data.uri).collect(),
        }
    }

    pub(crate) fn find(
        &self,
        range: &Range<u32>,
        prefix: Option<&str>,
    ) -> Result<Option<Namespace>> {
        let namespace = self.parsed[range.start as usize..range.end as usize]
            .iter()
            .find(|namespace| self.data[namespace.0 as usize].name == prefix);

        match namespace {
            Some(namespace) => Ok(Some(*namespace)),
            None => match prefix {
                None => Ok(None),
                Some(prefix) => ErrorKind::UnknownNamespace(prefix.to_owned()).into(),
            },
        }
    }

    pub(crate) fn len(&self) -> u32 {
        self.parsed.len() as u32
    }

    pub(crate) fn push(&mut self, data: NamespaceData<'input>) -> Result<Namespace> {
        debug_assert_ne!(data.name, Some(""));

        if self.parsed.len() > u32::MAX as usize {
            return ErrorKind::TooManyNamespaces.into();
        }

        let idx = self
            .sorted
            .binary_search_by(|namespace| self.data[namespace.0 as usize].cmp(&data));

        let namespace = match idx {
            Ok(idx) => self.sorted[idx],
            Err(idx) => {
                if self.data.len() > u16::MAX as usize {
                    return ErrorKind::TooManyNamespaces.into();
                }

                let namespace = Namespace(self.data.len() as u16);

                self.data.push(data);
                self.sorted.insert(idx, namespace);

                namespace
            }
        };

        self.parsed.push(namespace);

        Ok(namespace)
    }

    pub(crate) fn push_ref(&mut self, range: &Range<u32>, idx: u32) -> Result {
        let namespace = self.parsed[idx as usize];

        let name = &self.data[namespace.0 as usize].name;

        if self.parsed[range.start as usize..range.end as usize]
            .iter()
            .any(|namespace| &self.data[namespace.0 as usize].name == name)
        {
            return Ok(());
        }

        if self.parsed.len() > u32::MAX as usize {
            return ErrorKind::TooManyNamespaces.into();
        }

        self.parsed.push(namespace);

        Ok(())
    }
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct Namespace(u16);

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct NamespaceData<'input> {
    pub(crate) name: Option<&'input str>,
    pub(crate) uri: StringData<'input>,
}

const _SIZE_OF_NAMESPACE_DATA: () =
    assert!(size_of::<NamespaceData<'static>>() == (2 + 2) * size_of::<usize>());
