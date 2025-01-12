use std::ops::Range;

use crate::{
    StringData,
    error::{ErrorKind, Result},
};

#[derive(Clone, Default)]
pub struct Namespaces<'input> {
    data: Vec<NamespaceData<'input>>,
    sorted: Vec<Namespace>,
    parsed: Vec<Namespace>,
}

impl<'input> Namespaces<'input> {
    pub fn get(&self, namespace: Namespace) -> &str {
        self.data[namespace.0 as usize].uri.as_ref()
    }

    pub fn find(&self, range: Range<u32>, prefix: &str) -> Result<Option<Namespace>> {
        let prefix = if !prefix.is_empty() {
            Some(prefix)
        } else {
            None
        };

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

    pub fn len(&self) -> u32 {
        self.parsed.len() as u32
    }

    pub fn push(&mut self, data: NamespaceData<'input>) -> Result<Namespace> {
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

    pub fn push_ref(&mut self, offset: u32, idx: u32) -> Result {
        let namespace = self.parsed[idx as usize];

        let name = &self.data[namespace.0 as usize].name;

        if self.parsed[offset as usize..]
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

    pub fn shrink_to_fit(&mut self) {
        self.data.shrink_to_fit();
        self.sorted = Vec::new();
        self.parsed = Vec::new();
    }
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Namespace(u16);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct NamespaceData<'input> {
    pub name: Option<&'input str>,
    pub uri: StringData<'input>,
}
