use std::fmt;
use std::slice::Iter;

use crate::{Document, Name, NameData, nodes::Node, strings::StringData};

impl<'doc, 'input> Node<'doc, 'input> {
    pub fn has_attributes(self) -> bool {
        self.element_data()
            .is_some_and(|element| !element.attributes.is_empty())
    }

    pub fn attributes(self) -> Attributes<'doc, 'input> {
        let data = self
            .element_data()
            .map(|element| {
                &self.doc.attributes
                    [element.attributes.start as usize..element.attributes.end as usize]
            })
            .unwrap_or_default();

        Attributes {
            data: data.iter(),
            doc: self.doc,
        }
    }

    /// ```
    /// # use robinson::Document;
    /// let doc = Document::parse(r#"<root foo="bar" baz="qux"/>"#).unwrap();
    ///
    /// let baz = doc.root_element().attribute("baz");
    ///
    /// assert_eq!(baz, Some("qux"));
    pub fn attribute(self, name: &str) -> Option<&'doc str> {
        self.attributes()
            .find(|attribute| attribute.name() == name)
            .map(|attribute| attribute.value())
    }
}

#[derive(Clone, Copy)]
pub struct Attribute<'doc, 'input> {
    data: &'doc AttributeData<'input>,
    doc: &'doc Document<'input>,
}

impl<'doc, 'input> Attribute<'doc, 'input> {
    pub fn document(self) -> &'doc Document<'input> {
        self.doc
    }

    pub fn name(self) -> Name<'doc, 'input> {
        self.data.name.get(self.doc)
    }

    pub fn value(self) -> &'doc str {
        self.data.value.as_ref()
    }
}

#[derive(Debug)]
pub(crate) struct AttributeData<'input> {
    pub(crate) name: NameData<'input>,
    pub(crate) value: StringData<'input>,
}

const _SIZE_OF_ATTRIBUTE_DATA: () =
    assert!(size_of::<AttributeData<'static>>() == (3 + 2) * size_of::<usize>());

#[derive(Clone)]
pub struct Attributes<'doc, 'input> {
    data: Iter<'doc, AttributeData<'input>>,
    doc: &'doc Document<'input>,
}

impl<'doc, 'input> Iterator for Attributes<'doc, 'input> {
    type Item = Attribute<'doc, 'input>;

    fn next(&mut self) -> Option<Self::Item> {
        self.data.next().map(|data| Attribute {
            data,
            doc: self.doc,
        })
    }

    fn nth(&mut self, n: usize) -> Option<Self::Item> {
        self.data.nth(n).map(|data| Attribute {
            data,
            doc: self.doc,
        })
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.data.size_hint()
    }
}

impl ExactSizeIterator for Attributes<'_, '_> {}

impl DoubleEndedIterator for Attributes<'_, '_> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.data.next_back().map(|data| Attribute {
            data,
            doc: self.doc,
        })
    }
}

impl fmt::Debug for Attribute<'_, '_> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.debug_struct("Attribute")
            .field("name", &self.name())
            .field("value", &self.value())
            .finish()
    }
}

impl fmt::Debug for Attributes<'_, '_> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut fmt = fmt.debug_map();

        for attribute in self.clone() {
            fmt.entry(&attribute.name(), &attribute.value());
        }

        fmt.finish()
    }
}
