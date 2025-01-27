//! ```
//! use serde::Deserialize;
//! use robinson::serde::from_str;
//!
//! #[derive(Deserialize)]
//! struct Record {
//!     field: String,
//! }
//!
//! let record = from_str::<Record>("<record><field>foobar</field></record>").unwrap();
//!
//! assert_eq!(record.field, "foobar");
//! ```
#[cfg(feature = "raw-node")]
mod raw_node;

use std::borrow::Cow;
use std::char::ParseCharError;
use std::error::Error as StdError;
use std::fmt;
use std::iter::{Peekable, once};
use std::marker::PhantomData;
use std::num::{NonZeroUsize, ParseFloatError, ParseIntError};
use std::str::{FromStr, ParseBoolError};

use bit_vec::BitVec;
use serde::de;

use crate::{Attribute, Document, Error as XmlError, Node};

#[cfg(feature = "raw-node")]
pub use raw_node::RawNode;

pub fn from_str<T>(text: &str) -> Result<T, Box<Error>>
where
    T: de::DeserializeOwned,
{
    defaults().from_str(text)
}

pub fn from_doc<'de, 'input, T>(document: &'de Document<'input>) -> Result<T, Box<Error>>
where
    T: de::Deserialize<'de>,
{
    defaults().from_doc(document)
}

pub fn from_node<'de, 'input, T>(node: Node<'de, 'input>) -> Result<T, Box<Error>>
where
    T: de::Deserialize<'de>,
{
    defaults().from_node(node)
}

pub trait Options: Sized {
    #[allow(clippy::wrong_self_convention)]
    fn from_str<T>(self, text: &str) -> Result<T, Box<Error>>
    where
        T: de::DeserializeOwned,
    {
        let document = Document::parse(text).map_err(Error::ParseXml)?;
        self.from_doc(&document)
    }

    #[allow(clippy::wrong_self_convention)]
    fn from_doc<'de, 'input, T>(self, document: &'de Document<'input>) -> Result<T, Box<Error>>
    where
        T: de::Deserialize<'de>,
    {
        let node = document.root_element();
        self.from_node(node)
    }

    #[allow(clippy::wrong_self_convention)]
    fn from_node<'de, 'input, T>(self, node: Node<'de, 'input>) -> Result<T, Box<Error>>
    where
        T: de::Deserialize<'de>,
    {
        let len = node.document().len();

        let deserializer = Deserializer {
            source: Source::Node(node),
            temp: &mut Temp::new(len),
            options: PhantomData::<Self>,
        };

        T::deserialize(deserializer)
    }

    fn namespaces(self) -> Namespaces<Self> {
        Namespaces(PhantomData)
    }

    fn prefix_attr(self) -> PrefixAttr<Self> {
        PrefixAttr(PhantomData)
    }

    fn only_children(self) -> OnlyChildren<Self> {
        OnlyChildren(PhantomData)
    }

    #[doc(hidden)]
    const NAMESPACES: bool = false;

    #[doc(hidden)]
    const PREFIX_ATTR: bool = false;

    #[doc(hidden)]
    const ONLY_CHILDREN: bool = false;
}

#[doc(hidden)]
#[derive(Clone, Copy, Default, Debug)]
pub struct Defaults;

pub fn defaults() -> Defaults {
    Defaults
}

impl Options for Defaults {}

#[doc(hidden)]
#[derive(Clone, Copy, Default, Debug)]
pub struct Namespaces<O>(PhantomData<O>);

impl<O> Options for Namespaces<O>
where
    O: Options,
{
    const NAMESPACES: bool = true;
    const PREFIX_ATTR: bool = O::PREFIX_ATTR;
    const ONLY_CHILDREN: bool = O::ONLY_CHILDREN;
}

#[doc(hidden)]
#[derive(Clone, Copy, Default, Debug)]
pub struct PrefixAttr<O>(PhantomData<O>);

impl<O> Options for PrefixAttr<O>
where
    O: Options,
{
    const NAMESPACES: bool = O::NAMESPACES;
    const PREFIX_ATTR: bool = true;
    const ONLY_CHILDREN: bool = O::ONLY_CHILDREN;
}

#[doc(hidden)]
#[derive(Clone, Copy, Default, Debug)]
pub struct OnlyChildren<O>(PhantomData<O>);

impl<O> Options for OnlyChildren<O>
where
    O: Options,
{
    const NAMESPACES: bool = O::NAMESPACES;
    const PREFIX_ATTR: bool = O::PREFIX_ATTR;
    const ONLY_CHILDREN: bool = true;
}

struct Deserializer<'de, 'input, 'temp, O> {
    source: Source<'de, 'input>,
    temp: &'temp mut Temp,
    options: PhantomData<O>,
}

#[derive(Debug, Clone, Copy)]
enum Source<'de, 'input> {
    Node(Node<'de, 'input>),
    Attribute(Attribute<'de, 'input>),
    Text(Node<'de, 'input>),
}

struct Temp {
    visited: BitVec,
    buffer: String,
}

impl Temp {
    fn new(len: NonZeroUsize) -> Self {
        Self {
            visited: BitVec::from_elem(len.get(), false),
            buffer: String::new(),
        }
    }
}

impl<'de, 'input, O> Deserializer<'de, 'input, '_, O>
where
    O: Options,
{
    fn name(&mut self) -> &str {
        match &self.source {
            Source::Node(node) => {
                let name = node.name().unwrap();

                match name.namespace {
                    Some(namespace) if O::NAMESPACES => {
                        let buffer = &mut self.temp.buffer;
                        buffer.clear();

                        buffer.reserve(namespace.len() + 2 + name.local.len());

                        buffer.push('{');
                        buffer.push_str(namespace);
                        buffer.push('}');

                        buffer.push_str(name.local);

                        &*buffer
                    }
                    _ => name.local,
                }
            }
            Source::Attribute(attr) => {
                let name = attr.name();

                match name.namespace {
                    Some(namespace) if O::NAMESPACES => {
                        let buffer = &mut self.temp.buffer;
                        buffer.clear();

                        if O::PREFIX_ATTR {
                            buffer.reserve(3 + namespace.len() + name.local.len());

                            buffer.push('@');
                        } else {
                            buffer.reserve(2 + namespace.len() + name.local.len());
                        }

                        buffer.push('{');
                        buffer.push_str(namespace);
                        buffer.push('}');

                        buffer.push_str(name.local);

                        &*buffer
                    }
                    _ => {
                        if O::PREFIX_ATTR {
                            let buffer = &mut self.temp.buffer;
                            buffer.clear();

                            buffer.reserve(1 + name.local.len());

                            buffer.push('@');
                            buffer.push_str(name.local);

                            &*buffer
                        } else {
                            name.local
                        }
                    }
                }
            }
            Source::Text(_) => "$text",
        }
    }

    fn node(&self) -> Result<Node<'de, 'input>, Box<Error>> {
        match self.source {
            Source::Node(node) => Ok(node),
            Source::Attribute(_) | Source::Text(_) => Error::MissingNode.into(),
        }
    }

    fn children(
        &self,
    ) -> Result<impl Iterator<Item = Source<'de, 'input>> + use<'de, 'input, O>, Box<Error>> {
        let node = self.node()?;

        let children = node
            .children()
            .filter(|node| node.is_element())
            .map(Source::Node);

        Ok(children)
    }

    fn children_and_attributes(
        &self,
    ) -> Result<impl Iterator<Item = Source<'de, 'input>> + use<'de, 'input, O>, Box<Error>> {
        let node = self.node()?;

        let children = node
            .children()
            .filter(|node| node.is_element())
            .map(Source::Node);

        let attributes = node.attributes().map(Source::Attribute);

        let text = once(Source::Text(node));

        Ok(children.chain(attributes).chain(text))
    }

    fn siblings(&self) -> Result<impl Iterator<Item = Node<'de, 'de>> + use<'de, O>, Box<Error>> {
        let node = self.node()?;
        let name = node.name();

        Ok(node.next_siblings().filter(move |node1| {
            let name1 = node1.name();

            if O::NAMESPACES {
                name == name1
            } else {
                name.map(|name| name.local) == name1.map(|name| name.local)
            }
        }))
    }

    fn text(&self) -> Cow<'de, str> {
        match self.source {
            Source::Node(node) => node.child_text().unwrap_or_default(),
            Source::Attribute(attr) => Cow::Borrowed(attr.value()),
            Source::Text(node) => node.child_text().unwrap_or_default(),
        }
    }

    fn parse<T>(&self, map_err: fn(T::Err) -> Error) -> Result<T, Box<Error>>
    where
        T: FromStr,
    {
        self.text()
            .trim()
            .parse()
            .map_err(|err| map_err(err).into())
    }
}

impl<'de, O> de::Deserializer<'de> for Deserializer<'de, '_, '_, O>
where
    O: Options,
{
    type Error = Box<Error>;

    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_bool(self.parse(Error::ParseBool)?)
    }

    fn deserialize_i8<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_i8(self.parse(Error::ParseInt)?)
    }

    fn deserialize_i16<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_i16(self.parse(Error::ParseInt)?)
    }

    fn deserialize_i32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_i32(self.parse(Error::ParseInt)?)
    }

    fn deserialize_i64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_i64(self.parse(Error::ParseInt)?)
    }

    fn deserialize_u8<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_u8(self.parse(Error::ParseInt)?)
    }

    fn deserialize_u16<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_u16(self.parse(Error::ParseInt)?)
    }

    fn deserialize_u32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_u32(self.parse(Error::ParseInt)?)
    }

    fn deserialize_u64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_u64(self.parse(Error::ParseInt)?)
    }

    fn deserialize_f32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_f32(self.parse(Error::ParseFloat)?)
    }

    fn deserialize_f64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_f64(self.parse(Error::ParseFloat)?)
    }

    fn deserialize_char<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_char(self.parse(Error::ParseChar)?)
    }

    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        match self.text() {
            Cow::Borrowed(text) => visitor.visit_borrowed_str(text),
            Cow::Owned(text) => visitor.visit_string(text),
        }
    }

    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_str(visitor)
    }

    fn deserialize_bytes<V>(self, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        Error::NotSupported.into()
    }

    fn deserialize_byte_buf<V>(self, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        Error::NotSupported.into()
    }

    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_some(self)
    }

    fn deserialize_unit<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_unit()
    }

    fn deserialize_unit_struct<V>(
        self,
        _name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_unit(visitor)
    }

    fn deserialize_newtype_struct<V>(
        self,
        _name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_newtype_struct(self)
    }

    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_seq(SeqAccess {
            source: self.siblings()?,
            temp: self.temp,
            options: PhantomData::<O>,
        })
    }

    fn deserialize_tuple<V>(self, _len: usize, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_seq(visitor)
    }

    fn deserialize_tuple_struct<V>(
        self,
        _name: &'static str,
        _len: usize,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_seq(visitor)
    }

    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        if O::ONLY_CHILDREN {
            visitor.visit_map(MapAccess {
                source: self.children()?.peekable(),
                temp: self.temp,
                options: PhantomData::<O>,
            })
        } else {
            visitor.visit_map(MapAccess {
                source: self.children_and_attributes()?.peekable(),
                temp: self.temp,
                options: PhantomData::<O>,
            })
        }
    }

    fn deserialize_struct<V>(
        self,
        #[allow(unused_variables)] name: &'static str,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        #[cfg(feature = "raw-node")]
        let res =
            raw_node::deserialize_struct(self, name, move |this| this.deserialize_map(visitor));

        #[cfg(not(feature = "raw-node"))]
        let res = self.deserialize_map(visitor);

        res
    }

    fn deserialize_enum<V>(
        self,
        _name: &'static str,
        _variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        if O::ONLY_CHILDREN {
            visitor.visit_enum(EnumAccess {
                source: self.children()?,
                temp: self.temp,
                options: PhantomData::<O>,
            })
        } else {
            visitor.visit_enum(EnumAccess {
                source: self.children_and_attributes()?,
                temp: self.temp,
                options: PhantomData::<O>,
            })
        }
    }

    fn deserialize_identifier<V>(mut self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_str(self.name())
    }

    fn deserialize_any<V>(self, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        Error::NotSupported.into()
    }

    fn deserialize_ignored_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_unit(visitor)
    }
}

struct SeqAccess<'de, 'temp, I, O>
where
    I: Iterator<Item = Node<'de, 'de>>,
{
    source: I,
    temp: &'temp mut Temp,
    options: PhantomData<O>,
}

impl<'de, I, O> de::SeqAccess<'de> for SeqAccess<'de, '_, I, O>
where
    I: Iterator<Item = Node<'de, 'de>>,
    O: Options,
{
    type Error = Box<Error>;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: de::DeserializeSeed<'de>,
    {
        match self.source.next() {
            None => Ok(None),
            Some(node) => {
                self.temp.visited.set(node.id().get(), true);

                let deserializer = Deserializer {
                    source: Source::Node(node),
                    temp: &mut *self.temp,
                    options: PhantomData::<O>,
                };
                seed.deserialize(deserializer).map(Some)
            }
        }
    }
}

struct MapAccess<'de, 'input: 'de, 'temp, I, O>
where
    I: Iterator<Item = Source<'de, 'input>>,
{
    source: Peekable<I>,
    temp: &'temp mut Temp,
    options: PhantomData<O>,
}

impl<'de, 'input, I, O> de::MapAccess<'de> for MapAccess<'de, 'input, '_, I, O>
where
    I: Iterator<Item = Source<'de, 'input>>,
    O: Options,
{
    type Error = Box<Error>;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Self::Error>
    where
        K: de::DeserializeSeed<'de>,
    {
        loop {
            match self.source.peek() {
                None => return Ok(None),
                Some(source) => {
                    if let Source::Node(node) = source {
                        if self.temp.visited[node.id().get()] {
                            self.source.next().unwrap();
                            continue;
                        }
                    }

                    let deserailizer = Deserializer {
                        source: *source,
                        temp: &mut *self.temp,
                        options: PhantomData::<O>,
                    };
                    return seed.deserialize(deserailizer).map(Some);
                }
            }
        }
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Self::Error>
    where
        V: de::DeserializeSeed<'de>,
    {
        let source = self.source.next().unwrap();

        let deserializer = Deserializer {
            source,
            temp: &mut *self.temp,
            options: PhantomData::<O>,
        };
        seed.deserialize(deserializer)
    }
}

struct EnumAccess<'de, 'input: 'de, 'temp, I, O>
where
    I: Iterator<Item = Source<'de, 'input>>,
{
    source: I,
    temp: &'temp mut Temp,
    options: PhantomData<O>,
}

impl<'de, 'input, 'temp, I, O> de::EnumAccess<'de> for EnumAccess<'de, 'input, 'temp, I, O>
where
    I: Iterator<Item = Source<'de, 'input>>,
    O: Options,
{
    type Error = Box<Error>;
    type Variant = Deserializer<'de, 'input, 'temp, O>;

    fn variant_seed<V>(mut self, seed: V) -> Result<(V::Value, Self::Variant), Self::Error>
    where
        V: de::DeserializeSeed<'de>,
    {
        let source = self.source.next().ok_or(Error::MissingChildOrAttribute)?;

        let deserializer = Deserializer {
            source,
            temp: &mut *self.temp,
            options: PhantomData::<O>,
        };
        let value = seed.deserialize(deserializer)?;

        let deserializer = Deserializer {
            source,
            temp: &mut *self.temp,
            options: PhantomData::<O>,
        };
        Ok((value, deserializer))
    }
}

impl<'de, O> de::VariantAccess<'de> for Deserializer<'de, '_, '_, O>
where
    O: Options,
{
    type Error = Box<Error>;

    fn unit_variant(self) -> Result<(), Self::Error> {
        Ok(())
    }

    fn newtype_variant_seed<T>(self, seed: T) -> Result<T::Value, Self::Error>
    where
        T: de::DeserializeSeed<'de>,
    {
        seed.deserialize(self)
    }

    fn tuple_variant<V>(self, len: usize, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        de::Deserializer::deserialize_tuple(self, len, visitor)
    }

    fn struct_variant<V>(
        self,
        fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        de::Deserializer::deserialize_struct(self, "", fields, visitor)
    }
}

#[derive(Debug)]
pub enum Error {
    MissingNode,
    MissingChildOrAttribute,
    ParseXml(Box<XmlError>),
    ParseBool(ParseBoolError),
    ParseInt(ParseIntError),
    ParseFloat(ParseFloatError),
    ParseChar(ParseCharError),
    NotSupported,
    Custom(String),
}

impl<T> From<Error> for Result<T, Box<Error>> {
    #[cold]
    #[inline(never)]
    fn from(err: Error) -> Self {
        Err(Box::new(err))
    }
}

impl de::Error for Box<Error> {
    #[cold]
    #[inline(never)]
    fn custom<T: fmt::Display>(msg: T) -> Self {
        Box::new(Error::Custom(msg.to_string()))
    }
}

impl fmt::Display for Error {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::MissingNode => write!(fmt, "missing node"),
            Self::MissingChildOrAttribute => write!(fmt, "missing child or attribute"),
            Self::ParseXml(err) => write!(fmt, "XML parse error: {err}"),
            Self::ParseBool(err) => write!(fmt, "bool parse error: {err}"),
            Self::ParseInt(err) => write!(fmt, "int parse error: {err}"),
            Self::ParseFloat(err) => write!(fmt, "float parse error: {err}"),
            Self::ParseChar(err) => write!(fmt, "char parse error: {err}"),
            Self::NotSupported => write!(fmt, "not supported"),
            Self::Custom(msg) => write!(fmt, "custom error: {msg}"),
        }
    }
}

impl StdError for Error {
    fn source(&self) -> Option<&(dyn StdError + 'static)> {
        match self {
            Self::ParseXml(err) => Some(err),
            Self::ParseBool(err) => Some(err),
            Self::ParseInt(err) => Some(err),
            Self::ParseFloat(err) => Some(err),
            Self::ParseChar(err) => Some(err),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use serde::Deserialize;

    #[test]
    fn parse_bool() {
        let val = from_str::<bool>("<root>false</root>").unwrap();
        assert!(!val);
        let val = from_str::<bool>("<root>\n\ttrue\n</root>").unwrap();
        assert!(val);

        let res = from_str::<bool>("<root>foobar</root>");
        assert!(matches!(*res.unwrap_err(), Error::ParseBool(_err)));
    }

    #[test]
    fn parse_char() {
        let val = from_str::<char>("<root>x</root>").unwrap();
        assert_eq!(val, 'x');
        let val = from_str::<char>("<root>\n\ty\n</root>").unwrap();
        assert_eq!(val, 'y');

        let res = from_str::<char>("<root>xyz</root>");
        assert!(matches!(*res.unwrap_err(), Error::ParseChar(_err)));
    }

    #[test]
    fn empty_text() {
        let val = from_str::<String>("<root></root>").unwrap();
        assert!(val.is_empty());
    }

    #[test]
    fn children_and_attributes() {
        #[derive(Deserialize)]
        struct Root {
            attr: i32,
            child: u64,
        }

        let val = from_str::<Root>(r#"<root attr="23"><child>42</child></root>"#).unwrap();
        assert_eq!(val.attr, 23);
        assert_eq!(val.child, 42);
    }

    #[test]
    fn children_with_attributes() {
        #[derive(Deserialize)]
        struct Root {
            child: Child,
        }

        #[derive(Deserialize)]
        struct Child {
            attr: i32,
            #[serde(rename = "$text")]
            text: u64,
        }

        let val = from_str::<Root>(r#"<root><child attr="23">42</child></root>"#).unwrap();
        assert_eq!(val.child.attr, 23);
        assert_eq!(val.child.text, 42);
    }

    #[test]
    fn multiple_children() {
        #[derive(Deserialize)]
        struct Root {
            child: Vec<i32>,
            another_child: String,
        }

        let val = from_str::<Root>(r#"<root><child>23</child><another_child>foobar</another_child><child>42</child></root>"#).unwrap();
        assert_eq!(val.child, [23, 42]);
        assert_eq!(val.another_child, "foobar");
    }

    #[test]
    fn multiple_lists_of_multiple_children() {
        #[derive(Deserialize)]
        struct Root {
            child: Vec<i32>,
            another_child: Vec<String>,
        }

        let val = from_str::<Root>(r#"<root><child>23</child><another_child>foo</another_child><child>42</child><another_child>bar</another_child></root>"#).unwrap();
        assert_eq!(val.child, [23, 42]);
        assert_eq!(val.another_child, ["foo", "bar"]);
    }

    #[test]
    fn zero_of_multiple_children() {
        #[derive(Deserialize)]
        struct Root {
            #[serde(default)]
            child: Vec<i32>,
        }

        let val = from_str::<Root>(r#"<root></root>"#).unwrap();
        assert_eq!(val.child, []);
    }

    #[test]
    fn optional_child() {
        #[derive(Deserialize)]
        struct Root {
            child: Option<f32>,
        }

        let val = from_str::<Root>(r#"<root><child>23.42</child></root>"#).unwrap();
        assert_eq!(val.child, Some(23.42));

        let val = from_str::<Root>(r#"<root></root>"#).unwrap();
        assert_eq!(val.child, None);
    }

    #[test]
    fn optional_attribute() {
        #[derive(Deserialize)]
        struct Root {
            attr: Option<f64>,
        }

        let val = from_str::<Root>(r#"<root attr="23.42"></root>"#).unwrap();
        assert_eq!(val.attr, Some(23.42));

        let val = from_str::<Root>(r#"<root></root>"#).unwrap();
        assert_eq!(val.attr, None);
    }

    #[test]
    fn child_variants() {
        #[derive(Debug, PartialEq, Deserialize)]
        enum Root {
            Foo(Foo),
            Bar(Bar),
        }

        #[derive(Debug, PartialEq, Deserialize)]
        struct Foo {
            attr: i64,
        }

        #[derive(Debug, PartialEq, Deserialize)]
        struct Bar {
            child: u32,
        }

        let val = from_str::<Root>(r#"<root><Foo attr="23" /></root>"#).unwrap();
        assert_eq!(val, Root::Foo(Foo { attr: 23 }));

        let val = from_str::<Root>(r#"<root><Bar><child>42</child></Bar></root>"#).unwrap();
        assert_eq!(val, Root::Bar(Bar { child: 42 }));
    }

    #[test]
    fn attribute_variants() {
        #[derive(Debug, PartialEq, Deserialize)]
        enum Root {
            Foo(u32),
            Bar(i64),
        }

        let val = from_str::<Root>(r#"<root Foo="23" />"#).unwrap();
        assert_eq!(val, Root::Foo(23));

        let val = from_str::<Root>(r#"<root Bar="42" />"#).unwrap();
        assert_eq!(val, Root::Bar(42));
    }

    #[test]
    fn borrowed_str() {
        let doc = Document::parse("<root><child>foobar</child></root>").unwrap();

        #[derive(Deserialize)]
        struct Root<'a> {
            child: &'a str,
        }

        let val = from_doc::<Root>(&doc).unwrap();
        assert_eq!(val.child, "foobar");
    }

    #[test]
    fn unit_struct() {
        #[derive(Deserialize)]
        #[allow(dead_code)]
        struct Root {
            child: Child,
        }

        #[derive(Deserialize)]
        struct Child;

        from_str::<Root>(r#"<root><child /></root>"#).unwrap();

        from_str::<Root>(r#"<root><child>foobar</child></root>"#).unwrap();
    }

    #[test]
    fn unit_variant() {
        #[derive(Debug, Deserialize)]
        enum Root {
            Child,
        }

        from_str::<Root>(r#"<root><Child /></root>"#).unwrap();

        from_str::<Root>(r#"<root><Child>foobar</Child></root>"#).unwrap();
    }

    #[test]
    fn children_with_namespaces() {
        #[derive(Deserialize)]
        struct Root {
            #[serde(rename = "{http://name.space}child")]
            child: u64,
        }

        let val = defaults()
            .namespaces()
            .from_str::<Root>(r#"<root xmlns="http://name.space"><child>42</child></root>"#)
            .unwrap();
        assert_eq!(val.child, 42);

        let val = defaults()
            .namespaces()
            .from_str::<Root>(r#"<root xmlns:namespace="http://name.space"><namespace:child>42</namespace:child></root>"#)
            .unwrap();
        assert_eq!(val.child, 42);
    }

    #[test]
    fn attributes_with_namespaces() {
        #[derive(Deserialize)]
        struct Root {
            #[serde(rename = "{http://name.space}attr")]
            attr: i32,
        }

        let val = defaults()
            .namespaces()
            .from_str::<Root>(
                r#"<root xmlns:namespace="http://name.space" namespace:attr="23"></root>"#,
            )
            .unwrap();
        assert_eq!(val.attr, 23);
    }

    #[test]
    fn prefixed_attributes() {
        #[derive(Deserialize)]
        struct Root {
            #[serde(rename = "@attr")]
            attr: i32,
        }

        let val = defaults()
            .prefix_attr()
            .from_str::<Root>(r#"<root attr="23"></root>"#)
            .unwrap();
        assert_eq!(val.attr, 23);
    }

    #[test]
    fn prefixed_attributes_with_namespaces() {
        #[derive(Deserialize)]
        struct Root {
            #[serde(rename = "@{http://name.space}attr")]
            attr: i32,
        }

        let val = defaults()
            .namespaces()
            .prefix_attr()
            .from_str::<Root>(
                r#"<root xmlns:namespace="http://name.space" namespace:attr="23"></root>"#,
            )
            .unwrap();
        assert_eq!(val.attr, 23);
    }

    #[test]
    fn only_children_skips_attributes() {
        #[derive(Deserialize)]
        struct Root {
            child: u64,
            attr: Option<i32>,
        }

        let val = defaults()
            .from_str::<Root>(r#"<root attr="23"><child>42</child></root>"#)
            .unwrap();
        assert_eq!(val.child, 42);
        assert_eq!(val.attr, Some(23));

        let val = defaults()
            .only_children()
            .from_str::<Root>(r#"<root attr="23"><child>42</child></root>"#)
            .unwrap();
        assert_eq!(val.child, 42);
        assert_eq!(val.attr, None);
    }

    #[test]
    fn only_children_skips_text() {
        #[derive(Deserialize)]
        struct Root {
            child: u64,
            #[serde(rename = "$text")]
            text: Option<String>,
        }

        let val = defaults()
            .from_str::<Root>(r#"<root>text<child>42</child></root>"#)
            .unwrap();
        assert_eq!(val.child, 42);
        assert_eq!(val.text.as_deref(), Some("text"));

        let val = defaults()
            .only_children()
            .from_str::<Root>(r#"<root>text<child>42</child></root>"#)
            .unwrap();
        assert_eq!(val.child, 42);
        assert_eq!(val.text.as_deref(), None);
    }

    #[test]
    fn repeated_namespaced_elements() {
        #[derive(Deserialize)]
        struct Root {
            #[serde(rename = "{http://foo}child")]
            foo: Vec<u64>,
            #[serde(rename = "{http://bar}child")]
            bar: Vec<u64>,
        }

        let val = defaults()
            .namespaces()
            .from_str::<Root>(
                r#"<root xmlns:foo="http://foo" xmlns:bar="http://bar">
    <foo:child>1</foo:child>
    <bar:child>2</bar:child>
    <bar:child>3</bar:child>
    <foo:child>4</foo:child>
</root>"#,
            )
            .unwrap();
        assert_eq!(val.foo, [1, 4]);
        assert_eq!(val.bar, [2, 3]);
    }
}
