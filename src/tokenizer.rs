use std::mem::swap;

use memchr::{memchr, memmem::Finder};

use crate::{
    error::{Error, ErrorKind, Result},
    parser::Parser,
};

pub struct Tokenizer<'input> {
    text: &'input str,
    init_text: &'input str,
    pi: Finder<'static>,
    comment: Finder<'static>,
    cdata: Finder<'static>,
}

#[derive(Clone, Copy)]
pub enum ElementEnd<'input> {
    Empty,
    Close {
        prefix: &'input str,
        local: &'input str,
    },
    Open,
}

#[derive(Clone, Copy)]
pub enum Reference<'input> {
    Char(char),
    Entity(&'input str),
}

impl<'input> Tokenizer<'input> {
    pub fn new(text: &'input str) -> Self {
        Self {
            text,
            init_text: text,
            pi: Finder::new(b"?>"),
            comment: Finder::new(b"-->"),
            cdata: Finder::new(b"]]>"),
        }
    }

    pub fn with_text<F, T>(&mut self, text: &mut &'input str, f: F) -> Result<T>
    where
        F: FnOnce(&mut Self) -> Result<T>,
    {
        swap(&mut self.text, text);

        let res = f(self);

        if res.is_ok() {
            swap(&mut self.text, text);
        }

        res
    }

    #[cold]
    #[inline(never)]
    fn set_pos(&self, mut err: Box<Error>) -> Box<Error> {
        let mut line = 0;
        let mut pos = self.text.as_ptr().addr();

        for init_text in self.init_text.lines() {
            let init_pos = init_text.as_ptr().addr();

            if pos <= init_pos + init_text.len() {
                pos -= init_pos;
                break;
            } else {
                line += 1;
            }
        }

        err.pos = Some((line + 1, pos + 1));

        err
    }

    fn try_literal(&mut self, literal: &'static str) -> bool {
        match self.text.strip_prefix(literal) {
            Some(rest) => {
                self.text = rest;
                true
            }
            None => false,
        }
    }

    fn expect_literal(&mut self, literal: &'static str) -> Result {
        if !self.try_literal(literal) {
            return ErrorKind::ExpectedLiteral(literal).into();
        }

        Ok(())
    }

    fn try_space(&mut self) -> bool {
        let bytes = self.text.as_bytes();
        let mut pos = 0;

        while let Some(b' ' | b'\t' | b'\r' | b'\n') = bytes.get(pos) {
            pos += 1;
        }

        if pos != 0 {
            self.text = &self.text[pos..];
            true
        } else {
            false
        }
    }

    fn expect_space(&mut self) -> Result {
        if !self.try_space() {
            return ErrorKind::ExpectedSpace.into();
        }

        Ok(())
    }

    fn expect_quote(&mut self) -> Result<u8> {
        match self.text.as_bytes() {
            [quote @ b'"' | quote @ b'\'', ..] => {
                self.text = &self.text[1..];
                Ok(*quote)
            }
            _ => ErrorKind::ExpectedQuote.into(),
        }
    }

    pub fn parse(&mut self, parser: &mut Parser<'input>) -> Result {
        self.parse_document(parser).map_err(|err| self.set_pos(err))
    }

    fn parse_document(&mut self, parser: &mut Parser<'input>) -> Result {
        self.try_literal("\u{FEFF}");
        self.try_space();

        if self.try_literal("<?xml") {
            self.parse_declaration()?;
        }

        self.parse_miscellaneous()?;

        if self.try_literal("<!DOCTYPE") {
            self.parse_doctype(parser)?;
        }

        self.parse_miscellaneous()?;

        if self.try_literal("<") {
            self.parse_element(parser)?;
        }

        self.parse_miscellaneous()?;

        if !self.text.is_empty() {
            return ErrorKind::ExpectedEnd.into();
        }

        Ok(())
    }

    #[cold]
    fn parse_declaration(&mut self) -> Result {
        self.expect_space()?;

        let (prefix, local, _value) = self.parse_attribute()?;
        if !prefix.is_empty() || local != "version" {
            return ErrorKind::ExpectedLiteral("version").into();
        }

        self.try_space();
        if self.try_literal("?>") {
            return Ok(());
        }

        let (prefix, local, _value) = self.parse_attribute()?;
        if !prefix.is_empty() || local != "encoding" {
            return ErrorKind::ExpectedLiteral("encoding").into();
        }

        self.try_space();
        if self.try_literal("?>") {
            return Ok(());
        }

        let (prefix, local, _value) = self.parse_attribute()?;
        if !prefix.is_empty() || local != "standalone" {
            return ErrorKind::ExpectedLiteral("standalone").into();
        }

        self.try_space();
        self.expect_literal("?>")
    }

    #[cold]
    fn parse_doctype(&mut self, parser: &mut Parser<'input>) -> Result {
        self.parse_doctype_declaration()?;

        self.try_space();
        if self.try_literal(">") {
            return Ok(());
        }

        self.expect_literal("[")?;

        loop {
            self.try_space();

            if self.try_literal("<!ENTITY") {
                self.parse_entity_declaration(parser)?;
            } else if self.try_literal("<!--") {
                self.parse_comment()?;
            } else if self.try_literal("<?") {
                self.parse_pi()?;
            } else if self.try_literal("]") {
                self.try_space();
                self.expect_literal(">")?;

                return Ok(());
            } else {
                return ErrorKind::InvalidSyntax.into();
            }
        }
    }

    #[cold]
    fn parse_doctype_declaration(&mut self) -> Result {
        self.expect_space()?;

        let _name = self.parse_name()?;

        Ok(())
    }

    fn parse_entity_declaration(&mut self, parser: &mut Parser<'input>) -> Result {
        self.expect_space()?;

        let name = self.parse_name()?;
        self.expect_space()?;

        let quote = self.expect_quote()?;

        let Some(pos) = memchr(quote, self.text.as_bytes()) else {
            return ErrorKind::ExpectedQuote.into();
        };

        let (value, rest) = self.text.split_at(pos);
        self.text = &rest[1..];

        self.try_space();
        self.expect_literal(">")?;

        parser.push_entity(name, value);
        Ok(())
    }

    #[cold]
    fn parse_miscellaneous(&mut self) -> Result {
        loop {
            self.try_space();

            if self.try_literal("<!--") {
                self.parse_comment()?;
            } else if self.try_literal("<?") {
                self.parse_pi()?;
            } else {
                return Ok(());
            }
        }
    }

    pub fn parse_content(&mut self, parser: &mut Parser<'input>) -> Result {
        loop {
            if self.try_literal("<![CDATA[") {
                self.parse_cdata(parser)?;
            } else if self.try_literal("<!--") {
                self.parse_comment()?;
            } else if self.try_literal("<?") {
                self.parse_pi()?;
            } else if self.try_literal("</") {
                return self.parse_close_element(parser);
            } else if self.try_literal("<") {
                self.parse_element(parser)?;
            } else if self.text.is_empty() {
                return Ok(());
            } else {
                self.parse_text(parser)?;
            }
        }
    }

    fn parse_comment(&mut self) -> Result {
        let Some(pos) = self.comment.find(self.text.as_bytes()) else {
            return ErrorKind::ExpectedLiteral("-->").into();
        };

        self.text = &self.text[pos + 3..];

        Ok(())
    }

    fn parse_pi(&mut self) -> Result {
        let _name = self.parse_name()?;

        self.try_space();

        let Some(pos) = self.pi.find(self.text.as_bytes()) else {
            return ErrorKind::ExpectedLiteral("?>").into();
        };

        self.text = &self.text[pos + 2..];

        Ok(())
    }

    fn parse_text(&mut self, parser: &mut Parser<'input>) -> Result {
        debug_assert!(!self.text.is_empty());

        let pos = memchr(b'<', self.text.as_bytes()).unwrap_or(self.text.len());

        let (text, rest) = self.text.split_at(pos);
        self.text = rest;

        parser.append_text(self, text)
    }

    fn parse_cdata(&mut self, parser: &mut Parser<'input>) -> Result {
        let Some(pos) = self.cdata.find(self.text.as_bytes()) else {
            return ErrorKind::ExpectedLiteral("]]>").into();
        };

        let (cdata, rest) = self.text.split_at(pos);
        self.text = &rest[3..];

        parser.append_cdata(cdata)
    }

    fn parse_name(&mut self) -> Result<&'input str> {
        let mut pos = 0;

        loop {
            match self.text.as_bytes().get(pos) {
                Some(byte) if *byte < 128 => {
                    if CHECK_XML_NAME & (1 << byte) != 0 {
                        pos += 1;
                    } else {
                        break;
                    }
                }

                Some(_byte) => {
                    if check_xml_name(self.text, &mut pos) {
                        break;
                    }
                }

                None => break,
            }
        }

        let (name, rest) = self.text.split_at(pos);
        self.text = rest;

        if name.is_empty() {
            return ErrorKind::InvalidName.into();
        }

        Ok(name)
    }

    fn parse_qualname(&mut self) -> Result<(&'input str, &'input str)> {
        let mut prefix = None;

        let mut pos = 0;

        loop {
            match self.text.as_bytes().get(pos) {
                Some(b':') => {
                    if prefix.is_some() {
                        return ErrorKind::InvalidName.into();
                    }

                    prefix = Some(pos);

                    pos += 1;
                }

                Some(byte) if *byte < 128 => {
                    if CHECK_XML_NAME & (1 << byte) != 0 {
                        pos += 1;
                    } else {
                        break;
                    }
                }

                Some(_byte) => {
                    if check_xml_name(self.text, &mut pos) {
                        break;
                    }
                }

                None => break,
            }
        }

        let (qualname, rest) = self.text.split_at(pos);
        self.text = rest;

        let (prefix, local) = match prefix {
            Some(pos) => (&qualname[..pos], &qualname[pos + 1..]),
            None => ("", qualname),
        };

        if local.is_empty() {
            return ErrorKind::InvalidName.into();
        }

        Ok((prefix, local))
    }

    fn parse_element(&mut self, parser: &mut Parser<'input>) -> Result {
        let (prefix, local) = self.parse_qualname()?;

        parser.open_element(prefix, local)?;

        let open = loop {
            let space = self.try_space();

            if self.try_literal("/>") {
                parser.close_element(ElementEnd::Empty)?;

                break false;
            } else if self.try_literal(">") {
                parser.close_element(ElementEnd::Open)?;

                break true;
            } else {
                // An attribute must be preceded by whitespace.
                if !space {
                    return ErrorKind::ExpectedSpace.into();
                }

                let (prefix, local, value) = self.parse_attribute()?;

                parser.push_attribute(self, prefix, local, value)?;
            }
        };

        if open {
            self.parse_content(parser)?;
        }

        Ok(())
    }

    fn parse_close_element(&mut self, parser: &mut Parser<'input>) -> Result {
        let (prefix, local) = self.parse_qualname()?;

        self.try_space();
        self.expect_literal(">")?;

        parser.close_element(ElementEnd::Close { prefix, local })
    }

    fn parse_attribute(&mut self) -> Result<(&'input str, &'input str, &'input str)> {
        let (prefix, local) = self.parse_qualname()?;

        self.try_space();
        self.expect_literal("=")?;
        self.try_space();

        let quote = self.expect_quote()?;

        let Some(pos) = memchr(quote, self.text.as_bytes()) else {
            return ErrorKind::ExpectedQuote.into();
        };

        let (value, rest) = self.text.split_at(pos);
        self.text = &rest[1..];

        Ok((prefix, local, value))
    }

    pub fn parse_reference(&mut self) -> Result<Reference<'input>> {
        let Some(pos) = memchr(b';', self.text.as_bytes()) else {
            return ErrorKind::ExpectedLiteral(";").into();
        };

        let (value, rest) = self.text.split_at(pos);
        self.text = &rest[1..];

        let char_ = if let Some(value) = value.strip_prefix("#x") {
            let Ok(code) = u32::from_str_radix(value, 16) else {
                return ErrorKind::InvalidReference(value.to_owned()).into();
            };

            char::from_u32(code).unwrap_or('\u{FFFD}')
        } else if let Some(value) = value.strip_prefix("#") {
            let Ok(code) = value.parse() else {
                return ErrorKind::InvalidReference(value.to_owned()).into();
            };

            char::from_u32(code).unwrap_or('\u{FFFD}')
        } else {
            match value {
                "quot" => '"',
                "amp" => '&',
                "apos" => '\'',
                "lt" => '<',
                "gt" => '>',
                name => return Ok(Reference::Entity(name)),
            }
        };

        Ok(Reference::Char(char_))
    }
}

/*
import string

xml_name = set(string.ascii_letters + string.digits + "_-.:")

mask = 0

for idx in range(128):
    if chr(idx) in xml_name:
        mask |= 1 << idx

print(f"static CHECK_XML_NAME: u128 = 0b{mask:0128b};")
*/
static CHECK_XML_NAME: u128 = 0b00000111111111111111111111111110100001111111111111111111111111100000011111111111011000000000000000000000000000000000000000000000;

#[cold]
#[inline(never)]
fn check_xml_name(text: &str, pos: &mut usize) -> bool {
    let char_ = text[*pos..].chars().next().unwrap();

    if matches!(
        char_ as u32,
        0x0000B7
        | 0x0000C0..=0x0000D6
        | 0x0000D8..=0x0000F6
        | 0x0000F8..=0x0002FF
        | 0x000300..=0x00036F
        | 0x000370..=0x00037D
        | 0x00037F..=0x001FFF
        | 0x00200C..=0x00200D
        | 0x00203F..=0x002040
        | 0x002070..=0x00218F
        | 0x002C00..=0x002FEF
        | 0x003001..=0x00D7FF
        | 0x00F900..=0x00FDCF
        | 0x00FDF0..=0x00FFFD
        | 0x010000..=0x0EFFFF
    ) {
        *pos += char_.len_utf8();
        false
    } else {
        true
    }
}
