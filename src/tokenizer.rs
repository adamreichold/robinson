use std::mem::swap;

use memchr::memmem::Finder;

use crate::{
    error::{Error, ErrorKind, Result},
    parser::Parser,
    strings::{One, split_first, split_once},
};

pub(crate) struct Tokenizer<'input> {
    text: &'input str,
    init_text: &'input str,
    element_depth: u16,
    lt: One,
    colon: One,
    semicolon: One,
    quote: One,
    doublequote: One,
    pi: Finder<'static>,
    comment: Finder<'static>,
    cdata: Finder<'static>,
}

#[derive(Clone, Copy)]
pub(crate) enum Reference<'input> {
    Char(char),
    Entity(&'input str),
}

impl<'input> Tokenizer<'input> {
    pub(crate) fn new(text: &'input str) -> Self {
        Self {
            text,
            init_text: text,
            element_depth: 0,
            lt: One::new(b'<'),
            colon: One::new(b':'),
            semicolon: One::new(b';'),
            quote: One::new(b'\''),
            doublequote: One::new(b'"'),
            pi: Finder::new(b"?>"),
            comment: Finder::new(b"-->"),
            cdata: Finder::new(b"]]>"),
        }
    }

    pub(crate) fn element_depth(&self) -> u16 {
        self.element_depth
    }

    pub(crate) fn with_text<F, T>(&mut self, text: &mut &'input str, f: F) -> Result<T>
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
        let mut found = false;

        for init_text in self.init_text.lines() {
            let init_pos = init_text.as_ptr().addr();

            if pos <= init_pos + init_text.len() {
                pos -= init_pos;
                found = true;
                break;
            } else {
                line += 1;
            }
        }

        if !found {
            pos = 0;
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
        let mut space = false;

        while let Some((_space, rest)) = split_first(self.text, [b' ', b'\t', b'\r', b'\n']) {
            space = true;
            self.text = rest;
        }

        space
    }

    fn expect_space(&mut self) -> Result {
        if !self.try_space() {
            return ErrorKind::ExpectedSpace.into();
        }

        Ok(())
    }

    fn expect_quote(&mut self) -> Result<u8> {
        match split_first(self.text, [b'"', b'\'']) {
            Some((quote, rest)) => {
                self.text = rest;
                Ok(quote)
            }
            _ => ErrorKind::ExpectedQuote.into(),
        }
    }

    pub(crate) fn parse(&mut self, parser: &mut Parser<'input>) -> Result {
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

            if self.element_depth != 0 {
                self.parse_content(parser)?;

                if self.element_depth != 0 {
                    return ErrorKind::UnclosedRootElement.into();
                }
            }
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
        if prefix.is_some() || local != "version" {
            return ErrorKind::ExpectedLiteral("version").into();
        }

        self.try_space();
        if self.try_literal("?>") {
            return Ok(());
        }

        let (prefix, local, _value) = self.parse_attribute()?;
        if prefix.is_some() || local != "encoding" {
            return ErrorKind::ExpectedLiteral("encoding").into();
        }

        self.try_space();
        if self.try_literal("?>") {
            return Ok(());
        }

        let (prefix, local, _value) = self.parse_attribute()?;
        if prefix.is_some() || local != "standalone" {
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

        if self.try_space() {
            self.parse_external_reference()?;
        }

        Ok(())
    }

    fn parse_entity_declaration(&mut self, parser: &mut Parser<'input>) -> Result {
        self.expect_space()?;

        let name = self.parse_name()?;
        self.expect_space()?;

        let value = if let Some((_pub_id, _uri)) = self.parse_external_reference()? {
            None
        } else {
            let value = self.parse_quoted()?;

            Some(value)
        };

        self.try_space();
        self.expect_literal(">")?;

        if let Some(value) = value {
            parser.push_entity(name, value);
        }

        Ok(())
    }

    fn parse_external_reference(&mut self) -> Result<Option<(Option<&'input str>, &'input str)>> {
        if self.try_literal("PUBLIC") {
            self.expect_space()?;
            let pub_id = self.parse_quoted()?;

            self.expect_space()?;
            let uri = self.parse_quoted()?;

            Ok(Some((Some(pub_id), uri)))
        } else if self.try_literal("SYSTEM") {
            self.expect_space()?;
            let uri = self.parse_quoted()?;

            Ok(Some((None, uri)))
        } else {
            Ok(None)
        }
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

    pub(crate) fn parse_content(&mut self, parser: &mut Parser<'input>) -> Result {
        loop {
            if self.try_literal("<![CDATA[") {
                self.parse_cdata(parser)?;
            } else if self.try_literal("<!--") {
                self.parse_comment()?;
            } else if self.try_literal("<?") {
                self.parse_pi()?;
            } else if self.try_literal("</") {
                self.parse_close_element(parser)?;

                if self.element_depth == 0 {
                    return Ok(());
                }
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

        let pos = self
            .lt
            .find(self.text.as_bytes())
            .unwrap_or(self.text.len());

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
                Some(b'=' | b'/' | b'>' | b' ' | b'\t' | b'\r' | b'\n') => break,
                Some(_) => pos += 1,
                None => return ErrorKind::InvalidName.into(),
            }
        }

        let (name, rest) = self.text.split_at(pos);
        self.text = rest;

        if name.is_empty() {
            return ErrorKind::InvalidName.into();
        }

        Ok(name)
    }

    fn parse_qualname(&mut self) -> Result<(Option<&'input str>, &'input str)> {
        let qualname = self.parse_name()?;

        let (prefix, local) = match split_once(qualname, &self.colon) {
            Some((prefix, local)) => {
                if prefix.is_empty() || local.is_empty() {
                    return ErrorKind::InvalidName.into();
                }

                (Some(prefix), local)
            }
            None => (None, qualname),
        };

        Ok((prefix, local))
    }

    fn parse_quoted(&mut self) -> Result<&'input str> {
        let quote = self.expect_quote()?;

        let quote = if quote == b'"' {
            &self.doublequote
        } else {
            &self.quote
        };

        let Some((quoted, rest)) = split_once(self.text, quote) else {
            return ErrorKind::ExpectedQuote.into();
        };
        self.text = rest;

        Ok(quoted)
    }

    fn parse_element(&mut self, parser: &mut Parser<'input>) -> Result {
        let (prefix, local) = self.parse_qualname()?;

        parser.open_element(prefix, local)?;

        loop {
            let space = self.try_space();

            if self.try_literal("/>") {
                return parser.close_empty_element(self);
            } else if self.try_literal(">") {
                self.element_depth = match self.element_depth.checked_add(1) {
                    Some(element_depth) => element_depth,
                    None => return ErrorKind::TooManyNodes.into(),
                };

                return parser.close_open_element();
            } else {
                // An attribute must be preceded by whitespace.
                if !space {
                    return ErrorKind::ExpectedSpace.into();
                }

                let (prefix, local, value) = self.parse_attribute()?;

                parser.push_attribute(self, prefix, local, value)?;
            }
        }
    }

    fn parse_close_element(&mut self, parser: &mut Parser<'input>) -> Result {
        let (prefix, local) = self.parse_qualname()?;

        self.try_space();
        self.expect_literal(">")?;

        self.element_depth = match self.element_depth.checked_sub(1) {
            Some(element_depth) => element_depth,
            None => return ErrorKind::UnexpectedCloseElement.into(),
        };

        parser.close_element(self, prefix, local)
    }

    fn parse_attribute(&mut self) -> Result<(Option<&'input str>, &'input str, &'input str)> {
        let (prefix, local) = self.parse_qualname()?;

        self.try_space();
        self.expect_literal("=")?;
        self.try_space();

        let value = self.parse_quoted()?;

        Ok((prefix, local, value))
    }

    pub(crate) fn parse_reference(&mut self) -> Result<Reference<'input>> {
        let Some((value, rest)) = split_once(self.text, &self.semicolon) else {
            return ErrorKind::ExpectedLiteral(";").into();
        };
        self.text = rest;

        let char_ = if let Some(value) = value.strip_prefix("#x") {
            let Ok(code) = u32::from_str_radix(value, 16) else {
                return ErrorKind::InvalidReference(value.to_owned()).into();
            };

            char::from_u32(code).unwrap_or(char::REPLACEMENT_CHARACTER)
        } else if let Some(value) = value.strip_prefix("#") {
            let Ok(code) = value.parse() else {
                return ErrorKind::InvalidReference(value.to_owned()).into();
            };

            char::from_u32(code).unwrap_or(char::REPLACEMENT_CHARACTER)
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
