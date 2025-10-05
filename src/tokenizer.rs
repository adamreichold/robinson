use std::mem::swap;

use memchr::{memchr, memmem::Finder};

use crate::{
    error::{Error, ErrorKind, Result},
    parser::Parser,
    strings::{split_first, split_once},
};

pub(crate) struct Tokenizer<'input> {
    text: &'input str,
    init_text: &'input str,
    element_depth: u16,
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

    #[allow(unsafe_code)]
    fn parse_qualname(&mut self) -> Result<(Option<&'input str>, &'input str)> {
        let mut pos = 0;
        let mut prefix_pos = None;

        #[cfg(not(all(
            target_arch = "x86_64",
            any(target_feature = "ssse3", target_feature = "avx2")
        )))]
        parse_qualname_impl(self.text, &mut pos, &mut prefix_pos)?;

        // SAFETY: Conditional compilation ensures that the `ssse3` target feature is available.
        #[cfg(all(
            target_arch = "x86_64",
            all(target_feature = "ssse3", not(target_feature = "avx2"))
        ))]
        unsafe {
            parse_qualname_impl_ssse3(self.text, &mut pos, &mut prefix_pos)?;
        }

        // SAFETY: Conditional compilation ensures that the `avx2` target feature is available.
        #[cfg(all(target_arch = "x86_64", target_feature = "avx2"))]
        unsafe {
            parse_qualname_impl_avx2(self.text, &mut pos, &mut prefix_pos)?;
        }

        // SAFETY: `parse_qualname_impl*` guarantees an ASCII character at `pos`.
        let qualname = unsafe { self.text.get_unchecked(..pos) };
        self.text = unsafe { self.text.get_unchecked(pos..) };

        let (prefix, local) = match prefix_pos {
            Some(prefix_pos) => {
                // SAFETY: `parse_qualname_impl*` guarantees an ASCII character at `prefix_pos`.
                let prefix = unsafe { qualname.get_unchecked(..prefix_pos) };
                let local = unsafe { qualname.get_unchecked(prefix_pos + 1..) };

                if prefix.is_empty() {
                    return ErrorKind::InvalidName.into();
                }

                (Some(prefix), local)
            }
            None => (None, qualname),
        };

        if local.is_empty() {
            return ErrorKind::InvalidName.into();
        }

        Ok((prefix, local))
    }

    fn parse_quoted(&mut self) -> Result<&'input str> {
        let quote = self.expect_quote()?;

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
        let Some((value, rest)) = split_once(self.text, b';') else {
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

#[cfg_attr(
    all(
        target_arch = "x86_64",
        any(target_feature = "ssse3", target_feature = "avx2")
    ),
    cold
)]
#[cfg_attr(
    all(
        target_arch = "x86_64",
        any(target_feature = "ssse3", target_feature = "avx2")
    ),
    inline(never)
)]
#[cfg_attr(
    not(all(
        target_arch = "x86_64",
        any(target_feature = "ssse3", target_feature = "avx2")
    )),
    inline(always)
)]
fn parse_qualname_impl(text: &str, pos: &mut usize, prefix_pos: &mut Option<usize>) -> Result {
    loop {
        match text.as_bytes().get(*pos) {
            Some(b'=' | b'/' | b'>' | b' ' | b'\t' | b'\r' | b'\n') => return Ok(()),
            Some(b':') => {
                *prefix_pos = Some(*pos);

                *pos += 1;
            }
            Some(_) => *pos += 1,
            None => return ErrorKind::InvalidName.into(),
        }
    }
}

#[inline]
#[allow(unsafe_code)]
#[target_feature(enable = "ssse3")]
#[cfg(all(
    target_arch = "x86_64",
    all(target_feature = "ssse3", not(target_feature = "avx2"))
))]
unsafe fn parse_qualname_impl_ssse3(
    text: &str,
    pos: &mut usize,
    prefix_pos: &mut Option<usize>,
) -> Result {
    use std::arch::x86_64::*;

    const BYTES: [u8; 8] = [b'=', b'/', b'>', b' ', b'\t', b'\r', b'\n', b':'];

    #[repr(align(16))]
    struct AlignedTable([u8; 16]);

    static LO_BYTES: AlignedTable = {
        let mut lo_bytes = [0; 16];

        let mut idx = 0;
        while idx < BYTES.len() {
            let nibble = BYTES[idx] & 0xF;

            lo_bytes[nibble as usize] |= 1 << idx;

            idx += 1;
        }

        AlignedTable(lo_bytes)
    };

    static HI_BYTES: AlignedTable = {
        let mut hi_bytes = [0; 16];

        let mut idx = 0;
        while idx < BYTES.len() {
            let nibble = BYTES[idx] >> 4;

            hi_bytes[nibble as usize] |= 1 << idx;

            idx += 1;
        }

        AlignedTable(hi_bytes)
    };

    // SAFETY: These tables are initialized during program start
    // and sufficiently aligned via `repr(align(..))`.
    let lo_bytes = unsafe { _mm_load_si128(LO_BYTES.0.as_ptr() as *const __m128i) };
    let hi_bytes = unsafe { _mm_load_si128(HI_BYTES.0.as_ptr() as *const __m128i) };

    for chunk in text.as_bytes().chunks_exact(16) {
        // SAFETY: While unaligned, `chunks_exact` ensures 16 bytes of valid data.
        let chunk = unsafe { _mm_loadu_si128(chunk.as_ptr() as *const __m128i) };

        let lo_chunk = _mm_and_si128(chunk, _mm_set1_epi8(0xF));
        let lo_hits = _mm_shuffle_epi8(lo_bytes, lo_chunk);

        let hi_chunk = _mm_and_si128(_mm_srli_epi16(chunk, 4), _mm_set1_epi8(0xF));
        let hi_hits = _mm_shuffle_epi8(hi_bytes, hi_chunk);

        let hits = _mm_and_si128(lo_hits, hi_hits);

        let mask = _mm_cmpeq_epi8(hits, _mm_set1_epi8(0));
        let cl_mask = _mm_cmpeq_epi8(hits, _mm_set1_epi8(1 << 7));

        let mask = _mm_xor_si128(mask, cl_mask);

        let mask = _mm_movemask_epi8(mask) as u32;
        let cl_mask = _mm_movemask_epi8(cl_mask) as u32;

        if mask != 0xFF_FF {
            let off = mask.trailing_ones() as usize;

            if cl_mask != 0 {
                let cl_off = cl_mask.trailing_zeros() as usize;

                if cl_off < off {
                    *prefix_pos = Some(*pos + cl_off);
                }
            }

            *pos += off;

            return Ok(());
        }

        if cl_mask != 0 {
            *prefix_pos = Some(*pos + cl_mask.trailing_zeros() as usize);
        }

        *pos += 16;
    }

    parse_qualname_impl(text, pos, prefix_pos)
}

#[inline]
#[allow(unsafe_code)]
#[target_feature(enable = "avx2")]
#[cfg(all(target_arch = "x86_64", target_feature = "avx2"))]
unsafe fn parse_qualname_impl_avx2(
    text: &str,
    pos: &mut usize,
    prefix_pos: &mut Option<usize>,
) -> Result {
    use std::arch::x86_64::*;

    const BYTES: [u8; 8] = [b'=', b'/', b'>', b' ', b'\t', b'\r', b'\n', b':'];

    #[repr(align(32))]
    struct AlignedTable([u8; 32]);

    static LO_BYTES: AlignedTable = {
        let mut lo_bytes = [0; 32];

        let mut idx = 0;
        while idx < BYTES.len() {
            let nibble = BYTES[idx] & 0xF;

            lo_bytes[nibble as usize] |= 1 << idx;
            lo_bytes[16 + nibble as usize] |= 1 << idx;

            idx += 1;
        }

        AlignedTable(lo_bytes)
    };

    static HI_BYTES: AlignedTable = {
        let mut hi_bytes = [0; 32];

        let mut idx = 0;
        while idx < BYTES.len() {
            let nibble = BYTES[idx] >> 4;

            hi_bytes[nibble as usize] |= 1 << idx;
            hi_bytes[16 + nibble as usize] |= 1 << idx;

            idx += 1;
        }

        AlignedTable(hi_bytes)
    };

    // SAFETY: These tables are initialized during program start
    // and sufficiently aligned via `repr(align(..))`.
    let lo_bytes = unsafe { _mm256_load_si256(LO_BYTES.0.as_ptr() as *const __m256i) };
    let hi_bytes = unsafe { _mm256_load_si256(HI_BYTES.0.as_ptr() as *const __m256i) };

    for chunk in text.as_bytes().chunks_exact(32) {
        // SAFETY: While unaligned, `chunks_exact` ensures 32 bytes of valid data.
        let chunk = unsafe { _mm256_loadu_si256(chunk.as_ptr() as *const __m256i) };

        let lo_chunk = _mm256_and_si256(chunk, _mm256_set1_epi8(0xF));
        let lo_hits = _mm256_shuffle_epi8(lo_bytes, lo_chunk);

        let hi_chunk = _mm256_and_si256(_mm256_srli_epi16(chunk, 4), _mm256_set1_epi8(0xF));
        let hi_hits = _mm256_shuffle_epi8(hi_bytes, hi_chunk);

        let hits = _mm256_and_si256(lo_hits, hi_hits);

        let mask = _mm256_cmpeq_epi8(hits, _mm256_set1_epi8(0));
        let cl_mask = _mm256_cmpeq_epi8(hits, _mm256_set1_epi8(1 << 7));

        let mask = _mm256_xor_si256(mask, cl_mask);

        let mask = _mm256_movemask_epi8(mask) as u32;
        let cl_mask = _mm256_movemask_epi8(cl_mask) as u32;

        if mask != 0xFF_FF_FF_FF {
            let off = mask.trailing_ones() as usize;

            if cl_mask != 0 {
                let cl_off = cl_mask.trailing_zeros() as usize;

                if cl_off < off {
                    *prefix_pos = Some(*pos + cl_off);
                }
            }

            *pos += off;

            return Ok(());
        }

        if cl_mask != 0 {
            *prefix_pos = Some(*pos + cl_mask.trailing_zeros() as usize);
        }

        *pos += 32;
    }

    parse_qualname_impl(text, pos, prefix_pos)
}
