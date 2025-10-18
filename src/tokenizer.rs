#[cfg(all(
    target_arch = "x86_64",
    any(target_feature = "ssse3", target_feature = "avx2")
))]
use std::arch::x86_64::*;
use std::mem::swap;

#[cfg(all(
    target_arch = "x86_64",
    any(target_feature = "ssse3", target_feature = "avx2")
))]
use crate::memchr::Simd;
use crate::{
    error::{Error, ErrorKind, Result},
    memchr::{split_after, split_after_n, split_at, split_first},
    parser::Parser,
};

pub(crate) struct Tokenizer<'input> {
    text: &'input str,
    init_text: &'input str,
    element_depth: u16,
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
        let Some((_comment, rest)) = split_after_n(self.text, *b"-->") else {
            return ErrorKind::ExpectedLiteral("-->").into();
        };
        self.text = rest;

        Ok(())
    }

    fn parse_pi(&mut self) -> Result {
        let _name = self.parse_name()?;

        self.try_space();

        let Some((_pi, rest)) = split_after_n(self.text, *b"?>") else {
            return ErrorKind::ExpectedLiteral("?>").into();
        };
        self.text = rest;

        Ok(())
    }

    fn parse_text(&mut self, parser: &mut Parser<'input>) -> Result {
        debug_assert!(!self.text.is_empty());

        let (text, rest) = split_at(self.text, b'<');
        self.text = rest;

        parser.append_text(self, text)
    }

    fn parse_cdata(&mut self, parser: &mut Parser<'input>) -> Result {
        let Some((cdata, rest)) = split_after_n(self.text, *b"]]>") else {
            return ErrorKind::ExpectedLiteral("]]>").into();
        };
        self.text = rest;

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

        #[cfg(all(
            target_arch = "x86_64",
            all(target_feature = "ssse3", not(target_feature = "avx2"))
        ))]
        parse_qualname_impl_simd::<__m128i>(self.text, &mut pos, &mut prefix_pos)?;

        #[cfg(all(target_arch = "x86_64", target_feature = "avx2"))]
        parse_qualname_impl_simd::<__m256i>(self.text, &mut pos, &mut prefix_pos)?;

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

        let Some((quoted, rest)) = split_after(self.text, quote) else {
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
        let Some((value, rest)) = split_after(self.text, b';') else {
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

#[inline(always)]
#[allow(unsafe_code)]
#[cfg(all(
    target_arch = "x86_64",
    any(target_feature = "ssse3", target_feature = "avx2")
))]
fn parse_qualname_impl_simd<M>(
    text: &str,
    pos: &mut usize,
    prefix_pos: &mut Option<usize>,
) -> Result
where
    M: Simd,
{
    const BYTES: [u8; 8] = [b'=', b'/', b'>', b' ', b'\t', b'\r', b'\n', b':'];

    #[allow(dead_code)]
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

    assert!(M::BYTES <= 32);

    // SAFETY: The representation of `M: Simd` is equivalent
    // to `[u8; M::BYTES]` and `repr(align(..))` ensures alignment.
    let lo_bytes = unsafe { *(&LO_BYTES as *const AlignedTable as *const M) };
    let hi_bytes = unsafe { *(&HI_BYTES as *const AlignedTable as *const M) };

    for chunk in text.as_bytes().chunks_exact(M::BYTES) {
        // SAFETY: While unaligned, `chunks_exact` ensures `M::BYTES` of valid data.
        let chunk = unsafe { M::load_unaligned(chunk.as_ptr()) };

        let lo_chunk = chunk.and(M::splat(0xF));
        let lo_hits = lo_bytes.shuffle(lo_chunk);

        let hi_chunk = chunk.shift_right::<4>();
        let hi_hits = hi_bytes.shuffle(hi_chunk);

        let hits = lo_hits.and(hi_hits);

        let mask = hits.compare(M::splat(0));
        let cl_mask = hits.compare(M::splat(1 << 7));

        let mask = mask.xor(cl_mask);

        let mask = mask.movemask();
        let cl_mask = cl_mask.movemask();

        if mask != M::ALL {
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

        *pos += M::BYTES;
    }

    parse_qualname_impl(text, pos, prefix_pos)
}

#[cfg(all(test, not(miri)))]
#[cfg(all(
    target_arch = "x86_64",
    any(target_feature = "ssse3", target_feature = "avx2")
))]
mod tests {
    use super::*;

    use proptest::test_runner::TestRunner;

    #[test]
    #[cfg(all(
        target_arch = "x86_64",
        all(target_feature = "ssse3", not(target_feature = "avx2"))
    ))]
    fn parse_qualname_impl_ssse3_works() {
        TestRunner::default()
            .run(&".{0,200}", |text| {
                let mut pos1 = 0;
                let mut prefix_pos1 = None;
                let res1 = parse_qualname_impl(&text, &mut pos1, &mut prefix_pos1);

                let mut pos2 = 0;
                let mut prefix_pos2 = None;
                let res2 = parse_qualname_impl_simd::<__m128i>(&text, &mut pos2, &mut prefix_pos2);

                match (res1, res2) {
                    (Ok(()), Ok(())) => {
                        assert_eq!(pos1, pos2);
                        match (prefix_pos1, prefix_pos2) {
                            (Some(prefix_pos1), Some(prefix_pos2)) => {
                                assert_eq!(text.as_bytes()[prefix_pos1], b':');
                                assert_eq!(text.as_bytes()[prefix_pos2], b':');
                            }
                            (None, None) => (),
                            _ => panic!(),
                        }
                    }
                    (Err(_), Err(_)) => (),
                    _ => panic!(),
                }
                Ok(())
            })
            .unwrap();
    }

    #[test]
    #[cfg(all(target_arch = "x86_64", target_feature = "avx2"))]
    fn parse_qualname_impl_avx2_works() {
        TestRunner::default()
            .run(&".{0,200}", |text| {
                let mut pos1 = 0;
                let mut prefix_pos1 = None;
                let res1 = parse_qualname_impl(&text, &mut pos1, &mut prefix_pos1);

                let mut pos2 = 0;
                let mut prefix_pos2 = None;
                let res2 = parse_qualname_impl_simd::<__m256i>(&text, &mut pos2, &mut prefix_pos2);

                match (res1, res2) {
                    (Ok(()), Ok(())) => {
                        assert_eq!(pos1, pos2);
                        match (prefix_pos1, prefix_pos2) {
                            (Some(prefix_pos1), Some(prefix_pos2)) => {
                                assert_eq!(text.as_bytes()[prefix_pos1], b':');
                                assert_eq!(text.as_bytes()[prefix_pos2], b':');
                            }
                            (None, None) => (),
                            _ => panic!(),
                        }
                    }
                    (Err(_), Err(_)) => (),
                    _ => panic!(),
                }
                Ok(())
            })
            .unwrap();
    }
}
