use std::mem::take;

use crate::{
    error::{ErrorKind, Result},
    nodes::NodeId,
};

pub(crate) struct Strings<'input> {
    text: &'input str,
    buf: Box<str>,
    vals: Box<[(u32, u32)]>,
}

impl<'input> Strings<'input> {
    #[allow(unsafe_code)]
    pub(crate) fn get(&self, id: NodeId) -> &str {
        // SAFETY: All entries in `vals` are valid ranges into
        // `text` or `buf`, depending on their tag.
        unsafe { get(self.vals[id.get()], self.text, &self.buf) }
    }
}

pub(crate) struct StringsBuilder<'input> {
    text: &'input str,
    buf: String,
    vals: Vec<(u32, u32)>,
}

impl<'input> StringsBuilder<'input> {
    pub(crate) fn new(text: &'input str, cap: usize) -> Result<Self> {
        if text.len() > u32::MAX as usize {
            return ErrorKind::TooManyStrings.into();
        }

        Ok(Self {
            text,
            buf: String::new(),
            vals: Vec::with_capacity(cap),
        })
    }

    pub(crate) fn borrowed(&mut self, val: &str) -> Result<NodeId> {
        if val.len() & TAG as usize != 0 {
            return ErrorKind::TooManyStrings.into();
        }

        let text_addr = self.text.as_ptr().addr();
        let val_addr = val.as_ptr().addr();

        assert!(val_addr >= text_addr && val_addr + val.len() <= text_addr + self.text.len());

        let pos = val_addr - text_addr;

        let id = NodeId::new(self.vals.len())?;

        self.vals.push((pos as u32, val.len() as u32));

        Ok(id)
    }

    pub(crate) fn owned(&mut self, val: &str) -> Result<NodeId> {
        let mut buf = StringBuf::new(self, val.len());
        buf.push_str(val);
        buf.finish()
    }

    pub(crate) fn pop(&mut self, id: NodeId) {
        debug_assert_eq!(id.get() + 1, self.vals.len());

        if let Some(val) = self.vals.pop()
            && val.1 & TAG != 0
        {
            let pos = val.0 as usize;

            self.buf.truncate(pos);
        }
    }

    #[allow(unsafe_code)]
    pub(crate) fn get(&self, id: NodeId) -> &str {
        // SAFETY: All entries in `vals` are valid ranges into
        // `text` or `buf`, depending on their tag.
        unsafe { get(self.vals[id.get()], self.text, &self.buf) }
    }

    pub(crate) fn take(&mut self) -> Self {
        let buf = take(&mut self.buf);
        let vals = take(&mut self.vals);

        Self {
            text: self.text,
            buf,
            vals,
        }
    }

    pub(crate) fn build(self) -> Strings<'input> {
        Strings {
            text: self.text,
            buf: self.buf.into_boxed_str(),
            vals: self.vals.into_boxed_slice(),
        }
    }
}

const TAG: u32 = 1 << (u32::BITS - 1);

#[allow(unsafe_code)]
unsafe fn get<'a>(val: (u32, u32), text: &'a str, buf: &'a str) -> &'a str {
    let pos = val.0 as usize;

    if val.1 & TAG == 0 {
        let len = val.1 as usize;

        // SAFETY: `val` is untagged and hence a valid range into `text`.
        unsafe { text.get_unchecked(pos..pos + len) }
    } else {
        let len = (val.1 & !TAG) as usize;

        // SAFETY: `val` is tagged and hence a valid range into `buf`.
        unsafe { buf.get_unchecked(pos..pos + len) }
    }
}

pub(crate) struct StringBuf<'doc, 'input> {
    strings: &'doc mut StringsBuilder<'input>,
    pos: usize,
}

impl<'doc, 'input> StringBuf<'doc, 'input> {
    pub(crate) fn new(strings: &'doc mut StringsBuilder<'input>, cap: usize) -> Self {
        strings.buf.reserve(cap);

        let pos = strings.buf.len();

        Self { strings, pos }
    }

    pub(crate) fn push(&mut self, char_: char) {
        self.strings.buf.push(char_);
    }

    pub(crate) fn push_str(&mut self, str_: &str) {
        self.strings.buf.push_str(str_);
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.strings.buf.len() == self.pos
    }

    pub(crate) fn finish(self) -> Result<NodeId> {
        if self.strings.buf.len() > u32::MAX as usize {
            return ErrorKind::TooManyStrings.into();
        }

        let len = self.strings.buf.len() - self.pos;

        if len & TAG as usize != 0 {
            return ErrorKind::TooManyStrings.into();
        }

        let id = NodeId::new(self.strings.vals.len())?;

        self.strings.vals.push((self.pos as u32, len as u32 | TAG));

        Ok(id)
    }
}

pub(crate) fn cmp_names(lhs: &str, rhs: &str) -> bool {
    let len = lhs.len();
    if len != rhs.len() {
        return false;
    }

    let lhs = lhs.as_bytes();
    let rhs = rhs.as_bytes();

    if len >= 8 {
        let lhs_lo = u64::from_le_bytes(lhs[0..8].try_into().unwrap());
        let rhs_lo = u64::from_le_bytes(rhs[0..8].try_into().unwrap());
        if lhs_lo != rhs_lo {
            return false;
        } else if len == 8 {
            return true;
        }

        let lhs_hi = u64::from_le_bytes(lhs[len - 8..].try_into().unwrap());
        let rhs_hi = u64::from_le_bytes(rhs[len - 8..].try_into().unwrap());
        if lhs_hi != rhs_hi {
            return false;
        } else if len <= 16 {
            return true;
        }
    } else if len >= 4 {
        let lhs_lo = u32::from_le_bytes(lhs[0..4].try_into().unwrap());
        let rhs_lo = u32::from_le_bytes(rhs[0..4].try_into().unwrap());
        if lhs_lo != rhs_lo {
            return false;
        }

        let lhs_hi = u32::from_le_bytes(lhs[len - 4..].try_into().unwrap());
        let rhs_hi = u32::from_le_bytes(rhs[len - 4..].try_into().unwrap());
        return lhs_hi == rhs_hi;
    } else if len > 0 {
        let lhs_lo = lhs[0];
        let rhs_lo = rhs[0];
        if lhs_lo != rhs_lo {
            return false;
        }

        let lhs_mid = lhs[len / 2];
        let rhs_mid = rhs[len / 2];
        if lhs_mid != rhs_mid {
            return false;
        }

        let lhs_hi = lhs[len - 1];
        let rhs_hi = rhs[len - 1];
        return lhs_hi == rhs_hi;
    } else {
        return true;
    }

    lhs == rhs
}

pub(crate) fn cmp_opt_names(lhs: Option<&str>, rhs: Option<&str>) -> bool {
    match (lhs, rhs) {
        (Some(lhs), Some(rhs)) => cmp_names(lhs, rhs),
        (Some(_), None) | (None, Some(_)) => false,
        (None, None) => true,
    }
}

pub(crate) fn cmp_uris(lhs: &str, rhs: &str) -> bool {
    let len = lhs.len();
    if len != rhs.len() {
        return false;
    }

    let lhs = lhs.as_bytes();
    let rhs = rhs.as_bytes();

    if len >= 8 {
        let lhs_last = u64::from_le_bytes(lhs[len - 8..].try_into().unwrap());
        let rhs_last = u64::from_le_bytes(rhs[len - 8..].try_into().unwrap());
        if lhs_last != rhs_last {
            return false;
        }
    }

    lhs == rhs
}

pub(crate) fn cmp_opt_uris(lhs: Option<&str>, rhs: Option<&str>) -> bool {
    match (lhs, rhs) {
        (Some(lhs), Some(rhs)) => cmp_uris(lhs, rhs),
        (Some(_), None) | (None, Some(_)) => false,
        (None, None) => true,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn cmp_names_works() {
        assert!(cmp_names("", ""));
        assert!(cmp_names("gmd", "gmd"));
        assert!(!cmp_names("gmd", "gmx"));
        assert!(cmp_names("geonet", "geonet"));
        assert!(!cmp_names("geonet", "isonet"));
        assert!(cmp_names("CharacterString", "CharacterString"));
        assert!(!cmp_names("CharacterString", "CharacterVector"));
        assert!(cmp_names("administrativeArea", "administrativeArea"));
        assert!(!cmp_names("administrativeArea", "administrativeZone"));
        assert!(!cmp_names("geonet", "geo"));
    }

    #[test]
    fn cmp_uris_works() {
        assert!(!cmp_uris("geo", "iso"));
        assert!(cmp_uris("geonet", "geonet"));
        assert!(cmp_uris(
            "http://www.isotc211.org/2005/gmd",
            "http://www.isotc211.org/2005/gmd"
        ));
        assert!(!cmp_uris(
            "http://www.isotc211.org/2005/gmd",
            "http://www.isotc211.org/2005/gmx"
        ));
        assert!(!cmp_uris("geonet", "geo"));
    }
}
