#![allow(unsafe_code)]

use std::mem::take;

use memchr::memchr;

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
        assert_eq!(id.get() + 1, self.vals.len());

        let val = self.vals.pop().unwrap();

        if val.1 & TAG != 0 {
            let pos = val.0 as usize;

            self.buf.truncate(pos);
        }
    }

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

#[inline]
pub(crate) fn split_first<const N: usize>(str_: &str, bytes: [u8; N]) -> Option<(u8, &str)> {
    assert!(bytes.is_ascii());

    if let Some(&first) = str_.as_bytes().first()
        && bytes.contains(&first)
    {
        // SAFETY: `first` is a ASCII character hence followed by a character boundary.
        let rest = unsafe { str_.get_unchecked(1..) };

        return Some((first, rest));
    }

    None
}

#[inline]
pub(crate) fn split_once(str_: &str, delim: u8) -> Option<(&str, &str)> {
    assert!(delim.is_ascii());

    let pos = memchr(delim, str_.as_bytes())?;

    // SAFETY: `delim` is a ASCII character hence preceeded by a character boundary.
    let before = unsafe { str_.get_unchecked(..pos) };
    // SAFETY: `delim` is a ASCII character hence followed by a character boundary.
    let after = unsafe { str_.get_unchecked(pos + 1..) };

    Some((before, after))
}
