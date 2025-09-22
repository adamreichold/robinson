#![allow(unsafe_code)]

use memchr::memchr;

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
