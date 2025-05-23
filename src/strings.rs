#![allow(unsafe_code)]

use std::borrow::Cow;
use std::cmp::Ordering;
use std::marker::PhantomData;
use std::mem::{ManuallyDrop, size_of};
use std::ptr::{NonNull, read_unaligned, slice_from_raw_parts_mut};

use memchr::memchr;

pub(crate) struct StringData<'input> {
    len: usize,
    ptr: NonNull<u8>,
    marker: PhantomData<Cow<'input, str>>,
}

unsafe impl Send for StringData<'_> {}

unsafe impl Sync for StringData<'_> {}

const _SIZE_OF_STRING_DATA: () =
    assert!(size_of::<StringData<'static>>() == 2 * size_of::<usize>());

const TAG: usize = 1 << (usize::BITS - 1);

impl<'input> StringData<'input> {
    pub(crate) fn borrowed(val: &'input str) -> Self {
        Self::from_raw_parts::<false>(val.len(), val.as_ptr() as *mut u8)
    }

    pub(crate) fn owned(val: Box<str>) -> Self {
        Self::from_raw_parts::<true>(val.len(), Box::into_raw(val) as *mut u8)
    }

    fn from_raw_parts<const OWNED: bool>(mut len: usize, ptr: *mut u8) -> Self {
        // This is implied by `len <= isize::MAX`, i.e. the maximum object and array size,
        // c.f. <https://doc.rust-lang.org/stable/reference/types/numeric.html#r-type.numeric.int.size.isize>
        // and <https://doc.rust-lang.org/1.68.0/reference/behavior-considered-undefined.html#dangling-pointers>.
        debug_assert!(len & TAG == 0);

        if OWNED {
            len |= TAG;
        }

        Self {
            len,
            // SAFETY: We deconstructed a (boxed) string slice which is always non-null.
            ptr: unsafe { NonNull::new_unchecked(ptr) },
            marker: PhantomData,
        }
    }

    fn is_owned(&self) -> bool {
        self.len & TAG != 0
    }

    fn slice(&self) -> *mut str {
        let ptr = self.ptr.as_ptr();
        let len = self.len & !TAG;

        slice_from_raw_parts_mut(ptr, len) as *mut str
    }

    pub(crate) unsafe fn packed_ref<P>(_packed: &P, this: *const Self) -> &str {
        // SAFETY: `this` is a potentially unaligned pointer derived from `_packed`
        // and hence valid for the same lifetime as `_packed`.
        let this = ManuallyDrop::new(unsafe { read_unaligned(this) });

        // SAFETY: `this` will not be dropped and hence the string slice
        // lifetime can be extended for the full lifetime of `_packed`.
        unsafe { &*this.slice() }
    }
}

macro_rules! packed_string_data_ref {
    ($packed:expr, $field:ident) => {{
        #[allow(unsafe_code)]
        // SAFETY: We fulfill the contract of `StringData::packed_ref`
        // by passing a pointer derived from `$packed` using `$field`.
        unsafe {
            $crate::StringData::packed_ref($packed, ::std::ptr::addr_of!($packed.$field))
        }
    }};
}

pub(crate) use packed_string_data_ref;

impl Drop for StringData<'_> {
    fn drop(&mut self) {
        if self.is_owned() {
            // SAFETY: Since the tag is set, we originally deconstructed a boxed string slice.
            let _ = unsafe { Box::from_raw(self.slice()) };
        }
    }
}

impl AsRef<str> for StringData<'_> {
    fn as_ref(&self) -> &str {
        // SAFETY: We originally deconstructed a (boxed) string slice,
        // i.e. a byte slice containing a UTF-8 representation of characters,
        // c.f. <https://doc.rust-lang.org/reference/type-layout.html#str-layout>.
        unsafe { &*self.slice() }
    }
}

impl PartialEq for StringData<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.as_ref().eq(other.as_ref())
    }
}

impl Eq for StringData<'_> {}

impl PartialOrd for StringData<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for StringData<'_> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.as_ref().cmp(other.as_ref())
    }
}

#[inline]
pub(crate) fn split_first<const N: usize>(str_: &str, bytes: [u8; N]) -> Option<(u8, &str)> {
    assert!(bytes.is_ascii());

    if let Some(&first) = str_.as_bytes().first() {
        if bytes.contains(&first) {
            // SAFETY: `first` is a ASCII character hence followed by a character boundary.
            let rest = unsafe { str_.get_unchecked(1..) };

            return Some((first, rest));
        }
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
