#![allow(unsafe_code)]

use std::borrow::Cow;
use std::cmp::Ordering;
use std::marker::PhantomData;
use std::mem::size_of;
use std::ptr::{NonNull, slice_from_raw_parts_mut};

#[derive(Debug)]
pub struct StringData<'input> {
    len: usize,
    ptr: NonNull<u8>,
    marker: PhantomData<Cow<'input, str>>,
}

const _SIZE_OF_STRING_DATA: () =
    assert!(size_of::<StringData<'static>>() == 2 * size_of::<usize>());

const TAG: usize = 1 << (usize::BITS - 1);

impl<'input> StringData<'input> {
    pub fn borrowed(val: &'input str) -> Self {
        Self::from_raw_parts::<false>(val.len(), val.as_ptr() as *mut u8)
    }

    pub fn owned(val: Box<str>) -> Self {
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
}

impl Drop for StringData<'_> {
    fn drop(&mut self) {
        if self.is_owned() {
            // SAFETY: Since the tag is set, we originally deconstructed a boxed string slice.
            let _ = unsafe { Box::from_raw(self.slice()) };
        }
    }
}

impl Clone for StringData<'_> {
    fn clone(&self) -> Self {
        if self.is_owned() {
            Self::owned(self.as_ref().into())
        } else {
            Self {
                len: self.len,
                ptr: self.ptr,
                marker: self.marker,
            }
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
