#![allow(unsafe_code)]

#[cfg(target_arch = "x86_64")]
use std::arch::x86_64::*;

#[inline]
pub(crate) fn split_first<const N: usize>(text: &str, bytes: [u8; N]) -> Option<(u8, &str)> {
    assert!(bytes.is_ascii());

    if let Some(&first) = text.as_bytes().first()
        && bytes.contains(&first)
    {
        // SAFETY: `first` is a ASCII character, hence followed by a character boundary.
        let rest = unsafe { text.get_unchecked(1..) };

        return Some((first, rest));
    }

    None
}

#[inline]
pub(crate) fn split_at(text: &str, delim: u8) -> (&str, &str) {
    assert!(delim.is_ascii());

    let pos = memchr(delim, text.as_bytes()).unwrap_or(text.len());

    // SAFETY: `delim` is an ASCII character, hence preceeded by character boundary.
    let before = unsafe { text.get_unchecked(..pos) };
    let after = unsafe { text.get_unchecked(pos..) };

    (before, after)
}

#[inline]
pub(crate) fn split_after(text: &str, delim: u8) -> Option<(&str, &str)> {
    assert!(delim.is_ascii());

    let pos = memchr(delim, text.as_bytes())?;

    // SAFETY: `delim` is an ASCII character, hence surrounded by character boundaries.
    let before = unsafe { text.get_unchecked(..pos) };
    let after = unsafe { text.get_unchecked(pos + 1..) };

    Some((before, after))
}

#[inline]
pub(crate) fn split_after_n<const N: usize>(
    mut text: &str,
    delim: [u8; N],
) -> Option<(&str, &str)> {
    assert!(delim.is_ascii());

    loop {
        let pos = memchr(delim[0], text.as_bytes())?;

        // SAFETY: `delim[0]` is an ASCII character, hence preceeded by a character boundary.
        let after = unsafe { text.get_unchecked(pos..) };

        if after.as_bytes().starts_with(&delim) {
            // SAFETY: All of `delim` are ASCII characters, hence surrounded by character boundaries.
            let before = unsafe { text.get_unchecked(..pos) };
            let after = unsafe { after.get_unchecked(N..) };

            return Some((before, after));
        } else {
            // SAFETY: `delim[0]` is a ASCII character, hence followed by a character boundary.
            text = unsafe { after.get_unchecked(1..) };
        }
    }
}

#[inline]
#[cfg(not(target_arch = "x86_64"))]
pub(crate) fn memchr(needle: u8, haystack: &[u8]) -> Option<usize> {
    haystack.iter().position(move |&byte| byte == needle)
}

#[inline]
#[cfg(target_arch = "x86_64")]
pub(crate) fn memchr(needle: u8, haystack: &[u8]) -> Option<usize> {
    memchr_impl(
        haystack,
        Unroll::<4>,
        move |byte| byte == needle,
        // SAFETY: Conditional compilation ensures that at least the `sse2` target feature is available
        unsafe {
            let needle = _mm_set1_epi8(needle as i8);

            move |chunk| _mm_cmpeq_epi8(chunk, needle)
        },
        // SAFETY: `memchr_impl` will call this code only if the `avx2` target feature is available.
        #[cfg(target_feature = "avx2")]
        unsafe {
            let needle = _mm256_set1_epi8(needle as i8);

            move |chunk| _mm256_cmpeq_epi8(chunk, needle)
        },
    )
}

#[inline]
#[cfg(not(target_arch = "x86_64"))]
pub(crate) fn memchr2(needle1: u8, needle2: u8, haystack: &[u8]) -> Option<usize> {
    haystack
        .iter()
        .position(move |&byte| byte == needle1 || byte == needle2)
}

#[inline]
#[cfg(target_arch = "x86_64")]
pub(crate) fn memchr2(needle1: u8, needle2: u8, haystack: &[u8]) -> Option<usize> {
    memchr_impl(
        haystack,
        Unroll::<2>,
        move |byte| byte == needle1 || byte == needle2,
        // SAFETY: Conditional compilation ensures that at least the `sse2` target feature is available
        unsafe {
            let needle1 = _mm_set1_epi8(needle1 as i8);
            let needle2 = _mm_set1_epi8(needle2 as i8);

            move |chunk| {
                let mask1 = _mm_cmpeq_epi8(chunk, needle1);
                let mask2 = _mm_cmpeq_epi8(chunk, needle2);

                _mm_or_si128(mask1, mask2)
            }
        },
        // SAFETY: `memchr_impl` will call this code only if the `avx2` target feature is available.
        #[cfg(target_feature = "avx2")]
        unsafe {
            let needle1 = _mm256_set1_epi8(needle1 as i8);
            let needle2 = _mm256_set1_epi8(needle2 as i8);

            move |chunk| {
                let mask1 = _mm256_cmpeq_epi8(chunk, needle1);
                let mask2 = _mm256_cmpeq_epi8(chunk, needle2);

                _mm256_or_si256(mask1, mask2)
            }
        },
    )
}

#[inline]
#[cfg(not(target_arch = "x86_64"))]
pub(crate) fn memchr3(needle1: u8, needle2: u8, needle3: u8, haystack: &[u8]) -> Option<usize> {
    haystack
        .iter()
        .position(move |&byte| byte == needle1 || byte == needle2 || byte == needle3)
}

#[inline]
#[cfg(target_arch = "x86_64")]
pub(crate) fn memchr3(needle1: u8, needle2: u8, needle3: u8, haystack: &[u8]) -> Option<usize> {
    memchr_impl(
        haystack,
        Unroll::<1>,
        move |byte| byte == needle1 || byte == needle2 || byte == needle3,
        // SAFETY: Conditional compilation ensures that at least the `sse2` target feature is available
        unsafe {
            let needle1 = _mm_set1_epi8(needle1 as i8);
            let needle2 = _mm_set1_epi8(needle2 as i8);
            let needle3 = _mm_set1_epi8(needle3 as i8);

            move |chunk| {
                let mask1 = _mm_cmpeq_epi8(chunk, needle1);
                let mask2 = _mm_cmpeq_epi8(chunk, needle2);
                let mask3 = _mm_cmpeq_epi8(chunk, needle3);

                _mm_or_si128(_mm_or_si128(mask1, mask2), mask3)
            }
        },
        // SAFETY: `memchr_impl` will call this code only if the `avx2` target feature is available.
        #[cfg(target_feature = "avx2")]
        unsafe {
            let needle1 = _mm256_set1_epi8(needle1 as i8);
            let needle2 = _mm256_set1_epi8(needle2 as i8);
            let needle3 = _mm256_set1_epi8(needle3 as i8);

            move |chunk| {
                let mask1 = _mm256_cmpeq_epi8(chunk, needle1);
                let mask2 = _mm256_cmpeq_epi8(chunk, needle2);
                let mask3 = _mm256_cmpeq_epi8(chunk, needle3);

                _mm256_or_si256(_mm256_or_si256(mask1, mask2), mask3)
            }
        },
    )
}

#[cfg(target_arch = "x86_64")]
#[cold]
#[inline(always)]
fn cold() {}

#[cfg(target_arch = "x86_64")]
struct Unroll<const N: usize>;

#[inline(always)]
#[cfg(all(target_arch = "x86_64", not(target_feature = "avx2")))]
fn memchr_impl<const N: usize, F, G>(haystack: &[u8], _: Unroll<N>, f: F, g: G) -> Option<usize>
where
    F: Fn(u8) -> bool,
    G: Fn(__m128i) -> __m128i + Copy,
{
    #[inline(always)]
    unsafe fn impl_unaligned<G>(haystack: *const u8, g: G) -> Option<usize>
    where
        G: Fn(__m128i) -> __m128i,
    {
        // SAFETY: `haystack` points to at least 16 bytes of valid data.
        let chunk = unsafe { _mm_loadu_si128(haystack as *const __m128i) };

        let mask = g(chunk);

        // SAFETY: Conditional compilation ensures that the `sse2` target feature is available.
        let mask = unsafe { _mm_movemask_epi8(mask) };
        if mask != 0 {
            Some(mask.trailing_zeros() as usize)
        } else {
            None
        }
    }

    fn impl_unrolled<const N: usize, G>(haystack: &[u8], g: G) -> Option<usize>
    where
        G: Fn(__m128i) -> __m128i,
    {
        // SAFETY: The representation of `__m128i` is equivalent to `[u8; 16]`
        // and `align_to` ensures sufficient alignment.
        let (_prefix, aligned, suffix) = unsafe { haystack.align_to::<__m128i>() };

        let rest = if N == 4 {
            let (chunks, rest) = aligned.as_chunks::<4>();

            for chunks @ [chunk0, chunk1, chunk2, chunk3] in chunks {
                let mask0 = g(*chunk0);
                let mask1 = g(*chunk1);
                let mask2 = g(*chunk2);
                let mask3 = g(*chunk3);

                // SAFETY: Conditional compilation ensures that the `sse2` target feature is available.
                unsafe {
                    let mask01 = _mm_or_si128(mask0, mask1);
                    let mask23 = _mm_or_si128(mask2, mask3);
                    let mask = _mm_or_si128(mask01, mask23);

                    if _mm_movemask_epi8(mask) != 0 {
                        cold();

                        let mut pos = chunks.as_ptr().addr() - haystack.as_ptr().addr();

                        let mask0 = _mm_movemask_epi8(mask0);
                        if mask0 != 0 {
                            return Some(pos + mask0.trailing_zeros() as usize);
                        } else {
                            pos += 16;
                        }

                        let mask1 = _mm_movemask_epi8(mask1);
                        if mask1 != 0 {
                            return Some(pos + mask1.trailing_zeros() as usize);
                        } else {
                            pos += 16;
                        }

                        let mask2 = _mm_movemask_epi8(mask2);
                        if mask2 != 0 {
                            return Some(pos + mask2.trailing_zeros() as usize);
                        } else {
                            pos += 16;
                        }

                        let mask3 = _mm_movemask_epi8(mask3);
                        return Some(pos + mask3.trailing_zeros() as usize);
                    }
                }
            }

            rest
        } else if N == 2 {
            let (chunks, rest) = aligned.as_chunks::<2>();

            for chunks @ [chunk0, chunk1] in chunks {
                let mask0 = g(*chunk0);
                let mask1 = g(*chunk1);

                // SAFETY: Conditional compilation ensures that the `sse2` target feature is available.
                unsafe {
                    let mask = _mm_or_si128(mask0, mask1);

                    if _mm_movemask_epi8(mask) != 0 {
                        cold();

                        let mut pos = chunks.as_ptr().addr() - haystack.as_ptr().addr();

                        let mask0 = _mm_movemask_epi8(mask0);
                        if mask0 != 0 {
                            return Some(pos + mask0.trailing_zeros() as usize);
                        } else {
                            pos += 16;
                        }

                        let mask1 = _mm_movemask_epi8(mask1);
                        return Some(pos + mask1.trailing_zeros() as usize);
                    }
                }
            }

            rest
        } else if N == 1 {
            aligned
        } else {
            unimplemented!()
        };

        for chunk in rest {
            let mask = g(*chunk);

            // SAFETY: Conditional compilation ensures that the `avx2` target feature is available.
            unsafe {
                let mask = _mm_movemask_epi8(mask);
                if mask != 0 {
                    cold();

                    let pos = (chunk as *const __m128i).addr() - haystack.as_ptr().addr();

                    return Some(pos + mask.trailing_zeros() as usize);
                }
            }
        }

        if suffix.is_empty() {
            return None;
        }

        // SAFETY: `haystack` points to at least 16 bytes of valid data.
        unsafe {
            let pos = suffix.as_ptr().addr() - haystack.as_ptr().addr() + suffix.len() - 16;

            impl_unaligned(haystack.as_ptr().add(pos), g).map(|off| {
                cold();

                pos + off
            })
        }
    }

    if haystack.len() < 16 {
        return haystack.iter().position(|&byte| f(byte));
    }

    // SAFETY: `haystack` points to at least 16 bytes of valid data.
    if let Some(pos) = unsafe { impl_unaligned(haystack.as_ptr(), g) } {
        return Some(pos);
    }

    impl_unrolled::<N, _>(haystack, g)
}

#[inline(always)]
#[cfg(all(target_arch = "x86_64", target_feature = "avx2"))]
fn memchr_impl<const N: usize, F, G, H>(
    haystack: &[u8],
    _: Unroll<N>,
    f: F,
    g: G,
    h: H,
) -> Option<usize>
where
    F: Fn(u8) -> bool,
    G: Fn(__m128i) -> __m128i,
    H: Fn(__m256i) -> __m256i + Copy,
{
    #[inline(always)]
    fn impl_sse2<G>(haystack: &[u8], g: G) -> Option<usize>
    where
        G: Fn(__m128i) -> __m128i,
    {
        let pos = haystack.len() - 16;

        // SAFETY: `haystack` has a length of at least 16 and at most 31 bytes.
        let chunk0 = unsafe { _mm_loadu_si128(haystack.as_ptr() as *const __m128i) };
        let chunk1 = unsafe { _mm_loadu_si128(haystack.as_ptr().add(pos) as *const __m128i) };

        let mask0 = g(chunk0);
        let mask1 = g(chunk1);

        // SAFETY: Conditional compilation ensures that the `avx2` target feature is available.
        unsafe {
            let mask = _mm_or_si128(mask0, mask1);

            if _mm_testz_si128(mask, mask) == 0 {
                let mask0 = _mm_movemask_epi8(mask0);
                if mask0 != 0 {
                    return Some(mask0.trailing_zeros() as usize);
                }

                let mask1 = _mm_movemask_epi8(mask1);
                Some(pos + mask1.trailing_zeros() as usize)
            } else {
                None
            }
        }
    }

    #[inline(always)]
    fn impl_unaligned<H>(haystack: *const u8, h: H) -> Option<usize>
    where
        H: Fn(__m256i) -> __m256i,
    {
        // SAFETY: `haystack` points to at least 32 bytes of valid data.
        let chunk = unsafe { _mm256_loadu_si256(haystack as *const __m256i) };

        let mask = h(chunk);

        // SAFETY: Conditional compilation ensures that the `avx2` target feature is available.
        let mask = unsafe { _mm256_movemask_epi8(mask) };
        if mask != 0 {
            Some(mask.trailing_zeros() as usize)
        } else {
            None
        }
    }

    fn impl_unrolled<const N: usize, H>(haystack: &[u8], h: H) -> Option<usize>
    where
        H: Fn(__m256i) -> __m256i,
    {
        // SAFETY: The representation of `__m256i` is equivalent to `[u8; 32]`
        // and `align_to` ensures sufficient alignment.
        let (_prefix, aligned, suffix) = unsafe { haystack.align_to::<__m256i>() };

        let rest = if N == 4 {
            let (chunks, rest) = aligned.as_chunks::<4>();

            for chunks @ [chunk0, chunk1, chunk2, chunk3] in chunks {
                let mask0 = h(*chunk0);
                let mask1 = h(*chunk1);
                let mask2 = h(*chunk2);
                let mask3 = h(*chunk3);

                // SAFETY: Conditional compilation ensures that the `avx2` target feature is available.
                unsafe {
                    let mask01 = _mm256_or_si256(mask0, mask1);
                    let mask23 = _mm256_or_si256(mask2, mask3);
                    let mask = _mm256_or_si256(mask01, mask23);

                    if _mm256_testz_si256(mask, mask) == 0 {
                        cold();

                        let mut pos = chunks.as_ptr().addr() - haystack.as_ptr().addr();

                        let mask0 = _mm256_movemask_epi8(mask0);
                        if mask0 != 0 {
                            return Some(pos + mask0.trailing_zeros() as usize);
                        } else {
                            pos += 32;
                        }

                        let mask1 = _mm256_movemask_epi8(mask1);
                        if mask1 != 0 {
                            return Some(pos + mask1.trailing_zeros() as usize);
                        } else {
                            pos += 32;
                        }

                        let mask2 = _mm256_movemask_epi8(mask2);
                        if mask2 != 0 {
                            return Some(pos + mask2.trailing_zeros() as usize);
                        } else {
                            pos += 32;
                        }

                        let mask3 = _mm256_movemask_epi8(mask3);
                        return Some(pos + mask3.trailing_zeros() as usize);
                    }
                }
            }

            rest
        } else if N == 2 {
            let (chunks, rest) = aligned.as_chunks::<2>();

            for chunks @ [chunk0, chunk1] in chunks {
                let mask0 = h(*chunk0);
                let mask1 = h(*chunk1);

                // SAFETY: Conditional compilation ensures that the `avx2` target feature is available.
                unsafe {
                    let mask = _mm256_or_si256(mask0, mask1);

                    if _mm256_testz_si256(mask, mask) == 0 {
                        cold();

                        let mut pos = chunks.as_ptr().addr() - haystack.as_ptr().addr();

                        let mask0 = _mm256_movemask_epi8(mask0);
                        if mask0 != 0 {
                            return Some(pos + mask0.trailing_zeros() as usize);
                        } else {
                            pos += 32;
                        }

                        let mask1 = _mm256_movemask_epi8(mask1);
                        return Some(pos + mask1.trailing_zeros() as usize);
                    }
                }
            }

            rest
        } else if N == 1 {
            aligned
        } else {
            unimplemented!()
        };

        for chunk in rest {
            let mask = h(*chunk);

            // SAFETY: Conditional compilation ensures that the `avx2` target feature is available.
            unsafe {
                let mask = _mm256_movemask_epi8(mask);
                if mask != 0 {
                    cold();

                    let pos = (chunk as *const __m256i).addr() - haystack.as_ptr().addr();

                    return Some(pos + mask.trailing_zeros() as usize);
                }
            }
        }

        if suffix.is_empty() {
            return None;
        }

        // SAFETY: `haystack` points to at least 32 bytes of valid data.
        unsafe {
            let pos = suffix.as_ptr().addr() - haystack.as_ptr().addr() + suffix.len() - 32;

            impl_unaligned(haystack.as_ptr().add(pos), h).map(|off| {
                cold();

                pos + off
            })
        }
    }

    if haystack.len() < 32 {
        return if haystack.len() >= 16 {
            impl_sse2(haystack, g)
        } else {
            haystack.iter().position(|&byte| f(byte))
        };
    }

    if let Some(pos) = impl_unaligned(haystack.as_ptr(), h) {
        return Some(pos);
    }

    impl_unrolled::<N, _>(haystack, h)
}

pub(crate) fn memchr2_count(needle1: u8, needle2: u8, haystack: &[u8]) -> (usize, usize) {
    let mut count1 = 0;
    let mut count2 = 0;

    #[cfg(all(target_arch = "x86_64", target_feature = "avx2"))]
    memchr2_count_impl_avx2(needle1, needle2, haystack, &mut count1, &mut count2);

    #[cfg(all(target_arch = "x86_64", not(target_feature = "avx2")))]
    memchr2_count_impl_sse2(needle1, needle2, haystack, &mut count1, &mut count2);

    #[cfg(not(target_arch = "x86_64"))]
    memchr2_count_impl(needle1, needle2, haystack, &mut count1, &mut count2);

    (count1, count2)
}

#[inline]
fn memchr2_count_impl(
    needle1: u8,
    needle2: u8,
    haystack: &[u8],
    count1: &mut usize,
    count2: &mut usize,
) {
    for &byte in haystack {
        *count1 += (byte == needle1) as usize;
        *count2 += (byte == needle2) as usize;
    }
}

#[inline(always)]
#[cfg(all(target_arch = "x86_64", not(target_feature = "avx2")))]
fn memchr2_count_impl_sse2(
    needle1: u8,
    needle2: u8,
    haystack: &[u8],
    count1: &mut usize,
    count2: &mut usize,
) {
    // SAFETY: The representation of `__m128i` is equivalent to `[u8; 16]`
    // and `align_to` ensures sufficient alignment.
    let (prefix, aligned, suffix) = unsafe { haystack.align_to::<__m128i>() };

    memchr2_count_impl(needle1, needle2, prefix, count1, count2);

    // SAFETY: Conditional compilation ensures that the `sse2` target feature is available.
    unsafe {
        let needle1 = _mm_set1_epi8(needle1 as i8);
        let needle2 = _mm_set1_epi8(needle2 as i8);

        for &chunk in aligned {
            let hits1 = _mm_movemask_epi8(_mm_cmpeq_epi8(chunk, needle1));
            let hits2 = _mm_movemask_epi8(_mm_cmpeq_epi8(chunk, needle2));

            *count1 += hits1.count_ones() as usize;
            *count2 += hits2.count_ones() as usize;
        }
    }

    memchr2_count_impl(needle1, needle2, suffix, count1, count2);
}

#[inline(always)]
#[cfg(all(target_arch = "x86_64", target_feature = "avx2"))]
fn memchr2_count_impl_avx2(
    needle1: u8,
    needle2: u8,
    haystack: &[u8],
    count1: &mut usize,
    count2: &mut usize,
) {
    // SAFETY: The representation of `__m256i` is equivalent to `[u8; 32]`
    // and `align_to` ensures sufficient alignment.
    let (prefix, aligned, suffix) = unsafe { haystack.align_to::<__m256i>() };

    memchr2_count_impl(needle1, needle2, prefix, count1, count2);

    // SAFETY: Conditional compilation ensures that the `avx2` target feature is available.
    unsafe {
        let needle1 = _mm256_set1_epi8(needle1 as i8);
        let needle2 = _mm256_set1_epi8(needle2 as i8);

        for &chunk in aligned {
            let hits1 = _mm256_movemask_epi8(_mm256_cmpeq_epi8(chunk, needle1));
            let hits2 = _mm256_movemask_epi8(_mm256_cmpeq_epi8(chunk, needle2));

            *count1 += hits1.count_ones() as usize;
            *count2 += hits2.count_ones() as usize;
        }
    }

    memchr2_count_impl(needle1, needle2, suffix, count1, count2);
}

#[cfg(all(test, not(miri)))]
mod tests {
    use super::*;

    use std::iter::from_fn;

    use proptest::{collection::vec, num::u8::ANY as ANY_BYTE, test_runner::TestRunner};

    fn iter<F>(haystack: &[u8], f: F) -> impl Iterator<Item = usize>
    where
        F: Fn(&[u8]) -> Option<usize>,
    {
        let mut pos = 0;

        from_fn(move || {
            let pos1 = pos + f(&haystack[pos..])?;
            pos = pos1 + 1;
            Some(pos1)
        })
    }

    #[test]
    fn memchr_works() {
        TestRunner::default()
            .run(&(ANY_BYTE, vec(ANY_BYTE, ..=200)), |(needle, haystack)| {
                let pos1 = haystack
                    .iter()
                    .enumerate()
                    .filter(|&(_, &byte)| byte == needle)
                    .map(|(pos, _)| pos)
                    .collect::<Vec<_>>();

                let pos2 = iter(&haystack, |haystack| memchr(needle, haystack)).collect::<Vec<_>>();

                assert_eq!(pos1, pos2);
                Ok(())
            })
            .unwrap();
    }

    #[test]
    fn memchr2_works() {
        TestRunner::default()
            .run(
                &(ANY_BYTE, ANY_BYTE, vec(ANY_BYTE, ..=200)),
                |(needle1, needle2, haystack)| {
                    let pos1 = haystack
                        .iter()
                        .enumerate()
                        .filter(|&(_, &byte)| byte == needle1 || byte == needle2)
                        .map(|(pos, _)| pos)
                        .collect::<Vec<_>>();

                    let pos2 = iter(&haystack, |haystack| memchr2(needle1, needle2, haystack))
                        .collect::<Vec<_>>();

                    assert_eq!(pos1, pos2);
                    Ok(())
                },
            )
            .unwrap();
    }

    #[test]
    fn memchr3_works() {
        TestRunner::default()
            .run(
                &(ANY_BYTE, ANY_BYTE, ANY_BYTE, vec(ANY_BYTE, ..=200)),
                |(needle1, needle2, needle3, haystack)| {
                    let pos1 = haystack
                        .iter()
                        .enumerate()
                        .filter(|&(_, &byte)| byte == needle1 || byte == needle2 || byte == needle3)
                        .map(|(pos, _)| pos)
                        .collect::<Vec<_>>();

                    let pos2 = iter(&haystack, |haystack| {
                        memchr3(needle1, needle2, needle3, haystack)
                    })
                    .collect::<Vec<_>>();

                    assert_eq!(pos1, pos2);
                    Ok(())
                },
            )
            .unwrap();
    }

    #[test]
    fn memchr2_count_works() {
        TestRunner::default()
            .run(
                &(ANY_BYTE, ANY_BYTE, vec(ANY_BYTE, ..=200)),
                |(needle1, needle2, haystack)| {
                    let count11 = haystack.iter().filter(|&&byte| byte == needle1).count();
                    let count21 = haystack.iter().filter(|&&byte| byte == needle2).count();

                    let (count12, count22) = memchr2_count(needle1, needle2, &haystack);

                    assert_eq!(count11, count12);
                    assert_eq!(count21, count22,);
                    Ok(())
                },
            )
            .unwrap();
    }
}
