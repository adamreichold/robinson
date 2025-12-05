#![allow(unsafe_code)]

#[cfg(simd_neon)]
use core::arch::aarch64::*;
#[cfg(target_arch = "x86_64")]
use std::arch::x86_64::*;

#[inline]
pub(crate) fn split_first<'a>(text: &'a str, bytes: &[u8]) -> Option<(u8, &'a str)> {
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
pub(crate) fn split_after_n<'a>(mut text: &'a str, delim: &[u8]) -> Option<(&'a str, &'a str)> {
    assert!(!delim.is_empty() && delim.is_ascii());

    loop {
        let pos = memchr(delim[0], text.as_bytes())?;

        // SAFETY: `delim[0]` is an ASCII character, hence preceeded by a character boundary.
        let after = unsafe { text.get_unchecked(pos..) };

        if after.as_bytes().starts_with(delim) {
            // SAFETY: All of `delim` are ASCII characters, hence surrounded by character boundaries.
            let before = unsafe { text.get_unchecked(..pos) };
            let after = unsafe { after.get_unchecked(delim.len()..) };

            return Some((before, after));
        } else {
            // SAFETY: `delim[0]` is a ASCII character, hence followed by a character boundary.
            text = unsafe { after.get_unchecked(1..) };
        }
    }
}

#[inline]
pub(crate) fn memchr(needle: u8, haystack: &[u8]) -> Option<usize> {
    #[derive(Clone, Copy)]
    struct One(u8);

    impl Needle for One {
        #[inline(always)]
        fn cmp_byte(self, byte: u8) -> bool {
            byte == self.0
        }

        #[inline(always)]
        fn cmp_chunk<M>(self, chunk: M) -> M
        where
            M: Simd,
        {
            chunk.compare(M::splat(self.0))
        }

        const UNROLL: usize = 4;
    }

    memchr_impl(haystack, One(needle))
}

#[inline]
pub(crate) fn memchr2(needle1: u8, needle2: u8, haystack: &[u8]) -> Option<usize> {
    #[derive(Clone, Copy)]
    struct Two(u8, u8);

    impl Needle for Two {
        #[inline(always)]
        fn cmp_byte(self, byte: u8) -> bool {
            byte == self.0 || byte == self.1
        }

        #[inline(always)]
        fn cmp_chunk<M>(self, chunk: M) -> M
        where
            M: Simd,
        {
            let mask0 = chunk.compare(M::splat(self.0));
            let mask1 = chunk.compare(M::splat(self.1));

            mask0.or(mask1)
        }

        const UNROLL: usize = 2;
    }

    memchr_impl(haystack, Two(needle1, needle2))
}

#[inline]
pub(crate) fn memchr4(
    needle1: u8,
    needle2: u8,
    needle3: u8,
    needle4: u8,
    haystack: &[u8],
) -> Option<usize> {
    #[derive(Clone, Copy)]
    struct Four(u8, u8, u8, u8);

    impl Needle for Four {
        #[inline(always)]
        fn cmp_byte(self, byte: u8) -> bool {
            byte == self.0 || byte == self.1 || byte == self.2 || byte == self.3
        }

        #[inline(always)]
        fn cmp_chunk<M>(self, chunk: M) -> M
        where
            M: Simd,
        {
            let mask0 = chunk.compare(M::splat(self.0));
            let mask1 = chunk.compare(M::splat(self.1));
            let mask2 = chunk.compare(M::splat(self.2));
            let mask3 = chunk.compare(M::splat(self.3));

            let mask01 = mask0.or(mask1);
            let mask23 = mask2.or(mask3);
            mask01.or(mask23)
        }

        const UNROLL: usize = 1;
    }

    memchr_impl(haystack, Four(needle1, needle2, needle3, needle4))
}

#[allow(dead_code)]
trait Needle: Copy {
    fn cmp_byte(self, byte: u8) -> bool;

    fn cmp_chunk<M>(self, chunk: M) -> M
    where
        M: Simd;

    const UNROLL: usize;
}

#[inline(always)]
#[cfg(not(any(simd_sse2, simd_avx2, simd_neon)))]
fn memchr_impl<N>(haystack: &[u8], n: N) -> Option<usize>
where
    N: Needle,
{
    haystack.iter().position(move |&byte| n.cmp_byte(byte))
}

#[inline(always)]
#[cfg(simd_sse2)]
fn memchr_impl<N>(haystack: &[u8], n: N) -> Option<usize>
where
    N: Needle,
{
    memchr_impl_16::<__m128i, N>(haystack, n)
}

#[inline(always)]
#[cfg(simd_avx2)]
fn memchr_impl<N>(haystack: &[u8], n: N) -> Option<usize>
where
    N: Needle,
{
    memchr_impl_32::<__m128i, __m256i, N>(haystack, n)
}

#[inline(always)]
#[cfg(simd_neon)]
fn memchr_impl<N>(haystack: &[u8], n: N) -> Option<usize>
where
    N: Needle,
{
    memchr_impl_16::<uint8x16_t, N>(haystack, n)
}

#[inline(always)]
#[cfg(any(simd_sse2, simd_neon))]
fn memchr_impl_16<M16, N>(haystack: &[u8], n: N) -> Option<usize>
where
    M16: Simd,
    N: Needle,
{
    const {
        assert!(M16::BYTES == 16);
    }

    if haystack.len() < 16 {
        return haystack.iter().position(move |&byte| n.cmp_byte(byte));
    }

    // SAFETY: `haystack` points to at least 16 bytes of valid data.
    if let Some(pos) = unsafe { memchr_impl_unaligned::<M16, N>(haystack.as_ptr(), n) } {
        return Some(pos);
    }

    memchr_impl_unrolled::<M16, N>(haystack, n)
}

#[inline(always)]
#[cfg(simd_avx2)]
fn memchr_impl_32<M16, M32, N>(haystack: &[u8], n: N) -> Option<usize>
where
    M16: Simd,
    M32: Simd,
    N: Needle,
{
    const {
        assert!(M16::BYTES == 16);
        assert!(M32::BYTES == 32);
    }

    if haystack.len() < 32 {
        return if haystack.len() >= 16 {
            memchr_impl_overlapped::<M16, N>(haystack, n)
        } else {
            haystack.iter().position(move |&byte| n.cmp_byte(byte))
        };
    }

    if let Some(pos) = unsafe { memchr_impl_unaligned::<M32, N>(haystack.as_ptr(), n) } {
        return Some(pos);
    }

    memchr_impl_unrolled::<M32, N>(haystack, n)
}

#[inline(always)]
#[cfg(any(simd_sse2, simd_avx2, simd_neon))]
unsafe fn memchr_impl_unaligned<M, N>(haystack: *const u8, n: N) -> Option<usize>
where
    M: Simd,
    N: Needle,
{
    // SAFETY: `haystack` points to at least `M::BYTES` bytes of valid data.
    let chunk = unsafe { M::load_unaligned(haystack) };

    let mask = n.cmp_chunk(chunk).movemask();

    if mask != M::NONE {
        Some(M::first_set(mask))
    } else {
        None
    }
}

#[inline(always)]
#[cfg(simd_avx2)]
fn memchr_impl_overlapped<M, N>(haystack: &[u8], n: N) -> Option<usize>
where
    M: Simd,
    N: Needle,
{
    let pos = haystack.len() - M::BYTES;

    // SAFETY: `haystack` has a length of at least `M::BYTES` and at most `2 * M::BYTES - 1` bytes.
    let chunk0 = unsafe { M::load_unaligned(haystack.as_ptr()) };
    let chunk1 = unsafe { M::load_unaligned(haystack.as_ptr().add(pos)) };

    let mask0 = n.cmp_chunk(chunk0);
    let mask1 = n.cmp_chunk(chunk1);

    let mask = mask0.or(mask1);

    if mask.any() {
        let mask0 = mask0.movemask();
        if mask0 != M::NONE {
            return Some(M::first_set(mask0));
        }

        let mask1 = mask1.movemask();
        Some(pos + M::first_set(mask1))
    } else {
        None
    }
}

#[cfg(any(simd_sse2, simd_avx2, simd_neon))]
fn memchr_impl_unrolled<M, N>(haystack: &[u8], n: N) -> Option<usize>
where
    M: Simd,
    N: Needle,
{
    // SAFETY: The representation of `M: Simd` is equivalent to `[u8; M::BYTES]`
    // and `align_to` ensures sufficient alignment.
    let (_prefix, aligned, suffix) = unsafe { haystack.align_to::<M>() };

    let rest = if N::UNROLL == 4 {
        let (chunks, rest) = aligned.as_chunks::<4>();

        for chunks @ [chunk0, chunk1, chunk2, chunk3] in chunks {
            let mask0 = n.cmp_chunk(*chunk0);
            let mask1 = n.cmp_chunk(*chunk1);
            let mask2 = n.cmp_chunk(*chunk2);
            let mask3 = n.cmp_chunk(*chunk3);

            let mask01 = mask0.or(mask1);
            let mask23 = mask2.or(mask3);
            let mask = mask01.or(mask23);

            if mask.any() {
                cold();

                let mut pos = chunks.as_ptr().addr() - haystack.as_ptr().addr();

                let mask0 = mask0.movemask();
                if mask0 != M::NONE {
                    return Some(pos + M::first_set(mask0));
                } else {
                    pos += M::BYTES;
                }

                let mask1 = mask1.movemask();
                if mask1 != M::NONE {
                    return Some(pos + M::first_set(mask1));
                } else {
                    pos += M::BYTES;
                }

                let mask2 = mask2.movemask();
                if mask2 != M::NONE {
                    return Some(pos + M::first_set(mask2));
                } else {
                    pos += M::BYTES;
                }

                let mask3 = mask3.movemask();
                return Some(pos + M::first_set(mask3));
            }
        }

        rest
    } else if N::UNROLL == 2 {
        let (chunks, rest) = aligned.as_chunks::<2>();

        for chunks @ [chunk0, chunk1] in chunks {
            let mask0 = n.cmp_chunk(*chunk0);
            let mask1 = n.cmp_chunk(*chunk1);

            let mask = mask0.or(mask1);

            if mask.any() {
                cold();

                let mut pos = chunks.as_ptr().addr() - haystack.as_ptr().addr();

                let mask0 = mask0.movemask();
                if mask0 != M::NONE {
                    return Some(pos + M::first_set(mask0));
                } else {
                    pos += M::BYTES;
                }

                let mask1 = mask1.movemask();
                return Some(pos + M::first_set(mask1));
            }
        }

        rest
    } else if N::UNROLL == 1 {
        aligned
    } else {
        unimplemented!()
    };

    for chunk in rest {
        let mask = n.cmp_chunk(*chunk).movemask();

        if mask != M::NONE {
            cold();

            let pos = (chunk as *const M).addr() - haystack.as_ptr().addr();

            return Some(pos + M::first_set(mask));
        }
    }

    if suffix.is_empty() {
        return None;
    }

    // SAFETY: `haystack` points to at least `M::BYTES` bytes of valid data.
    unsafe {
        let pos = suffix.as_ptr().addr() - haystack.as_ptr().addr() + suffix.len() - M::BYTES;

        memchr_impl_unaligned::<M, N>(haystack.as_ptr().add(pos), n).map(|off| {
            cold();

            pos + off
        })
    }
}

pub(crate) fn memchr2_count(needle1: u8, needle2: u8, haystack: &[u8]) -> (usize, usize) {
    let mut count1 = 0;
    let mut count2 = 0;

    #[cfg(not(any(simd_sse2, simd_avx2, simd_neon)))]
    memchr2_count_impl(needle1, needle2, haystack, &mut count1, &mut count2);

    #[cfg(simd_sse2)]
    memchr2_count_impl_simd::<__m128i>(needle1, needle2, haystack, &mut count1, &mut count2);

    #[cfg(simd_avx2)]
    memchr2_count_impl_simd::<__m256i>(needle1, needle2, haystack, &mut count1, &mut count2);

    #[cfg(simd_neon)]
    memchr2_count_impl_simd::<uint8x16_t>(needle1, needle2, haystack, &mut count1, &mut count2);

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
#[cfg(any(simd_sse2, simd_avx2, simd_neon))]
fn memchr2_count_impl_simd<M>(
    needle1: u8,
    needle2: u8,
    haystack: &[u8],
    count1: &mut usize,
    count2: &mut usize,
) where
    M: Simd,
{
    // SAFETY: The representation of `M: Simd` is equivalent to `[u8; M::BYTES]`
    // and `align_to` ensures sufficient alignment.
    let (prefix, aligned, suffix) = unsafe { haystack.align_to::<M>() };

    memchr2_count_impl(needle1, needle2, prefix, count1, count2);

    for &chunk in aligned {
        let hits1 = chunk.compare(M::splat(needle1)).movemask();
        let hits2 = chunk.compare(M::splat(needle2)).movemask();

        *count1 += M::count_set(hits1);
        *count2 += M::count_set(hits2);
    }

    memchr2_count_impl(needle1, needle2, suffix, count1, count2);
}

#[allow(dead_code, clippy::missing_safety_doc)]
pub(crate) unsafe trait Simd: Copy {
    const BYTES: usize;

    type Mask: PartialEq;

    const NONE: Self::Mask;

    const ALL: Self::Mask;

    fn splat(byte: u8) -> Self;

    unsafe fn load_unaligned(bytes: *const u8) -> Self;

    fn compare(self, other: Self) -> Self;

    fn and(self, other: Self) -> Self;

    fn or(self, other: Self) -> Self;

    fn xor(self, other: Self) -> Self;

    fn shift_right<const BITS: i32>(self) -> Self;

    fn shuffle(self, _other: Self) -> Self;

    fn any(self) -> bool;

    fn movemask(self) -> Self::Mask;

    fn first_set(mask: Self::Mask) -> usize;

    fn first_unset(mask: Self::Mask) -> usize;

    fn count_set(mask: Self::Mask) -> usize;
}

// SAFETY: Conditional compilation ensures that the `sse2` target feature is available.
#[cfg(target_arch = "x86_64")]
unsafe impl Simd for __m128i {
    const BYTES: usize = 16;

    type Mask = u32;

    const NONE: Self::Mask = 0;

    const ALL: Self::Mask = 0xFF_FF;

    #[inline(always)]
    fn splat(byte: u8) -> Self {
        unsafe { _mm_set1_epi8(byte as i8) }
    }

    #[inline(always)]
    unsafe fn load_unaligned(bytes: *const u8) -> Self {
        unsafe { _mm_loadu_si128(bytes.cast()) }
    }

    #[inline(always)]
    fn compare(self, other: Self) -> Self {
        unsafe { _mm_cmpeq_epi8(self, other) }
    }

    #[inline(always)]
    fn and(self, other: Self) -> Self {
        unsafe { _mm_and_si128(self, other) }
    }

    #[inline(always)]
    fn or(self, other: Self) -> Self {
        unsafe { _mm_or_si128(self, other) }
    }

    #[inline(always)]
    fn xor(self, other: Self) -> Self {
        unsafe { _mm_xor_si128(self, other) }
    }

    #[inline(always)]
    fn shift_right<const BITS: i32>(self) -> Self {
        unsafe { _mm_srli_epi16(self, BITS).and(Self::splat(0xF)) }
    }

    #[inline(always)]
    #[cfg(target_feature = "ssse3")]
    fn shuffle(self, other: Self) -> Self {
        unsafe { _mm_shuffle_epi8(self, other) }
    }

    #[inline(always)]
    #[cfg(not(target_feature = "ssse3"))]
    fn shuffle(self, _other: Self) -> Self {
        unimplemented!()
    }

    #[inline(always)]
    #[cfg(target_feature = "sse4.1")]
    fn any(self) -> bool {
        unsafe { _mm_testz_si128(self, self) == 0 }
    }

    #[inline(always)]
    #[cfg(not(target_feature = "sse4.1"))]
    fn any(self) -> bool {
        self.movemask() != 0
    }

    #[inline(always)]
    fn movemask(self) -> Self::Mask {
        unsafe { _mm_movemask_epi8(self) as u32 }
    }

    #[inline(always)]
    fn first_set(mask: Self::Mask) -> usize {
        mask.trailing_zeros() as usize
    }

    #[inline(always)]
    fn first_unset(mask: Self::Mask) -> usize {
        mask.trailing_ones() as usize
    }

    #[inline(always)]
    fn count_set(mask: Self::Mask) -> usize {
        mask.count_ones() as usize
    }
}

// SAFETY: Conditional compilation ensures that the `avx2` target feature is available.
#[cfg(simd_avx2)]
unsafe impl Simd for __m256i {
    const BYTES: usize = 32;

    type Mask = u32;

    const NONE: Self::Mask = 0;

    const ALL: Self::Mask = 0xFF_FF_FF_FF;

    #[inline(always)]
    fn splat(byte: u8) -> Self {
        unsafe { _mm256_set1_epi8(byte as i8) }
    }

    #[inline(always)]
    unsafe fn load_unaligned(bytes: *const u8) -> Self {
        unsafe { _mm256_loadu_si256(bytes.cast()) }
    }

    #[inline(always)]
    fn compare(self, other: Self) -> Self {
        unsafe { _mm256_cmpeq_epi8(self, other) }
    }

    #[inline(always)]
    fn and(self, other: Self) -> Self {
        unsafe { _mm256_and_si256(self, other) }
    }

    #[inline(always)]
    fn or(self, other: Self) -> Self {
        unsafe { _mm256_or_si256(self, other) }
    }

    #[inline(always)]
    fn xor(self, other: Self) -> Self {
        unsafe { _mm256_xor_si256(self, other) }
    }

    #[inline(always)]
    fn shift_right<const BITS: i32>(self) -> Self {
        unsafe { _mm256_srli_epi16(self, BITS).and(Self::splat(0xF)) }
    }

    #[inline(always)]
    fn shuffle(self, other: Self) -> Self {
        unsafe { _mm256_shuffle_epi8(self, other) }
    }

    #[inline(always)]
    fn any(self) -> bool {
        unsafe { _mm256_testz_si256(self, self) == 0 }
    }

    #[inline(always)]
    fn movemask(self) -> Self::Mask {
        unsafe { _mm256_movemask_epi8(self) as u32 }
    }

    #[inline(always)]
    fn first_set(mask: Self::Mask) -> usize {
        mask.trailing_zeros() as usize
    }

    #[inline(always)]
    fn first_unset(mask: Self::Mask) -> usize {
        mask.trailing_ones() as usize
    }

    #[inline(always)]
    fn count_set(mask: Self::Mask) -> usize {
        mask.count_ones() as usize
    }
}

// SAFETY: Conditional compilation ensures that the `neon` target feature is available.
#[cfg(simd_neon)]
unsafe impl Simd for uint8x16_t {
    const BYTES: usize = 16;

    type Mask = u64;

    const NONE: Self::Mask = 0;

    const ALL: Self::Mask = 0xFF_FF_FF_FF_FF_FF_FF_FF;

    #[inline(always)]
    fn splat(byte: u8) -> Self {
        unsafe { vdupq_n_u8(byte) }
    }

    #[inline(always)]
    unsafe fn load_unaligned(bytes: *const u8) -> Self {
        unsafe { vld1q_u8(bytes) }
    }

    #[inline(always)]
    fn compare(self, other: Self) -> Self {
        unsafe { vceqq_u8(self, other) }
    }

    #[inline(always)]
    fn and(self, other: Self) -> Self {
        unsafe { vandq_u8(self, other) }
    }

    #[inline(always)]
    fn or(self, other: Self) -> Self {
        unsafe { vorrq_u8(self, other) }
    }

    #[inline(always)]
    fn xor(self, other: Self) -> Self {
        unsafe { veorq_u8(self, other) }
    }

    #[inline(always)]
    fn shift_right<const BITS: i32>(self) -> Self {
        unsafe { vshrq_n_u8(self, BITS) }
    }

    #[inline(always)]
    fn shuffle(self, other: Self) -> Self {
        unsafe { vqtbl1q_u8(self, other) }
    }

    #[inline(always)]
    fn any(self) -> bool {
        unsafe { vmaxvq_u32(vreinterpretq_u32_u8(self)) != 0 }
    }

    #[inline(always)]
    fn movemask(self) -> Self::Mask {
        unsafe {
            let half = vshrn_n_u16(vreinterpretq_u16_u8(self), 4);
            vget_lane_u64(vreinterpret_u64_u8(half), 0)
        }
    }

    #[inline(always)]
    fn first_set(mask: Self::Mask) -> usize {
        mask.trailing_zeros() as usize >> 2
    }

    #[inline(always)]
    fn first_unset(mask: Self::Mask) -> usize {
        mask.trailing_ones() as usize >> 2
    }

    #[inline(always)]
    fn count_set(mask: Self::Mask) -> usize {
        mask.count_ones() as usize >> 2
    }
}

#[cfg(any(simd_sse2, simd_avx2, simd_neon))]
#[cold]
#[inline(always)]
fn cold() {}

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
    fn memchr4_works() {
        TestRunner::default()
            .run(
                &(
                    ANY_BYTE,
                    ANY_BYTE,
                    ANY_BYTE,
                    ANY_BYTE,
                    vec(ANY_BYTE, ..=200),
                ),
                |(needle1, needle2, needle3, needle4, haystack)| {
                    let pos1 = haystack
                        .iter()
                        .enumerate()
                        .filter(|&(_, &byte)| {
                            byte == needle1 || byte == needle2 || byte == needle3 || byte == needle4
                        })
                        .map(|(pos, _)| pos)
                        .collect::<Vec<_>>();

                    let pos2 = iter(&haystack, |haystack| {
                        memchr4(needle1, needle2, needle3, needle4, haystack)
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
