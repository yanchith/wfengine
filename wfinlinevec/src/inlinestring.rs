use core::borrow::Borrow;
use core::borrow::BorrowMut;
use core::cmp::Ordering;
use core::convert::AsMut;
use core::convert::AsRef;
use core::fmt;
use core::hash::Hash;
use core::hash::Hasher;
use core::ops::Deref;
use core::ops::DerefMut;
use core::str;

use crate::CapacityError;
use crate::inlinevec::InlineVec;

// TODO(jt): @Memory The length is u16 in InlineVec. If we wanted really small strings, we could
// make the length u8 and save that one byte.
#[repr(C)]
// Deriving Clone, Copy is fine, because they work the same way bitwise and don't need any extra
// knowledge about the data representation.
//
// Deriving PartialEq, Eq and Default is okay to delegate to InlineVec, because they work the same
// way on raw bytes as they do on UTF8.
//
// However, PartialOrd, Ord and Hash need to be implemented manually, because so that their impls
// match that of &str (UTF8).
#[derive(Clone, Copy, PartialEq, Eq, Default)]
pub struct InlineString<const N: usize>(InlineVec<u8, N>);

impl<const N: usize> InlineString<N> {
    #[inline]
    pub fn new() -> Self {
        Self(InlineVec::new())
    }

    #[inline]
    pub fn capacity(&self) -> usize {
        self.0.capacity()
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.0.len()
    }

    #[inline]
    pub fn try_push_str<'a>(&mut self, s: &'a str) -> Result<(), CapacityError<&'a str>> {
        if s.len() > self.0.remaining_capacity() {
            return Err(CapacityError { value: s });
        }

        for &byte in s.as_bytes() {
            // SAFETY: We checked the capacity ahead of time.
            unsafe {
                self.0.push_unchecked(byte);
            }
        }

        Ok(())
    }

    #[inline]
    pub fn push_str(&mut self, s: &str) {
        match self.try_push_str(s) {
            Ok(()) => (),
            Err(_) => panic!(
                "Failed to push string of length {}: InlineString at full capacity ({}).",
                s.len(),
                N,
            ),
        }
    }

    #[inline]
    pub fn clear(&mut self) {
        self.0.clear();
    }

    #[inline]
    pub fn truncate(&mut self, len: usize) {
        if len < self.0.len() {
            assert!(self.as_str().is_char_boundary(len));
            self.0.truncate(len);
        }
    }

    #[inline]
    #[allow(clippy::missing_safety_doc)]
    pub unsafe fn set_len(&mut self, len: usize) {
        unsafe {
            self.0.set_len(len);
        }
    }

    #[inline]
    pub fn as_str(&self) -> &str {
        // SAFETY: We only ever push valid UTF-8 fragments to the inner vec.
        unsafe { str::from_utf8_unchecked(self.0.as_slice()) }
    }

    #[inline]
    pub fn as_mut_str(&mut self) -> &mut str {
        // SAFETY: We only ever push valid UTF-8 fragments to the inner vec.
        unsafe { str::from_utf8_unchecked_mut(self.0.as_mut_slice()) }
    }
}

impl<const N: usize> fmt::Debug for InlineString<N> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "\"{}\"", self.as_str())
    }
}

impl<const N: usize> PartialEq<&str> for InlineString<N> {
    #[inline]
    fn eq(&self, other: &&str) -> bool {
        self.as_str() == *other
    }
}

impl<const N: usize> PartialOrd for InlineString<N> {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<const N: usize> Ord for InlineString<N> {
    #[inline]
    fn cmp(&self, other: &Self) -> Ordering {
        // Order the same way &str would, as u8s are not chars.
        self.as_str().cmp(other.as_str())
    }
}

impl<const N: usize> Hash for InlineString<N> {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        // Hash exactly the same way &str would. If we didn't do this, the borrowed lookup keys for
        // hashmaps would hash differently to InlineStrings.
        self.as_str().hash(state);
    }
}

impl<const N: usize> Deref for InlineString<N> {
    type Target = str;

    #[inline]
    fn deref(&self) -> &str {
        self.as_str()
    }
}

impl<const N: usize> DerefMut for InlineString<N> {
    #[inline]
    fn deref_mut(&mut self) -> &mut str {
        self.as_mut_str()
    }
}

impl<const N: usize> AsRef<str> for InlineString<N> {
    #[inline]
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl<const N: usize> AsMut<str> for InlineString<N> {
    #[inline]
    fn as_mut(&mut self) -> &mut str {
        self.as_mut_str()
    }
}

impl<const N: usize> Borrow<str> for InlineString<N> {
    #[inline]
    fn borrow(&self) -> &str {
        self.as_str()
    }
}

impl<const N: usize> BorrowMut<str> for InlineString<N> {
    #[inline]
    fn borrow_mut(&mut self) -> &mut str {
        self.as_mut_str()
    }
}

impl<const N: usize> fmt::Write for InlineString<N> {
    #[inline]
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.try_push_str(s).map_err(|_| fmt::Error)
    }
}

// TODO(jt): @Cleanup Implement the safe transmute trait from Rust's core instead.
#[cfg(feature = "bytemuck")]
mod bytemuck_impl {
    use core::fmt;

    use super::*;

    // TODO(jt): InlineStringBits has to be public, becuase it is mentioned as an item in a trait impl,
    // but we currently do not re-export it from the crate, making it unnamable. If this becomes a
    // problem, we can re-evaluate.
    //
    // IMPORTANT(jt): InlineStringBits has to have the same layout as InlineString.
    #[repr(C)]
    #[derive(Clone, Copy)]
    pub struct InlineStringBits<const N: usize> {
        len: u16,
        data: [u8; N],
    }

    // TODO(jt): @Cleanup Not sure why Debug is required for Bits. Maybe the derive macro slaps on too
    // many constraints?
    impl<const N: usize> fmt::Debug for InlineStringBits<N> {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            let len = self.len;
            let data = &self.data;
            write!(f, "InlineString {{ len: {len}, data: {data:?} }}")
        }
    }

    // TODO(jt): @Cleanup Zeroable is oldstyle bytemuck, but currently it is a prerequisite for
    // AnyBitPattern. Maybe we can remove this one day?
    //
    // SAFETY: u16 and u8 are AnyBitPattern, InlineStringBits is Zeroable.
    unsafe impl<const N: usize> bytemuck::Zeroable for InlineStringBits<N> {}
    // SAFETY: u16 and u8 are AnyBitPattern, so is InlineStringBits. AnyBitPattern just means we can safely cast
    // to InlineStringBits, which is then further checked.
    unsafe impl<const N: usize> bytemuck::AnyBitPattern for InlineStringBits<N> {}

    // SAFETY: u16 and u8 are CheckedBitPattern, so is InlineString. InlineString knows how to check its own data.
    unsafe impl<const N: usize> bytemuck::CheckedBitPattern for InlineString<N> {
        type Bits = InlineStringBits<N>;

        #[inline]
        fn is_valid_bit_pattern(bits: &Self::Bits) -> bool {
            if usize::from(bits.len) > N {
                return false;
            }

            let bytes = &bits.data[0..usize::from(bits.len)];
            match str::from_utf8(bytes) {
                Ok(_) => true,
                Err(_) => false,
            }
        }
    }

    // SAFETY: u16 and u8 are NoUninit, so is InlineVec. This is because u16 and u8 have no padding
    // bytes, and we manually zero the memory of InlineVec when creating it and we preserve it
    // initialized across copies by tracking the padding in a union.
    unsafe impl<const N: usize> bytemuck::NoUninit for InlineString<N> {}
}

// TODO(jt): @Cleanup Move this impl to wfserialize.
#[cfg(feature = "serialize")]
mod serialize_impl {
    use core::alloc::Allocator;

    use super::*;

    impl<A: Allocator + Clone, const N: usize> wfserialize::Deserialize<A> for InlineString<N> {
        fn deserialize<NA>(node: &wfserialize::Node<NA>, _: A) -> Result<Self, wfserialize::DeserializeError>
        where
            NA: Allocator + Clone,
        {
            match node {
                wfserialize::Node::String(string) => {
                    if string.len() > N {
                        return Err(wfserialize::DeserializeError::StringCapacity {
                            expected_up_to: N,
                            got: string.len(),
                        });
                    }

                    let mut s = InlineString::new();
                    s.push_str(unsafe { str::from_utf8_unchecked(string) });

                    Ok(s)
                }
                other => Err(wfserialize::DeserializeError::Kind {
                    expected: wfserialize::Kind::String,
                    got: other.kind(),
                }),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_push_len() {
        let mut s: InlineString<12> = InlineString::new();
        assert!(s.len() == 0);

        s.push_str("Cowabunga!");
        assert!(s.len() == 10);

        let res = s.try_push_str("!!");
        assert!(res.is_ok());

        let res = s.try_push_str("?");
        assert!(res.is_err());

        assert!(s == "Cowabunga!!!");
    }
}
