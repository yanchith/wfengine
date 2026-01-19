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
use crate::TryFromError;
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
    pub const fn new() -> Self {
        Self(InlineVec::new())
    }

    #[inline]
    pub fn try_from_str(s: &str) -> Result<Self, TryFromError> {
        let mut inlinestring = Self::new();
        inlinestring.try_push_str(s).map_err(|_| TryFromError)?;

        Ok(inlinestring)
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
    pub fn try_push(&mut self, c: char) -> Result<(), CapacityError<char>> {
        if c.len_utf8() > self.0.remaining_capacity() {
            return Err(CapacityError { value: c });
        }

        // TODO(jt): @Speed Decode directly into the final buffer... but it likely doesn't matter,
        // because pushing chars is already wrong and slow.
        let mut buf = [0; 4];
        let s = c.encode_utf8(&mut buf);

        for &byte in s.as_bytes() {
            // SAFETY: We checked the capacity ahead of time.
            unsafe {
                self.0.push_unchecked(byte);
            }
        }

        Ok(())
    }

    #[inline]
    pub fn try_push_str<'a>(&mut self, s: &'a str) -> Result<(), CapacityError<&'a str>> {
        if s.len() > self.0.remaining_capacity() {
            return Err(CapacityError { value: s });
        }

        // TODO(jt): @Speed Set length once and do block copy.
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
        // Because we could have deserialized an invalid string, we can not use from_utf8_unchecked
        // and instead panic on invalid data here. This is not great, but I don't know what sort of
        // assumption does Rust/LLVM make w.r.t. utf8 strings and why invalid utf8 is (memory)
        // unsafe... Maybe it skips bounds checking or something?
        //
        // TODO(jt): @Speed Provide _unchecked version of this function to reclaim the speed? (It
        // would still make the check in debug builds).
        str::from_utf8(self.0.as_slice()).unwrap()
    }

    #[inline]
    pub fn as_mut_str(&mut self) -> &mut str {
        // Because we could have deserialized an invalid string, we can not use from_utf8_unchecked
        // and instead panic on invalid data here. This is not great, but I don't know what sort of
        // assumption does Rust/LLVM make w.r.t. utf8 strings and why invalid utf8 is (memory)
        // unsafe... Maybe it skips bounds checking or something?
        //
        // TODO(jt): @Speed Provide _unchecked version of this function to reclaim the speed? (It
        // would still make the check in debug builds).
        str::from_utf8_mut(self.0.as_mut_slice()).unwrap()
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

impl<const N: usize> TryFrom<&str> for InlineString<N> {
    type Error = TryFromError;

    #[inline]
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Self::try_from_str(value).map_err(|_| TryFromError)
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

    //
    // InlineString used to implement CheckedBitPattern, but it was a lot of noise, and we are slowly
    // phasing out our use of CheckedBitPattern across the codebase, becuase we'd like to always be
    // able to deserialize data cheaply, and only perform validation when it is important to us.
    //

    // SAFETY: InlineString data is u8, which is AnyBitPattern. Moreso, the InlineVec header is
    // AnyBitPattern, because whenever we use the length stored there, we make sure to clamp it to
    // the capacity, so that we do not access out of bounds memory. If the length is incorrect, the
    // program is incorrect also, but the worst thing that can happen is accessing the AnyBitPattern
    // garbage bytes between length and capacity.
    unsafe impl<T: Clone + Copy + bytemuck::AnyBitPattern, const N: usize> bytemuck::AnyBitPattern for InlineVec<T, N> {}

    // SAFETY: u16 and u8 are NoUninit, so is InlineVec. This is because u16 and u8 have no padding
    // bytes, and we manually zero the memory of InlineVec when creating it and we preserve it
    // initialized across copies by tracking the padding in a union.
    unsafe impl<const N: usize> bytemuck::NoUninit for InlineString<N> {}
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
