use core::borrow::Borrow;
use core::borrow::BorrowMut;
use core::cmp::Ordering;
use core::fmt;
use core::hash::Hash;
use core::hash::Hasher;
use core::mem::ManuallyDrop;
use core::mem::MaybeUninit;
use core::ops::Deref;
use core::ops::DerefMut;
use core::ops::Index;
use core::ops::IndexMut;
use core::ptr;
use core::slice;

use crate::CapacityError;

#[repr(C)]
#[derive(Clone, Copy)]
union Header<T> {
    // TODO(jt): @Memory Let the caller pick this integer type, so that they can save a byte and
    // avoid trailing padding issues in more situations.
    len: u16,
    // SAFETY: The padding here exists so that the compiler is forced to track it. When we create
    // the inlinevec, we initialize everything, including this padding, through a pointer write. The
    // compiler then has to emit code that copies our initialized padding. This makes it safe to
    // cast the inlinevec to a &[u8].
    _padding: ManuallyDrop<T>,
}

impl<T> Header<T> {
    const fn get(&self) -> u16 {
        // SAFETY: We never create the padding variant of the enum.
        unsafe { self.len }
    }

    const fn set(&mut self, len: u16) {
        *self = Self { len };
    }

    const fn inc(&mut self) {
        // SAFETY: We never create the padding variant of the enum.
        let len = unsafe { &mut self.len };
        *len += 1;
    }

    const fn dec(&mut self) {
        // SAFETY: We never create the padding variant of the enum.
        let len = unsafe { &mut self.len };
        *len -= 1;
    }
}

impl<T> PartialEq for Header<T> {
    fn eq(&self, rhs: &Self) -> bool {
        let lhs_len = self.get();
        let rhs_len = rhs.get();

        lhs_len == rhs_len
    }
}

impl<T> Eq for Header<T> {}

impl<T> Hash for Header<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_u16(self.get());
    }
}

impl<T> fmt::Debug for Header<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let len = self.get();
        write!(f, "{len}")
    }
}

#[repr(C)]
// Deriving Clone and Copy is fine, because they work the same way bitwise and don't need any extra
// knowledge about the data representation.
//
// However, PartialEq and Eq, PartialOrd, Ord and Hash need to (manually) compare the data as though
// it was a slice.
#[derive(Clone, Copy)]
pub struct InlineVec<T: Clone + Copy, const N: usize> {
    len: Header<T>,
    data: [MaybeUninit<T>; N],
}

impl<T: Clone + Copy, const N: usize> InlineVec<T, N> {
    /// Creates a new InlineVec storing T and having capacity for N elements.
    ///
    /// Currently, N must fit in u16.
    ///
    /// ```compile_fail
    /// use wfinlinevec::InlineVec;
    /// let s: InlineVec<i32, 99999> = InlineVec::new();
    /// ```
    ///
    /// # Warning
    ///
    /// The combination of T and N must be such that it doesn't produce trailing padding in
    /// InlineVec. This is checked at compile time. E.g. this works:
    ///
    /// ```
    /// use wfinlinevec::InlineVec;
    /// let s: InlineVec<u8, 2> = InlineVec::new();
    /// ```
    ///
    /// And this doesn't, becuase the vector's header has align_of at least 2 (depeding on T), so it
    /// creates trailing padding for uneven N:
    ///
    /// ```compile_fail
    /// use wfinlinevec::InlineVec;
    /// let s: InlineVec<u8, 1> = InlineVec::new();
    /// ```
    ///
    /// This also doesn't, for the same reason:
    ///
    /// ```compile_fail
    /// use wfinlinevec::InlineVec;
    /// let s: InlineVec<[u8; 3], 3> = InlineVec::new();
    /// ```
    #[inline]
    pub const fn new() -> Self {
        const {
            if usize::BITS > u16::BITS {
                assert!(N <= u16::MAX as usize)
            }
        }

        // We would have trailing padding, if T's align were 1 (e.g. we are storing UTF8 data),
        // which is UB, if we bytecasted. This could happen for any align(1) T, also if
        // size_of(T)>1.
        //
        // TODO(jt): In addition to this assert, we could let the caller pick the integer type to
        // store the length, which would make more T and N combinations correct.
        const {
            let total_size = size_of::<Self>();
            let header_size = size_of::<Header<T>>();
            let data_size = size_of::<[MaybeUninit<T>; N]>();

            // TODO(jt): Once possible to do in const contexts, add a more info to the assert.
            assert!(total_size == header_size + data_size);
        }

        // SAFETY: Bytecasting the InlineVec can observe padding bytes between self.len and
        // self.data, so we make sure they are zeroed first. We have to zero the bytes with
        // ptr::write_bytes, because MaybeUninit::zeroed or mem::zeroed don't actually zero
        // padding.
        let mut v: MaybeUninit<Self> = MaybeUninit::uninit();
        let p: *mut Self = v.as_mut_ptr();

        // Zero the entirety of our memory.
        unsafe { p.write_bytes(0, 1) };

        unsafe { v.assume_init() }
    }

    #[inline]
    pub const fn len(&self) -> usize {
        // TODO(jt): @Cleanup usize::from, once there are const_trait_impl
        self.len.get() as usize
    }

    #[inline]
    pub const fn capacity(&self) -> usize {
        N
    }

    #[inline]
    pub const fn remaining_capacity(&self) -> usize {
        self.capacity() - self.len()
    }

    // TODO(jt): Implement pub fn swap_remove(&mut self, index: usize) -> T
    //
    // TODO(jt): Implement pub fn retain<F>(&mut self, f: F) (where F takes a &mut, so we don't have
    // to do a retain_mut)

    #[inline]
    pub fn try_push(&mut self, value: T) -> Result<(), CapacityError<T>> {
        debug_assert!(usize::from(self.len.get()) <= N);
        if usize::from(self.len.get()) == N {
            return Err(CapacityError { value });
        }

        unsafe {
            self.push_unchecked(value);
        }

        Ok(())
    }

    #[inline]
    pub fn push(&mut self, value: T) {
        match self.try_push(value) {
            Ok(()) => (),
            Err(_) => panic!("Failed to push value: InlineVec at full capacity ({N})."),
        }
    }

    #[inline]
    #[allow(clippy::missing_safety_doc)]
    pub unsafe fn push_unchecked(&mut self, value: T) {
        let value_slot = unsafe { self.data.get_unchecked_mut(usize::from(self.len.get())) };
        *value_slot = MaybeUninit::new(value);

        self.len.inc();
    }

    #[inline]
    pub fn pop(&mut self) -> Option<T> {
        if self.len.get() == 0 {
            return None;
        }

        let value_slot = unsafe { self.data.get_unchecked(usize::from(self.len.get() - 1)) };
        let value = unsafe { value_slot.assume_init_read() };

        self.len.dec();

        Some(value)
    }

    #[inline]
    pub fn try_insert(&mut self, index: usize, value: T) -> Result<(), CapacityError<T>> {
        debug_assert!(usize::from(self.len.get()) <= N);
        if usize::from(self.len.get()) == N {
            return Err(CapacityError { value });
        }

        assert!(index <= self.len());

        let data_ptr: *mut [MaybeUninit<T>; N] = &raw mut self.data;
        let data_ptr: *mut MaybeUninit<T> = data_ptr as *mut MaybeUninit<T>;

        // Upshift elements first. The element at index will be temporarily doubled.

        // SAFETY: add index is okay, because index is in bounds (of capacity, not necessarily len).
        let copy_src_ptr: *mut MaybeUninit<T> = unsafe { data_ptr.add(index) };
        // SAFETY: add one is okay, because we still have capacity.
        let copy_dst_ptr: *mut MaybeUninit<T> = unsafe { copy_src_ptr.add(1) };
        let copy_count = self.len() - index;

        unsafe {
            ptr::copy(copy_src_ptr, copy_dst_ptr, copy_count);
        }

        // Write new value to element at index.
        let value_slot = unsafe { self.data.get_unchecked_mut(index) };
        *value_slot = MaybeUninit::new(value);

        self.len.inc();

        Ok(())
    }

    #[inline]
    pub fn insert(&mut self, index: usize, value: T) {
        match self.try_insert(index, value) {
            Ok(()) => (),
            Err(_) => panic!("Failed to insert value: InlineVec at full capacity ({N})."),
        }
    }

    #[inline]
    pub fn remove(&mut self, index: usize) -> T {
        assert!(self.len() > 0);
        assert!(index < self.len());

        // Copy out element first.
        let value_slot = unsafe { self.data.get_unchecked(index) };
        let value = unsafe { value_slot.assume_init_read() };

        // Downshift elements above, but only if there is anything to downshift. We have to
        // special-case this, because pointer::add invokes UB immediately when called, instead of
        // when actually used.
        if index < self.len() - 1 {
            let data_ptr: *mut [MaybeUninit<T>; N] = &raw mut self.data;
            let data_ptr: *mut MaybeUninit<T> = data_ptr as *mut MaybeUninit<T>;

            // SAFETY: add index is okay, because index is in bounds of len.
            let copy_dst_ptr: *mut MaybeUninit<T> = unsafe { data_ptr.add(index) };
            // SAFETY: add one is okay, because index+1 is in bounds of len (and thus also
            // capacity), so there's things to copy without adddressing out of the buffer.
            let copy_src_ptr: *mut MaybeUninit<T> = unsafe { copy_dst_ptr.add(1) };
            let copy_count = self.len() - index - 1;

            unsafe {
                ptr::copy(copy_src_ptr, copy_dst_ptr, copy_count);
            }
        }

        self.len.dec();

        value
    }

    #[inline]
    pub fn get(&self, index: usize) -> Option<&T> {
        if index >= usize::from(self.len.get()) {
            return None;
        }

        let value_slot = unsafe { self.data.get_unchecked(index) };
        let value = unsafe { value_slot.assume_init_ref() };

        Some(value)
    }

    #[inline]
    pub fn get_mut(&mut self, index: usize) -> Option<&mut T> {
        if index >= usize::from(self.len.get()) {
            return None;
        }

        let value_slot = unsafe { self.data.get_unchecked_mut(index) };
        let value = unsafe { value_slot.assume_init_mut() };

        Some(value)
    }

    #[inline]
    pub fn clear(&mut self) {
        self.len.set(0);
    }

    #[inline]
    pub fn truncate(&mut self, len: usize) {
        if len < usize::from(self.len.get()) {
            // Casting to u16 should never cause loss of data here, because we verify that len is
            // smaller than our current len.
            self.len.set(len as u16);
        }
    }

    #[inline]
    #[allow(clippy::missing_safety_doc)]
    pub unsafe fn set_len(&mut self, len: usize) {
        debug_assert!(len < N);
        self.len.set(len as u16);
    }

    #[inline]
    pub fn as_slice(&self) -> &[T] {
        let len = usize::from(self.len.get());
        let s = unsafe { self.data.get_unchecked(0..len) };

        // SAFETY: Everything up to len is initialized.
        unsafe { slice_assume_init_ref(s) }
    }

    #[inline]
    pub fn as_mut_slice(&mut self) -> &mut [T] {
        let len = usize::from(self.len.get());
        let s = unsafe { self.data.get_unchecked_mut(0..len) };

        // SAFETY: Everything up to len is initialized.
        unsafe { slice_assume_init_mut(s) }
    }

    #[inline]
    pub fn iter(&self) -> slice::Iter<'_, T> {
        self.as_slice().iter()
    }

    #[inline]
    pub fn iter_mut(&mut self) -> slice::IterMut<'_, T> {
        self.as_mut_slice().iter_mut()
    }
}

impl<T: Clone + Copy + fmt::Debug, const N: usize> fmt::Debug for InlineVec<T, N> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_list().entries(self.iter()).finish()
    }
}

impl<T: Clone + Copy + PartialEq, const N: usize> PartialEq for InlineVec<T, N> {
    #[inline]
    fn eq(&self, rhs: &Self) -> bool {
        self.as_slice() == rhs.as_slice()
    }
}

impl<T: Clone + Copy + PartialEq, const N: usize> PartialEq<[T]> for InlineVec<T, N> {
    #[inline]
    fn eq(&self, rhs: &[T]) -> bool {
        self.as_slice() == rhs
    }
}

impl<T: Clone + Copy + Eq, const N: usize> Eq for InlineVec<T, N> {}

impl<T: Clone + Copy + PartialOrd, const N: usize> PartialOrd for InlineVec<T, N> {
    #[inline]
    fn partial_cmp(&self, rhs: &Self) -> Option<Ordering> {
        self.as_slice().partial_cmp(rhs.as_slice())
    }
}

impl<T: Clone + Copy + Ord, const N: usize> Ord for InlineVec<T, N> {
    #[inline]
    fn cmp(&self, rhs: &Self) -> Ordering {
        self.as_slice().cmp(rhs.as_slice())
    }
}

impl<T: Clone + Copy + Hash, const N: usize> Hash for InlineVec<T, N> {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        // Hash exactly the same way a slice would. If we didn't do this, the borrowed lookup keys
        // for hashmaps would hash differently to InlineVecs.
        self.as_slice().hash(state);
    }
}

impl<T: Clone + Copy, const N: usize> Default for InlineVec<T, N> {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

impl<T: Clone + Copy, const N: usize> Index<usize> for InlineVec<T, N> {
    type Output = T;

    #[inline]
    fn index(&self, index: usize) -> &T {
        self.get(index).unwrap()
    }
}

impl<T: Clone + Copy, const N: usize> IndexMut<usize> for InlineVec<T, N> {
    #[inline]
    fn index_mut(&mut self, index: usize) -> &mut T {
        self.get_mut(index).unwrap()
    }
}

impl<T: Clone + Copy, const N: usize> Deref for InlineVec<T, N> {
    type Target = [T];

    #[inline]
    fn deref(&self) -> &[T] {
        self.as_slice()
    }
}

impl<T: Clone + Copy, const N: usize> DerefMut for InlineVec<T, N> {
    #[inline]
    fn deref_mut(&mut self) -> &mut [T] {
        self.as_mut_slice()
    }
}

impl<T: Clone + Copy, const N: usize> AsRef<[T]> for InlineVec<T, N> {
    #[inline]
    fn as_ref(&self) -> &[T] {
        self.as_slice()
    }
}

impl<T: Clone + Copy, const N: usize> AsMut<[T]> for InlineVec<T, N> {
    #[inline]
    fn as_mut(&mut self) -> &mut [T] {
        self.as_mut_slice()
    }
}

impl<T: Clone + Copy, const N: usize> Borrow<[T]> for InlineVec<T, N> {
    #[inline]
    fn borrow(&self) -> &[T] {
        self.as_slice()
    }
}

impl<T: Clone + Copy, const N: usize> BorrowMut<[T]> for InlineVec<T, N> {
    #[inline]
    fn borrow_mut(&mut self) -> &mut [T] {
        self.as_mut_slice()
    }
}

pub struct InlineVecIter<T: Clone + Copy, const N: usize> {
    next_index: usize,
    data: InlineVec<T, N>,
}

impl<T: Clone + Copy, const N: usize> Iterator for InlineVecIter<T, N> {
    type Item = T;

    #[inline]
    fn next(&mut self) -> Option<T> {
        if self.next_index == self.data.len() {
            None
        } else {
            let value_slot = unsafe { self.data.data.get_unchecked(self.next_index) };
            let value = unsafe { value_slot.assume_init_read() };
            self.next_index += 1;

            Some(value)
        }
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        let remaining_len = self.data.len() - self.next_index;

        (remaining_len, Some(remaining_len))
    }
}

impl<T: Clone + Copy, const N: usize> ExactSizeIterator for InlineVecIter<T, N> {}

impl<T: Clone + Copy, const N: usize> IntoIterator for InlineVec<T, N> {
    type Item = T;
    type IntoIter = InlineVecIter<T, N>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        Self::IntoIter {
            next_index: 0,
            data: self,
        }
    }
}

impl<'a, T: Clone + Copy, const N: usize> IntoIterator for &'a InlineVec<T, N> {
    type Item = &'a T;
    type IntoIter = slice::Iter<'a, T>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a, T: Clone + Copy, const N: usize> IntoIterator for &'a mut InlineVec<T, N> {
    type Item = &'a mut T;
    type IntoIter = slice::IterMut<'a, T>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}

// TODO(jt): @Cleanup Implement the safe transmute trait from Rust's core instead.
#[cfg(feature = "bytemuck")]
mod bytemuck_impl {
    use core::fmt;

    use super::*;

    // TODO(jt): InlineVecBits has to be public, becuase it is mentioned as an item in a trait impl,
    // but we currently do not re-export it from the crate, making it unnamable. If this becomes a
    // problem, we can re-evaluate.
    //
    // IMPORTANT(jt): InlineVecBits has to have the same layout as InlineVec.
    #[repr(C)]
    #[derive(Clone, Copy)]
    pub struct InlineVecBits<T: Clone + Copy + bytemuck::AnyBitPattern, const N: usize> {
        len: u16,
        data: [T; N],
    }

    // TODO(jt): @Cleanup Not sure why Debug is required for Bits. Maybe the derive macro slaps on too
    // many constraints?
    impl<T: Clone + Copy + fmt::Debug + bytemuck::AnyBitPattern, const N: usize> fmt::Debug for InlineVecBits<T, N> {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            let len = self.len;
            let data = &self.data;
            write!(f, "InlineVec {{ len: {len}, data: {data:?} }}")
        }
    }

    // TODO(jt): @Cleanup Zeroable is oldstyle bytemuck, but currently it is a prerequisite for
    // AnyBitPattern. Maybe we can remove this one day?
    //
    // SAFETY: If T is AnyBitPattern, InlineVecBits is Zeroable.
    unsafe impl<T: Clone + Copy + bytemuck::AnyBitPattern, const N: usize> bytemuck::Zeroable for InlineVecBits<T, N> {}
    // SAFETY: If T is AnyBitPattern, so is InlineVecBits. AnyBitPattern just means we can safely cast
    // to InlineVecBits, which is then further checked.
    unsafe impl<T: Clone + Copy + bytemuck::AnyBitPattern, const N: usize> bytemuck::AnyBitPattern for InlineVecBits<T, N> {}

    // SAFETY: If T is CheckedBitPattern, so is InlineVec. InlineVec knows how to check its own data,
    // and it attempts to check each value it contains in a loop.
    unsafe impl<T: Clone + Copy + bytemuck::CheckedBitPattern, const N: usize> bytemuck::CheckedBitPattern
        for InlineVec<T, N>
    {
        type Bits = InlineVecBits<T::Bits, N>;

        #[inline]
        fn is_valid_bit_pattern(bits: &Self::Bits) -> bool {
            if usize::from(bits.len) > N {
                return false;
            }

            for i in 0..usize::from(bits.len) {
                let value = &bits.data[i];
                if !T::is_valid_bit_pattern(value) {
                    return false;
                }
            }

            true
        }
    }

    // SAFETY: If T is NoUninit, so is InlineVec. This is because T: NoUninit forces all padding
    // bytes to be initialized for Ts, we manually zero the memory of InlineVec when creating it and
    // we preserve it initialized across copies by tracking the leading padding in a union and
    // making sure there is no trailing padding at compile time.
    unsafe impl<T: Clone + Copy + bytemuck::NoUninit, const N: usize> bytemuck::NoUninit for InlineVec<T, N> {}
}

// TODO(jt): @Cleanup Move this impl to wfserialize.
#[cfg(feature = "serialize")]
mod serialize_impl {
    use core::alloc::Allocator;

    use super::*;

    impl<T: Clone + Copy + wfserialize::Deserialize<A>, A: Allocator + Clone, const N: usize>
        wfserialize::Deserialize<A> for InlineVec<T, N>
    {
        fn deserialize<NA>(node: &wfserialize::Node<NA>, allocator: A) -> Result<Self, wfserialize::DeserializeError>
        where
            NA: Allocator + Clone,
        {
            match node {
                wfserialize::Node::Array(array) => {
                    if array.len() > N {
                        return Err(wfserialize::DeserializeError::ArrayCapacity {
                            expected_up_to: N,
                            got: array.len(),
                        });
                    }

                    let mut inlinevec: InlineVec<T, N> = InlineVec::new();

                    for item_node in array {
                        let item = T::deserialize(item_node, allocator.clone())?;
                        inlinevec.push(item);
                    }

                    Ok(inlinevec)
                }
                other => Err(wfserialize::DeserializeError::Kind {
                    expected: wfserialize::Kind::Array,
                    got: other.kind(),
                }),
            }
        }
    }
}

// TODO(jt): @Cleanup Use slice::assume_init_ref instead, once it stabilizes.
const unsafe fn slice_assume_init_ref<T>(s: &[MaybeUninit<T>]) -> &[T] {
    // Code taken from the unstable slice::assume_init_ref.
    //
    // SAFETY: casting `slice` to a `*const [T]` is safe since the caller guarantees that
    // `slice` is initialized, and `MaybeUninit` is guaranteed to have the same layout as `T`.
    // The pointer obtained is valid since it refers to memory owned by `slice` which is a
    // reference and thus guaranteed to be valid for reads.
    unsafe { &*(s as *const [MaybeUninit<T>] as *const [T]) }
}

// TODO(jt): @Cleanup Use slice::assume_init_mut instead, once it stabilizes.
const unsafe fn slice_assume_init_mut<T>(s: &mut [MaybeUninit<T>]) -> &mut [T] {
    // Code taken from the unstable slice::assume_init_mut.
    //
    // SAFETY: Similar to slice_assume_init_ref, but we are also starting with a mutable reference,
    // so it should be fine to produce one.
    unsafe { &mut *(s as *mut [MaybeUninit<T>] as *mut [T]) }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_push_get_len_pop() {
        let mut v: InlineVec<i32, 4> = InlineVec::new();

        assert!(v.len() == 0);

        v.push(12);
        assert!(v.len() == 1);

        v.push(13);
        assert!(v.len() == 2);

        assert!(v.get(0) == Some(&12));
        assert!(v.get(1) == Some(&13));
        assert!(v.get(2) == None);
        assert!(v.get(3) == None);

        assert!(v.pop() == Some(13));
        assert!(v.len() == 1);
        assert!(v.pop() == Some(12));
        assert!(v.len() == 0);
    }

    #[test]
    fn test_insert() {
        let mut v: InlineVec<i32, 4> = InlineVec::new();
        assert!(v.len() == 0);

        v.insert(0, 12);
        assert!(v.len() == 1);
        assert!(v.as_slice() == &[12]);

        v.insert(0, 13);
        assert!(v.len() == 2);
        assert!(v.as_slice() == &[13, 12]);

        v.insert(2, 14);
        assert!(v.len() == 3);
        assert!(v.as_slice() == &[13, 12, 14]);
    }

    #[test]
    fn test_remnove() {
        let mut v: InlineVec<i32, 4> = InlineVec::new();
        assert!(v.len() == 0);

        v.push(0);
        v.push(1);
        v.push(2);
        v.push(3);
        assert!(v.len() == 4);
        assert!(v.as_slice() == &[0, 1, 2, 3]);

        // Check case with no downshift.
        v.remove(3);
        assert!(v.len() == 3);
        assert!(v.as_slice() == &[0, 1, 2]);

        v.push(3);
        assert!(v.len() == 4);
        assert!(v.as_slice() == &[0, 1, 2, 3]);

        v.remove(0);
        assert!(v.len() == 3);
        assert!(v.as_slice() == &[1, 2, 3]);

        v.remove(1);
        assert!(v.len() == 2);
        assert!(v.as_slice() == &[1, 3]);
    }

    #[cfg(feature = "bytemuck")]
    #[test]
    fn test_bytemuck_bytes_of() {
        let mut v: InlineVec<i32, 4> = InlineVec::new();

        v.push(12);
        v.push(13);
        assert!(v.len() == 2);

        let bytes = bytemuck::bytes_of(&v);
        assert!(bytes == &[2, 0, 0, 0, 12, 0, 0, 0, 13, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]);
    }

    #[cfg(feature = "bytemuck")]
    #[test]
    fn test_bytemuck_bytes_of_after_copy() {
        let mut v: InlineVec<i32, 4> = InlineVec::new();

        v.push(12);
        v.push(13);
        assert!(v.len() == 2);

        let u = v;

        let vbytes = bytemuck::bytes_of(&v);
        assert!(vbytes == &[2, 0, 0, 0, 12, 0, 0, 0, 13, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]);

        // This would be UB, if the padding didn't get copied
        let ubytes = bytemuck::bytes_of(&u);
        assert!(ubytes == &[2, 0, 0, 0, 12, 0, 0, 0, 13, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]);
    }

    #[test]
    fn test_into_iter() {
        let mut v: InlineVec<i32, 4> = InlineVec::new();

        v.push(12);
        v.push(13);

        let d = Vec::from_iter(v);

        assert!(d == &[12, 13]);
    }

    #[test]
    fn test_iter() {
        let mut v: InlineVec<i32, 4> = InlineVec::new();

        v.push(12);
        v.push(13);

        let d = Vec::from_iter(v.iter().copied());

        assert!(d == &[12, 13]);
    }

    #[test]
    fn test_iter_mut() {
        let mut v: InlineVec<i32, 4> = InlineVec::new();

        v.push(12);
        v.push(13);

        for x in &mut v {
            *x += 1;
        }

        let d = Vec::from_iter(v.iter().copied());

        assert!(d == &[13, 14]);
    }
}
