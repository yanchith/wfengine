#![no_std]
#![feature(allocator_api)]

extern crate alloc;

use alloc::vec::Vec;
use core::alloc::Allocator;
use core::alloc::Layout;
use core::fmt;
use core::hash::Hash;
use core::hash::Hasher;
use core::mem;
use core::mem::MaybeUninit;
use core::ops::Index;
use core::ops::IndexMut;
use core::ptr;
use core::ptr::NonNull;
use core::slice;

#[repr(C)]
struct Slab<T, const N: usize> {
    len: usize,
    mask: [bool; N],
    data: [MaybeUninit<T>; N],
}

#[repr(C)]
// Our Default location is 0-0. Analogously, 0 is the default usize, and it is a valid index for some arrays.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
#[derive(bytemuck::AnyBitPattern, bytemuck::NoUninit)]
pub struct SlabLoc {
    pub slab_index: u32,
    pub slot_index: u32,
}

impl SlabLoc {
    pub const ZERO: SlabLoc = SlabLoc {
        slab_index: 0,
        slot_index: 0,
    };
}

impl fmt::Debug for SlabLoc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}-{}", self.slab_index, self.slot_index)
    }
}

impl fmt::Display for SlabLoc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}-{}", self.slab_index, self.slot_index)
    }
}

pub struct SlabArray<T, A: Allocator + Clone, const N: usize = 128> {
    // TODO(jt): @Speed JAI's Bucket_Array tracks two things we don't (see its code below), and
    // therefore has faster insertion:
    //
    // 1) 'unfull_buckets', so that it can instantly find a bucket to insert to.
    //
    // 2) 'lowest_maybe_not_occupied', so that in some cases it can find a slot in that bucket
    //    faster, by cutting down the search space.
    //
    // We do neither of these, but maybe we should? Or maybe we should do something better?
    //
    // In addition to 'lowest_maybe_not_occupied' (or maybe instead of it?), we could search for
    // unoccupied slots with SIMD (or "SIMD")? If we bitpack the mask, we could crunch through 128
    // or 256 (or 64 in case of "SIMD") indices with one compare. After we find a block with an
    // empty slot, we'd still have to downshift or BSF to find the index.
    //
    // UPDATE: 'lowest_maybe_not_occupied' is surprisingly good at cutting down insert times. When I
    // tried to do the above in JAI, I couldn't beat it except in the most extreme
    // circumstances. Let's definitely do that in addition to "SIMD"?
    //
    // We could do something similar for slabs instead of 'unfull_buckets'. This would require us to
    // store the bucket length next to the bucket pointer in our metadata instead of in the bucket
    // itself, or store the "unfull bit" in the pointer itself (if we force slab alignment to at
    // least 2). This way we don't have to load the bucket to see if it has space. We could even
    // check multiple buckets with SIMD.
    len: usize,
    allocator: A,

    // Contrary to the main idea of the Slab, when this grows, it will have to move, leaving an
    // unusable hole in memory of linear allocators. Fortunately, this is just relatively small
    // metadata compared to the main data stored by the SlabArray, especially when N is large.
    slabs: Vec<NonNull<Slab<T, N>>, A>,
}

impl<T, A: Allocator + Clone, const N: usize> SlabArray<T, A, N> {
    /// Creates a new slab array with an allocator.
    ///
    /// # Compile time constraints
    ///
    /// Slab arrays do have a few limits, enforced in compile-time. However, as of 15.5.2024, `cargo
    /// check` will not catch these compile time errors, only `cargo build` will. This is because
    /// the errors originate in generic instantiations that rustc skips in `cargo check`.
    ///
    /// Slab arrays do not support zero-sized types:
    ///
    /// ```compile_fail
    /// let _sa: SlabArray<(), 16, _> = SlabArray::new_in(alloc::alloc::Global)
    /// ```
    /// Slab arrays also do not support N larger than [`u32::MAX`]:
    ///
    /// ```compile_fail
    /// const TOO_MUCH: usize = u32::MAX as usize + 1;
    /// let _sa: SlabArray<i32, _, TOO_MUCH> = SlabArray::new_in(alloc::alloc::Global);
    /// ```
    #[inline]
    pub fn new_in(allocator: A) -> Self {
        Self::with_capacity_in(0, allocator)
    }

    #[inline]
    pub fn with_capacity_in(capacity: usize, allocator: A) -> Self {
        // We don't support zero-sized types.
        const { assert!(size_of::<T>() > 0) };

        // Because our location type stores indices as u32, check ahead of time,
        // if we can represent all indices.
        const {
            if usize::BITS > u32::BITS {
                assert!(N <= u32::MAX as usize);
            }
        }

        // We only support power of two N, because that allows us to efficiently compute SlabLocs
        // out out of indices.
        const { assert!(usize::is_power_of_two(N)) };

        let slabs_capacity = capacity / N + usize::from(capacity % N != 0);
        let mut slabs = Vec::with_capacity_in(slabs_capacity, allocator.clone());
        for _ in 0..slabs_capacity {
            allocate_slab(allocator.clone(), &mut slabs);
        }

        Self {
            len: 0,
            allocator,
            slabs,
        }
    }

    // TODO(jt): @Speed @Memory Add SlabArray::with_metadata_capacity that reserves space in the
    // metadata array (to prevent realloc holes), but does not allocate_slab.

    #[inline]
    pub fn allocator(&self) -> &A {
        &self.allocator
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.len
    }

    #[inline]
    pub fn capacity(&self) -> usize {
        self.slabs.len() * N
    }

    #[inline]
    pub fn reserve(&mut self, additional: usize) {
        let additional_slabs_capacity = {
            let capacity_requested = self.len.saturating_add(additional);
            let capacity_have = self.capacity();

            if capacity_requested > capacity_have {
                let difference = capacity_requested - capacity_have;
                difference / N + usize::from(difference % N != 0)
            } else {
                0
            }
        };

        if additional_slabs_capacity > 0 {
            self.slabs.reserve(additional_slabs_capacity);
            for _ in 0..additional_slabs_capacity {
                allocate_slab(self.allocator.clone(), &mut self.slabs);
            }
        }
    }

    #[inline]
    pub fn reserve_for_loc(&mut self, loc: SlabLoc) {
        let slab_index = loc.slab_index as usize;
        let slot_index = loc.slot_index as usize;

        assert!(slot_index < N);

        if slab_index >= self.slabs.len() {
            let additional_slabs_capacity = slab_index - self.slabs.len() + 1;

            self.slabs.reserve(additional_slabs_capacity);
            for _ in 0..additional_slabs_capacity {
                allocate_slab(self.allocator.clone(), &mut self.slabs);
            }
        }
    }

    #[inline]
    pub fn clear(&mut self) {
        // If any of the destructors panic, leak the data instead of trying
        // to drop it twice.
        self.len = 0;

        for slab in &mut self.slabs {
            let slab_mut = unsafe { slab.as_mut() };

            // Copy mask into temporary, because we clear the occupancy before
            // attempting to run destructors, so that we can do that outside of
            // the loop, so that the loop can be elided, if T doesn't have a
            // Drop impl.
            let slab_mask = slab_mut.mask;

            // If any of the destructors panic, leak the data instead of trying
            // to drop it twice.
            slab_mut.len = 0;
            slab_mut.mask = [false; N];

            if mem::needs_drop::<T>() {
                for i in 0..N {
                    if slab_mask[i] {
                        unsafe {
                            slab_mut.data[i].assume_init_drop();
                        }
                    }
                }
            }
        }
    }

    #[inline]
    pub fn find_next_unoccupied_loc(&self) -> Option<SlabLoc> {
        let mut slab_index = 0;
        let mut slab_ptr: *mut Slab<T, N> = ptr::null_mut();

        for (index, &slab) in self.slabs.iter().enumerate() {
            let slab_ref = unsafe { slab.as_ref() };
            if slab_ref.len < N {
                // Should be ok to cast, because allocate_slab checks, whether we can contain the
                // slab_index in a u32.
                slab_index = index as u32;
                slab_ptr = slab.as_ptr();

                break;
            }
        }

        // TODO(jt): @Speed We can detect the worst case ahead of time! If self.len == capacity, we
        // know we are done and return None.
        if slab_ptr == ptr::null_mut() {
            return None;
        }

        debug_assert!(slab_ptr != ptr::null_mut());

        let slab_mut = unsafe { slab_ptr.as_mut().unwrap_unchecked() };
        let mut slot_index: Option<usize> = None;

        for i in 0..N {
            if !slab_mut.mask[i] {
                slot_index = Some(i);
                break;
            }
        }

        debug_assert!(slot_index.is_some());
        if let Some(slot_index) = slot_index {
            Some(SlabLoc {
                slab_index,
                slot_index: slot_index as u32,
            })
        } else {
            None
        }
    }

    #[inline]
    pub fn push(&mut self, value: T) -> SlabLoc {
        let mut slab_index = 0;
        let mut slab_ptr: *mut Slab<T, N> = ptr::null_mut();

        for (index, &slab) in self.slabs.iter().enumerate() {
            let slab_ref = unsafe { slab.as_ref() };
            if slab_ref.len < N {
                // Should be ok to cast, because allocate_slab checks, whether we can contain the
                // slab_index in a u32.
                slab_index = index as u32;
                slab_ptr = slab.as_ptr();

                break;
            }
        }

        // TODO(jt): @Speed We can detect the worst case ahead of time! If self.len == capacity, we
        // know we'll have to allocate a new slab without having to search them all.
        if slab_ptr == ptr::null_mut() {
            let slabs_len = self.slabs.len();

            // allocate_slab below panics, if u32 can't hold the new index.
            slab_index = slabs_len as u32;
            slab_ptr = allocate_slab(self.allocator.clone(), &mut self.slabs);
        }

        debug_assert!(slab_ptr != ptr::null_mut());

        let slab_mut = unsafe { slab_ptr.as_mut().unwrap_unchecked() };
        let mut slot_index: Option<usize> = None;

        for i in 0..N {
            if !slab_mut.mask[i] {
                slot_index = Some(i);
                break;
            }
        }

        let slot_index = slot_index.unwrap();

        slab_mut.data[slot_index].write(value);
        slab_mut.mask[slot_index] = true;
        slab_mut.len += 1;

        self.len += 1;

        SlabLoc {
            slab_index,
            // This cast should be ok, because we statically check that slot
            // indices fit in u32.
            slot_index: slot_index as u32,
        }
    }

    #[inline]
    pub fn insert(&mut self, loc: SlabLoc, value: T) -> Option<T> {
        let slab_index = loc.slab_index as usize;
        let slot_index = loc.slot_index as usize;

        let mut slab = self.slabs[slab_index];
        let slab_mut = unsafe { slab.as_mut() };

        let is_occupied = slab_mut.mask[slot_index];

        if is_occupied {
            let previous_value = unsafe { slab_mut.data[slot_index].assume_init_read() };

            slab_mut.data[slot_index].write(value);

            Some(previous_value)
        } else {
            slab_mut.data[slot_index].write(value);
            slab_mut.mask[slot_index] = true;

            slab_mut.len += 1;
            self.len += 1;

            None
        }
    }

    #[inline]
    pub fn get_by_index(&self, index: usize) -> Option<&T> {
        // Even if the resulting indices don't fit u32, it doesn't really matter, since we index by usize internally.
        //
        // TODO(jt): @Speed idiv is slow, but we could const assert that N is a power of two and
        // turn the divide into a shift and the modulo into a mask.
        let slab_index = index / N;
        let slot_index = index % N;

        match self.slabs.get(slab_index) {
            Some(slab) => {
                let slab_ref = unsafe { slab.as_ref() };
                match slab_ref.mask.get(slot_index) {
                    Some(mask) => {
                        if *mask {
                            let slot = unsafe { slab_ref.data.get_unchecked(slot_index) };
                            let value_ref = unsafe { slot.assume_init_ref() };

                            Some(value_ref)
                        } else {
                            None
                        }
                    }
                    None => None,
                }
            }
            None => None,
        }
    }

    #[inline]
    pub fn get_by_index_mut(&mut self, index: usize) -> Option<&mut T> {
        // Even if the resulting indices don't fit u32, it doesn't really matter, since we index by usize internally.
        //
        // TODO(jt): @Speed idiv is slow, but we could const assert that N is a power of two and
        // turn the divide into a shift and the modulo into a mask.
        let slab_index = index / N;
        let slot_index = index % N;

        match self.slabs.get_mut(slab_index) {
            Some(slab) => {
                let slab_mut = unsafe { slab.as_mut() };
                match slab_mut.mask.get(slot_index) {
                    Some(mask) => {
                        if *mask {
                            let slot = unsafe { slab_mut.data.get_unchecked_mut(slot_index) };
                            let value_mut = unsafe { slot.assume_init_mut() };

                            Some(value_mut)
                        } else {
                            None
                        }
                    }
                    None => None,
                }
            }
            None => None,
        }
    }

    #[inline]
    pub fn get(&self, loc: SlabLoc) -> Option<&T> {
        match self.slabs.get(loc.slab_index as usize) {
            Some(slab) => {
                let slot_index = loc.slot_index as usize;
                let slab_ref = unsafe { slab.as_ref() };
                match slab_ref.mask.get(slot_index) {
                    Some(mask) => {
                        if *mask {
                            let slot = unsafe { slab_ref.data.get_unchecked(slot_index) };
                            let value_ref = unsafe { slot.assume_init_ref() };

                            Some(value_ref)
                        } else {
                            None
                        }
                    }
                    None => None,
                }
            }
            None => None,
        }
    }

    #[inline]
    pub fn get_mut(&mut self, loc: SlabLoc) -> Option<&mut T> {
        match self.slabs.get_mut(loc.slab_index as usize) {
            Some(slab) => {
                let slot_index = loc.slot_index as usize;
                let slab_mut = unsafe { slab.as_mut() };
                match slab_mut.mask.get(slot_index) {
                    Some(mask) => {
                        if *mask {
                            let slot = unsafe { slab_mut.data.get_unchecked_mut(slot_index) };
                            let value_mut = unsafe { slot.assume_init_mut() };

                            Some(value_mut)
                        } else {
                            None
                        }
                    }
                    None => None,
                }
            }
            None => None,
        }
    }

    #[inline]
    pub fn remove(&mut self, loc: SlabLoc) -> Option<T> {
        match self.slabs.get_mut(loc.slab_index as usize) {
            Some(slab) => {
                let slot_index = loc.slot_index as usize;
                let slab_mut = unsafe { slab.as_mut() };
                match slab_mut.mask.get_mut(slot_index) {
                    Some(mask) => {
                        if *mask {
                            let slot = unsafe { slab_mut.data.get_unchecked_mut(slot_index) };
                            let value = unsafe { slot.assume_init_read() };

                            *mask = false;
                            slab_mut.len -= 1;

                            self.len -= 1;

                            Some(value)
                        } else {
                            None
                        }
                    }
                    None => None,
                }
            }
            None => None,
        }
    }

    #[inline]
    pub fn retain<F>(&mut self, mut f: F)
    where
        F: FnMut(SlabLoc, &T) -> bool,
    {
        for (index, slab) in &mut self.slabs.iter_mut().enumerate() {
            let slab_mut = unsafe { slab.as_mut() };

            for i in 0..N {
                if slab_mut.mask[i] {
                    let slot = unsafe { slab_mut.data.get_unchecked_mut(i) };
                    let value_mut = unsafe { slot.assume_init_mut() };

                    let loc = SlabLoc {
                        // Casts should be okay, because we check that slot_index fits u32 when
                        // constructing the SlabArray, and we aren't allowing more than u32::MAX-1
                        // slabs in allocate_slab.
                        slab_index: index as u32,
                        slot_index: i as u32,
                    };

                    if !f(loc, value_mut) {
                        // If the destructor panics, leak the data instead of
                        // trying to run the destructor again.
                        slab_mut.mask[i] = false;
                        slab_mut.len -= 1;

                        self.len -= 1;

                        unsafe {
                            slot.assume_init_drop();
                        }
                    }
                }
            }
        }
    }

    #[inline]
    pub fn iter(&self) -> Iter<'_, T, N> {
        Iter {
            len: self.len,
            slabs: &self.slabs,

            slab_index: 0,
            slot_index: 0,
            returned: 0,
        }
    }

    #[inline]
    pub fn iter_mut(&mut self) -> IterMut<'_, T, N> {
        IterMut {
            len: self.len,
            slabs: &mut self.slabs,

            slab_index: 0,
            slot_index: 0,
            returned: 0,
        }
    }
}

impl<T: Clone, A: Allocator + Clone, const N: usize> SlabArray<T, A, N> {
    /// Like [`SlabArray::clone_from`], but allows the allocators to differ.
    #[inline]
    pub fn populate_from<B: Allocator + Clone>(&mut self, other: &SlabArray<T, B, N>) {
        self.clear();

        if other.capacity() > self.capacity() {
            let additional = other.capacity() - self.len();
            self.reserve(additional);
        }

        debug_assert!(self.slabs.len() >= other.slabs.len());

        for (self_slab, &other_slab) in self.slabs.iter_mut().zip(other.slabs.iter()) {
            clone_slab(self_slab.as_ptr(), other_slab);
        }

        self.len = other.len;
    }
}

impl<T, A: Allocator + Clone, const N: usize> Drop for SlabArray<T, A, N> {
    fn drop(&mut self) {
        // If any of the destructors panic, leak the data instead of trying
        // to drop it twice.
        self.len = 0;

        let slab_layout = Layout::new::<Slab<T, N>>();
        for slab in self.slabs.iter_mut() {
            let slab_mut = unsafe { slab.as_mut() };

            // Copy mask into temporary, because we clear the occupancy before
            // attempting to run destructors, so that we can do that outside of
            // the loop, so that the loop can be elided, if T doesn't have a
            // Drop impl.
            let slab_mask = slab_mut.mask;

            // If any of the destructors panic, leak the data instead of trying
            // to drop it twice.
            slab_mut.len = 0;
            slab_mut.mask = [false; N];

            if mem::needs_drop::<T>() {
                for i in 0..N {
                    if slab_mask[i] {
                        unsafe {
                            slab_mut.data[i].assume_init_drop();
                        }
                    }
                }
            }

            let ptr: NonNull<u8> = slab.cast();
            unsafe {
                self.allocator.deallocate(ptr, slab_layout);
            }
        }
    }
}

impl<T: fmt::Debug, A: Allocator + Clone, const N: usize> fmt::Debug for SlabArray<T, A, N> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_map().entries(self.iter()).finish()
    }
}

impl<T: Clone, A: Allocator + Clone, const N: usize> Clone for SlabArray<T, A, N> {
    fn clone(&self) -> Self {
        let mut slabs = Vec::with_capacity_in(self.slabs.len(), self.allocator.clone());

        let slab_layout = Layout::new::<Slab<T, N>>();
        for &slab in &self.slabs {
            match self.allocator.allocate(slab_layout) {
                Ok(ptr) => {
                    let ptr: NonNull<Slab<T, N>> = ptr.cast();
                    let p = ptr.as_ptr();

                    clone_slab(p, slab);

                    slabs.push(ptr);
                }
                Err(err) => {
                    let size = slab_layout.size();
                    let align = slab_layout.align();
                    panic!("Failed to allocate layout size={size} align={align}: {err}");
                }
            }
        }

        Self {
            len: self.len,
            allocator: self.allocator.clone(),
            slabs,
        }
    }

    fn clone_from(&mut self, other: &Self) {
        self.populate_from(other);
    }
}

impl<T: PartialEq, A: Allocator + Clone, B: Allocator + Clone, const N: usize> PartialEq<SlabArray<T, B, N>>
    for SlabArray<T, A, N>
{
    fn eq(&self, other: &SlabArray<T, B, N>) -> bool {
        if self.len != other.len {
            return false;
        }

        for (self_slab, other_slab) in self.slabs.iter().zip(other.slabs.iter()) {
            let self_slab_ref = unsafe { self_slab.as_ref() };
            let other_slab_ref = unsafe { other_slab.as_ref() };

            if self_slab_ref.len != other_slab_ref.len {
                return false;
            }

            if self_slab_ref.mask != other_slab_ref.mask {
                return false;
            }

            for i in 0..N {
                if self_slab_ref.mask[i] {
                    let self_value = unsafe { self_slab_ref.data[i].assume_init_ref() };
                    let other_value = unsafe { other_slab_ref.data[i].assume_init_ref() };

                    if self_value != other_value {
                        return false;
                    }
                }
            }
        }

        true
    }
}

impl<T: Eq, A: Allocator + Clone, const N: usize> Eq for SlabArray<T, A, N> {}

impl<T: Hash, A: Allocator + Clone, const N: usize> Hash for SlabArray<T, A, N> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.len.hash(state);

        for slab in &self.slabs {
            let slab_ref = unsafe { slab.as_ref() };

            slab_ref.len.hash(state);
            slab_ref.mask.hash(state);

            for i in 0..N {
                if slab_ref.mask[i] {
                    let value = unsafe { slab_ref.data[i].assume_init_ref() };
                    value.hash(state);
                }
            }
        }
    }
}

impl<T, A: Allocator + Clone, const N: usize> Index<usize> for SlabArray<T, A, N> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        self.get_by_index(index).unwrap()
    }
}

impl<T, A: Allocator + Clone, const N: usize> IndexMut<usize> for SlabArray<T, A, N> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        self.get_by_index_mut(index).unwrap()
    }
}

impl<T, A: Allocator + Clone, const N: usize> Index<SlabLoc> for SlabArray<T, A, N> {
    type Output = T;

    fn index(&self, index: SlabLoc) -> &Self::Output {
        self.get(index).unwrap()
    }
}

impl<T, A: Allocator + Clone, const N: usize> IndexMut<SlabLoc> for SlabArray<T, A, N> {
    fn index_mut(&mut self, index: SlabLoc) -> &mut Self::Output {
        self.get_mut(index).unwrap()
    }
}

impl<'a, T, A: Allocator + Clone, const N: usize> IntoIterator for &'a SlabArray<T, A, N> {
    type Item = (SlabLoc, &'a T);
    type IntoIter = Iter<'a, T, N>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a, T, A: Allocator + Clone, const N: usize> IntoIterator for &'a mut SlabArray<T, A, N> {
    type Item = (SlabLoc, &'a mut T);
    type IntoIter = IterMut<'a, T, N>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}

#[derive(Debug)]
pub struct Iter<'a, T, const N: usize> {
    len: usize,
    slabs: &'a [NonNull<Slab<T, N>>],

    slab_index: usize,
    slot_index: usize,
    returned: usize,
}

impl<'a, T, const N: usize> Iterator for Iter<'a, T, N> {
    type Item = (SlabLoc, &'a T);

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(slab) = self.slabs.get(self.slab_index) {
            let slab_ref = unsafe { slab.as_ref() };

            if slab_ref.len > 0 {
                for i in self.slot_index..N {
                    if slab_ref.mask[i] {
                        self.slot_index = i + 1;
                        self.returned += 1;

                        let slot = &slab_ref.data[i];
                        let value_ref = unsafe { slot.assume_init_ref() };

                        // Casts should be safe, because we never insert more than
                        // u32::MAX-1 slabs and we check that N also fits in u32.
                        let slab_index = self.slab_index as u32;
                        let slot_index = i as u32;

                        return Some((SlabLoc { slab_index, slot_index }, value_ref));
                    }
                }
            }

            self.slab_index += 1;
            self.slot_index = 0;
        }

        None
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let remaining_len = self.len - self.returned;
        (remaining_len, Some(remaining_len))
    }
}

impl<T, const N: usize> ExactSizeIterator for Iter<'_, T, N> {}

#[derive(Debug)]
pub struct IterMut<'a, T, const N: usize> {
    len: usize,
    slabs: &'a mut [NonNull<Slab<T, N>>],

    slab_index: usize,
    slot_index: usize,
    returned: usize,
}

impl<'a, T, const N: usize> Iterator for IterMut<'a, T, N> {
    type Item = (SlabLoc, &'a mut T);

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(slab) = self.slabs.get_mut(self.slab_index) {
            let slab_mut = unsafe { slab.as_mut() };

            if slab_mut.len > 0 {
                for i in self.slot_index..N {
                    if slab_mut.mask[i] {
                        self.slot_index = i + 1;
                        self.returned += 1;

                        let slot = &mut slab_mut.data[i];
                        let value_mut = unsafe { slot.assume_init_mut() };

                        // Casts should be safe, because we never insert more than
                        // u32::MAX-1 slabs and we check that N also fits in u32.
                        let slab_index = self.slab_index as u32;
                        let slot_index = i as u32;

                        return Some((SlabLoc { slab_index, slot_index }, value_mut));
                    }
                }
            }

            self.slab_index += 1;
            self.slot_index = 0;
        }

        None
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let remaining_len = self.len - self.returned;
        (remaining_len, Some(remaining_len))
    }
}

impl<T, const N: usize> ExactSizeIterator for IterMut<'_, T, N> {}

fn allocate_slab<T, A: Allocator, const N: usize>(
    allocator: A,
    slabs: &mut Vec<NonNull<Slab<T, N>>, A>,
) -> *mut Slab<T, N> {
    // Check if the new slab index fits in u32, if usize is larger than u32 and Vec::push below
    // wouldn't catch the overflow. For smaller usizes, we delegate the panic to Vec::push.
    if usize::BITS > u32::BITS {
        let new_slab_index = slabs.len();
        assert!(new_slab_index < u32::MAX as usize);
    }

    let slab_layout = Layout::new::<Slab<T, N>>();
    match allocator.allocate(slab_layout) {
        Ok(ptr) => {
            let ptr: NonNull<Slab<T, N>> = ptr.cast();
            let p = ptr.as_ptr();

            unsafe { (&raw mut (*p).len).write(0) };
            unsafe { (&raw mut (*p).mask).write([false; N]) };

            slabs.push(ptr);

            p
        }
        Err(err) => {
            let size = slab_layout.size();
            let align = slab_layout.align();
            panic!("Failed to allocate layout size={size} align={align}: {err}");
        }
    }
}

fn clone_slab<T: Clone, const N: usize>(dst: *mut Slab<T, N>, src: NonNull<Slab<T, N>>) {
    let src_ref = unsafe { src.as_ref() };

    unsafe { (&raw mut (*dst).len).write(src_ref.len) };
    unsafe { (&raw mut (*dst).mask).write(src_ref.mask) };

    let dst_slice_raw: *mut [MaybeUninit<T>] = unsafe { &raw mut (*dst).data };
    let dst_slice_ptr = dst_slice_raw as *mut MaybeUninit<T>;
    let dst_slice_mut = unsafe { slice::from_raw_parts_mut(dst_slice_ptr, N) };

    for i in 0..N {
        if src_ref.mask[i] {
            let data_ref: &T = unsafe { src_ref.data[i].assume_init_ref() };
            dst_slice_mut[i] = MaybeUninit::new(data_ref.clone());
        }
    }
}

#[cfg(test)]
mod tests {
    use alloc::alloc::Global;
    use core::sync::atomic::AtomicUsize;
    use core::sync::atomic::Ordering;

    use oorandom::Rand32;

    use super::*;

    #[test]
    fn test_slab_general() {
        const SLAB_SIZE: usize = 2;

        let mut s: SlabArray<i32, _, SLAB_SIZE> = SlabArray::new_in(Global);
        assert!(s.len() == 0);
        assert!(s.capacity() == 0);

        let loc0 = s.push(0);
        assert!(
            loc0 == SlabLoc {
                slab_index: 0,
                slot_index: 0
            }
        );
        assert!(s.len() == 1);
        assert!(s.capacity() == SLAB_SIZE);

        let loc1 = s.push(1);
        assert!(
            loc1 == SlabLoc {
                slab_index: 0,
                slot_index: 1
            }
        );
        assert!(s.len() == 2);
        assert!(s.capacity() == SLAB_SIZE);

        let loc2 = s.push(2);
        assert!(
            loc2 == SlabLoc {
                slab_index: 1,
                slot_index: 0
            }
        );
        assert!(s.len() == 3);
        assert!(s.capacity() == 2 * SLAB_SIZE);

        let loc3 = s.push(3);
        assert!(
            loc3 == SlabLoc {
                slab_index: 1,
                slot_index: 1
            }
        );
        assert!(s.len() == 4);
        assert!(s.capacity() == 2 * SLAB_SIZE);

        assert!(s.get(loc0) == Some(&0));
        assert!(s.get(loc1) == Some(&1));
        assert!(s.get(loc2) == Some(&2));
        assert!(s.get(loc3) == Some(&3));

        {
            let value = s.get_mut(loc0).unwrap();
            *value = 1;
        }

        assert!(s.get(loc0) == Some(&1));

        assert!(s.remove(loc0) == Some(1));
        assert!(s.len() == 3);
        assert!(s.capacity() == 2 * SLAB_SIZE);

        assert!(s.remove(loc2) == Some(2));
        assert!(s.len() == 2);
        assert!(s.capacity() == 2 * SLAB_SIZE);

        assert!(s.get(loc0) == None);
        assert!(s.get(loc1) == Some(&1));
        assert!(s.get(loc2) == None);
        assert!(s.get(loc3) == Some(&3));

        let loc00 = s.push(10);
        assert!(loc00 == loc0);
        assert!(s.len() == 3);
        assert!(s.capacity() == 2 * SLAB_SIZE);
    }

    #[test]
    fn test_slab_reserve() {
        const SLAB_SIZE: usize = 2;

        let mut s: SlabArray<i32, _, SLAB_SIZE> = SlabArray::new_in(Global);
        assert!(s.len() == 0);
        assert!(s.capacity() == 0);
        assert!(s.find_next_unoccupied_loc() == None);

        let loc0 = SlabLoc {
            slab_index: 0,
            slot_index: 0,
        };
        let loc1 = SlabLoc {
            slab_index: 0,
            slot_index: 1,
        };
        let _loc2 = SlabLoc {
            slab_index: 1,
            slot_index: 0,
        };
        let _loc3 = SlabLoc {
            slab_index: 1,
            slot_index: 1,
        };

        s.reserve(0);
        assert!(s.len() == 0);
        assert!(s.capacity() == 0);
        assert!(s.find_next_unoccupied_loc() == None);

        s.reserve(1);
        assert!(s.len() == 0);
        assert!(s.capacity() == 2);
        assert!(s.find_next_unoccupied_loc() == Some(loc0));

        s.reserve(2);
        assert!(s.len() == 0);
        assert!(s.capacity() == 2);
        assert!(s.find_next_unoccupied_loc() == Some(loc0));

        s.reserve(1);
        assert!(s.len() == 0);
        assert!(s.capacity() == 2);
        assert!(s.find_next_unoccupied_loc() == Some(loc0));

        s.insert(loc0, 0);
        assert!(s.len() == 1);
        assert!(s.capacity() == 2);
        assert!(s.find_next_unoccupied_loc() == Some(loc1));

        s.reserve(2);
        assert!(s.len() == 1);
        assert!(s.capacity() == 4);
        assert!(s.find_next_unoccupied_loc() == Some(loc1));
    }

    #[test]
    fn test_slab_find_next_unoccupied_insert_reserve_for_loc() {
        const SLAB_SIZE: usize = 2;

        let mut s: SlabArray<i32, _, SLAB_SIZE> = SlabArray::new_in(Global);
        assert!(s.len() == 0);
        assert!(s.capacity() == 0);
        assert!(s.find_next_unoccupied_loc() == None);

        let loc0 = SlabLoc {
            slab_index: 0,
            slot_index: 0,
        };
        let loc1 = SlabLoc {
            slab_index: 0,
            slot_index: 1,
        };
        let loc2 = SlabLoc {
            slab_index: 1,
            slot_index: 0,
        };
        let loc3 = SlabLoc {
            slab_index: 1,
            slot_index: 1,
        };

        s.reserve_for_loc(loc0);
        assert!(s.find_next_unoccupied_loc() == Some(loc0));
        assert!(s.len() == 0);
        assert!(s.capacity() == SLAB_SIZE);

        s.insert(loc0, 0);
        assert!(s.find_next_unoccupied_loc() == Some(loc1));
        assert!(s.len() == 1);
        assert!(s.capacity() == SLAB_SIZE);

        s.reserve_for_loc(loc2);
        assert!(s.find_next_unoccupied_loc() == Some(loc1));
        assert!(s.len() == 1);
        assert!(s.capacity() == 2 * SLAB_SIZE);

        s.insert(loc1, 1);
        assert!(s.find_next_unoccupied_loc() == Some(loc2));
        assert!(s.len() == 2);
        assert!(s.capacity() == 2 * SLAB_SIZE);

        s.insert(loc2, 2);
        assert!(s.find_next_unoccupied_loc() == Some(loc3));
        assert!(s.len() == 3);
        assert!(s.capacity() == 2 * SLAB_SIZE);

        assert!(s.remove(loc1) == Some(1));
        assert!(s.find_next_unoccupied_loc() == Some(loc1));
        assert!(s.len() == 2);
        assert!(s.capacity() == 2 * SLAB_SIZE);
    }

    #[test]
    #[should_panic]
    fn test_slab_insert_oob() {
        const SLAB_SIZE: usize = 2;

        let mut s: SlabArray<i32, _, SLAB_SIZE> = SlabArray::new_in(Global);
        s.insert(
            SlabLoc {
                slab_index: 0,
                slot_index: 0,
            },
            42,
        );
    }

    #[test]
    fn test_slab_with_capacity() {
        const SLAB_SIZE: usize = 2;

        let s: SlabArray<i32, _, SLAB_SIZE> = SlabArray::with_capacity_in(0, Global);
        assert!(s.len() == 0);
        assert!(s.capacity() == 0);
        assert!(s.find_next_unoccupied_loc() == None);

        drop(s);

        let loc0 = SlabLoc {
            slab_index: 0,
            slot_index: 0,
        };
        let loc1 = SlabLoc {
            slab_index: 0,
            slot_index: 1,
        };
        let loc2 = SlabLoc {
            slab_index: 1,
            slot_index: 0,
        };

        let mut s: SlabArray<i32, _, SLAB_SIZE> = SlabArray::with_capacity_in(3, Global);
        assert!(s.len() == 0);
        assert!(s.capacity() == 4);
        assert!(s.find_next_unoccupied_loc() == Some(loc0));

        s.insert(loc1, 1);
        assert!(s.len() == 1);
        assert!(s.capacity() == 4);
        assert!(s.find_next_unoccupied_loc() == Some(loc0));

        s.push(0);
        assert!(s.len() == 2);
        assert!(s.capacity() == 4);
        assert!(s.find_next_unoccupied_loc() == Some(loc2));
    }

    #[test]
    fn test_slab_clone() {
        const SLAB_SIZE: usize = 2;

        let loc0 = SlabLoc {
            slab_index: 0,
            slot_index: 0,
        };
        let loc1 = SlabLoc {
            slab_index: 0,
            slot_index: 1,
        };
        let loc2 = SlabLoc {
            slab_index: 1,
            slot_index: 0,
        };
        let loc3 = SlabLoc {
            slab_index: 1,
            slot_index: 1,
        };

        let mut s1: SlabArray<i32, _, SLAB_SIZE> = SlabArray::new_in(Global);
        s1.push(0);
        s1.push(1);
        s1.push(2);

        assert!(s1.len() == 3);
        assert!(s1.capacity() == 4);
        assert!(s1.find_next_unoccupied_loc() == Some(loc3));

        let s2 = s1.clone();
        assert!(s2.len() == 3);
        assert!(s2.capacity() == 4);
        assert!(s2.find_next_unoccupied_loc() == Some(loc3));

        assert!(s2.get(loc0) == Some(&0));
        assert!(s2.get(loc1) == Some(&1));
        assert!(s2.get(loc2) == Some(&2));
        assert!(s2.get(loc3) == None);

        assert!(s1.get(loc0) == s2.get(loc0));
        assert!(s1.get(loc1) == s2.get(loc1));
        assert!(s1.get(loc2) == s2.get(loc2));
        assert!(s1.get(loc3) == s2.get(loc3));

        drop(s1);

        assert!(s2.len() == 3);
        assert!(s2.capacity() == 4);
        assert!(s2.find_next_unoccupied_loc() == Some(loc3));

        assert!(s2.get(loc0) == Some(&0));
        assert!(s2.get(loc1) == Some(&1));
        assert!(s2.get(loc2) == Some(&2));
        assert!(s2.get(loc3) == None);
    }

    #[test]
    fn test_slab_clone_from() {
        const SLAB_SIZE: usize = 2;

        let loc0 = SlabLoc {
            slab_index: 0,
            slot_index: 0,
        };
        let loc1 = SlabLoc {
            slab_index: 0,
            slot_index: 1,
        };
        let loc2 = SlabLoc {
            slab_index: 1,
            slot_index: 0,
        };
        let loc3 = SlabLoc {
            slab_index: 1,
            slot_index: 1,
        };

        let mut s1: SlabArray<i32, _, SLAB_SIZE> = SlabArray::new_in(Global);
        s1.push(0);
        s1.push(1);
        s1.push(2);

        assert!(s1.len() == 3);
        assert!(s1.capacity() == 4);
        assert!(s1.find_next_unoccupied_loc() == Some(loc3));

        let mut s2: SlabArray<i32, _, SLAB_SIZE> = SlabArray::with_capacity_in(69, Global);
        assert!(s2.len() == 0);
        assert!(s2.capacity() == 70);
        assert!(s2.find_next_unoccupied_loc() == Some(loc0));

        s2.clone_from(&s1);
        assert!(s2.len() == 3);
        assert!(s2.capacity() == 70);
        assert!(s2.find_next_unoccupied_loc() == Some(loc3));

        assert!(s2.get(loc0) == Some(&0));
        assert!(s2.get(loc1) == Some(&1));
        assert!(s2.get(loc2) == Some(&2));
        assert!(s2.get(loc3) == None);

        assert!(s1.get(loc0) == s2.get(loc0));
        assert!(s1.get(loc1) == s2.get(loc1));
        assert!(s1.get(loc2) == s2.get(loc2));
        assert!(s1.get(loc3) == s2.get(loc3));

        drop(s1);

        assert!(s2.len() == 3);
        assert!(s2.capacity() == 70);
        assert!(s2.find_next_unoccupied_loc() == Some(loc3));

        assert!(s2.get(loc0) == Some(&0));
        assert!(s2.get(loc1) == Some(&1));
        assert!(s2.get(loc2) == Some(&2));
        assert!(s2.get(loc3) == None);
    }

    #[test]
    fn test_slab_clear() {
        static COUNT: AtomicUsize = AtomicUsize::new(0);

        // We don't ever read the value inside, but Slab doesn't support ZSTs.
        struct Counted(#[allow(dead_code)] usize);

        impl Counted {
            fn new() -> Self {
                let c = COUNT.fetch_add(1, Ordering::AcqRel);
                Self(c)
            }
        }

        impl Drop for Counted {
            fn drop(&mut self) {
                COUNT.fetch_sub(1, Ordering::AcqRel);
            }
        }

        const SLAB_SIZE: usize = 2;

        let mut s: SlabArray<Counted, _, SLAB_SIZE> = SlabArray::new_in(Global);
        s.push(Counted::new());
        s.push(Counted::new());
        s.push(Counted::new());
        s.push(Counted::new());
        s.push(Counted::new());

        assert!(COUNT.load(Ordering::Acquire) == 5);

        s.clear();

        assert!(COUNT.load(Ordering::Acquire) == 0);
    }

    #[test]
    fn test_slab_drop() {
        static COUNT: AtomicUsize = AtomicUsize::new(0);

        // We don't ever read the value inside, but Slab doesn't support ZSTs.
        struct Counted(#[allow(dead_code)] usize);

        impl Counted {
            fn new() -> Self {
                let c = COUNT.fetch_add(1, Ordering::AcqRel);
                Self(c)
            }
        }

        impl Drop for Counted {
            fn drop(&mut self) {
                COUNT.fetch_sub(1, Ordering::AcqRel);
            }
        }

        const SLAB_SIZE: usize = 2;

        let mut s: SlabArray<Counted, _, SLAB_SIZE> = SlabArray::new_in(Global);
        s.push(Counted::new());
        s.push(Counted::new());
        s.push(Counted::new());
        s.push(Counted::new());
        s.push(Counted::new());

        assert!(COUNT.load(Ordering::Acquire) == 5);

        drop(s);

        assert!(COUNT.load(Ordering::Acquire) == 0);
    }

    #[test]
    fn test_slab_iter() {
        const SLAB_SIZE: usize = 2;

        let mut s: SlabArray<usize, _, SLAB_SIZE> = SlabArray::new_in(Global);
        s.push(0);
        s.push(1);
        s.push(2);
        s.push(3);
        s.push(4);

        let loc0 = SlabLoc {
            slab_index: 0,
            slot_index: 0,
        };
        let loc1 = SlabLoc {
            slab_index: 0,
            slot_index: 1,
        };
        let loc2 = SlabLoc {
            slab_index: 1,
            slot_index: 0,
        };
        let loc3 = SlabLoc {
            slab_index: 1,
            slot_index: 1,
        };
        let loc4 = SlabLoc {
            slab_index: 2,
            slot_index: 0,
        };

        let expected1 = [(loc0, 0), (loc1, 1), (loc2, 2), (loc3, 3), (loc4, 4)];

        assert!(s.len() == expected1.len());
        for (i, (loc, &value)) in s.iter().enumerate() {
            assert!(expected1[i] == (loc, value));
        }

        s.remove(loc1).unwrap();
        s.remove(loc2).unwrap();

        let expected2 = [(loc0, 0), (loc3, 3), (loc4, 4)];

        assert!(s.len() == expected2.len());
        for (i, (loc, &value)) in s.iter().enumerate() {
            assert!(expected2[i] == (loc, value));
        }

        for (loc, value) in s.iter_mut() {
            if loc == loc3 {
                *value = 42;
            }
        }

        let expected3 = [(loc0, 0), (loc3, 42), (loc4, 4)];

        assert!(s.len() == expected3.len());
        for (i, (loc, &value)) in s.iter().enumerate() {
            assert!(expected3[i] == (loc, value));
        }
    }

    #[test]
    fn test_slab_retain() {
        static COUNT: AtomicUsize = AtomicUsize::new(0);

        // We don't ever read the value inside, but Slab doesn't support ZSTs.
        struct Counted(#[allow(dead_code)] usize);

        impl Counted {
            fn new(value: usize) -> Self {
                COUNT.fetch_add(1, Ordering::AcqRel);
                Self(value)
            }
        }

        impl Drop for Counted {
            fn drop(&mut self) {
                COUNT.fetch_sub(1, Ordering::AcqRel);
            }
        }

        const SLAB_SIZE: usize = 2;

        let mut s: SlabArray<Counted, _, SLAB_SIZE> = SlabArray::new_in(Global);
        s.push(Counted::new(0));
        s.push(Counted::new(1));
        s.push(Counted::new(2));
        s.push(Counted::new(3));
        s.push(Counted::new(4));

        let loc0 = SlabLoc {
            slab_index: 0,
            slot_index: 0,
        };
        let loc1 = SlabLoc {
            slab_index: 0,
            slot_index: 1,
        };
        let loc2 = SlabLoc {
            slab_index: 1,
            slot_index: 0,
        };
        let loc3 = SlabLoc {
            slab_index: 1,
            slot_index: 1,
        };
        let loc4 = SlabLoc {
            slab_index: 2,
            slot_index: 0,
        };

        assert!(s.len() == 5);
        assert!(s.capacity() == 6);
        assert!(COUNT.load(Ordering::Acquire) == 5);

        assert!(s[loc0].0 == 0);
        assert!(s[loc1].0 == 1);
        assert!(s[loc2].0 == 2);
        assert!(s[loc3].0 == 3);
        assert!(s[loc4].0 == 4);

        s.retain(|_, value| value.0 % 2 == 1);

        assert!(s.len() == 2);
        assert!(s.capacity() == 6);
        assert!(COUNT.load(Ordering::Acquire) == 2);

        assert!(s.get(loc0).is_none());
        assert!(s[loc1].0 == 1);
        assert!(s.get(loc2).is_none());
        assert!(s[loc3].0 == 3);
        assert!(s.get(loc4).is_none());
    }

    // This just exists so that we can run with miri every time, but if we are serious, we should be
    // running miri on the larget fuzz test too from time to time.
    #[test]
    fn fuzz_slab_very_cheap() {
        fuzz_slab::<1>(100);
        fuzz_slab::<16>(100);
        fuzz_slab::<256>(100);
    }

    #[test]
    #[cfg_attr(miri, ignore)]
    fn fuzz_slab_cheap() {
        fuzz_slab::<1>(1000);
        fuzz_slab::<2>(1000);
        fuzz_slab::<4>(1000);
        fuzz_slab::<8>(1000);
        fuzz_slab::<16>(1000);
        fuzz_slab::<32>(1000);
        fuzz_slab::<256>(1000);
        fuzz_slab::<1024>(1000);
    }

    fn fuzz_slab<const N: usize>(count: usize) {
        let mut r = Rand32::new(0);
        let mut s: SlabArray<i32, _, N> = SlabArray::new_in(Global);

        fn rand_loc(r: &mut Rand32) -> SlabLoc {
            SlabLoc {
                slab_index: r.rand_u32() % 1000,
                slot_index: r.rand_u32() % 1000,
            }
        }

        // Try a lower iteration count when running with miri. It can be quite the wait.
        for _ in 0..count {
            match r.rand_u32() % 9 {
                0 => {
                    s.push(r.rand_i32());
                }

                1 => {
                    let _ = s.get(rand_loc(&mut r));
                }

                2 => {
                    if let Some(value) = s.get_mut(rand_loc(&mut r)) {
                        *value = r.rand_i32();
                    }
                }

                3 => {
                    let _ = s.remove(rand_loc(&mut r));
                }

                4 => {
                    for (_, value) in s.iter() {
                        let _v = *value;
                    }
                }

                5 => {
                    for (loc, value) in s.iter_mut() {
                        if (loc.slab_index + loc.slot_index) % 2 == 0 {
                            *value = r.rand_i32();
                        }
                    }
                }

                #[allow(clippy::assigning_clones)]
                // We are explicitly testing .clone(), Clippy, please.
                6 => {
                    s = s.clone();
                }

                7 => {
                    let mut t: SlabArray<i32, _, N> = SlabArray::with_capacity_in(r.rand_u32() as usize % 1000, Global);
                    t.clone_from(&s);

                    s = t;
                }

                8 => {
                    s.clear();
                }

                _ => unreachable!(),
            }
        }
    }
}
