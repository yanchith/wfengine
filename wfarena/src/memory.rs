use core::alloc::Layout;
use core::mem;
use core::mem::MaybeUninit;
use core::ops::Deref;
use core::ops::DerefMut;
use core::ptr::NonNull;

use crate::arena::Arena;
use crate::arena::ArenaScope;

// TODO(jt): @Correctness @Cleanup This file is one of the hairiest uses of unsafe in the entire
// codebase. There's raw pointers, uninitialized memory, static lifetime extension, pinning and
// self-referentiality. Currently, it seems to be working well, but initially miri detected aliasing
// violations with its Stacked Borrows model in the test_memory test. These were fixed (by changing
// &mut Pinned to *mut Pinned), so that the mutable reference to the entire pinned structure doesn't
// compete with the shared self-reference inside the pinned structure. Stacked Borrows reported a
// retagging there, either because the memory location of these references was the same, or maybe
// because of some pointer provenance thing I don't understand. Can we clean this up somehow? Maybe with
// some newer borrowchecker, in the future?

struct Pinned {
    arena: MaybeUninit<Arena>,
    scope: MaybeUninit<ArenaScope<'static>>,
}

/// A safe-ish abstraction for an [`Arena`] coupled with data utilizing the arena for its
/// allocations. Dereferences to `<T>`.
///
/// Allows mostly-safe resetting the arena and reinitializing the data with [`Memory::reset`]. Also
/// allows for mostly-safe dropping of the data and reclaiming of the arena with
/// [`Memory::drop_and_reclaim_arena`].
///
/// Default [`Drop`] leaks both the arena and the data.
///
/// [`Memory`] can be moved around like a regular struct, even though the data has a shared
/// reference to the arena. This is achieved by storing the arena in stable memory (inside its own
/// allocation) and passing a derived [`ArenaScope`] to the initializer. The main arena is not reset
/// while [`Memory`] is live, only the scoped arena is.
///
/// # Safety
///
/// The only unsafety comes from the fact that the arena could leak outside, e.g. with
/// [`Vec::allocator`], and once leaked, resetting the arena is UB.
///
/// For instance, there may be data around that used the leaked arena for its allocations. The data
/// is now potentially overwritten.
///
/// However, more subtly, just storing the shared reference to the arena and calling [`Memory::reset`]
/// is already UB, because resetting takes a `&mut` to the arena, and `&mut`s must be unique.
pub struct Memory<T> {
    // SAFETY: The implicitly generated drop code leaks the pinned arena so that it doesn't attempt
    // to deallocate (in case it was used to allocate memory not tracked by us). It also leaks the
    // value, although we could try to correctly drop that, instead.
    pinned: *mut Pinned,
    initializer: fn(arena: &'static Arena) -> T,
    value: MaybeUninit<T>,
}

impl<T> Memory<T> {
    /// Creates new [`Memory`] by taking an arena and an initializer for the data.
    pub fn new(arena: Arena, initializer: fn(arena: &'static Arena) -> T) -> Self {
        // Because we'll be giving out static references to the arena (or more precisely, its scoped
        // arena), we must make sure that the arena and its scope don't move until this struct is
        // dropped.
        //
        // We ensure its stable memory location by allocating a bit of memory from the arena itself
        // and copying the arena inside.
        //
        // To not lose the arena (stored in itself) when it is reset, we also store an arena scope
        // in the stable memory. This scope is the one we'll be handing out to the initializer
        // function and resetting in Memory::reset, so that the allocation containing the arena and
        // the scope is left untouched.

        // Prepare uninitialized memory to store the arena and the scope. The memory is allocated
        // from the arena itself.
        let pinned_bytes: NonNull<[u8]> = arena.allocate(Layout::new::<Pinned>()).unwrap();
        let pinned: NonNull<Pinned> = pinned_bytes.cast();

        // SAFETY: The pointer is properly aligned, because we allocated it with the type's layout.
        unsafe {
            pinned.write(Pinned {
                arena: MaybeUninit::uninit(),
                scope: MaybeUninit::uninit(),
            });
        }

        let pinned: *mut Pinned = pinned.as_ptr();

        // Move arena in, and only then create a scope from it, because the scope has a reference to
        // the arena, which we need to be valid for the entirety of its existence.
        //
        // SAFETY: Multiple things happen here:
        //
        // - We are writing to a pointer we just created, so it should be valid.
        //
        // - We can assume_init the arena, because we just initialized it.
        //
        // - We extend the lifetime of the scope's internal reference, because we there's no way to
        //   tie it to the same struct (yet? maybe ever, if we consider mutability). This erasing is
        //   safe, because we never move the arena until Memory::drop_and_reclaim_arena, where we
        //   drop the scope first.
        unsafe {
            (*pinned).arena.write(arena);
            (*pinned)
                .scope
                .write(static_scope((*pinned).arena.assume_init_ref().scope_unchecked()));
        }

        // SAFETY: The pinned arenas are always initialized after this point.
        let scope = unsafe { (*pinned).scope.assume_init_ref() };

        // SAFETY: It is okay to pretend the reference to the scoped arena lives forever, because
        // the value will always be dropped before it would observe any violations - either us
        // dropping the scoped arena in Memory::drop_and_reclaim_arena, or the reference becoming
        // invalid in Memory::reset, where we create a &mut to the same arena.
        let value = initializer(unsafe { static_lifetime(scope.arena()) });

        Self {
            pinned,
            initializer,
            value: MaybeUninit::new(value),
        }
    }

    // TODO(jt): @Speed Make a version of this function that doesn't run the drop code.
    /// Drops the value, resets the arena and then re-initializes the value with the initializer.
    ///
    /// # Safety
    ///
    /// This is unsafe, because we can't prove a reference to the arena hasn't been leaked from the
    /// dereferenced value and used to allocate data outside of [`Self`].
    ///
    /// Note that just storing a shared reference to the arena outside of [`Self`], even if it is
    /// not used, is UB once [`Self::reset`] is called.
    pub unsafe fn reset(memory: &mut Self) {
        // SAFETY: The value is almost always initialized, except right after this point, but we
        // promptly reinitialize it directly afterwards.
        unsafe {
            memory.value.assume_init_drop();
        }

        // SAFETY: The pinned arenas are always initialized.
        let scope = unsafe { (*memory.pinned).scope.assume_init_mut() };

        scope.arena_mut().reset();

        // SAFETY: It is okay to pretend the reference to the scoped arena lives forever, because
        // the value will always be dropped before it would observe any violations - either us
        // dropping the scoped arena in Memory::drop_and_reclaim_arena, or the reference becoming
        // invalid in Memory::reset (this function), where we create a &mut to the same arena.
        //
        // TODO(jt): @Correctness If the initializer panics, our assume_init_drop assumptions above
        // might not be valid. Should we worry about this?
        let value = (memory.initializer)(unsafe { static_lifetime(scope.arena()) });

        memory.value.write(value);
    }

    pub fn allocated_size(memory: &Self) -> usize {
        // SAFETY: The pinned arenas are always initialized.
        let scope = unsafe { (*memory.pinned).scope.assume_init_ref() };
        scope.arena().allocated_size()
    }

    pub fn reserved_size(memory: &Self) -> usize {
        // SAFETY: The pinned arenas are always initialized.
        let scope = unsafe { (*memory.pinned).scope.assume_init_ref() };
        scope.arena().reserved_size()
    }

    /// Drops the value and returns the reset arena.
    ///
    /// Unlike the implicit [`Drop`], this doesn't leak the value or the arena, but is therefore
    /// unsafe, because we can't prove the arena hasn't been used to allocate things outside of
    /// `Memory<T>`.
    ///
    /// # Safety
    ///
    /// This is unsafe, because we can't prove a reference to the arena hasn't been leaked from the
    /// dereferenced value and used to allocate data outside of [`Self`].
    ///
    /// Note that just storing a shared reference to the arena outside of [`Self`], even if it is
    /// not used, is UB once [`Self::drop_and_reclaim_arena`] is called.
    #[allow(dead_code)]
    pub unsafe fn drop_and_reclaim_arena(mut memory: Self) -> Arena {
        // SAFETY: The value is initialized at this point.
        unsafe {
            memory.value.assume_init_drop();
        }

        // SAFETY: The scope is initialized. Drop it first, becuase its drop code will modify the
        // arena in the stable location.
        unsafe {
            (*memory.pinned).scope.assume_init_drop();
        }

        // SAFETY: The arena is initialized. We read it out only after we have dropped the scope, so
        // no reference points to it.
        let mut arena = unsafe { (*memory.pinned).arena.assume_init_read() };
        arena.reset();

        // We could reconstruct the box and drop it here, but that wouldn't really do anything. It
        // is still in our arena, but no-one can access it anymore. Once the arena allocates new
        // things, it will be overwritten.

        arena
    }
}

impl<T> Deref for Memory<T> {
    type Target = T;

    fn deref(&self) -> &T {
        unsafe { self.value.assume_init_ref() }
    }
}

impl<T> DerefMut for Memory<T> {
    fn deref_mut(&mut self) -> &mut T {
        unsafe { self.value.assume_init_mut() }
    }
}

unsafe fn static_lifetime(arena: &Arena) -> &'static Arena {
    unsafe { mem::transmute(arena) }
}

unsafe fn static_scope(scope: ArenaScope<'_>) -> ArenaScope<'static> {
    unsafe { mem::transmute(scope) }
}

#[cfg(test)]
mod tests {
    extern crate alloc;

    use alloc::vec;
    use alloc::vec::Vec;
    use core::ptr::NonNull;

    use super::*;

    #[test]
    fn test_memory() {
        struct State<'a> {
            data: Vec<u8, &'a Arena>,
        }

        fn default_state(arena: &'static Arena) -> State<'static> {
            State {
                data: Vec::new_in(arena),
            }
        }

        let allocated_memory = vec![0u8; 1024 << 20];

        let allocated_memory_capacity = allocated_memory.capacity();
        let allocated_memory_slice = allocated_memory.leak();
        let allocated_memory_ptr = NonNull::new(allocated_memory_slice.as_mut_ptr()).unwrap();
        let allocated_memory_len = allocated_memory_slice.len();

        let arena_result = unsafe { Arena::with_first_block(allocated_memory_ptr, allocated_memory_len) };

        let arena = arena_result.unwrap();

        let mut memory = Memory::new(arena, default_state);
        assert!(memory.data.len() == 0);

        memory.data.push(42);
        assert!(memory.data.len() == 1);
        assert!(memory.data[0] == 42);

        unsafe { Memory::reset(&mut memory) };
        assert!(memory.data.len() == 0);

        memory.data.push(43);
        assert!(memory.data.len() == 1);
        assert!(memory.data[0] == 43);

        let mut arena = unsafe { Memory::drop_and_reclaim_arena(memory) };

        arena.reset();

        drop(arena);

        // Deallocate, so that miri doesn't complain.
        unsafe {
            Vec::from_raw_parts(
                allocated_memory_ptr.as_ptr(),
                allocated_memory_len,
                allocated_memory_capacity,
            );
        }
    }
}
