use core::alloc::AllocError;
use core::alloc::Allocator;
use core::alloc::Layout;
use core::cell::Cell;
use core::ffi::c_void;
use core::mem::ManuallyDrop;
use core::num::NonZeroUsize;
use core::ops::Deref;
use core::ptr;
use core::ptr::NonNull;

pub const ARENA_HEADER_SIZE: usize = size_of::<ArenaBlockHeader>();

const FLAG_NONE: u64 = 0;
const FLAG_CAN_CHAIN: u64 = 1 << 0;
const FLAG_CAN_RELEASE_FIRST_BLOCK: u64 = 1 << 1;

#[repr(C, align(8))]
struct ArenaBlockHeader {
    size_before_this_block: usize,

    base: NonNull<u8>,
    start: NonNull<u8>,
    end: NonNull<u8>,

    allocated: Cell<NonNull<u8>>,
    committed: Cell<NonNull<u8>>,

    reserve_size: usize,

    prev: *mut ArenaBlockHeader,
}

#[derive(Debug)]
pub struct ArenaInitError;

// TODO(jt): Add an optional FREELIST compile-time parameter (const generic). Ryan's arena in
// RADDebugger has it too. It could be useful to have a more a flexible arena for longer lifetime
// objects that we not just allocate, but also occasionally deallocate, like a strings or geometry
// storage.
//
// TODO(jt): If we allowed de-committing memory (via vm_decommit) when resetting down to a size,
// we'd gain the benefit of being able to release memory back to the OS even within one reserved
// block, making the huge-reserved-block implementations have the benefits of both worlds. The
// branch predictor could even predict we won't need to check for block overruns.

/// An arena. Implements [`core::alloc::Allocator`].
#[repr(C)]
#[derive(Debug)]
pub struct Arena {
    current: Cell<NonNull<ArenaBlockHeader>>,

    // TODO(jt): We can in theory pass malloc/free as vm_reserve/vm_release and a noop for
    // vm_commit. This would make it compatible with systems without virtual memory. Let's test this
    // out, and document it, if it is possible.
    vm_reserve: fn(size: usize) -> *mut c_void,
    vm_commit: unsafe fn(ptr: *mut c_void, size: usize) -> bool,
    vm_release: unsafe fn(ptr: *mut c_void, size: usize),
    vm_page_size: usize,

    reserve_size: usize,
    commit_size: usize,

    flags: u64,

    reset_min: usize,

    #[cfg(any(debug_assertions, feature = "safe_scopes"))]
    scoped: Cell<bool>,
}

impl Arena {
    /// Creates an arena backed by a single memory block, from which it fulfills allocation
    /// requests.
    ///
    /// # Safety
    ///
    /// The memory described by the `block` pointer and `size` must be *dereferencable*, as
    /// described in [`core::ptr`] module documentation.
    pub unsafe fn with_memory_block(block: NonNull<u8>, size: usize) -> Result<Self, ArenaInitError> {
        fn noop_reserve(_size: usize) -> *mut c_void {
            ptr::null_mut()
        }

        fn noop_commit(_ptr: *mut c_void, _size: usize) -> bool {
            false
        }

        fn noop_release(_ptr: *mut c_void, _size: usize) {}

        unsafe {
            Self::with_optional_memory_block_and_virtual_memory(
                Some(block),
                size,
                noop_reserve,
                noop_commit,
                noop_release,
                // page_size, reserve_size, and commit_size must be nonzero, so that align_to doesn't assert,
                // but they will never be used, because chaining is disabled.
                1,
                1,
                1,
                false,
            )
        }
    }

    /// Creates an arena that acquires its memory from the virtual memory system.
    ///
    /// The arena starts out by reserving a block of memory, and will use that to fulfill allocation
    /// requests. When the block runs out and `allow_chaining` is set, the arena allocates another
    /// block.
    ///
    /// Each allocated blocks starts out with at least `reserve_size` reserved memory and
    /// `commit_size` pre-committed memory, however these are adjusted by the page size as well as
    /// the size and alignment of the current allocation request.
    ///
    /// `vm_reserve` is used to reserve pages from the OS. A valid implementation can e.g. call
    /// `VirtualAlloc` with `MEM_RESERVE` on Windows or `mmap(2)` with `MAP_PRIVATE|MAP_ANONYMOUS`
    /// on Unix. On failure, `vm_reserve` returns a null pointer.
    ///
    /// `vm_commit` is used to commit reserved pages. This can be `VirtualAlloc` with `MEM_COMMIT`
    /// on Windows, and `mprotect(2)` with `PROT_READ|PROT_WRITE` on Unix, unless the memory
    /// was already reserved with these flags, in which case the OS commits it automatically as it
    /// is accessed. On failure, `vm_commit` returns false.
    ///
    /// Finally, `vm_release` is used to release pages back to the OS. This can be a `VirtualFree`
    /// call on Windows, or a `munmap(2)` on Unix.
    ///
    /// `vm_page_size` is the page size on the current system. It can be queried with
    /// `GetSystemInfo` on Windows, and with `sysconf(_SC_PAGE_SIZE)` on Unix.
    ///
    /// # Safety
    ///
    /// `vm_reserve`, `vm_commit` and `vm_release` must do what they say they do.
    ///
    /// `vm_page_size` must be the page size on the current system.
    pub unsafe fn with_virtual_memory(
        vm_reserve: fn(size: usize) -> *mut c_void,
        vm_commit: unsafe fn(ptr: *mut c_void, size: usize) -> bool,
        vm_release: unsafe fn(ptr: *mut c_void, size: usize),
        vm_page_size: usize,
        reserve_size: usize,
        commit_size: usize,
        allow_chaining: bool,
    ) -> Result<Self, ArenaInitError> {
        unsafe {
            Self::with_optional_memory_block_and_virtual_memory(
                None,
                0,
                vm_reserve,
                vm_commit,
                vm_release,
                vm_page_size,
                reserve_size,
                commit_size,
                allow_chaining,
            )
        }
    }

    /// Creates an arena that acquires its memory from the virtual memory system, but the first
    /// block can be passed in by the caller.
    ///
    /// # Safety
    ///
    /// See [`Self::with_memory_block`] and [`Self::with_virtual_memory`] for more information.
    pub unsafe fn with_optional_memory_block_and_virtual_memory(
        block: Option<NonNull<u8>>,
        size: usize,
        vm_reserve: fn(size: usize) -> *mut c_void,
        vm_commit: unsafe fn(ptr: *mut c_void, size: usize) -> bool,
        vm_release: unsafe fn(ptr: *mut c_void, size: usize),
        vm_page_size: usize,
        reserve_size: usize,
        commit_size: usize,
        allow_chaining: bool,
    ) -> Result<Self, ArenaInitError> {
        assert!(reserve_size.is_power_of_two());
        assert!(commit_size.is_power_of_two());
        assert!(reserve_size >= commit_size);

        // Make sure we can at least allocate the header in new blocks.
        let reserve_size = align_to(reserve_size, usize::max(vm_page_size, size_of::<ArenaBlockHeader>()));
        let commit_size = align_to(commit_size, usize::max(vm_page_size, size_of::<ArenaBlockHeader>()));

        let mut flags = FLAG_NONE;
        if allow_chaining {
            flags |= FLAG_CAN_CHAIN;
        }

        let current = if let Some(block) = block {
            // block:       pointer to the block the user provided
            // block_base:  pointer to our block header, same as block if it was sufficiently aligned
            // block_start: pointer to start of memory we will be allocating
            // block_end:   pointer to the byte one past the memory we will be allocating

            let block_addr = addr(block);

            // Can we compute the end of our memory block without overflowing?
            let block_end_addr = block_addr.checked_add(size).ok_or(ArenaInitError)?;

            // Will we be able to service at least one largest possible allocation request without
            // overflowing?
            //
            // For that to be true, our end pointer must be smaller or equal to isize::MAX, because
            // Layout::from_size_align says that size, rounded up to the nearest multiple of align, must
            // not overflow isize. Basically, because...
            //
            //     isize::MAX as usize + isize::MAX as usize < usize::MAX
            //
            // ... we guarantee here, that we are able to do at least one add without overflowing usize,
            // after which the regular out-of-memory branch will trigger, because the resulting pointer
            // would be after our end pointer.
            //
            // The above is a theoretical worry anyway, as on today's hardware we don't really use the
            // full 64 bits of a pointer. E.g. On Intel, even with 5-level paging, only 57-bits are used.
            if block_end_addr > isize::MAX as usize {
                return Err(ArenaInitError);
            }

            // Align the block for header and check whether we have enough space to store it.
            let block_base_addr = align_to(block_addr, align_of::<ArenaBlockHeader>());
            if block_end_addr < block_base_addr {
                return Err(ArenaInitError);
            }
            if block_end_addr - block_base_addr < size_of::<ArenaBlockHeader>() {
                return Err(ArenaInitError);
            }

            let block_start_addr = block_base_addr + size_of::<ArenaBlockHeader>();

            let block_base = block.with_addr(unsafe { nz(block_base_addr) });
            let block_start = block.with_addr(unsafe { nz(block_start_addr) });
            let block_end = block.with_addr(unsafe { nz(block_end_addr) });

            let header = ArenaBlockHeader {
                size_before_this_block: 0,

                base: block_base,
                start: block_start,
                end: block_end,

                allocated: Cell::new(block_start),
                committed: Cell::new(block_end), // The given block is already commited.

                reserve_size: size, // Won't be read

                prev: ptr::null_mut(),
            };

            let current: NonNull<ArenaBlockHeader> = block_base.cast();
            unsafe { current.write(header) };

            current
        } else {
            flags |= FLAG_CAN_RELEASE_FIRST_BLOCK;

            let current = allocate_and_init_block(
                vm_reserve,
                vm_commit,
                vm_page_size,
                reserve_size,
                commit_size,
                ptr::null_mut(),
                0,
            )
            .map_err(|_| ArenaInitError)?;

            current
        };

        Ok(Self {
            current: Cell::new(current),

            vm_reserve,
            vm_commit,
            vm_release,
            vm_page_size,

            reserve_size,
            commit_size,

            flags,

            reset_min: 0,

            #[cfg(any(debug_assertions, feature = "safe_scopes"))]
            scoped: Cell::new(false),
        })
    }

    /// Creates a scoped arena out of this one.
    ///
    /// Once the scoped arena is done being used and dropped, this arena reclaims all memory used by
    /// the scope.
    ///
    /// This arena can't be used to allocate new allocations or grow existing allocations while the
    /// guard ([`ArenaScope`]) returned by this function is dropped, nor can it create more
    /// scopes. This is ensured with runtime checks.
    ///
    /// # Warning
    ///
    /// Even with all the runtime checks, it is still possible to use scopes incorrectly without
    /// realizing it. The following code does not panic, but it could as well, if the initial
    /// capacity of `shared_data` changed.
    ///
    /// ```no_run
    /// #![feature(allocator_api)]
    ///
    /// use wfarena::Arena;
    ///
    /// fn use_arena_scopes_incorrectly(arena: &Arena) {
    ///     let mut shared_data: Vec<i32, _> = Vec::with_capacity_in(4, arena);
    ///
    ///     shared_data.push(1);
    ///     shared_data.push(3);
    ///     shared_data.push(3);
    ///
    ///     {
    ///         let scope = arena.scope();
    ///
    ///         // we haven't exhausted the vector's capacity, so it doesn't request more memory from
    ///         // the arena here. If it had, the code would panic.
    ///         shared_data.push(7);
    ///     }
    /// }
    /// ```
    #[cfg(feature = "safe_scopes")]
    pub fn scope(&self) -> ArenaScope<'_> {
        #[cfg(any(debug_assertions, feature = "safe_scopes"))]
        {
            assert!(!self.scoped.get(), "Can't use, if there's an active scope");
            self.scoped.set(true);
        }

        let reset = self.allocated_size();

        ArenaScope {
            arena: ManuallyDrop::new(Arena {
                current: Cell::new(self.current.get()),

                vm_reserve: self.vm_reserve,
                vm_commit: self.vm_commit,
                vm_release: self.vm_release,
                vm_page_size: self.vm_page_size,

                reserve_size: self.reserve_size,
                commit_size: self.commit_size,

                flags: self.flags,

                reset_min: reset,

                #[cfg(any(debug_assertions, feature = "safe_scopes"))]
                scoped: Cell::new(false),
            }),
            arena_parent: self,

            reset,
        }
    }

    /// Creates a scoped arena out of this one.
    ///
    /// When built with with debug_assertions or with the safe_scopes feature, this is identical to
    /// [`Self::scope`], except it is still marked unsafe.
    ///
    /// In builds without debug_assertions or the safe_scopes feature, arenas won't perform checks
    /// on the hot path of allocation, and it is possible to for code that uses scopes to UB.
    ///
    /// # Safety
    ///
    /// In builds with debug_assertions or the safe_scopes feature, this is completely safe at the
    /// cost of an additional branch for every allocator operation. Instead of invoking UB,
    /// incorrect usage will cause a panic.
    ///
    /// In builds without debug_assertions or the safe_scopes feature, the caller is responsible for
    /// not using this arena to allocate, or make other scopes while the scope exists, otherwise UB
    /// can happen by the arena handing out the same memory to mutlitple callers.
    ///
    /// Even in the unsafe builds, there is a runtime check outside of hot code, in the drop
    /// implementation for [`ArenaScope`], to help detect incorrect usage. However, at the time the
    /// check would run, UB has already been invoked, so it can't be relied upon to always trigger.
    pub unsafe fn scope_unchecked(&self) -> ArenaScope<'_> {
        #[cfg(any(debug_assertions, feature = "safe_scopes"))]
        {
            assert!(!self.scoped.get(), "Can't use, if there's an active scope");
            self.scoped.set(true);
        }

        let reset = self.allocated_size();

        ArenaScope {
            arena: ManuallyDrop::new(Arena {
                current: Cell::new(self.current.get()),

                vm_reserve: self.vm_reserve,
                vm_commit: self.vm_commit,
                vm_release: self.vm_release,
                vm_page_size: self.vm_page_size,

                reserve_size: self.reserve_size,
                commit_size: self.commit_size,

                flags: self.flags,

                reset_min: reset,

                #[cfg(any(debug_assertions, feature = "safe_scopes"))]
                scoped: Cell::new(false),
            }),
            arena_parent: self,

            reset,
        }
    }

    /// Returns amount of allocated memory in bytes, including internal bookkeeping.
    ///
    /// This value can be stale, if there is a scope (e.g. [`Self::scope_unchecked`]) active.
    pub fn allocated_size(&self) -> usize {
        let current = unsafe { self.current.get().as_ref() };

        let size_before_current = current.size_before_this_block;
        let base = addr(current.base);
        let allocated = addr_in_cell(&current.allocated);

        debug_assert!(base <= allocated);
        allocated - base + size_before_current
    }

    /// Returns amount of commited memory in bytes, including internal bookkeeping.
    ///
    /// This value can be stale, if there is a scope (e.g. [`Self::scope_unchecked`]) active.
    pub fn committed_size(&self) -> usize {
        let current = unsafe { self.current.get().as_ref() };

        let size_before_current = current.size_before_this_block;
        let base = addr(current.base);
        let committed = addr_in_cell(&current.committed);

        debug_assert!(base <= committed);
        committed - base + size_before_current
    }

    /// Returns amount of reserved memory in bytes, including internal bookkeeping.
    ///
    /// This value can be stale, if there is a scope (e.g. [`Self::scope_unchecked`]) active.
    pub fn reserved_size(&self) -> usize {
        let current = unsafe { self.current.get().as_ref() };

        let size_before_current = current.size_before_this_block;
        let base = addr(current.base);
        let end = addr(current.end);

        debug_assert!(base <= end);
        end - base + size_before_current
    }

    /// Returns number of reserved blocks.
    ///
    /// This value can be stale, if there is a scope (e.g. [`Self::scope_unchecked`]) active.
    pub fn count_blocks(&self) -> usize {
        let mut block_count = 1;

        let mut current = unsafe { self.current.get().as_ref() };
        while current.prev != ptr::null_mut() {
            current = unsafe { &*current.prev };
            block_count += 1;
        }

        block_count
    }

    /// Resets the arena and releases all blocks back to the OS except the very first block.
    #[inline]
    pub fn reset(&mut self) {
        self.reset_to(0);
    }

    /// Resets the arena down to given position and releases all blocks above it to the OS except
    /// the very first block.
    #[inline]
    pub fn reset_to(&mut self, reset: usize) {
        // SAFETY: This is fine, because we have a &mut.
        unsafe { self.reset_to_unchecked(reset) };
    }

    /// Resets the arena down to given position and releases all blocks above it to the OS except
    /// the very first block.
    ///
    /// This is unsafe, because it only requires a shared reference, making it possible to
    /// invalidate allocated data.
    ///
    /// # Safety
    ///
    /// After resetting, data stored above the reset mark becomes eligible to overwrite, but this is
    /// not enforced by Rust's lifetime tracking.
    ///
    /// Accessing data after being overwritten will likely lead to UB, either because of aliasing
    /// violations, or type specific constaints being violated.
    ///
    /// Thus, care must be made to not reset the arena below where live data resides.
    #[inline]
    pub unsafe fn reset_to_unchecked(&self, reset: usize) {
        let reset = usize::max(self.reset_min, reset);

        let vm_release = self.vm_release;
        let mut current = unsafe { self.current.get().as_ref() };

        while current.prev != ptr::null_mut() && current.size_before_this_block > reset {
            let prev = current.prev;
            unsafe {
                vm_release(current as *const ArenaBlockHeader as *mut c_void, current.reserve_size);
            }

            current = unsafe { &*prev };
        }

        let size_before_current = current.size_before_this_block;
        let base = addr(current.base);
        let reset_bottom = size_before_current + size_of::<ArenaBlockHeader>();

        let new_allocated = usize::max(reset_bottom, reset) - size_before_current + base;
        current
            .allocated
            .set(current.base.with_addr(unsafe { nz(new_allocated) }));

        self.current.set(NonNull::from(current));
    }

    /// Allocates the arena in its own memory and returns a reference with a static lifetime.
    ///
    /// Leaked arenas are useful, because they can be stored in structs along with things using the them.
    ///
    /// ```
    /// #![feature(allocator_api)]
    ///
    /// use wfarena::Arena;
    ///
    /// struct Entity {
    ///     x: f32,
    ///     y: f32,
    ///     hp: u32,
    /// }
    ///
    /// struct Entities {
    ///     arena: &'static Arena,
    ///     data: Vec<Entity, &'static Arena>,
    /// }
    ///
    /// impl Entities {
    ///     fn new(arena: Arena) -> Self {
    ///         let arena = arena.leak().unwrap();
    ///         Self {
    ///             arena,
    ///             data: Vec::new_in(arena),
    ///         }
    ///     }
    /// }
    /// ```
    ///
    /// Drop code, if any, doesn't run for leaked arenas. To drop a leaked Arena, call the unsafe
    /// [`ptr::read`] first.
    ///
    /// Resetting leaked arenas is always incorrect, as it overwrites the arena's own data.
    pub fn leak(self) -> Result<&'static Self, AllocError> {
        let p: NonNull<[u8]> = self.allocate(Layout::new::<Self>())?;
        let p: NonNull<Self> = p.cast();
        unsafe {
            p.write(self);
        }

        Ok(unsafe { p.as_ref() })
    }

    /// Attempts to allocate a block memory.
    ///
    /// Allocating can fail, in which case [`AllocError`] is returned.
    #[inline(always)] // Disable #[inline] temporarily to view asm.
    pub fn allocate(&self, layout: Layout) -> Result<NonNull<[u8]>, AllocError> {
        #[cfg(any(debug_assertions, feature = "safe_scopes"))]
        {
            assert!(!self.scoped.get(), "Can't use, if there's an active scope");
        }

        let mut current = unsafe { self.current.get().as_ref() };
        let mut allocated_pre = align_to(addr_in_cell(&current.allocated), layout.align());
        let mut allocated_post = allocated_pre + layout.size();

        #[cfg(debug_assertions)]
        {
            let current_base = addr(current.base);
            let current_start = addr(current.start);
            let current_end = addr(current.end);

            let current_allocated = addr_in_cell(&current.allocated);
            let current_committed = addr_in_cell(&current.committed);

            debug_assert!(current_base < current_end);
            debug_assert!(current_start < current_end);
            debug_assert!(current_allocated <= current_end);
            debug_assert!(current_allocated <= current_committed);
            debug_assert!(current_committed <= current_end);

            // These asserts only hold if the first block was allocated from virtual memory, but not
            // when it was given to us by the caller.
            //
            // debug_assert!(align_to(current_base, self.vm_page_size) == current_base);
            // debug_assert!(align_to(current_commit, self.vm_page_size) == current_commit);
            // debug_assert!(align_to(current_end, self.vm_page_size) == current_end);
        }

        // Reserve a new block, if needed
        if allocated_post > addr(current.end) {
            if self.flags & FLAG_CAN_CHAIN == 0 {
                return Err(AllocError);
            }

            let required_size = align_to(size_of::<ArenaBlockHeader>(), layout.align()) + layout.size();
            let mut reserve_size = self.reserve_size;
            let mut commit_size = self.commit_size;

            if required_size > reserve_size {
                let aligned_required_size = align_to(required_size, self.vm_page_size);
                reserve_size = aligned_required_size;
                commit_size = aligned_required_size;
            }

            let block_base = allocate_and_init_block(
                self.vm_reserve,
                self.vm_commit,
                self.vm_page_size,
                reserve_size,
                commit_size,
                self.current.get().as_ptr(),
                addr(current.end) - addr(current.base) + current.size_before_this_block,
            )?;

            self.current.set(block_base);

            current = unsafe { block_base.as_ref() };
            allocated_pre = align_to(addr_in_cell(&current.allocated), layout.align());
            allocated_post = allocated_pre + layout.size();
        }

        // Commit pages, if needed
        if allocated_post > addr_in_cell(&current.committed) {
            if addr_in_cell(&current.committed) == addr(current.end) {
                return Err(AllocError);
            }

            let vm_commit = self.vm_commit;

            let committed_post = usize::min(align_to(allocated_post, self.commit_size), addr(current.end));
            let commit_size = committed_post - addr_in_cell(&current.committed);
            if unsafe { !vm_commit(current.committed.get().as_ptr() as *mut c_void, commit_size) } {
                return Err(AllocError);
            }

            let new_committed = unsafe { current.committed.get().add(commit_size) };
            current.committed.set(new_committed);
        }

        current
            .allocated
            .set(current.base.with_addr(unsafe { nz(allocated_post) }));
        let ptr = current.base.with_addr(unsafe { nz(allocated_pre) });

        Ok(NonNull::slice_from_raw_parts(ptr, layout.size()))
    }

    /// Deallocate the memory referenced by `ptr`.
    ///
    /// Deallocating always succeeds, but memory is not reclaimed. To reclaim memory, see
    /// [`Arena::reset`] and [`Arena::scope`] or [`Arena::scope_unchecked`].
    ///
    /// # Safety
    ///
    /// This is actually safe, even if [`Allocator::deallocate`] is not.
    #[inline(always)] // Disable #[inline] temporarily to view asm.
    pub unsafe fn deallocate(&self, _ptr: NonNull<u8>, _layout: Layout) {
        // This is actually totally fine, because we don't really do anything here, but asserting
        // here may help catch bugs.
        #[cfg(any(debug_assertions, feature = "safe_scopes"))]
        {
            assert!(!self.scoped.get(), "Can't use, if there's an active scope");
        }
    }

    /// Attempts to extend the memory block.
    ///
    /// If growing in-place would fail, because the request doesn't attempt to grow the last
    /// allocation, the allocator falls back to [`Self::allocate`] and copies the data to the new
    /// memory.
    ///
    /// Growing can also fail, if there is not enough memory to grow, in which case [`AllocError`]
    /// is returned.
    ///
    /// While [`Allocator::grow`] does not require `new_layout.align()` to be smaller or equal to
    /// `old_layout.align()`, this allocator does, and returns [`AllocError`] in this case.
    ///
    /// # Safety
    ///
    /// See [`Allocator::grow`].
    #[inline(always)] // Disable #[inline] temporarily to view asm.
    pub unsafe fn grow(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Result<NonNull<[u8]>, AllocError> {
        #[cfg(any(debug_assertions, feature = "safe_scopes"))]
        {
            assert!(!self.scoped.get(), "Can't use, if there's an active scope");
        }

        if old_layout.align() < new_layout.align() {
            return Err(AllocError);
        }

        // Safety docs of Allocator::grow say new size *must* be GTE old size,
        // so we don't have to validate that for correctness.
        #[cfg(debug_assertions)]
        if old_layout.size() > new_layout.size() {
            return Err(AllocError);
        }

        // TODO(jt): @Speed Detect last allocation and attempt to grow in-place instead? From Rust's
        // standard library, this is only useful for Vec-based collections, but that is not nothing.
        let new_ptr = self.allocate(new_layout)?;

        // SAFETY: This is ok, because Self::allocate always gives fresh
        // memory, so the regions won't overlap.
        unsafe {
            let new_ptr: NonNull<u8> = new_ptr.cast();
            ptr::copy_nonoverlapping(ptr.as_ptr(), new_ptr.as_ptr(), old_layout.size());
        }

        Ok(new_ptr)
    }

    /// Behaves like [`Self::grow`], but also ensures that the new contents are set to zero before
    /// being returned.
    ///
    /// # Safety
    ///
    /// See [`Allocator::grow_zeroed`].
    #[inline(always)] // Disable #[inline] temporarily to view asm.
    pub unsafe fn grow_zeroed(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Result<NonNull<[u8]>, AllocError> {
        #[cfg(any(debug_assertions, feature = "safe_scopes"))]
        {
            assert!(!self.scoped.get(), "Can't use, if there's an active scope");
        }

        unsafe {
            let mut result = self.grow(ptr, old_layout, new_layout)?;
            result.as_mut()[old_layout.size()..].fill(0);

            Ok(result)
        }
    }

    /// Attempts to shrink the memory block.
    ///
    /// Deallocating always succeeds, but memory is not reclaimed. To reclaim memory, see
    /// [`Arena::reset`] and [`Arena::scope`] or [`Arena::scope_unchecked`].
    ///
    /// # Safety
    ///
    /// This is actually safe, even if [`Allocator::shrink`] is not.
    #[inline(always)] // Disable #[inline] temporarily to view asm.
    pub unsafe fn shrink(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Result<NonNull<[u8]>, AllocError> {
        // This is actually totally fine, because we don't really do anything here, but asserting
        // here may help catch bugs.
        #[cfg(any(debug_assertions, feature = "safe_scopes"))]
        {
            assert!(!self.scoped.get(), "Can't use, if there's an active scope");
        }

        if old_layout.align() < new_layout.align() {
            return Err(AllocError);
        }

        // Safety docs of Allocator::shrink say new size *must* be LTE old size,
        // so we don't have to validate that for correctness.
        #[cfg(debug_assertions)]
        if old_layout.size() < new_layout.size() {
            return Err(AllocError);
        }

        Ok(NonNull::slice_from_raw_parts(ptr, new_layout.size()))
    }
}

impl Drop for Arena {
    fn drop(&mut self) {
        let vm_release = self.vm_release;
        let mut current = unsafe { self.current.get().as_ref() };

        while current.prev != ptr::null_mut() {
            let prev = current.prev;
            unsafe {
                vm_release(current as *const ArenaBlockHeader as *mut c_void, current.reserve_size);
            }

            current = unsafe { &*prev };
        }

        if self.flags & FLAG_CAN_RELEASE_FIRST_BLOCK > 0 {
            unsafe { vm_release(current as *const ArenaBlockHeader as *mut c_void, current.reserve_size) }
        }
    }
}

unsafe impl Allocator for Arena {
    #[inline(always)]
    fn allocate(&self, layout: Layout) -> Result<NonNull<[u8]>, AllocError> {
        Arena::allocate(self, layout)
    }

    // TODO(jt): We could also do a less naive version of Allocator::allocate_zeroed by tracking
    // which memory comes freshly from the OS and which we already handed out.

    #[inline(always)]
    unsafe fn deallocate(&self, ptr: NonNull<u8>, layout: Layout) {
        unsafe { Arena::deallocate(self, ptr, layout) }
    }

    #[inline(always)]
    unsafe fn grow(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Result<NonNull<[u8]>, AllocError> {
        unsafe { Arena::grow(self, ptr, old_layout, new_layout) }
    }

    #[inline(always)]
    unsafe fn grow_zeroed(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Result<NonNull<[u8]>, AllocError> {
        unsafe { Arena::grow_zeroed(self, ptr, old_layout, new_layout) }
    }

    #[inline(always)]
    unsafe fn shrink(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Result<NonNull<[u8]>, AllocError> {
        unsafe { Arena::shrink(self, ptr, old_layout, new_layout) }
    }
}

pub struct ArenaScope<'a> {
    // We mustn't run the drop code for a scoped Arena.
    arena: ManuallyDrop<Arena>,
    arena_parent: &'a Arena,

    reset: usize,
}

impl ArenaScope<'_> {
    pub fn arena(&self) -> &Arena {
        &self.arena
    }

    pub fn arena_mut(&mut self) -> &mut Arena {
        &mut self.arena
    }
}

impl Drop for ArenaScope<'_> {
    fn drop(&mut self) {
        #[cfg(any(debug_assertions, feature = "safe_scopes"))]
        {
            debug_assert!(!self.arena.scoped.get());

            assert!(self.arena_parent.scoped.get());
            self.arena_parent.scoped.set(false); // Unlock the parent arena for subsequent use.
        }

        self.arena_parent.current.set(self.arena.current.get());
        unsafe {
            self.arena_parent.reset_to_unchecked(self.reset);
        }
    }
}

impl Deref for ArenaScope<'_> {
    type Target = Arena;

    fn deref(&self) -> &Self::Target {
        &self.arena
    }
}

#[inline(never)]
fn allocate_and_init_block(
    vm_reserve: fn(size: usize) -> *mut c_void,
    vm_commit: unsafe fn(ptr: *mut c_void, size: usize) -> bool,
    vm_page_size: usize,
    reserve_size: usize,
    commit_size: usize,
    prev: *mut ArenaBlockHeader,
    size_before_this_block: usize,
) -> Result<NonNull<ArenaBlockHeader>, AllocError> {
    let block = vm_reserve(reserve_size);
    if block == ptr::null_mut() {
        return Err(AllocError);
    }

    if unsafe { !vm_commit(block, commit_size) } {
        return Err(AllocError);
    }

    let block_base_addr = block as usize;
    debug_assert!(block_base_addr == align_to(block_base_addr, align_of::<ArenaBlockHeader>()));
    debug_assert!(block_base_addr == align_to(block_base_addr, vm_page_size));

    let block_base = unsafe { NonNull::new_unchecked(block) };

    let block_start_addr = block_base_addr + size_of::<ArenaBlockHeader>();
    let block_end_addr = block_base_addr + reserve_size;
    let block_committed_addr = block_base_addr + commit_size;

    let block_start: NonNull<u8> = block_base.with_addr(unsafe { nz(block_start_addr) }).cast();
    let block_end: NonNull<u8> = block_base.with_addr(unsafe { nz(block_end_addr) }).cast();
    let block_committed: NonNull<u8> = block_base.with_addr(unsafe { nz(block_committed_addr) }).cast();

    let header = ArenaBlockHeader {
        size_before_this_block,

        base: block_base.cast(),
        start: block_start,
        end: block_end,

        allocated: Cell::new(block_start),
        committed: Cell::new(block_committed),

        reserve_size,

        prev,
    };

    let block_base: NonNull<ArenaBlockHeader> = block_base.cast();
    unsafe { block_base.write(header) };

    Ok(block_base)
}

fn addr(ptr: NonNull<u8>) -> usize {
    ptr.addr().get()
}

fn addr_in_cell(ptr: &Cell<NonNull<u8>>) -> usize {
    ptr.get().addr().get()
}

unsafe fn nz(addr: usize) -> NonZeroUsize {
    debug_assert!(addr > 0);
    unsafe { NonZeroUsize::new_unchecked(addr) }
}

fn align_to(addr: usize, align: usize) -> usize {
    debug_assert!(align > 0);
    debug_assert!(align.is_power_of_two());

    // Layout::from_size_align says that size, rounded up to the nearest multiple of align, must
    // not overflow isize. This means that align can be at most 1<<62 (on platforms with 64-bit
    // usize/isize).
    //
    // Generalized, align always has to be:
    debug_assert!(align <= 1 << (usize::BITS - 2));

    let mask = align - 1;
    (addr + mask) & !mask
}

#[cfg(test)]
mod tests {
    extern crate alloc;
    extern crate test;

    use alloc::alloc::Global;
    use alloc::boxed::Box;
    use alloc::vec::Vec;
    use core::hint::black_box;
    use core::mem;

    use oorandom::Rand32;
    use test::Bencher;

    use super::*;

    #[test]
    fn test_align_to() {
        assert!(align_to(1, 1) == 1);
        assert!(align_to(1, 2) == 2);
        assert!(align_to(1, 4) == 4);
        assert!(align_to(1, 8) == 8);

        assert!(align_to(2, 1) == 2);
        assert!(align_to(2, 2) == 2);
        assert!(align_to(2, 4) == 4);
        assert!(align_to(2, 8) == 8);

        assert!(align_to(3, 1) == 3);
        assert!(align_to(3, 2) == 4);
        assert!(align_to(3, 4) == 4);
        assert!(align_to(3, 8) == 8);

        assert!(align_to(4, 1) == 4);
        assert!(align_to(4, 2) == 4);
        assert!(align_to(4, 4) == 4);
        assert!(align_to(4, 8) == 8);

        assert!(align_to(5, 1) == 5);
        assert!(align_to(5, 2) == 6);
        assert!(align_to(5, 4) == 8);
        assert!(align_to(5, 8) == 8);
    }

    #[test]
    fn test_arena_simple_usage() {
        let memory = allocate_memory_block(1 << 20, align_of::<ArenaBlockHeader>()); // Align for header, otherwise this is harder to test.
        let arena = unsafe { Arena::with_memory_block(memory.ptr(), memory.len()).unwrap() };
        let mut expected_size = size_of::<ArenaBlockHeader>();

        assert!(arena.allocated_size() == expected_size);
        assert!(arena.committed_size() == 1 << 20);
        assert!(arena.reserved_size() == 1 << 20);

        let _align8 = Box::new_in(42u64, &arena);
        expected_size += 8;

        assert!(arena.allocated_size() == expected_size);
        assert!(arena.committed_size() == 1 << 20);
        assert!(arena.reserved_size() == 1 << 20);

        let _align1 = Box::new_in(42u8, &arena);
        expected_size += 1;

        assert!(arena.allocated_size() == expected_size);
        assert!(arena.committed_size() == 1 << 20);
        assert!(arena.reserved_size() == 1 << 20);

        let _align2 = Box::new_in(42u16, &arena);
        expected_size += 3;

        assert!(arena.allocated_size() == expected_size);
        assert!(arena.committed_size() == 1 << 20);
        assert!(arena.reserved_size() == 1 << 20);

        let _align4 = Box::new_in(42u32, &arena);
        expected_size += 4;

        assert!(arena.allocated_size() == expected_size);
        assert!(arena.committed_size() == 1 << 20);
        assert!(arena.reserved_size() == 1 << 20);
    }

    #[test]
    fn fuzz_arena_with_memory_block() {
        let block_size = 2 << 30;
        let block_align = 1;
        let memory = allocate_memory_block(block_size, block_align);
        let arena = unsafe { Arena::with_memory_block(memory.ptr(), memory.len()).unwrap() };

        if cfg!(miri) {
            fuzz_arena(arena, 500, 10);
        } else {
            fuzz_arena(arena, 500, 50);
        }
    }

    #[test]
    // TODO(jt): Miri does not support virtual memory yet on either Windows nor Unix.
    #[cfg_attr(miri, ignore)]
    fn fuzz_arena_with_virtual_memory_contiguous() {
        for commit_size in [4 << 10, 16 << 10, 32 << 20] {
            let reserve_size = 2 << 30;
            let arena = unsafe {
                Arena::with_virtual_memory(
                    vm_reserve,
                    vm_commit,
                    vm_release,
                    vm_page_size(),
                    reserve_size,
                    commit_size,
                    false,
                )
                .unwrap()
            };

            if cfg!(miri) {
                fuzz_arena(arena, 500, 10);
            } else {
                fuzz_arena(arena, 500, 50);
            }
        }
    }

    #[test]
    // TODO(jt): Miri does not support virtual memory yet on either Windows nor Unix.
    #[cfg_attr(miri, ignore)]
    fn fuzz_arena_with_virtual_memory_chained() {
        for commit_size in [4 << 10, 16 << 10, 32 << 20] {
            let reserve_size = 256 << 20;
            let arena = unsafe {
                Arena::with_virtual_memory(
                    vm_reserve,
                    vm_commit,
                    vm_release,
                    vm_page_size(),
                    reserve_size,
                    commit_size,
                    true,
                )
                .unwrap()
            };

            if cfg!(miri) {
                fuzz_arena(arena, 500, 10);
            } else {
                fuzz_arena(arena, 500, 50);
            }
        }
    }

    fn fuzz_arena(arena: Arena, op_count: usize, reset_count: usize) {
        let mut r = Rand32::new(0);

        // This is more than a page size on every system I know (x64 is 4k, M1 is 16k), so that it
        // stresses the system a little.
        #[derive(Clone, Copy)]
        #[allow(dead_code)]
        struct Over9000([u8; (16 << 10) + 31]);
        let over9000 = unsafe { mem::zeroed() };

        for _ in 0..reset_count {
            let scope = unsafe { arena.scope_unchecked() };
            let arena = scope.arena();

            let mut v0: Vec<u8, _> = Vec::new_in(&arena);
            let mut v1: Vec<Over9000, _> = Vec::new_in(&arena);

            for _ in 0..op_count {
                match r.rand_u32() % 2 {
                    0 => {
                        v0.push(r.rand_u32() as u8);
                    }

                    1 => {
                        v1.push(over9000);
                    }

                    _ => unreachable!(),
                }
            }

            for _ in 0..op_count {
                match r.rand_u32() % 4 {
                    0 => {
                        v0.push(r.rand_u32() as u8);
                    }

                    1 => {
                        v1.push(over9000);
                    }

                    2 => {
                        v0.shrink_to_fit();
                    }

                    3 => {
                        v1.shrink_to_fit();
                    }

                    _ => unreachable!(),
                }
            }

            for _ in 0..op_count {
                match r.rand_u32() % 6 {
                    0 => {
                        v0.push(r.rand_u32() as u8);
                    }

                    1 => {
                        v1.push(over9000);
                    }

                    2 => {
                        v0.shrink_to_fit();
                    }

                    3 => {
                        v1.shrink_to_fit();
                    }

                    4 => {
                        v0 = Vec::new_in(&arena);
                    }

                    5 => {
                        v1 = Vec::new_in(&arena);
                    }

                    _ => unreachable!(),
                }
            }
        }
    }

    macro_rules! benchmark {
        ($bench_fn:ident, $bench_ident:ident, $count:expr, $size:expr, $align:expr) => {
            #[bench]
            #[cfg_attr(miri, ignore)]
            fn $bench_ident(b: &mut Bencher) {
                $bench_fn(b, black_box($count), black_box($size), black_box($align));
            }
        };
    }

    benchmark!(bench_arena_block, arena_block_00100_0016_16, 100, 16, 16);
    benchmark!(bench_arena_block, arena_block_01000_0016_16, 1000, 16, 16);
    benchmark!(bench_arena_block, arena_block_10000_0016_16, 10000, 16, 16);

    benchmark!(bench_arena_block, arena_block_00100_0128_16, 100, 128, 16);
    benchmark!(bench_arena_block, arena_block_01000_0128_16, 1000, 128, 16);
    benchmark!(bench_arena_block, arena_block_10000_0128_16, 10000, 128, 16);

    benchmark!(bench_arena_block, arena_block_00100_1024_16, 100, 1024, 16);
    benchmark!(bench_arena_block, arena_block_01000_1024_16, 1000, 1024, 16);
    benchmark!(bench_arena_block, arena_block_10000_1024_16, 10000, 1024, 16);

    benchmark!(
        bench_arena_vm_contiguous,
        arena_vm_contiguous_00100_0016_16,
        100,
        16,
        16
    );
    benchmark!(
        bench_arena_vm_contiguous,
        arena_vm_contiguous_01000_0016_16,
        1000,
        16,
        16
    );
    benchmark!(
        bench_arena_vm_contiguous,
        arena_vm_contiguous_10000_0016_16,
        10000,
        16,
        16
    );

    benchmark!(
        bench_arena_vm_contiguous,
        arena_vm_contiguous_00100_0128_16,
        100,
        128,
        16
    );
    benchmark!(
        bench_arena_vm_contiguous,
        arena_vm_contiguous_01000_0128_16,
        1000,
        128,
        16
    );
    benchmark!(
        bench_arena_vm_contiguous,
        arena_vm_contiguous_10000_0128_16,
        10000,
        128,
        16
    );

    benchmark!(
        bench_arena_vm_contiguous,
        arena_vm_contiguous_00100_1024_16,
        100,
        1024,
        16
    );
    benchmark!(
        bench_arena_vm_contiguous,
        arena_vm_contiguous_01000_1024_16,
        1000,
        1024,
        16
    );
    benchmark!(
        bench_arena_vm_contiguous,
        arena_vm_contiguous_10000_1024_16,
        10000,
        1024,
        16
    );

    benchmark!(bench_arena_vm_chained, arena_vm_chained_00100_0016_16, 100, 16, 16);
    benchmark!(bench_arena_vm_chained, arena_vm_chained_01000_0016_16, 1000, 16, 16);
    benchmark!(bench_arena_vm_chained, arena_vm_chained_10000_0016_16, 10000, 16, 16);

    benchmark!(bench_arena_vm_chained, arena_vm_chained_00100_0128_16, 100, 128, 16);
    benchmark!(bench_arena_vm_chained, arena_vm_chained_01000_0128_16, 1000, 128, 16);
    benchmark!(bench_arena_vm_chained, arena_vm_chained_10000_0128_16, 10000, 128, 16);

    benchmark!(bench_arena_vm_chained, arena_vm_chained_00100_1024_16, 100, 1024, 16);
    benchmark!(bench_arena_vm_chained, arena_vm_chained_01000_1024_16, 1000, 1024, 16);
    benchmark!(bench_arena_vm_chained, arena_vm_chained_10000_1024_16, 10000, 1024, 16);

    benchmark!(bench_bumpalo, bumpalo_00100_0016_16, 100, 16, 16);
    benchmark!(bench_bumpalo, bumpalo_01000_0016_16, 1000, 16, 16);
    benchmark!(bench_bumpalo, bumpalo_10000_0016_16, 10000, 16, 16);

    benchmark!(bench_bumpalo, bumpalo_00100_0128_16, 100, 128, 16);
    benchmark!(bench_bumpalo, bumpalo_01000_0128_16, 1000, 128, 16);
    benchmark!(bench_bumpalo, bumpalo_10000_0128_16, 10000, 128, 16);

    benchmark!(bench_bumpalo, bumpalo_00100_1024_16, 100, 1024, 16);
    benchmark!(bench_bumpalo, bumpalo_01000_1024_16, 1000, 1024, 16);
    benchmark!(bench_bumpalo, bumpalo_10000_1024_16, 10000, 1024, 16);

    benchmark!(bench_global, global_00100_0016_16, 100, 16, 16);
    benchmark!(bench_global, global_01000_0016_16, 1000, 16, 16);
    benchmark!(bench_global, global_10000_0016_16, 10000, 16, 16);

    benchmark!(bench_global, global_00100_0128_16, 100, 128, 16);
    benchmark!(bench_global, global_01000_0128_16, 1000, 128, 16);
    benchmark!(bench_global, global_10000_0128_16, 10000, 128, 16);

    benchmark!(bench_global, global_00100_1024_16, 100, 1024, 16);
    benchmark!(bench_global, global_01000_1024_16, 1000, 1024, 16);
    benchmark!(bench_global, global_10000_1024_16, 10000, 1024, 16);

    fn bench_arena_block(b: &mut Bencher, allocation_count: usize, allocation_size: usize, allocation_align: usize) {
        let block_size = 2 * allocation_count * allocation_size;
        let block_align = 16;
        let memory = allocate_memory_block(block_size, block_align);
        let mut arena = unsafe { Arena::with_memory_block(memory.ptr(), memory.len()).unwrap() };

        let layout = Layout::from_size_align(allocation_size, allocation_align).unwrap();

        b.iter(|| {
            arena.reset();

            for _ in 0..allocation_count {
                let p = black_box(arena.allocate(layout).unwrap());
                unsafe {
                    use_allocation(p);
                }
            }
        });
    }

    fn bench_arena_vm_contiguous(
        b: &mut Bencher,
        allocation_count: usize,
        allocation_size: usize,
        allocation_align: usize,
    ) {
        let reserve_size = 16 << 30;
        let commit_size = 32 << 20;
        let mut arena = unsafe {
            Arena::with_virtual_memory(
                vm_reserve,
                vm_commit,
                vm_release,
                vm_page_size(),
                reserve_size,
                commit_size,
                false,
            )
            .unwrap()
        };

        let layout = Layout::from_size_align(allocation_size, allocation_align).unwrap();

        b.iter(|| {
            arena.reset();

            for _ in 0..allocation_count {
                let p = black_box(arena.allocate(layout).unwrap());
                unsafe {
                    use_allocation(p);
                }
            }
        });
    }

    fn bench_arena_vm_chained(
        b: &mut Bencher,
        allocation_count: usize,
        allocation_size: usize,
        allocation_align: usize,
    ) {
        let reserve_size = 256 << 20;
        let commit_size = 32 << 20;
        let mut arena = unsafe {
            Arena::with_virtual_memory(
                vm_reserve,
                vm_commit,
                vm_release,
                vm_page_size(),
                reserve_size,
                commit_size,
                true,
            )
            .unwrap()
        };

        let layout = Layout::from_size_align(allocation_size, allocation_align).unwrap();

        b.iter(|| {
            arena.reset();

            for _ in 0..allocation_count {
                let p = black_box(arena.allocate(layout).unwrap());
                unsafe {
                    use_allocation(p);
                }
            }
        });
    }

    fn bench_bumpalo(b: &mut Bencher, allocation_count: usize, allocation_size: usize, allocation_align: usize) {
        let mut bump: bumpalo::Bump = bumpalo::Bump::new();

        let layout = Layout::from_size_align(allocation_size, allocation_align).unwrap();

        b.iter(|| {
            bump.reset();

            for _ in 0..allocation_count {
                let p = black_box((&bump).allocate(layout).unwrap());
                unsafe {
                    use_allocation(p);
                }
            }
        });
    }

    fn bench_global(b: &mut Bencher, allocation_count: usize, allocation_size: usize, allocation_align: usize) {
        let layout = Layout::from_size_align(allocation_size, allocation_align).unwrap();

        b.iter(|| {
            for _ in 0..allocation_count {
                let p = black_box(Global.allocate(layout).unwrap());
                unsafe {
                    use_allocation(p);
                }

                unsafe {
                    Global.deallocate(p.cast(), layout);
                }
            }
        });
    }

    unsafe fn use_allocation(p: NonNull<[u8]>) {
        let p = p.as_ptr() as *mut u8;

        unsafe {
            *p = 42;
        }
    }

    #[cfg(target_family = "windows")]
    fn vm_page_size() -> usize {
        use windows_sys::Win32::System::SystemInformation::GetSystemInfo;
        use windows_sys::Win32::System::SystemInformation::SYSTEM_INFO;

        let mut system_info: SYSTEM_INFO = unsafe { mem::zeroed() };
        unsafe {
            GetSystemInfo(&mut system_info);
        }

        system_info.dwPageSize as usize
    }

    #[cfg(target_family = "unix")]
    fn vm_page_size() -> usize {
        use libc::_SC_PAGE_SIZE;
        use libc::sysconf;

        let page_size = unsafe { sysconf(_SC_PAGE_SIZE) };
        if page_size < 1 {
            panic!("Unsupported page size {page_size}");
        }

        page_size as usize
    }

    #[cfg(target_family = "windows")]
    fn vm_reserve(size: usize) -> *mut c_void {
        use windows_sys::Win32::System::Memory::MEM_RESERVE;
        use windows_sys::Win32::System::Memory::PAGE_READWRITE;
        use windows_sys::Win32::System::Memory::VirtualAlloc;

        unsafe { VirtualAlloc(ptr::null(), size, MEM_RESERVE, PAGE_READWRITE) }
    }

    #[cfg(target_family = "windows")]
    fn vm_commit(ptr: *mut c_void, size: usize) -> bool {
        use windows_sys::Win32::System::Memory::MEM_COMMIT;
        use windows_sys::Win32::System::Memory::PAGE_READWRITE;
        use windows_sys::Win32::System::Memory::VirtualAlloc;

        let p = unsafe { VirtualAlloc(ptr, size, MEM_COMMIT, PAGE_READWRITE) };
        p != ptr::null_mut()
    }

    #[cfg(target_family = "windows")]
    fn vm_release(ptr: *mut c_void, _size: usize) {
        use windows_sys::Win32::System::Memory::MEM_RELEASE;
        use windows_sys::Win32::System::Memory::VirtualFree;

        unsafe {
            // NOTE(jt): Size is not used when releasing on Windows, but it is used elsewhere.
            VirtualFree(ptr, 0, MEM_RELEASE);
        }
    }

    #[cfg(target_family = "unix")]
    fn vm_reserve(size: usize) -> *mut c_void {
        use libc::MAP_ANONYMOUS;
        use libc::MAP_PRIVATE;
        use libc::PROT_NONE;
        use libc::mmap;

        // NOTE(jt): This could pass in PROT_READ|PROT_WRITE, which would make the OS handle commits
        // and we wouldn't have to do anything in vm_commit.
        unsafe { mmap(ptr::null_mut(), size, PROT_NONE, MAP_PRIVATE | MAP_ANONYMOUS, 0, 0) }
    }

    #[cfg(target_family = "unix")]
    fn vm_commit(ptr: *mut c_void, size: usize) -> bool {
        use libc::PROT_READ;
        use libc::PROT_WRITE;
        use libc::mprotect;

        // NOTE(jt): If mmap was passed in PROT_READ|PROT_WRITE in vm_reserve, we don't have to do
        // anything in here.
        unsafe {
            mprotect(ptr, size, PROT_READ | PROT_WRITE);
        }

        true
    }

    #[cfg(target_family = "unix")]
    fn vm_release(ptr: *mut c_void, size: usize) {
        use libc::munmap;

        unsafe {
            munmap(ptr, size);
        }
    }

    struct MemoryBlock {
        ptr: NonNull<[u8]>,
        layout: Layout,
    }

    impl MemoryBlock {
        fn ptr(&self) -> NonNull<u8> {
            self.ptr.cast()
        }

        fn len(&self) -> usize {
            self.ptr.len()
        }
    }

    impl Drop for MemoryBlock {
        fn drop(&mut self) {
            unsafe {
                Global.deallocate(self.ptr.cast(), self.layout);
            }
        }
    }

    fn allocate_memory_block(size: usize, align: usize) -> MemoryBlock {
        let layout = Layout::from_size_align(size, align).unwrap();

        // Allocate a megabyte of unaligned memory. Miri doesn't align allocated memory unless we
        // request it, but other allocators probably align this to at least 16.
        let ptr = Global.allocate(layout).unwrap();

        MemoryBlock { ptr, layout }
    }
}
