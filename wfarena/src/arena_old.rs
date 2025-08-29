// TODO(jt): After having a look at rfj's arena in RAD Debugger, I think we could make arena
// creation more flexible for the user:
//
// To create an arena, one needs either a block of memory, a set of functions to manage virtual
// memory (usually defined by a platform layer), or both.
//
// The arena receive an initial block of memory, that is assumed to already be reserved and
// committed. If the arena isn't passed this block, it uses vm_reserve and vm_commit to get it (it
// needs to either get the vm functions if it doesn't get this block). This makes it possible to
// create an arena from existing memory, when vm functions are not accessible.
//
// For virtual memory: although the arena could have just one huge block of virtual memory reserved
// and commit it as needed, it can (optionally) also chain multiple such blocks (each is first
// reserved in entirety and committed as needed). This way, it can release memory back to the OS
// block by block, when they are not used anymore. Reserve and commit sizes are parameters.
//
// All of the above allows for flexible uses:
//
// - Can create arenas with just the initial block.
// - Can create single-block arenas that reserves a huge amount of virtual address in advance, but can't grow beyond.
// - Can create a multi-block arena that reserves a moderately sized amount of virtual address space for each block.
//
// For inspiration, look at base_arena.h and base_arena.c in RADDebugger.

//! The Arena, sometimes also called a bump allocator or linear allocator, is an allocator
//! internally backed by a contiguous (in virtual address space) region of memory, or multiple
//! chained blocks of contiguous memory (although this implementation doesn't do that). It handles
//! allocation requests by subdividing this memory into mutually exclusive slices and handing those
//! out to the callers.
//!
//! Arenas provide very fast, constant-time, allocations at the cost of not being able to control
//! deallocation of individual objects. They are well-suited for short-running programs such as
//! scripts, as well as long-running programs with clearly defined lifetimes for allocated memory,
//! such as simulations, game engines or web servers. In short-running programs, the caller doesn't
//! worry about deallocation at all and let's the OS clean up the memory after the process
//! terminates. In long-running programs with clear lifetimes, the caller explicitly deallocates all
//! memory at the appropriate time and place in the program.
//!
//! # Arena deallocation
//!
//! There is no way to deallocate a single allocation from the Arena. The deallocation methods from
//! [`core::alloc::Allocator`] are implemented, but don't reclaim memory.
//!
//! Instead, deallocation is done en masse with [`Arena::reset`], which resets the internal bump
//! pointer back to the start of the arena's region of memory. It is up to the using code to call
//! [`Arena::reset`] in appropriate times, e.g. when a cycle of work has completed and the memory
//! associated with can be safely forgotten. Such a cycle can for example be responding to a network
//! request, rendering a frame of an application, or doing a step of a simulation.
//!
//! Additionally, the arena also supports more eager memory reclamation with scopes (see
//! [`Arena::scope`]).
//!
//! # Creating an arena
//!
//! The Arena is created by giving it a region a memory to sub-allocate. It can operate in one of
//! two modes, chosen at its creation:
//!
//! - It can govern a slice of memory obtained from the caller, e.g. using the global allocator (see
//!   [`Arena::with_base_and_capacity`], [`Arena::with_non_null_slice`]),
//!
//! - It can govern a slice of reserved virtual memory, which it commits to the OS as needed (see
//!   [`Arena::with_virtual_memory`]).
//!
//! # Arena safety
//!
//! Creating an arena is unsafe, becuase the creator has to make sure the arena has exclusive access
//! to the memory it is sub-allocating. The exception are the two feature-gated virtual memory
//! constructors, which reserve virtual memory from the OS on behalf of the caller.
//!
//! Parts of the [`core::alloc::Allocator`] trait are also unsafe. However, a lot of times Arena
//! isn't used directly, and only serves as a parameter for a collection, which then uses the memory
//! from the allocator to store its data. This should be completely safe.
//!
//! # Optional arena features
//!
//! The crate can be compiled with the following features for additional functionality:
//!
//! - `virtual_memory_windows`: enables safe arena creation using virtual memory on Windows. Pulls
//!   in additional dependencies.
//!
//! - `virtual_memory_unix`: enables safe arena creationg using virtual memory on Unix-like
//!   operating systems. Pulls in additional dependencies.

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

#[derive(Debug)]
pub struct ArenaInitError;

/// A small arena allocator, implementing [`core::alloc::Allocator`].
#[repr(C)]
#[derive(Debug, PartialEq, Eq)]
pub struct Arena {
    base: NonNull<u8>,
    head: Cell<NonNull<u8>>,
    end: Cell<NonNull<u8>>,

    vm_allocator:
        Option<unsafe fn(base: *const c_void, page_size: usize, old_capacity: usize, new_capacity: usize) -> bool>,
    // Ideally we wouldn't have vm_deallocator and let the OS deal with the mapped memory once the
    // process terminates, but someone might have the idea of creating and destroying arenas in
    // their programs.
    vm_deallocator: Option<unsafe fn(base: *const c_void, max_capacity: usize)>,
    vm_base: NonNull<u8>,
    vm_max_capacity: usize,
    vm_page_size: usize,

    #[cfg(any(debug_assertions, feature = "safe_scopes"))]
    scoped: Cell<bool>,
}

impl Arena {
    /// Creates a new arena allocator governing the provided memory.
    ///
    /// # Safety
    ///
    /// The memory described by the `base` pointer and `capacity` must be
    /// *dereferencable*, as described in [`core::ptr`] module documentation.
    pub unsafe fn with_base_and_capacity(base: NonNull<u8>, capacity: usize) -> Result<Self, ArenaInitError> {
        let base_usize = base.as_ptr() as usize;

        // Check we can compute the end of our allocation without overflowing.
        let end_usize = base_usize.checked_add(capacity).ok_or(ArenaInitError)?;

        // Check that we won't have to check for overflow when allocating.
        //
        // Layout::from_size_align states that size, rounded up to the nearest multiple of align,
        // must not overflow isize. This means that, for 64-bit isize/usize, align can be at most
        // 1<<62. It also means that the aligned size of any requested allocation can not be larger
        // than isize::MAX.
        //
        // Basically, we guarantee here, that we are able to do at least one add without overflowing
        // usize, after which the regular out-of-memory branch will trigger, because we couldn't
        // service an allocation request that large anyway.
        //
        // Moreover, on today's hardware we don't really use the full 64-bits of a pointer
        // anyway. On Intel, even with 5-level paging, only 57-bits are used.
        //
        // The branch below is overly paranoid and should never trigger for computers of the 21st
        // century.
        if end_usize > isize::MAX as usize {
            return Err(ArenaInitError);
        }

        let end_usize_nonzero = NonZeroUsize::new(end_usize).ok_or(ArenaInitError)?;
        let end = base.with_addr(end_usize_nonzero);

        Ok(Self {
            base,
            head: Cell::new(base),
            end: Cell::new(end),

            vm_allocator: None,
            vm_deallocator: None,
            vm_base: base,
            vm_page_size: 0,
            vm_max_capacity: 0,

            #[cfg(any(debug_assertions, feature = "safe_scopes"))]
            scoped: Cell::new(false),
        })
    }

    /// Creates a new arena allocator governing the provided memory.
    ///
    /// # Safety
    ///
    /// The memory described by the `slice` pointer must be *dereferencable*, as described in
    /// [`core::ptr`] module documentation.
    pub unsafe fn with_non_null_slice(slice: NonNull<[u8]>) -> Result<Self, ArenaInitError> {
        // TODO(jt): @Cleanup This nonnull slice cast can be replaced with NonNull::as_non_null_ptr()
        // https://github.com/rust-lang/rust/issues/74265
        unsafe { Self::with_base_and_capacity(slice.cast(), slice.len()) }
    }

    /// Creates a new arena allocator governing the virtual memory starting at the provided base
    /// pointer.
    ///
    /// When created this way, Arena starts with zero capacity, but but has the ability to grow its
    /// capacity by calling the provided `vm_allocator` function, which is required to map new
    /// pages into the virtual address range of the arena. `vm_allocator` receives both the old
    /// and new capacity of the arena, and should return true if the mapping of new memory between
    /// those two offsets succeeded.
    ///
    /// The arena attempts to grow its capacity by calling `vm_allocator`, when its current capacity
    /// isn't enough to service the next allocation request, in which case it calls `vm_allocator`
    /// with the minimum new capacity it needs, aligned up to nearest `page_size`. Arena won't call
    /// `vm_allocator`, if the new capacity would exceed the `max_capacity` parameter.
    ///
    /// On Windows, this can be used in conjunction with `VirtualAlloc` called to both `MEM_RESERVE`
    /// an address range ahead of time, and to `MEM_COMMIT` parts of the address range as required
    /// inside `vm_allocator`. Note that `MEM_COMMIT` isn't actually committing the pages yet, and
    /// the actual commit will happen in an OS page fault handler once the memory is read from or
    /// written to.
    ///
    /// The alternative for Unix systems is `mmap(2)`, which in combination with `MAP_PRIVATE |
    /// MAP_ANONYMOUS` reserves an address range which is then automatically committed by the OS as
    /// memory is written to.
    ///
    /// Both of these approaches make it possible to reserve a large amount of contiguous virtual
    /// memory ahead of time and commit it as needed. This is useful in situations, where a program
    /// needs multiple large arenas to manage its memory, but can not easily guess the upper bounds
    /// of the arenas' sizes.
    ///
    /// Once the memory is not used anymore, it is the responsibility of the caller to make sure the
    /// reserved and allocated memory is released back to the OS. This can either be by letting the
    /// process exit and the OS clean up, or manually calling `VirtualFree` (Windows) or `munmap(2)`
    /// (Unix).
    ///
    /// # Safety
    ///
    /// The memory described by the `base` pointer must be *dereferencable*, as described in
    /// [`core::ptr`] module documentation.
    ///
    /// `vm_allocator` must return true only if new memory is successfully mapped to the requested
    /// address range.
    ///
    /// `max_capacity` must correspond with the reserved capacity of virtual memory (e.g. one passed
    /// to `VirtualAlloc`).
    ///
    /// `page_size` must be the page size on the current system. On many x64 systems this is 4k, but
    /// this can change with e.g. large page support, and can be different on other systems, such as
    /// the Apple's M1.
    pub unsafe fn with_virtual_memory(
        base: NonNull<u8>,
        vm_allocator: unsafe fn(
            base: *const c_void,
            page_size: usize,
            old_capacity: usize,
            new_capacity: usize,
        ) -> bool,
        vm_deallocator: unsafe fn(base: *const c_void, max_capacity: usize),
        max_capacity: usize,
        page_size: usize,
    ) -> Result<Self, ArenaInitError> {
        let base_usize = base.as_ptr() as usize;

        // Check that memory is aligned to the given page size.
        if base_usize != align_to(base_usize, page_size) {
            return Err(ArenaInitError);
        }

        // Check that the capacity limit aligns to page size.
        if max_capacity != align_to(max_capacity, page_size) {
            return Err(ArenaInitError);
        }

        Ok(Self {
            base,
            head: Cell::new(base),
            end: Cell::new(base),

            vm_allocator: Some(vm_allocator),
            vm_deallocator: Some(vm_deallocator),
            vm_base: base,
            vm_max_capacity: max_capacity,
            vm_page_size: page_size,

            #[cfg(any(debug_assertions, feature = "safe_scopes"))]
            scoped: Cell::new(false),
        })
    }

    /// Like [`Self::with_virtual_memory`], but safe and can unmap memory on drop. Reserves and
    /// commits memory using the Win32 `VirtualAlloc` function.
    ///
    /// Windows only.
    #[cfg(all(target_family = "windows", feature = "virtual_memory_windows"))]
    pub fn with_virtual_memory_windows(max_capacity: usize, unmap_on_drop: bool) -> Result<Self, ArenaInitError> {
        use core::mem;

        use windows_sys::Win32::System::Memory::MEM_COMMIT;
        use windows_sys::Win32::System::Memory::MEM_RELEASE;
        use windows_sys::Win32::System::Memory::MEM_RESERVE;
        use windows_sys::Win32::System::Memory::PAGE_READWRITE;
        use windows_sys::Win32::System::Memory::VirtualAlloc;
        use windows_sys::Win32::System::Memory::VirtualFree;
        use windows_sys::Win32::System::SystemInformation::GetSystemInfo;
        use windows_sys::Win32::System::SystemInformation::SYSTEM_INFO;

        // Pages are usually 4096 bytes on systems where Windows runs, but we can also ask Windows.
        let mut system_info: SYSTEM_INFO = unsafe { mem::zeroed() };
        unsafe {
            GetSystemInfo(&mut system_info);
        }
        let page_size = system_info.dwPageSize as usize;

        // On Windows, we reserve the address range ahead of time with VirtualAlloc and MEM_RESERVE. As
        // we require more memory, we call VirtualAlloc again with MEM_COMMIT (in the callback we give
        // to the Arena) on the range we'd like to access. Note that MEM_COMMIT still isn't
        // "MEM_ACTUALLY_REALLY_COMMIT", and there will still be a page fault on Windows the first time
        // we access each new page, after which the page will finally be mapped.
        let memory = unsafe { VirtualAlloc(ptr::null(), max_capacity, MEM_RESERVE, PAGE_READWRITE) };
        if memory == ptr::null_mut() {
            return Err(ArenaInitError);
        }

        let base: NonNull<u8> = NonNull::new(memory as *mut u8).unwrap();

        unsafe fn vm_allocator(
            base: *const c_void,
            _page_size: usize,
            old_capacity: usize,
            new_capacity: usize,
        ) -> bool {
            // We assume Arena never calls this function to allocate over max capacity.

            // Compute the range we'd like to access and MEM_COMMIT it.
            let commit_base = unsafe { base.add(old_capacity) };
            let commit_size = new_capacity - old_capacity;
            let p = unsafe { VirtualAlloc(commit_base, commit_size, MEM_COMMIT, PAGE_READWRITE) };

            // If committing fails, VirtualAlloc returns null, and we must not use the new pages.
            p != ptr::null_mut()
        }

        unsafe fn vm_deallocator(base: *const c_void, _max_capacity: usize) {
            unsafe {
                VirtualFree(base as *mut c_void, 0, MEM_RELEASE);
            }
        }

        unsafe fn vm_deallocator_noop(_base: *const c_void, _max_capacity: usize) {}

        unsafe {
            Self::with_virtual_memory(
                base,
                vm_allocator,
                if unmap_on_drop {
                    vm_deallocator
                } else {
                    vm_deallocator_noop
                },
                max_capacity,
                page_size,
            )
        }
    }

    /// Like [`Self::with_virtual_memory`], but safe and can unmap memory on drop. Reserves and
    /// commits memory using the libc `mmap(2)` function.
    ///
    /// Unix only.
    #[cfg(all(target_family = "unix", feature = "virtual_memory_unix"))]
    pub fn with_virtual_memory_unix(max_capacity: usize, unmap_on_drop: bool) -> Result<Self, ArenaInitError> {
        use libc::_SC_PAGE_SIZE;
        use libc::MAP_ANONYMOUS;
        use libc::MAP_PRIVATE;
        use libc::PROT_READ;
        use libc::PROT_WRITE;
        use libc::mmap;
        use libc::munmap;
        use libc::sysconf;

        // Pages can be anything here, e.g. 4096 bytes x64, or 16384 on Apple's M1 computers.
        let page_size = unsafe { sysconf(_SC_PAGE_SIZE) };
        if page_size < 1 {
            return Err(ArenaInitError);
        }

        let page_size: usize = page_size as usize;

        // On Unix systems, unlike on Windows, there is no separate step for reserving and
        // commiting. mmap(2) reserves the memory, and it is mapped in the OS fault handlers when first
        // accessed. Our callback therefore doesn't need to do any work besides checking that the
        // allocator didn't go outside of the reserved address range.
        let memory = unsafe {
            mmap(
                ptr::null_mut(),
                max_capacity,
                PROT_READ | PROT_WRITE,
                MAP_PRIVATE | MAP_ANONYMOUS,
                0,
                0,
            )
        };

        if memory == ptr::null_mut() {
            return Err(ArenaInitError);
        }

        let base: NonNull<u8> = NonNull::new(memory as *mut u8).unwrap();

        unsafe fn vm_allocator(
            _base: *const c_void,
            _page_size: usize,
            _old_capacity: usize,
            _new_capacity: usize,
        ) -> bool {
            // We assume Arena never calls this function to allocate over max capacity. Also, we
            // don't really need to do anything for mmap.
            true
        }

        unsafe fn vm_deallocator(base: *const c_void, max_capacity: usize) {
            unsafe {
                munmap(base as *mut c_void, max_capacity);
            }
        }

        unsafe fn vm_deallocator_noop(_base: *const c_void, _max_capacity: usize) {}

        unsafe {
            Self::with_virtual_memory(
                base,
                vm_allocator,
                if unmap_on_drop {
                    vm_deallocator
                } else {
                    vm_deallocator_noop
                },
                max_capacity,
                page_size,
            )
        }
    }

    /// Create a scoped arena out of this one.
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

        ArenaScope {
            arena: ManuallyDrop::new(Arena {
                // Base of the new arena is the head of the current.
                base: self.head.get(),
                head: Cell::new(self.head.get()),
                end: Cell::new(self.end.get()),

                vm_allocator: self.vm_allocator,
                vm_deallocator: self.vm_deallocator,
                vm_base: self.vm_base,
                vm_max_capacity: self.vm_max_capacity,
                vm_page_size: self.vm_page_size,

                #[cfg(any(debug_assertions, feature = "safe_scopes"))]
                scoped: Cell::new(false),
            }),
            arena_parent: self,
        }
    }

    /// Create a scoped arena out of this one.
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

        ArenaScope {
            arena: ManuallyDrop::new(Arena {
                // Base of the new arena is the head of the current.
                base: self.head.get(),
                head: Cell::new(self.head.get()),
                end: Cell::new(self.end.get()),

                vm_allocator: self.vm_allocator,
                vm_deallocator: self.vm_deallocator,
                vm_base: self.vm_base,
                vm_max_capacity: self.vm_max_capacity,
                vm_page_size: self.vm_page_size,

                #[cfg(any(debug_assertions, feature = "safe_scopes"))]
                scoped: Cell::new(false),
            }),
            arena_parent: self,
        }
    }

    /// Returns the base pointer of this allocator.
    pub fn base(&self) -> NonNull<u8> {
        self.base
    }

    /// Returns the head pointer of this allocator.
    pub fn head(&self) -> NonNull<u8> {
        self.head.get()
    }

    /// Returns all available memory in this allocator in bytes.
    ///
    /// This value can be stale, if there is a scope (e.g. [`Self::scope_unchecked`]) active.
    pub fn capacity(&self) -> usize {
        let base = self.base.as_ptr() as usize;
        let end = self.end.get().as_ptr() as usize;

        debug_assert!(base <= end);
        end - base
    }

    /// Returns the used memory in the arena in bytes.
    ///
    /// This value can be stale, if there is a scope (e.g. [`Self::scope_unchecked`]) active.
    pub fn used_capacity(&self) -> usize {
        let base = self.base.as_ptr() as usize;
        let head = self.head.get().as_ptr() as usize;

        debug_assert!(base <= head);
        head - base
    }

    /// Returns remaining available memory in the arena in bytes. If the arena is backed by virtual
    /// memory, the reported capacity is only up to the last allocated page, and there may be more
    /// memory available in the system.
    ///
    /// This value can be stale, if there is a scope (e.g. [`Self::scope_unchecked`]) active.
    pub fn remaining_capacity(&self) -> usize {
        let head = self.head.get().as_ptr() as usize;
        let end = self.end.get().as_ptr() as usize;

        debug_assert!(head <= end);
        end - head
    }

    /// Resets the allocator, moving the head pointer back to the beginning.
    #[inline]
    pub fn reset(&mut self) {
        self.head.set(self.base);
    }

    /// Resets the allocator without requiring unique access, moving the head pointer back to the
    /// beginning.
    ///
    /// # Safety
    ///
    /// After calling this, any data stored in the allocator invalid, but this is not enforced by
    /// Rust's lifetime tracking.
    ///
    /// It is also not safe to call this on a reference created by [`Self::leak`], as that will
    /// overwrite the data of the Arena itself.
    #[inline]
    pub unsafe fn reset_unchecked(&self) {
        self.head.set(self.base);
    }

    /// Allocates the arena in its own memory and returns a reference with a static lifetime.
    ///
    /// Resetting a leaked arena with [`Self::reset_unchecked`] is always incorrect.
    ///
    /// Drop code, if any, doesn't run for leaked arenas. To drop a leaked Arena, call the unsafe
    /// [`ptr::read`] first. Note that only VM arenas with `unmap_on_drop: true` do things on drop.
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
    pub fn leak(self) -> Result<&'static Self, AllocError> {
        let p: NonNull<[u8]> = self.allocate(Layout::new::<Self>())?;
        let p: NonNull<Arena> = p.cast();
        {
            let p = p.as_ptr();
            unsafe { *p = self };
        }

        Ok(unsafe { p.as_ref() })
    }

    /// Attempts to allocate memory.
    ///
    /// Allocating can fail, if there is not enough memory, in which case [`AllocError`] is
    /// returned.
    #[inline(always)] // Disable #[inline] temporarily to view asm.
    pub fn allocate(&self, layout: Layout) -> Result<NonNull<[u8]>, AllocError> {
        #[cfg(any(debug_assertions, feature = "safe_scopes"))]
        {
            assert!(!self.scoped.get(), "Can't use, if there's an active scope");
        }

        //
        // Arena::allocate is slightly but consistently slower than bumpalo's
        // Bump::try_alloc_layout_fast, both with preallocated memory and virtual memory. Bumpalo is
        // growing downwards, which lets it skip a little bit of alignment math at the expense of
        // not being able to grow allocations inline. In assembly, bumpalo has three dependency
        // chains that mostly run in parallel, the longest one being ~4 instructions. We have just
        // one dependency chain, but it is ~7 instructions long. I believe supporting
        // Allocator::grow and being slightly slower is a good tradeoff, but we can revisit.
        //

        let head = self.head.get().as_ptr() as usize;
        let end = self.end.get().as_ptr() as usize;

        // Align the pointer to support storing the requested layout and add the size.
        //
        // Note that this skips the overflow check for both the alignment and the add. This is okay,
        // because Layout::from_size_align guarantees us that requested size, rounded up to the
        // requested align, doesn't overflow isize. We also check ourselves that our end pointer
        // doesn't overflow isize (when it is initialized and also each time it changes). Adding
        // together isize::MAX + isize::MAX still can't overflow usize, so we know we can't overflow
        // usize in a single allocation request.
        //
        // If we ever get even close to overflowing, our out-of-bounds check will trigger after the
        // add, preventing the next, potentially overflowing add.
        //
        // In reality, getting even close to overflowing can't happen on today's hardware, where we
        // use at most 57 bits of a pointer on 5-level paging on Intel.
        let ptr = align_to(head, layout.align());
        let new_head = ptr + layout.size();

        if new_head > end {
            unsafe {
                if !self.try_allocate_pages_for_head(new_head) {
                    return Err(AllocError);
                }
            }
        }

        self.head.set(unsafe { self.make_pointer(new_head) });

        let ptr = unsafe { self.make_pointer(ptr) };

        Ok(NonNull::slice_from_raw_parts(ptr, layout.size()))
    }

    /// Deallocate memory.
    ///
    /// Deallocating always succeeds, but memory is not reclaimed. Single-allocation memory
    /// reclamation in arenas is very fussy and would only work for certain usage patterns
    /// anyway. Instead, see [`Arena::reset`] and [`Arena::scope`].
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

    /// Attempts to grow an existing allocation in-place.
    ///
    /// Since this is a arena allocator, it avoids work that amounts to more than just simple
    /// pointer arithmetic.
    ///
    /// However, [`Self::grow`] (and [`Self::grow_zeroed`]) is an exception to the above rule: if
    /// growing would fail, because the request doesn't attempt to grow the last allocation, the
    /// allocator falls back to [`Self::allocate`] and copies the data to the new memory.
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

        if self.is_last_allocation(ptr, old_layout) {
            // ptr is assumed to have been produced by this allocator, and
            // therefore already properly aligned. New size is checked to be GTE
            // old size. We can therefore compute the new head by offsetting the
            // provided pointer without knowing, where the current head is.

            let end = self.end.get().as_ptr() as usize;

            // Note that this skips the overflow check for the add. This is okay, because
            // Layout::from_size_align guarantees us that requested size, rounded up to the
            // requested align, doesn't overflow isize. We also check ourselves that our end pointer
            // doesn't overflow isize. Adding together isize::MAX + isize::MAX still can't overflow
            // usize, so we know we can't overflow usize in a single add.
            //
            // If we ever get even close to overflowing, our out-of-bounds check will trigger after
            // the add, preventing the next, potentially overflowing add.
            //
            // In reality, getting even close to overflowing can't happen on today's hardware, where
            // we use at most 57 bits of a pointer on 5-level paging on Intel.
            let new_head = (ptr.as_ptr() as usize) + new_layout.size();

            if new_head > end {
                unsafe {
                    if !self.try_allocate_pages_for_head(new_head) {
                        return Err(AllocError);
                    }
                }
            }

            self.head.set(unsafe { self.make_pointer(new_head) });

            Ok(NonNull::slice_from_raw_parts(ptr, new_layout.size()))
        } else {
            let new_ptr = self.allocate(new_layout)?;

            // SAFETY: This is ok, because Self::allocate always gives fresh
            // memory, so the regions won't overlap.
            unsafe {
                // TODO(jt): @Cleanup This nonnull slice cast can be replaced with NonNull::as_non_null_ptr()
                // https://github.com/rust-lang/rust/issues/74265
                let new_ptr: NonNull<u8> = new_ptr.cast();
                ptr::copy_nonoverlapping(ptr.as_ptr(), new_ptr.as_ptr(), old_layout.size());
            }

            Ok(new_ptr)
        }
    }

    /// Same as [`Self::grow`], but also fills memory with zeros.
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

    /// Shrink existing allocation.
    ///
    /// Shrinking always succeeds, but memory is not reclaimed. Single-allocation memory reclamation
    /// in arenas is very fussy and would only work for certain usage patterns anyway. Instead, see
    /// [`Arena::reset`] and [`Arena::scope`].
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

    fn is_last_allocation(&self, ptr: NonNull<u8>, layout: Layout) -> bool {
        // Find out, if ptr is our last allocation. This can spuriously return false, because we
        // have to align our allocations based on the layout we receive. If we insert alignment
        // padding, we can't detect last allocations prior to that padding.
        //
        // Otherwise, assuming the caller gave us correct data, we can reason about the following:
        //
        // When we make an allocation, we have a head, which is not necessarilly aligned to carry
        // out the requested allocation. We align this head to produce a pointer which fits the
        // requested alignment, and give it to the user. From this pointer, we compute a new head by
        // adding the requested size.
        //
        // Because we don't have to reconstruct the old head, but only validate the pointer, we can
        // just repeat the neccessary computations here again and see if we get to the current head.
        let ptr = ptr.as_ptr() as usize;
        // No need for overflow checks - we'd fail allocating this in the first place it if
        // overflowed.
        let new_head = ptr + layout.size();

        self.head.get().as_ptr() as usize == new_head
    }

    unsafe fn try_allocate_pages_for_head(&self, head: usize) -> bool {
        if let Some(vm_allocator) = self.vm_allocator {
            // old_capacity should already be aligned to page size, no need to align it ourselves.
            let old_vm_capacity = self.end.get().as_ptr() as usize - self.vm_base.as_ptr() as usize;
            let new_vm_capacity = align_to(head, self.vm_page_size) - self.vm_base.as_ptr() as usize;

            if new_vm_capacity > self.vm_max_capacity {
                return false;
            }

            unsafe {
                if vm_allocator(
                    self.vm_base.as_ptr() as *const c_void,
                    self.vm_page_size,
                    old_vm_capacity,
                    new_vm_capacity,
                ) {
                    let new_end = self.vm_base.as_ptr().add(new_vm_capacity);
                    let new_end_usize = new_end as usize;

                    // Check that we won't have to check for overflow when allocating.
                    //
                    // Layout::from_size_align states that size, rounded up to the nearest multiple of
                    // align, must not overflow isize. This means that, for 64-bit isize/usize, align
                    // can be at most 1<<62. It also means that the aligned size of any requested
                    // allocation can not be larger than isize::MAX.
                    //
                    // Basically, we guarantee here, that we are able to do at least one add without overflowing
                    // usize, after which the regular out-of-memory branch will trigger, because we couldn't
                    // service an allocation request that large anyway.
                    //
                    // Moreover, on today's hardware we don't really use the full 64-bits of a pointer
                    // anyway. On Intel, even with 5-level paging, only 57-bits are used.
                    //
                    // The branch below is overly paranoid and should never trigger for computers of the
                    // 21st century.
                    if new_end_usize > isize::MAX as usize {
                        return false;
                    }

                    self.end.set(NonNull::new_unchecked(new_end));

                    return true;
                }
            }
        }

        false
    }

    /// Make a pointer with provenance of [`Arena::base`].
    ///
    /// # Safety
    ///
    /// - `addr` must not be zero.
    /// - `addr` must be point to a region of memory with the same provenance as the base pointer.
    unsafe fn make_pointer(&self, addr: usize) -> NonNull<u8> {
        self.base.with_addr(unsafe { NonZeroUsize::new_unchecked(addr) })
    }
}

impl Drop for Arena {
    fn drop(&mut self) {
        if let Some(vm_deallocator) = self.vm_deallocator {
            unsafe {
                vm_deallocator(self.vm_base.as_ptr() as *const c_void, self.vm_max_capacity);
            }
        }
    }
}

unsafe impl Allocator for Arena {
    #[inline(always)]
    fn allocate(&self, layout: Layout) -> Result<NonNull<[u8]>, AllocError> {
        Arena::allocate(self, layout)
    }

    // Allocator::allocate_zeroed has a default we can't improve upon.

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
    // We mustn't run the drop code for Arena unless it is the root Arena.
    arena: ManuallyDrop<Arena>,
    arena_parent: &'a Arena,
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

        // If the parent head moved, UB already happened, but it's still good to report this.
        //
        // So what does this check actually do? Because all deallocation functions don't do
        // anything, we know that the only way the parent head can change is to increase. If a
        // change to parent arena occurs, its head must have increased, and couldn't have decreased
        // and then increased again to the same place. We thus know we detect each change to the
        // parent.
        assert!(
            self.arena_parent.head.get() == self.arena.base,
            "Head of the parent arena has moved since it was scoped.",
        );

        // If we are running in VM mode, our end pointer might have moved, so we have to tell our
        // parent about that.
        self.arena_parent.end.set(self.arena.end.get());
    }
}

impl Deref for ArenaScope<'_> {
    type Target = Arena;

    fn deref(&self) -> &Self::Target {
        &self.arena
    }
}

fn align_to(n: usize, align: usize) -> usize {
    debug_assert!(align > 0);
    debug_assert!(align.is_power_of_two());

    // Layout::from_size_align states that size, rounded up to the nearest multiple of align, must
    // not overflow isize. This means that align can be at most 2^63 (on platforms with 64-bit
    // usize/isize).
    //
    // Also, Even with 5-level paging on x64, virtual memory addresses can't use more than 57 bits
    // (12+5*9). The arithmetic below can't really crash on today's hardware.
    //
    // On 32-bit platforms (e.g. wasm), align can be at most 2^30.
    //
    // Generalized, align always has to be at most 1 << (usize::BITS - 2).

    debug_assert!(align <= 1 << (usize::BITS - 2));

    let mask = align - 1;
    (n + mask) & !mask
}

#[cfg(test)]
mod tests {
    extern crate alloc;
    extern crate test;

    use alloc::alloc::Global;
    use alloc::boxed::Box;
    use alloc::vec::Vec;
    use core::hint::black_box;

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
    fn test_simple_usage() {
        let memory = allocate_memory_block();
        let arena = unsafe { Arena::with_non_null_slice(memory.0).unwrap() };

        assert!(arena.head() == arena.base());

        // Align head pointer to 8
        let _a8 = Box::new_in(42u64, &arena);
        let aligned_base = arena.head();
        assert!(is_aligned_to(aligned_base, 8));

        let _a1 = Box::new_in(42u8, &arena);

        assert!(arena.head().as_ptr() as usize == aligned_base.as_ptr() as usize + 1);
        assert!(is_aligned_to(arena.head(), 1));

        let _a2 = Box::new_in(42u16, &arena);

        assert!(arena.head().as_ptr() as usize == aligned_base.as_ptr() as usize + 4);
        assert!(is_aligned_to(arena.head(), 2));

        let _a4 = Box::new_in(42u32, &arena);
        assert!(arena.head().as_ptr() as usize == aligned_base.as_ptr() as usize + 8);
        assert!(is_aligned_to(arena.head(), 4));
    }

    #[test]
    fn fuzz_arena_cheap() {
        fuzz_arena(1000, 10);
    }

    fn fuzz_arena(op_count: usize, reset_count: usize) {
        let memory = allocate_memory_block();
        let mut arena = unsafe { Arena::with_non_null_slice(memory.0).unwrap() };

        let mut r = Rand32::new(0);

        for _ in 0..reset_count {
            arena.reset();

            let mut v0: Vec<u8, _> = Vec::new_in(&arena);
            let mut v1: Vec<u32, _> = Vec::new_in(&arena);

            for _ in 0..op_count {
                match r.rand_u32() % 2 {
                    0 => {
                        v0.push(r.rand_u32() as u8);
                    }

                    1 => {
                        v1.push(r.rand_u32());
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
                        v1.push(r.rand_u32());
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
                        v1.push(r.rand_u32());
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

    #[cfg(any(feature = "virtual_memory_windows", feature = "virtual_memory_unix"))]
    benchmark!(bench_arena_vm, arena_vm_00100_0016_16, 100, 16, 16);
    #[cfg(any(feature = "virtual_memory_windows", feature = "virtual_memory_unix"))]
    benchmark!(bench_arena_vm, arena_vm_01000_0016_16, 1000, 16, 16);
    #[cfg(any(feature = "virtual_memory_windows", feature = "virtual_memory_unix"))]
    benchmark!(bench_arena_vm, arena_vm_10000_0016_16, 10000, 16, 16);

    #[cfg(any(feature = "virtual_memory_windows", feature = "virtual_memory_unix"))]
    benchmark!(bench_arena_vm, arena_vm_00100_0128_16, 100, 128, 16);
    #[cfg(any(feature = "virtual_memory_windows", feature = "virtual_memory_unix"))]
    benchmark!(bench_arena_vm, arena_vm_01000_0128_16, 1000, 128, 16);
    #[cfg(any(feature = "virtual_memory_windows", feature = "virtual_memory_unix"))]
    benchmark!(bench_arena_vm, arena_vm_10000_0128_16, 10000, 128, 16);

    #[cfg(any(feature = "virtual_memory_windows", feature = "virtual_memory_unix"))]
    benchmark!(bench_arena_vm, arena_vm_00100_1024_16, 100, 1024, 16);
    #[cfg(any(feature = "virtual_memory_windows", feature = "virtual_memory_unix"))]
    benchmark!(bench_arena_vm, arena_vm_01000_1024_16, 1000, 1024, 16);
    #[cfg(any(feature = "virtual_memory_windows", feature = "virtual_memory_unix"))]
    benchmark!(bench_arena_vm, arena_vm_10000_1024_16, 10000, 1024, 16);

    benchmark!(bench_arena, arena_00100_0016_16, 100, 16, 16);
    benchmark!(bench_arena, arena_01000_0016_16, 1000, 16, 16);
    benchmark!(bench_arena, arena_10000_0016_16, 10000, 16, 16);

    benchmark!(bench_arena, arena_00100_0128_16, 100, 128, 16);
    benchmark!(bench_arena, arena_01000_0128_16, 1000, 128, 16);
    benchmark!(bench_arena, arena_10000_0128_16, 10000, 128, 16);

    benchmark!(bench_arena, arena_00100_1024_16, 100, 1024, 16);
    benchmark!(bench_arena, arena_01000_1024_16, 1000, 1024, 16);
    benchmark!(bench_arena, arena_10000_1024_16, 10000, 1024, 16);

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

    #[cfg(any(feature = "virtual_memory_windows", feature = "virtual_memory_unix"))]
    fn bench_arena_vm(b: &mut Bencher, allocation_count: usize, allocation_size: usize, allocation_align: usize) {
        #[cfg(target_family = "windows")]
        let mut arena = Arena::with_virtual_memory_windows(64 << 30, true).unwrap();

        #[cfg(target_family = "unix")]
        let mut arena = Arena::with_virtual_memory_unix(64 << 30, true).unwrap();

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

    fn bench_arena(b: &mut Bencher, allocation_count: usize, allocation_size: usize, allocation_align: usize) {
        let capacity = allocation_count * allocation_size;

        let mut v = Vec::with_capacity_in(capacity, Global);
        v.resize(capacity, 0);

        let memory = Vec::leak(v);
        let memory_ptr = NonNull::new(memory.as_mut_ptr()).unwrap();
        let mut arena = unsafe { Arena::with_base_and_capacity(memory_ptr, capacity).unwrap() };

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

    fn is_aligned_to(ptr: NonNull<u8>, align: usize) -> bool {
        assert!(align > 0);
        assert!(align.is_power_of_two());

        let p = ptr.as_ptr() as usize;
        assert!(p > 0);

        let mask = align - 1;
        let inc = p + mask;
        let p_align = inc & !mask;

        p == p_align
    }

    struct MemoryBlock(NonNull<[u8]>);

    impl Drop for MemoryBlock {
        fn drop(&mut self) {
            unsafe {
                // TODO(jt): @Cleanup This nonnull slice cast can be replaced with NonNull::as_non_null_ptr()
                // https://github.com/rust-lang/rust/issues/74265
                deallocate_memory_block(self.0.cast());
            }
        }
    }

    fn allocate_memory_block() -> MemoryBlock {
        let layout = Layout::array::<u8>(1 << 20)
            .unwrap()
            .align_to(1)
            .unwrap()
            .pad_to_align();

        // Allocate a megabyte of unaligned memory. Miri doesn't align allocated memory unless we
        // request it, but other allocators probably align this to at least 16.
        let memory_ptr = Global.allocate(layout).unwrap();

        MemoryBlock(memory_ptr)
    }

    // We deallocate the memory so that miri doesn't report a leak. In general, we still want leak
    // reporting.
    //
    // SAFETY: We have to use the exact same layout when deallocating.
    unsafe fn deallocate_memory_block(base: NonNull<u8>) {
        let layout = Layout::array::<u8>(1 << 20)
            .unwrap()
            .align_to(1)
            .unwrap()
            .pad_to_align();

        unsafe {
            Global.deallocate(base, layout);
        }
    }
}
