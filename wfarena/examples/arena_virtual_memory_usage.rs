#[cfg(any(target_family = "windows", target_family = "unix"))]
use core::ffi::c_void;
#[cfg(any(target_family = "windows", target_family = "unix"))]
use core::ptr;

#[cfg(any(target_family = "windows", target_family = "unix"))]
use wfarena::Arena;

// Arenas can be used with virtual memory. Instead of giving the Arena a region of memory it can use
// directly, we give a set of OS functions to manage virtual memory.
//
// For example,This way, we can be lazy about finding large-enough contiguous address ranges for the
// Arena to use ahead of time, and can just reserve huge amounts of virtual memory ahead of time,
// and let the virtual memory system handle the mapping. The reported memory use of our program
// stays low until we actually start committing the memory we reserved.
//
// If we allow arena chaining, we can explicitly trade off speed for the ability to release reserved
// memory back the OS by tweaking reserve_size size and commit_size. Each time an arena is reset, it
// releases all blocks except the first one back to the OS.
//
// Configuring reserve_size: 64 << 30, commit_size 32 << 20, allow_chaining: false, we get a huge
// reserved block that commits as needed, and is never release back to the OS while the arena
// exists.
//
// Configuring reserve_size: 256 << 20, commit_size << 20, allow_chaining: true, we get an
// infinitely growing arena out of moderately sized blocks that can be released back to the OS, if
// they are not needed anymore.
//

#[cfg(not(any(target_family = "windows", target_family = "unix")))]
fn main() {
    println!("This examle only works on Windows or on Unix systems");
}

#[cfg(any(target_family = "windows", target_family = "unix"))]
fn main() {
    println!("Running arena virtual memory example:\n\n");

    let mut arena = unsafe {
        Arena::with_virtual_memory(
            vm_reserve,
            vm_commit,
            vm_release,
            vm_page_size(),
            256 << 10,
            32 << 10,
            true,
        )
        .unwrap()
    };

    println!(
        "Arena (before use) has reserved size {}KB, commited size {}KB, allocated size {}KB in {} blocks",
        arena.reserved_size() >> 10,
        arena.committed_size() >> 10,
        arena.allocated_size() >> 10,
        arena.count_blocks(),
    );

    allocate_and_use(&mut arena);

    println!(
        "Arena (after use) has reserved size {}KB, commited size {}KB, allocated size {}KB in {} blocks",
        arena.reserved_size() >> 10,
        arena.committed_size() >> 10,
        arena.allocated_size() >> 10,
        arena.count_blocks(),
    );

    println!("Done");
}

#[cfg(any(target_family = "windows", target_family = "unix"))]
fn allocate_and_use(arena: &mut Arena) {
    use core::alloc::Layout;

    for i in 1..50 {
        let size: usize = i << 10;
        let align: usize = usize::next_power_of_two(i);
        let layout = Layout::from_size_align(size, align).unwrap();

        let p = arena.allocate(layout).unwrap();
        let p = p.as_ptr() as *mut u8;

        unsafe {
            *p = 42;
        }

        let x = unsafe { *p };
        println!("Allocated and used {p:?}");

        assert!(x == 42);
    }
}

#[cfg(target_family = "windows")]
fn vm_page_size() -> usize {
    use windows_sys::Win32::System::SystemInformation::GetSystemInfo;
    use windows_sys::Win32::System::SystemInformation::SYSTEM_INFO;

    let mut system_info: SYSTEM_INFO = unsafe { core::mem::zeroed() };
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
    use libc::PROT_READ;
    use libc::PROT_WRITE;
    use libc::mmap;

    unsafe {
        mmap(
            ptr::null_mut(),
            size,
            PROT_READ | PROT_WRITE,
            MAP_PRIVATE | MAP_ANONYMOUS,
            0,
            0,
        )
    }
}

#[cfg(target_family = "unix")]
fn vm_commit(_ptr: *mut c_void, _size: usize) -> bool {
    // NOTE(jt): On Unix systems, unlike on Windows, we can skip the commit step, if we specify
    // PROT_READ|PROT_WRITE when reserving. mmap(2) reserves the memory, and it is mapped in the
    // OS fault handlers when first accessed. If we didn't specify those when reserving, we'd
    // have to call mprotect(2) here.
    true
}

#[cfg(target_family = "unix")]
fn vm_release(ptr: *mut c_void, size: usize) {
    use libc::munmap;

    unsafe {
        munmap(ptr, size);
    }
}
