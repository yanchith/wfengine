use core::alloc::Layout;
use core::ptr::NonNull;

use wfarena::Arena;

fn main() {
    let memory = vec![0u8; 1024 << 20];

    let memory_slice = memory.leak();
    let memory_ptr = NonNull::new(memory_slice.as_mut_ptr()).unwrap();
    let memory_len = memory_slice.len();

    let arena_result = unsafe { Arena::with_first_block(memory_ptr, memory_len) };

    let mut arena = arena_result.unwrap();

    let p = arena.allocate(Layout::new::<[u64; 1024]>()).unwrap();
    println!("Allocated memory at {p:?}");

    unsafe {
        *(p.as_ptr() as *mut [u64; 1024]) = [42; 1024];
    }
    println!("Written to memory");

    unsafe {
        let values: [u64; 1024] = *(p.as_ptr() as *mut [u64; 1024]);
        let v = values[0];
        println!("First value is {v}");
    }

    println!(
        "Arena has reserved size {}KB, allocated size {}KB",
        arena.reserved_size() >> 10,
        arena.allocated_size() >> 10,
    );

    arena.reset();

    println!("Done");
}
