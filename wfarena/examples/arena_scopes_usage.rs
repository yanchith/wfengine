#![feature(allocator_api)]

use core::alloc::Layout;
use core::ptr::NonNull;

use wfarena::ARENA_HEADER_SIZE;
use wfarena::Arena;

const LAYOUT: Layout = Layout::new::<u128>();

//
// If we know that we'll be entering a part of the program after which we can reset all of the
// allocations made inside, we can do so explicitly with Arena::scope. After the scope guard is
// dropped, the arena will once again have available the memory that was used while the scope was
// active.
//
// Scopes are also useful for bulk processing of data. For example, we may have to process a large
// amount of data spread across thousands of smaller files. We'd like to use arenas for this, but
// not reset them entirely after each processed batch, because maybe we still care about the data
// stored inside. Scopes allow us to reset just the tail part of the arena:
//
// fn process_data(temp: &Arena, files: &[File]) {
//     for f in files {
//         // Each time through the loop, temp gets reset back to state
//         // it was in at the beginning of the function.
//         process_file(arena.scope(), f);
//     }
// }
//

fn main() {
    let mut memory = [0u128; 1024];
    let memory_base: NonNull<u8> = NonNull::new(&mut memory).unwrap().cast();

    let mut arena = unsafe { Arena::with_first_block(memory_base, size_of_val(&memory)).unwrap() };

    assert!(arena.allocated_size() == ARENA_HEADER_SIZE);

    {
        let scope_guard = unsafe { arena.scope_unchecked() };
        let scope = scope_guard.arena();
        // We can use the scope to allocate data here. The scope will be automatically cleaned up at
        // the end of this block (when the scope guard is dropped).

        let p = scope.allocate(LAYOUT).unwrap();
        let p = p.as_ptr() as *mut u8;

        assert!(scope.allocated_size() == ARENA_HEADER_SIZE + size_of::<u128>());

        unsafe {
            *p = 42;
        }

        // We can't use the original arena while the scope exists. Had we used Arena::scope, this
        // would be checked at runtime each time the allocator is used.
        //
        // let _ = arena.allocate(LAYOUT).unwrap();

        // We can't move the arena while a scope exists, because the scope guard has a drop impl
        // which forces the compiler to keep it alive until the end of the block. The following code
        // fails at compile-time:
        //
        // let another_arena: Arena = unsafe { make_bad_arena() };
        // let a = core::mem::replace(&mut arena, another_arena);
        // core::mem::forget(a);
    }

    assert!(arena.allocated_size() == ARENA_HEADER_SIZE); // The arena is empty once the scope ended.

    // We can use the original arena again!
    {
        let p = arena.allocate(LAYOUT).unwrap();
        let p = p.as_ptr() as *mut u8;

        assert!(arena.allocated_size() == ARENA_HEADER_SIZE + size_of::<u128>());

        unsafe {
            *p = 42;
        }
    }

    arena.reset();

    // Scopes are really useful for processing an unknown number of inputs when we need temporary
    // memory to process each one.

    let mut shared_data = Vec::new_in(&arena);
    for i in 0..10 {
        shared_data.push(i);
    }

    let mut result = 0;
    for j in 0..10 {
        let scope = unsafe { arena.scope_unchecked() };
        result += process(&scope, j, &shared_data);
    }
    println!("Result is {result}");

    println!("Done");
}

// This function doesn't do anything useful, but it does allocate a few things in temp while also
// using data allocated in temp previously.
fn process(temp: &Arena, value: i32, data: &[i32]) -> i32 {
    let mut x = value;

    let mut vec = Vec::new_in(temp);
    for v in data {
        if x % 2 == 0 {
            vec.push(x * v);
        }
        x += v;
    }

    vec.iter().sum()
}

// Make an invalid arena. It won't really be used, we just need a quick placeholder.
#[allow(dead_code)]
unsafe fn make_bad_arena() -> Arena {
    panic!()
}
