#![feature(allocator_api)]

use core::alloc::Layout;
use core::ptr::NonNull;
use std::alloc::Global;

use wfarena::Arena;
use wfinlinevec::InlineString;
use wfinlinevec::InlineVec;

#[test]
fn test_deserialize_in_global_dictionary_struct() {
    #[derive(Debug, PartialEq)]
    #[derive(wfserialize::Deserialize)]
    #[serialize(allocator = "Global")]
    struct Foo {
        i: i32,
        u: u8,
        f: f64,
        s: InlineString<16>,
        a: InlineVec<u8, 32>,
        v: Vec<u8, Global>,
    }

    let str = r#"Foo { i: -1, u: 0, f: 123.23, s: "Peekaboo", a: [2, 3, 5], v: [1, 2, 3]}"#;
    let res = wfserialize::deserialize_in::<Foo, _, _>(str, Global, Global);
    let slice: &[u8] = &[2, 3, 5];

    #[rustfmt::skip]
    assert!(res == Ok(Foo {
        i: -1,
        u: 0,
        f: 123.23,
        s: InlineString::try_from("Peekaboo").unwrap(),
        a: InlineVec::try_from(slice).unwrap(),
        v: vec![1, 2, 3],
    }));
}

#[test]
fn test_deserialize_in_global_vec() {
    #[derive(Debug, PartialEq)]
    #[derive(wfserialize::Deserialize)]
    #[serialize(allocator = "Global")]
    struct Foo(Vec<u8, Global>);

    let str = r#"Foo ([1, 2, 3, 4])"#;
    let res = wfserialize::deserialize_in::<Foo, _, _>(str, Global, Global);

    assert!(res == Ok(Foo(vec![1, 2, 3, 4])));
}

#[test]
fn test_deserialize_in_global_vec_nested() {
    #[derive(Debug, PartialEq)]
    #[derive(wfserialize::Deserialize)]
    #[serialize(allocator = "Global")]
    struct Foo(Vec<Vec<u8, Global>, Global>);

    let str = r#"Foo ([[1, 2], [3], [4]])"#;
    let res = wfserialize::deserialize_in::<Foo, _, _>(str, Global, Global);

    assert!(res == Ok(Foo(vec![vec![1, 2], vec![3], vec![4]])));
}

#[test]
fn test_deserialize_in_arena_vec() {
    #[derive(Debug, PartialEq)]
    #[derive(wfserialize::Deserialize)]
    #[serialize(allocator = "&'a Arena")]
    struct Foo<'a>(Vec<u8, &'a Arena>);

    let b = allocate_memory_block(1 << 20, 16);
    let arena = unsafe { Arena::with_first_block(b.ptr(), b.len()).unwrap() };

    let str = r#"Foo ([1, 2, 3, 4])"#;
    let res = wfserialize::deserialize_in::<Foo, _, _>(str, &arena, &arena);

    assert!(res == Ok(Foo(vec_in([1, 2, 3, 4], &arena))));
}

#[test]
fn test_deserialize_in_arena_vec_nested() {
    #[derive(Debug, PartialEq)]
    #[derive(wfserialize::Deserialize)]
    #[serialize(allocator = "&'a Arena")]
    struct Foo<'a>(Vec<Vec<u8, &'a Arena>, &'a Arena>);

    let b = allocate_memory_block(1 << 20, 16);
    let arena = unsafe { Arena::with_first_block(b.ptr(), b.len()).unwrap() };

    let str = r#"Foo ([[1, 2], [3], [4]])"#;
    let res = wfserialize::deserialize_in::<Foo, _, _>(str, &arena, &arena);

    #[rustfmt::skip]
    assert!(res == Ok(Foo(vec_in([
        vec_in([1, 2], &arena),
        vec_in([3], &arena),
        vec_in([4], &arena),
    ], &arena))));
}

#[test]
fn test_deserialize_rename_unit_struct() {
    #[derive(Debug, PartialEq)]
    #[derive(wfserialize::Deserialize)]
    #[serialize(rename = "MrNiceGuy")]
    struct ActuallyChaos;

    let str = "MrNiceGuy";
    let res = wfserialize::deserialize_in::<ActuallyChaos, _, _>(str, Global, Global);

    assert!(res == Ok(ActuallyChaos));
}

#[test]
fn test_deserialize_rename_tuple_struct() {
    #[derive(Debug, PartialEq)]
    #[derive(wfserialize::Deserialize)]
    #[serialize(rename = "MrNiceGuy")]
    struct ActuallyChaos(i32, f64);

    let str = "MrNiceGuy (1, 42.0)";
    let res = wfserialize::deserialize_in::<ActuallyChaos, _, _>(str, Global, Global);

    assert!(res == Ok(ActuallyChaos(1, 42.0)));
}

#[test]
fn test_deserialize_rename_dictionary_struct() {
    #[derive(Debug, PartialEq)]
    #[derive(wfserialize::Deserialize)]
    #[serialize(rename = "MrNiceGuy")]
    struct ActuallyChaos {
        left_hand: i32,
        right_hand: f64,
    }

    let str = "MrNiceGuy { left_hand: 1, right_hand :42.0 }";
    let res = wfserialize::deserialize_in::<ActuallyChaos, _, _>(str, Global, Global);

    #[rustfmt::skip]
    assert!(res == Ok(ActuallyChaos {
        left_hand: 1,
        right_hand: 42.0
    }));
}

#[test]
fn test_deserialize_in_enum_unit_variant() {
    #[derive(Debug, PartialEq)]
    #[derive(wfserialize::Deserialize)]
    #[serialize(allocator = "Global")]
    enum TheWorldIs {
        Full,
        Of(u32, i32),
        Difficult { choices: Vec<u8, Global> },
    }

    let str = "Full";
    let res = wfserialize::deserialize_in::<TheWorldIs, _, _>(str, Global, Global);

    assert!(res == Ok(TheWorldIs::Full));
}

#[test]
fn test_deserialize_in_enum_tuple_variant() {
    #[derive(Debug, PartialEq)]
    #[derive(wfserialize::Deserialize)]
    #[serialize(allocator = "Global")]
    enum TheWorldIs {
        Full,
        Of(u32, i32),
        Difficult { choices: Vec<u8, Global> },
    }

    let str = "Of (234524, -123132)";
    let res = wfserialize::deserialize_in::<TheWorldIs, _, _>(str, Global, Global);

    assert!(res == Ok(TheWorldIs::Of(234524, -123132)));
}

#[test]
fn test_deserialize_in_enum_map_variant() {
    #[derive(Debug, PartialEq)]
    #[derive(wfserialize::Deserialize)]
    #[serialize(allocator = "Global")]
    enum TheWorldIs {
        Full,
        Of(u32, i32),
        Difficult { choices: Vec<u8, Global> },
    }

    let str = r#"Difficult {
      choices: [0, 1, 1, 8, 9, 9, 9, 0, 1, 9, 1, 1, 0, 1, 1, 8, 7, 2, 5, 3],
    }"#;
    let res = wfserialize::deserialize_in::<TheWorldIs, _, _>(str, Global, Global);

    #[rustfmt::skip]
    assert!(res == Ok(TheWorldIs::Difficult {
        choices: vec![0, 1, 1, 8, 9, 9, 9, 0, 1, 9, 1, 1, 0, 1, 1, 8, 7, 2, 5, 3],
    }));
}

fn vec_in<T, A: core::alloc::Allocator, const N: usize>(array: [T; N], allocator: A) -> Vec<T, A> {
    let mut v = Vec::new_in(allocator);
    v.extend(array);

    v
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
        use core::alloc::Allocator;
        unsafe {
            Global.deallocate(self.ptr.cast(), self.layout);
        }
    }
}

fn allocate_memory_block(size: usize, align: usize) -> MemoryBlock {
    use core::alloc::Allocator;
    let layout = Layout::from_size_align(size, align).unwrap();

    // Allocate a megabyte of unaligned memory. Miri doesn't align allocated memory unless we
    // request it, but other allocators probably align this to at least 16.
    let ptr = Global.allocate(layout).unwrap();

    MemoryBlock { ptr, layout }
}
