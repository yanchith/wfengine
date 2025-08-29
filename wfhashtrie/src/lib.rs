#![no_std]
#![feature(allocator_api)]
// Enable benchmarking
#![cfg_attr(test, feature(test))]

use core::alloc::Allocator;
use core::alloc::Layout;
use core::borrow::Borrow;
use core::hash::BuildHasher;
use core::hash::Hash;
use core::hash::Hasher;
use core::marker::PhantomData;
use core::ops::Index;
use core::ops::IndexMut;
use core::ptr;
use core::ptr::NonNull;

#[repr(C)]
struct Node<K, V> {
    key: K,
    value: V,
    children: [*mut Node<K, V>; 4],
    parent: *mut Node<K, V>,
}

/// An arena-friendly dictionary based on a Hash Trie.
///
/// It supports insertion and retrieval, but not deletion. New memory is allocated as needed, but
/// already allocated trie nodes are never deallocated or copied.
///
/// The Hash Trie is based on the design of Nullprogram (Chris Wellon) and NRK, full post here: Full
/// post here: <https://nullprogram.com/blog/2023/09/30/>.
///
/// The core concepts are descripbed by the following function:
///
/// ```c
/// valtype *upsert(hashmap **m, keytype key, arena *perm)
/// {
///     for (uint64_t h = hash(key); *m; h <<= 2) {
///         if (equals(key, (*m)->key)) {
///             return &(*m)->value;
///         }
///         m = &(*m)->child[h>>62];
///     }
///     if (!perm) {
///         return 0;
///     }
///     *m = new(perm, hashmap);
///     (*m)->key = key
///     return &(*m)->value;
/// }
/// ```
///
/// Unlike Rust's hashmap (hashbrown), the random state S is the last type param here, so we can
/// default it. We almost always want to set the allocator, but not always care about the hasher, as
/// long as the default one is good enough.
///
/// The default hasher for the Hash Trie is FNV1a.
// TODO(yan): Implement good Debug (we currently just derive it).
// TODO(yan): Derive implement PartialEq/Eq.
#[derive(Debug)]
pub struct HashTrie<K, V, A: Allocator, S = HashTrieBuildHasher> {
    root: *mut Node<K, V>,
    len: usize,
    allocator: A,
    hasher: S,
}

impl<K, V, A: Allocator> HashTrie<K, V, A, HashTrieBuildHasher> {
    #[inline]
    pub fn new_in(allocator: A) -> Self {
        Self {
            root: ptr::null_mut(),
            len: 0,
            allocator,
            hasher: HashTrieBuildHasher,
        }
    }
}

impl<K, V, A: Allocator, S: BuildHasher> HashTrie<K, V, A, S> {
    #[inline]
    pub fn with_hasher_in(hasher: S, allocator: A) -> Self {
        Self {
            root: ptr::null_mut(),
            len: 0,
            allocator,
            hasher,
        }
    }
}

impl<K: Eq + Hash, V, A: Allocator, S: BuildHasher> HashTrie<K, V, A, S> {
    #[inline]
    pub fn insert(&mut self, key: K, value: V) -> Option<V> {
        let mut h = self.hasher.hash_one(&key);
        let mut parent: *mut Node<K, V> = ptr::null_mut();
        let mut node: &mut *mut Node<K, V> = &mut self.root;

        unsafe {
            while *node != ptr::null_mut() {
                if key == (**node).key {
                    let v = ptr::replace(&mut (**node).value, value);
                    return Some(v);
                }

                // Note: we could change the tree branching to any power of two by having a
                // different size of the pointer arrays in the nodes and shifting out more
                // bits. However, benchmarks by the Nullprogram and NRK show 4-way branching is the
                // performance-memory sweet-spot.

                // Even if usize is 32-bit or 16-bit, this should still behave correctly, because it
                // just shifts the top two bits down to get the index (0..4).
                parent = *node;
                node = &mut (**node).children[(h >> 62) as usize];

                // At depth 32 we shift out all the bits. The hash will only ever produce a zero
                // index, and the tree branch degrages to a linked list with very fat nodes.
                h <<= 2;
            }
        }

        debug_assert!(*node == ptr::null_mut());

        let node_layout = Layout::new::<Node<K, V>>();
        match self.allocator.allocate(node_layout) {
            Ok(ptr) => {
                let ptr: NonNull<Node<K, V>> = ptr.cast();
                let p = ptr.as_ptr();

                // Initialize the node without reading. The memory is not strictly uninitialized,
                // but it may not contain valid bit patterns for K and V.
                unsafe {
                    (&raw mut (*p).key).write(key);
                    (&raw mut (*p).value).write(value);
                    (&raw mut (*p).children).write([ptr::null_mut(); 4]);
                    (&raw mut (*p).parent).write(parent);
                }

                *node = p;

                self.len += 1;
            }
            Err(err) => {
                let size = node_layout.size();
                let align = node_layout.align();
                panic!("Failed to allocate layout size={size} align={align}: {err}");
            }
        }

        None
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.len
    }

    #[inline]
    pub fn get<Q>(&self, key: &Q) -> Option<&V>
    where
        K: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        let mut h = self.hasher.hash_one(key);
        let mut node: &*mut Node<K, V> = &self.root;

        unsafe {
            while *node != ptr::null_mut() {
                let node_key = &(**node).key;
                if key == node_key.borrow() {
                    let v = &(**node).value;
                    return Some(v);
                }

                // Even if usize is 32-bit or 16-bit, this should still behave correctly, because it
                // just shifts the top two bits down to get the index (0..4).
                node = &(**node).children[(h >> 62) as usize];

                // At depth 32 we shift out all the bits. The hash will only ever produce a zero
                // index, and the tree branch degrages to a linked list with very fat nodes.
                h <<= 2;
            }
        }

        debug_assert!(*node == ptr::null_mut());

        None
    }

    #[inline]
    pub fn get_mut<Q>(&mut self, key: &Q) -> Option<&mut V>
    where
        K: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        let mut h = self.hasher.hash_one(key);
        let mut node: &mut *mut Node<K, V> = &mut self.root;

        unsafe {
            while *node != ptr::null_mut() {
                let node_key = &(**node).key;
                if key == node_key.borrow() {
                    let v = &mut (**node).value;
                    return Some(v);
                }

                // Even if usize is 32-bit or 16-bit, this should still behave correctly, because it
                // just shifts the top two bits down to get the index (0..4).
                node = &mut (**node).children[(h >> 62) as usize];

                // At depth 32 we shift out all the bits. The hash will only ever produce a zero
                // index, and the tree branch degrages to a linked list with very fat nodes.
                h <<= 2;
            }
        }

        debug_assert!(*node == ptr::null_mut());

        None
    }

    #[inline]
    pub fn iter(&self) -> HashTrieIter<'_, K, V> {
        HashTrieIter {
            next_node: self.root,
            _phantom: PhantomData,
        }
    }

    #[inline]
    pub fn iter_mut(&mut self) -> HashTrieIterMut<'_, K, V> {
        HashTrieIterMut {
            next_node: self.root,
            _phantom: PhantomData,
        }
    }
}

impl<K: Eq + Hash, V: Default, A: Allocator, S: BuildHasher> HashTrie<K, V, A, S> {
    // TODO(yan): Consider doing the entry API in addition to this, or maybe instead? This is more
    // elegant, but entry is more consistent with Rust.
    #[inline]
    pub fn get_or_insert(&mut self, key: K) -> &mut V {
        let mut h = self.hasher.hash_one(&key);
        let mut parent: *mut Node<K, V> = ptr::null_mut();
        let mut node: &mut *mut Node<K, V> = &mut self.root;

        unsafe {
            while *node != ptr::null_mut() {
                if key == (**node).key {
                    return &mut (**node).value;
                }

                // Even if usize is 32-bit or 16-bit, this should still behave correctly, because it
                // just shifts the top two bits down to get the index (0..4).
                parent = *node;
                node = &mut (**node).children[(h >> 62) as usize];

                // At depth 32 we shift out all the bits. The hash will only ever produce a zero
                // index, and the tree branch degrages to a linked list with very fat nodes.
                h <<= 2;
            }
        }

        debug_assert!(*node == ptr::null_mut());

        let node_layout = Layout::new::<Node<K, V>>();
        match self.allocator.allocate(node_layout) {
            Ok(ptr) => {
                let ptr: NonNull<Node<K, V>> = ptr.cast();
                let p = ptr.as_ptr();

                // Initialize the node without reading. The memory is not strictly uninitialized,
                // but it may not contain valid bit patterns for K and V.
                unsafe {
                    (&raw mut (*p).key).write(key);
                    (&raw mut (*p).value).write(V::default());
                    (&raw mut (*p).children).write([ptr::null_mut(); 4]);
                    (&raw mut (*p).parent).write(parent);
                }

                *node = p;

                self.len += 1;

                unsafe { &mut (*p).value }
            }
            Err(err) => {
                let size = node_layout.size();
                let align = node_layout.align();
                panic!("Failed to allocate layout size={size} align={align}: {err}");
            }
        }
    }
}

impl<K: Eq + Hash + Clone, V: Clone, A: Allocator + Clone, S: BuildHasher + Clone> Clone for HashTrie<K, V, A, S> {
    fn clone(&self) -> Self {
        let mut h = HashTrie::with_hasher_in(self.hasher.clone(), self.allocator.clone());

        // TODO(yan): @Speed We don't need to through insert hashing. We could just clone the nodes.
        for (k, v) in self.iter() {
            h.insert(k.clone(), v.clone());
        }

        h
    }
}

impl<K, V, A: Allocator, S> Drop for HashTrie<K, V, A, S> {
    fn drop(&mut self) {
        // TODO(yan): @Correctness This is recursive.
        unsafe fn deallocate_node<K, V, A: Allocator>(node: *mut Node<K, V>, allocator: &A) {
            // TODO(yan): @Speed can we prove the compiler does nothing here if these types don't
            // need dropping?
            //
            // SAFETY: These should have been properly initialized by the code that created
            // them. Right? Right?!
            unsafe {
                ptr::drop_in_place(&mut (*node).key);
                ptr::drop_in_place(&mut (*node).value);

                for child in (*node).children {
                    // SAFETY: Only attempt to deallocate nun-null nodes.
                    if child != ptr::null_mut() {
                        deallocate_node(child, allocator)
                    }
                }
            }

            // SAFETY: The pointer that gets passed here must not be null, so we can do
            // NonNull::new_unchecked.
            let node_ptr: NonNull<u8> = unsafe { NonNull::new_unchecked(node).cast() };
            let node_layout = Layout::new::<Node<K, V>>();

            unsafe {
                allocator.deallocate(node_ptr, node_layout);
            }
        }

        // SAFETY: We have to pass in a non-null pointer that was allocated by our allocator.
        unsafe {
            if self.root != ptr::null_mut() {
                deallocate_node(self.root, &self.allocator);
            }
        }
    }
}

impl<K: Eq + Hash + Borrow<Q>, Q: Eq + Hash + ?Sized, V, A: Allocator, S: BuildHasher> Index<&Q>
    for HashTrie<K, V, A, S>
{
    type Output = V;

    #[inline]
    fn index(&self, key: &Q) -> &Self::Output {
        self.get(key).unwrap()
    }
}

impl<K: Eq + Hash + Borrow<Q>, Q: Eq + Hash + ?Sized, V, A: Allocator, S: BuildHasher> IndexMut<&Q>
    for HashTrie<K, V, A, S>
{
    #[inline]
    fn index_mut(&mut self, key: &Q) -> &mut Self::Output {
        self.get_mut(key).unwrap()
    }
}

impl<'a, K: Eq + Hash, V, A: Allocator, S: BuildHasher> IntoIterator for &'a HashTrie<K, V, A, S> {
    type Item = (&'a K, &'a V);
    type IntoIter = HashTrieIter<'a, K, V>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a, K: Eq + Hash, V, A: Allocator, S: BuildHasher> IntoIterator for &'a mut HashTrie<K, V, A, S> {
    type Item = (&'a K, &'a mut V);
    type IntoIter = HashTrieIterMut<'a, K, V>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}

pub struct HashTrieIter<'a, K, V> {
    next_node: *mut Node<K, V>,
    _phantom: PhantomData<&'a ()>,
}

impl<'a, K: 'a, V: 'a> Iterator for HashTrieIter<'a, K, V> {
    type Item = (&'a K, &'a V);

    fn next(&mut self) -> Option<Self::Item> {
        if self.next_node != ptr::null_mut() {
            let node = self.next_node;

            // Get key and value from the current node.
            let key = unsafe { &(*node).key };
            let value = unsafe { &(*node).value };

            // Traverse to the next node.
            let c0 = unsafe { (*node).children[0] };
            let c1 = unsafe { (*node).children[1] };
            let c2 = unsafe { (*node).children[2] };
            let c3 = unsafe { (*node).children[3] };
            let parent = unsafe { (*node).parent };

            if c0 != ptr::null_mut() {
                self.next_node = c0;
            } else if c1 != ptr::null_mut() {
                self.next_node = c1;
            } else if c2 != ptr::null_mut() {
                self.next_node = c2;
            } else if c3 != ptr::null_mut() {
                self.next_node = c3;
            } else if parent != ptr::null_mut() {
                let mut child_node = node;
                let mut parent_node = parent;
                let mut next_node = ptr::null_mut();

                while next_node == ptr::null_mut() && parent_node != ptr::null_mut() {
                    // Skip child zero, because we either came from there or not, but will never
                    // want to traverse that way again.
                    let mut try_next_node = 1;
                    for i in 1..4 {
                        let c = unsafe { (*parent_node).children[i] };
                        if c == child_node {
                            try_next_node = i + 1;
                            break;
                        }
                    }

                    for i in try_next_node..4 {
                        let c = unsafe { (*parent_node).children[i] };
                        if c != ptr::null_mut() {
                            next_node = c;
                            break;
                        }
                    }

                    child_node = parent_node;
                    parent_node = unsafe { (*parent_node).parent };
                }

                self.next_node = next_node;
            } else {
                self.next_node = ptr::null_mut();
            }

            return Some((key, value));
        }

        None
    }
}

pub struct HashTrieIterMut<'a, K, V> {
    next_node: *mut Node<K, V>,
    _phantom: PhantomData<&'a mut ()>,
}

impl<'a, K: 'a, V: 'a> Iterator for HashTrieIterMut<'a, K, V> {
    type Item = (&'a K, &'a mut V);

    fn next(&mut self) -> Option<Self::Item> {
        if self.next_node != ptr::null_mut() {
            let node = self.next_node;

            // Get key and value from the current node.
            let key = unsafe { &(*node).key };
            let value = unsafe { &mut (*node).value };

            // Traverse to the next node.
            let c0 = unsafe { (*node).children[0] };
            let c1 = unsafe { (*node).children[1] };
            let c2 = unsafe { (*node).children[2] };
            let c3 = unsafe { (*node).children[3] };
            let parent = unsafe { (*node).parent };

            if c0 != ptr::null_mut() {
                self.next_node = c0;
            } else if c1 != ptr::null_mut() {
                self.next_node = c1;
            } else if c2 != ptr::null_mut() {
                self.next_node = c2;
            } else if c3 != ptr::null_mut() {
                self.next_node = c3;
            } else if parent != ptr::null_mut() {
                let mut child_node = node;
                let mut parent_node = parent;
                let mut next_node = ptr::null_mut();

                while next_node == ptr::null_mut() && parent_node != ptr::null_mut() {
                    // Skip child zero, because we either came from there or not, but will never
                    // want to traverse that way again.
                    let mut try_next_node = 1;
                    for i in 1..4 {
                        let c = unsafe { (*parent_node).children[i] };
                        if c == child_node {
                            try_next_node = i + 1;
                            break;
                        }
                    }

                    for i in try_next_node..4 {
                        let c = unsafe { (*parent_node).children[i] };
                        if c != ptr::null_mut() {
                            next_node = c;
                            break;
                        }
                    }

                    child_node = parent_node;
                    parent_node = unsafe { (*parent_node).parent };
                }

                self.next_node = next_node;
            } else {
                self.next_node = ptr::null_mut();
            }

            return Some((key, value));
        }

        None
    }
}

/// The default [`BuildHasher`] for [`HashTrie`]. Currently FNV1a, but can change any time.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct HashTrieBuildHasher;

impl BuildHasher for HashTrieBuildHasher {
    type Hasher = HashTrieHasher;

    #[inline]
    fn build_hasher(&self) -> Self::Hasher {
        HashTrieHasher::new()
    }
}

/// The default [`Hasher`] for [`HashTrie`]. Currently FNV1a, but can change any time.
pub struct HashTrieHasher(u64);

impl HashTrieHasher {
    #[inline]
    fn new() -> Self {
        Self(0xcbf29ce484222325)
    }
}

impl Hasher for HashTrieHasher {
    #[inline]
    fn write(&mut self, bytes: &[u8]) {
        for &byte in bytes {
            self.0 ^= u64::from(byte);
            self.0 = self.0.wrapping_mul(0x100000001b3);
        }
    }

    #[inline]
    fn finish(&self) -> u64 {
        self.0
    }
}

#[cfg(test)]
mod tests {
    extern crate alloc;
    extern crate test;

    use alloc::alloc::Global;
    use alloc::string::String;
    use alloc::vec::Vec;
    use core::hash::BuildHasherDefault;
    use core::mem;

    use hashbrown::HashMap;
    use test::Bencher;
    use wfarena::Arena;

    use super::*;

    #[test]
    fn test_int_keys() {
        let mut h: HashTrie<i32, i32, _> = HashTrie::new_in(Global);
        assert!(h.len() == 0);

        h.insert(13, 42);
        assert!(h.len() == 1);

        h.insert(15, 55);
        assert!(h.len() == 2);

        h.insert(1, 0);
        assert!(h.len() == 3);

        assert!(h.get(&13) == Some(&42));
        assert!(h.get(&15) == Some(&55));
        assert!(h.get(&1) == Some(&0));

        assert!(h[&13] == 42);
        assert!(h[&15] == 55);
        assert!(h[&1] == 0);

        let v0 = h.get_mut(&13).unwrap();
        *v0 = 43;

        assert!(h.len() == 3);
        assert!(h.get(&13) == Some(&43));

        let v1 = &mut h[&15];
        *v1 = 9999;

        assert!(h[&15] == 9999);
    }

    #[test]
    fn test_string_keys() {
        let mut h: HashTrie<String, i32, _> = HashTrie::new_in(Global);
        assert!(h.len() == 0);

        h.insert(String::from("13"), 42);
        assert!(h.len() == 1);

        h.insert(String::from("15"), 55);
        assert!(h.len() == 2);

        h.insert(String::from("1"), 0);
        assert!(h.len() == 3);

        assert!(h.get("13") == Some(&42));
        assert!(h.get("15") == Some(&55));
        assert!(h.get("1") == Some(&0));

        assert!(h["13"] == 42);
        assert!(h["15"] == 55);
        assert!(h["1"] == 0);

        let v0 = h.get_mut("13").unwrap();
        *v0 = 43;

        assert!(h.len() == 3);
        assert!(h.get("13") == Some(&43));

        let v1 = &mut h["15"];
        *v1 = 9999;

        assert!(h["15"] == 9999);
    }

    #[test]
    fn test_int_keys_custom_hasher() {
        let hasher = BuildHasherDefault::<ahash::AHasher>::default();
        let mut h: HashTrie<i32, i32, _, _> = HashTrie::with_hasher_in(hasher, Global);
        assert!(h.len() == 0);

        h.insert(13, 42);
        assert!(h.len() == 1);

        h.insert(15, 55);
        assert!(h.len() == 2);

        h.insert(1, 0);
        assert!(h.len() == 3);

        assert!(h.get(&13) == Some(&42));
        assert!(h.get(&15) == Some(&55));
        assert!(h.get(&1) == Some(&0));

        assert!(h[&13] == 42);
        assert!(h[&15] == 55);
        assert!(h[&1] == 0);

        let v0 = h.get_mut(&13).unwrap();
        *v0 = 43;

        assert!(h.len() == 3);
        assert!(h.get(&13) == Some(&43));

        let v1 = &mut h[&15];
        *v1 = 9999;

        assert!(h[&15] == 9999);
    }

    #[test]
    fn test_string_keys_custom_hasher() {
        let hasher = BuildHasherDefault::<ahash::AHasher>::default();
        let mut h: HashTrie<String, i32, _, _> = HashTrie::with_hasher_in(hasher, Global);
        assert!(h.len() == 0);

        h.insert(String::from("13"), 42);
        assert!(h.len() == 1);

        h.insert(String::from("15"), 55);
        assert!(h.len() == 2);

        h.insert(String::from("1"), 0);
        assert!(h.len() == 3);

        assert!(h.get("13") == Some(&42));
        assert!(h.get("15") == Some(&55));
        assert!(h.get("1") == Some(&0));

        assert!(h["13"] == 42);
        assert!(h["15"] == 55);
        assert!(h["1"] == 0);

        let v0 = h.get_mut("13").unwrap();
        *v0 = 43;

        assert!(h.len() == 3);
        assert!(h.get("13") == Some(&43));

        let v1 = &mut h["15"];
        *v1 = 9999;

        assert!(h["15"] == 9999);
    }

    #[test]
    fn test_get_or_insert() {
        let mut h: HashTrie<String, i32, _, _> = HashTrie::new_in(Global);
        assert!(h.len() == 0);

        let v0 = h.get_or_insert(String::from("13"));
        *v0 = 42;
        assert!(h.len() == 1);
        assert!(h["13"] == 42);

        h.get_or_insert(String::from("14"));
        assert!(h.len() == 2);
        assert!(h["14"] == 0);
    }

    #[test]
    fn test_iter() {
        // TODO(yan): @Cleanup Miri (at compile time) has an issue with HashMap::new and
        // HashMap::new_in and requires that we pass in the hasher as well. Uhh, whatever.
        let hasher = BuildHasherDefault::<ahash::AHasher>::default();
        let mut h: HashTrie<i32, i32, _, _> = HashTrie::new_in(Global);
        let mut e: HashMap<i32, i32, _, _> = HashMap::with_hasher_in(hasher, Global);

        for i in 0..10 {
            h.insert(i, i);
            e.insert(i, i);
        }

        assert!(h.len() == 10);
        assert!(e.len() == 10);

        for (k, _) in &h {
            e.remove(k);
        }

        assert!(e.len() == 0);
    }

    #[test]
    fn test_iter_mut() {
        // TODO(yan): @Cleanup Miri (at compile time) has an issue with HashMap::new and
        // HashMap::new_in and requires that we pass in the hasher as well. Uhh, whatever.
        let hasher = BuildHasherDefault::<ahash::AHasher>::default();
        let mut h: HashTrie<i32, i32, _, _> = HashTrie::new_in(Global);
        let mut e: HashMap<i32, i32, _, _> = HashMap::with_hasher_in(hasher, Global);

        for i in 0..10 {
            h.insert(i, i);
            e.insert(i, i);
        }

        assert!(h.len() == 10);
        assert!(e.len() == 10);

        for (k, v) in &mut h {
            e.remove(k);
            *v += 1;
        }

        assert!(e.len() == 0);
    }

    macro_rules! benchmark {
        ($bench_fn:ident, $bench_ident:ident, $n:expr) => {
            #[bench]
            #[cfg_attr(miri, ignore)]
            fn $bench_ident(b: &mut Bencher) {
                $bench_fn(b, $n);
            }
        };
    }

    benchmark!(bench_insert_hashtrie, insert_hashtrie_0128, 128);
    benchmark!(bench_insert_hashtrie, insert_hashtrie_0256, 256);
    benchmark!(bench_insert_hashtrie, insert_hashtrie_0512, 512);
    benchmark!(bench_insert_hashtrie, insert_hashtrie_1024, 1024);
    benchmark!(bench_insert_hashtrie, insert_hashtrie_2048, 2048);
    benchmark!(bench_insert_hashtrie, insert_hashtrie_4096, 4096);
    benchmark!(bench_insert_hashtrie, insert_hashtrie_8192, 8192);

    benchmark!(bench_insert_hashtrie_arena, insert_hashtrie_arena_0128, 128);
    benchmark!(bench_insert_hashtrie_arena, insert_hashtrie_arena_0256, 256);
    benchmark!(bench_insert_hashtrie_arena, insert_hashtrie_arena_0512, 512);
    benchmark!(bench_insert_hashtrie_arena, insert_hashtrie_arena_1024, 1024);
    benchmark!(bench_insert_hashtrie_arena, insert_hashtrie_arena_2048, 2048);
    benchmark!(bench_insert_hashtrie_arena, insert_hashtrie_arena_4096, 4096);
    benchmark!(bench_insert_hashtrie_arena, insert_hashtrie_arena_8192, 8192);

    benchmark!(bench_insert_hashmap, insert_hashmap_0128, 128);
    benchmark!(bench_insert_hashmap, insert_hashmap_0256, 256);
    benchmark!(bench_insert_hashmap, insert_hashmap_0512, 512);
    benchmark!(bench_insert_hashmap, insert_hashmap_1024, 1024);
    benchmark!(bench_insert_hashmap, insert_hashmap_2048, 2048);
    benchmark!(bench_insert_hashmap, insert_hashmap_4096, 4096);
    benchmark!(bench_insert_hashmap, insert_hashmap_8192, 8192);

    benchmark!(bench_insert_hashmap_arena, insert_hashmap_arena_0128, 128);
    benchmark!(bench_insert_hashmap_arena, insert_hashmap_arena_0256, 256);
    benchmark!(bench_insert_hashmap_arena, insert_hashmap_arena_0512, 512);
    benchmark!(bench_insert_hashmap_arena, insert_hashmap_arena_1024, 1024);
    benchmark!(bench_insert_hashmap_arena, insert_hashmap_arena_2048, 2048);
    benchmark!(bench_insert_hashmap_arena, insert_hashmap_arena_4096, 4096);
    benchmark!(bench_insert_hashmap_arena, insert_hashmap_arena_8192, 8192);

    benchmark!(bench_get_hashtrie, get_hashtrie_0128, 128);
    benchmark!(bench_get_hashtrie, get_hashtrie_0256, 256);
    benchmark!(bench_get_hashtrie, get_hashtrie_0512, 512);
    benchmark!(bench_get_hashtrie, get_hashtrie_1024, 1024);
    benchmark!(bench_get_hashtrie, get_hashtrie_2048, 2048);
    benchmark!(bench_get_hashtrie, get_hashtrie_4096, 4096);
    benchmark!(bench_get_hashtrie, get_hashtrie_8192, 8192);

    benchmark!(bench_get_hashtrie_arena, get_hashtrie_arena_0128, 128);
    benchmark!(bench_get_hashtrie_arena, get_hashtrie_arena_0256, 256);
    benchmark!(bench_get_hashtrie_arena, get_hashtrie_arena_0512, 512);
    benchmark!(bench_get_hashtrie_arena, get_hashtrie_arena_1024, 1024);
    benchmark!(bench_get_hashtrie_arena, get_hashtrie_arena_2048, 2048);
    benchmark!(bench_get_hashtrie_arena, get_hashtrie_arena_4096, 4096);
    benchmark!(bench_get_hashtrie_arena, get_hashtrie_arena_8192, 8192);

    benchmark!(bench_get_hashmap, get_hashmap_0128, 128);
    benchmark!(bench_get_hashmap, get_hashmap_0256, 256);
    benchmark!(bench_get_hashmap, get_hashmap_0512, 512);
    benchmark!(bench_get_hashmap, get_hashmap_1024, 1024);
    benchmark!(bench_get_hashmap, get_hashmap_2048, 2048);
    benchmark!(bench_get_hashmap, get_hashmap_4096, 4096);
    benchmark!(bench_get_hashmap, get_hashmap_8192, 8192);

    benchmark!(bench_get_hashmap_arena, get_hashmap_arena_0128, 128);
    benchmark!(bench_get_hashmap_arena, get_hashmap_arena_0256, 256);
    benchmark!(bench_get_hashmap_arena, get_hashmap_arena_0512, 512);
    benchmark!(bench_get_hashmap_arena, get_hashmap_arena_1024, 1024);
    benchmark!(bench_get_hashmap_arena, get_hashmap_arena_2048, 2048);
    benchmark!(bench_get_hashmap_arena, get_hashmap_arena_4096, 4096);
    benchmark!(bench_get_hashmap_arena, get_hashmap_arena_8192, 8192);

    fn bench_insert_hashtrie(b: &mut Bencher, n: usize) {
        let data = make_bench_data(n);

        b.iter(|| {
            let hasher = BuildHasherDefault::<ahash::AHasher>::default();
            let mut h: HashTrie<u32, u32, _, _> = HashTrie::with_hasher_in(hasher, Global);
            for &elem in &data {
                h.insert(elem, elem);
            }

            h.len()
        });
    }

    fn bench_insert_hashtrie_arena(b: &mut Bencher, n: usize) {
        let data = make_bench_data(n);

        let block = allocate_memory_block(1 << 20, 16);
        let mut arena = unsafe { Arena::with_memory_block(block.ptr(), block.len()).unwrap() };

        b.iter(|| {
            arena.reset();

            let hasher = BuildHasherDefault::<ahash::AHasher>::default();
            let mut h: HashTrie<u32, u32, _, _> = HashTrie::with_hasher_in(hasher, &arena);
            for &elem in &data {
                h.insert(elem, elem);
            }

            let len = h.len();
            mem::forget(h);

            len
        });
    }

    fn bench_insert_hashmap(b: &mut Bencher, n: usize) {
        let data = make_bench_data(n);

        b.iter(|| {
            let hasher = BuildHasherDefault::<ahash::AHasher>::default();
            let mut h: HashMap<u32, u32, _, _> = HashMap::with_hasher_in(hasher, Global);
            for &elem in &data {
                h.insert(elem, elem);
            }

            h.len()
        });
    }

    fn bench_insert_hashmap_arena(b: &mut Bencher, n: usize) {
        let data = make_bench_data(n);

        let block = allocate_memory_block(1 << 20, 16);
        let mut arena = unsafe { Arena::with_memory_block(block.ptr(), block.len()).unwrap() };

        b.iter(|| {
            arena.reset();

            let hasher = BuildHasherDefault::<ahash::AHasher>::default();
            let mut h: HashMap<u32, u32, _, _> = HashMap::with_hasher_in(hasher, &arena);
            for &elem in &data {
                h.insert(elem, elem);
            }

            let len = h.len();
            mem::forget(h);

            len
        });
    }

    fn bench_get_hashtrie(b: &mut Bencher, n: usize) {
        let data = make_bench_data(n);

        let hasher = BuildHasherDefault::<ahash::AHasher>::default();
        let mut h: HashTrie<u32, u32, _, _> = HashTrie::with_hasher_in(hasher, Global);
        for &elem in &data {
            h.insert(elem, elem);
        }

        b.iter(|| {
            let mut sum: u32 = 0;

            for elem in &data {
                sum = sum.wrapping_add(h[elem]);
            }

            sum
        });
    }

    fn bench_get_hashtrie_arena(b: &mut Bencher, n: usize) {
        let data = make_bench_data(n);

        let block = allocate_memory_block(1 << 20, 16);
        let arena = unsafe { Arena::with_memory_block(block.ptr(), block.len()).unwrap() };

        let hasher = BuildHasherDefault::<ahash::AHasher>::default();
        let mut h: HashTrie<u32, u32, _, _> = HashTrie::with_hasher_in(hasher, &arena);
        for &elem in &data {
            h.insert(elem, elem);
        }

        b.iter(|| {
            let mut sum: u32 = 0;

            for elem in &data {
                sum = sum.wrapping_add(h[elem]);
            }

            sum
        });
    }

    fn bench_get_hashmap(b: &mut Bencher, n: usize) {
        let data = make_bench_data(n);

        let hasher = BuildHasherDefault::<ahash::AHasher>::default();
        let mut h: HashMap<u32, u32, _, _> = HashMap::with_hasher_in(hasher, Global);
        for &elem in &data {
            h.insert(elem, elem);
        }

        b.iter(|| {
            let mut sum: u32 = 0;

            for elem in &data {
                sum = sum.wrapping_add(h[elem]);
            }

            sum
        });
    }

    fn bench_get_hashmap_arena(b: &mut Bencher, n: usize) {
        let data = make_bench_data(n);

        let block = allocate_memory_block(1 << 20, 16);
        let arena = unsafe { Arena::with_memory_block(block.ptr(), block.len()).unwrap() };

        let hasher = BuildHasherDefault::<ahash::AHasher>::default();
        let mut h: HashMap<u32, u32, _, _> = HashMap::with_hasher_in(hasher, &arena);
        for &elem in &data {
            h.insert(elem, elem);
        }

        b.iter(|| {
            let mut sum: u32 = 0;

            for elem in &data {
                sum = sum.wrapping_add(h[elem]);
            }

            sum
        });
    }

    fn make_bench_data(n: usize) -> Vec<u32, Global> {
        let mut rng = oorandom::Rand32::new(0);
        let mut data = Vec::new_in(Global);
        data.resize(n, 0);
        for elem in &mut data {
            *elem = rng.rand_u32();
        }

        data
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
}
