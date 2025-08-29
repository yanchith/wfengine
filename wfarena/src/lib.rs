#![no_std]
#![feature(allocator_api)]
// Enable benchmarking
#![cfg_attr(test, feature(test))]

//! This library contains [`Arena`], an arena allocator that implements the allocator API
//! ([`core::alloc::Allocator`]). It also contains [`Memory`], a helper structure that couples the
//! Arena with the data allocated inside into a single unit that can be moved as one.
//!
//! See individual module docummentation for more.

mod arena;
mod memory;

pub use crate::arena::ARENA_HEADER_SIZE;
pub use crate::arena::Arena;
pub use crate::arena::ArenaInitError;
pub use crate::arena::ArenaScope;
pub use crate::memory::Memory;
