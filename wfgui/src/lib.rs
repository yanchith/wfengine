#![no_std]
#![feature(allocator_api)]

// TODO(yan): Don't even depend on alloc, if we ever have the time to write our
// own collections, or Vec/hashbrown becomes libcore.
extern crate alloc;

mod core;
mod widgets;

pub use crate::core::*;
pub use crate::widgets::*;
