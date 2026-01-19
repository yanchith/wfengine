#![feature(allocator_api)]

mod inlinestring;
mod inlinevec;

pub use inlinestring::InlineString;
pub use inlinevec::InlineVec;
pub use inlinevec::InlineVecIter;

#[derive(Debug)]
pub struct CapacityError<T> {
    pub value: T,
}

#[derive(Debug)]
pub struct TryFromError;
