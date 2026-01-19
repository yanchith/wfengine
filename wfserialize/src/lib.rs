#![no_std]
#![feature(allocator_api)]

extern crate alloc;

mod fmt;
mod parser;

use alloc::vec::Vec;
use core::alloc::Allocator;
use core::str;

use wfinlinevec::InlineString;
use wfinlinevec::InlineVec;
pub use wfserialize_derive::Deserialize;
pub use wfserialize_derive::Serialize;

pub use crate::fmt::ArrayLikeFormatter;
pub use crate::fmt::DictionaryLikeFormatter;
pub use crate::fmt::Formatter;
pub use crate::parser::Kind;
pub use crate::parser::Node;
pub use crate::parser::ParseError;
pub use crate::parser::Token;
pub use crate::parser::parse_objects_in;

pub const ARRAY_LIKE_MULTILINE_THRESHOLD: usize = 4;
pub const DICTIONARY_LIKE_MULTILINE_THRESHOLD: usize = 1;

pub fn serialize<T: Serialize, A: Allocator>(value: T, allocator: A) -> Vec<u8, A> {
    let mut f = Formatter::new_in(allocator);
    value.serialize(&mut f);

    f.into_data()
}

//
// TODO(jt): @Speed @Memory Instead of parsing and allocating the tree which we then pass to T::deserialize, parse
// the input directly in Deserialize. For all leaf types, we write the code ourselves. For derived
// Deserialize, we generate the parser in the proc macro, utilizing the Lexer from parser.rs.
//
pub fn deserialize_in<T, TA, A>(str: &str, temp: TA, allocator: A) -> Result<T, DeserializeError>
where
    T: Deserialize<A>,
    TA: Allocator + Clone,
    A: Allocator + Clone,
{
    let tree = parse_objects_in(str, temp)?;
    let value: T = T::deserialize(&tree, allocator)?;

    Ok(value)
}

pub trait Serialize {
    fn serialize<A: Allocator>(&self, f: &mut fmt::Formatter<A>);
}

impl Serialize for bool {
    fn serialize<A: Allocator>(&self, f: &mut fmt::Formatter<A>) {
        f.boolean(*self)
    }
}

impl Serialize for i8 {
    fn serialize<A: Allocator>(&self, f: &mut fmt::Formatter<A>) {
        f.int(*self)
    }
}

impl Serialize for i16 {
    fn serialize<A: Allocator>(&self, f: &mut fmt::Formatter<A>) {
        f.int(*self)
    }
}

impl Serialize for i32 {
    fn serialize<A: Allocator>(&self, f: &mut fmt::Formatter<A>) {
        f.int(*self)
    }
}

impl Serialize for i64 {
    fn serialize<A: Allocator>(&self, f: &mut fmt::Formatter<A>) {
        f.int(*self)
    }
}

impl Serialize for u8 {
    fn serialize<A: Allocator>(&self, f: &mut fmt::Formatter<A>) {
        f.uint(*self)
    }
}

impl Serialize for u16 {
    fn serialize<A: Allocator>(&self, f: &mut fmt::Formatter<A>) {
        f.uint(*self)
    }
}

impl Serialize for u32 {
    fn serialize<A: Allocator>(&self, f: &mut fmt::Formatter<A>) {
        f.uint(*self)
    }
}

impl Serialize for u64 {
    fn serialize<A: Allocator>(&self, f: &mut fmt::Formatter<A>) {
        f.uint(*self)
    }
}

impl Serialize for usize {
    fn serialize<A: Allocator>(&self, f: &mut fmt::Formatter<A>) {
        f.uint(u64::try_from(*self).unwrap())
    }
}

impl Serialize for f32 {
    fn serialize<A: Allocator>(&self, f: &mut fmt::Formatter<A>) {
        f.float(*self)
    }
}

impl Serialize for f64 {
    fn serialize<A: Allocator>(&self, f: &mut fmt::Formatter<A>) {
        f.float(*self)
    }
}

impl<T: Serialize> Serialize for [T] {
    fn serialize<A: Allocator>(&self, f: &mut fmt::Formatter<A>) {
        f.array(self.len() > ARRAY_LIKE_MULTILINE_THRESHOLD, |array_fmt| {
            for item in self {
                array_fmt.item(|item_fmt| {
                    item.serialize(item_fmt);
                });
            }
        });
    }
}

macro_rules! impl_serialize_for_tuple {
    ( $length:expr; $( $ty:ident )+ ) => {
        impl<$($ty: Serialize),+> Serialize for ($($ty,)+) {
            #[allow(non_snake_case)]
            fn serialize<A: Allocator>(&self, f: &mut fmt::Formatter<A>) {
                let ($($ty,)+) = self;
                f.tuple($length > ARRAY_LIKE_MULTILINE_THRESHOLD, |tuple_fmt| {
                    $(tuple_fmt.item(|item_fmt| $ty.serialize(item_fmt));)+
                });
            }
        }
    };
}

impl_serialize_for_tuple! { 1; T1 }
impl_serialize_for_tuple! { 2; T1 T2 }
impl_serialize_for_tuple! { 3; T1 T2 T3 }
impl_serialize_for_tuple! { 4; T1 T2 T3 T4 }
impl_serialize_for_tuple! { 5; T1 T2 T3 T4 T5 }
impl_serialize_for_tuple! { 6; T1 T2 T3 T4 T5 T6 }
impl_serialize_for_tuple! { 7; T1 T2 T3 T4 T5 T6 T7 }
impl_serialize_for_tuple! { 8; T1 T2 T3 T4 T5 T6 T7 T8 }

impl Serialize for str {
    fn serialize<A: Allocator>(&self, f: &mut fmt::Formatter<A>) {
        f.string(self)
    }
}

impl<T: Serialize> Serialize for Option<T> {
    fn serialize<A: Allocator>(&self, f: &mut fmt::Formatter<A>) {
        match self {
            None => f.null(),
            Some(node) => node.serialize(f),
        }
    }
}

#[cfg(feature = "wfmath")]
impl Serialize for wfmath::IVec2 {
    fn serialize<A: Allocator>(&self, f: &mut fmt::Formatter<A>) {
        self.to_array().serialize(f)
    }
}

#[cfg(feature = "wfmath")]
impl Serialize for wfmath::IVec3 {
    fn serialize<A: Allocator>(&self, f: &mut fmt::Formatter<A>) {
        self.to_array().serialize(f)
    }
}

#[cfg(feature = "wfmath")]
impl Serialize for wfmath::IVec4 {
    fn serialize<A: Allocator>(&self, f: &mut fmt::Formatter<A>) {
        self.to_array().serialize(f)
    }
}

#[cfg(feature = "wfmath")]
impl Serialize for wfmath::IBox2 {
    fn serialize<A: Allocator>(&self, f: &mut fmt::Formatter<A>) {
        let rect = [self.x, self.y, self.width, self.height];
        rect.serialize(f)
    }
}

#[cfg(feature = "wfmath")]
impl Serialize for wfmath::Vec2 {
    fn serialize<A: Allocator>(&self, f: &mut fmt::Formatter<A>) {
        self.to_array().serialize(f)
    }
}

#[cfg(feature = "wfmath")]
impl Serialize for wfmath::Vec3 {
    fn serialize<A: Allocator>(&self, f: &mut fmt::Formatter<A>) {
        self.to_array().serialize(f)
    }
}

#[cfg(feature = "wfmath")]
impl Serialize for wfmath::Vec4 {
    fn serialize<A: Allocator>(&self, f: &mut fmt::Formatter<A>) {
        self.to_array().serialize(f)
    }
}

#[cfg(feature = "wftime")]
impl Serialize for wftime::Nanos {
    fn serialize<A: Allocator>(&self, f: &mut fmt::Formatter<A>) {
        let nanos = self.as_nanos();
        let hi = nanos >> 64;
        let lo = nanos & u128::from(u64::MAX);

        f.dictionary_struct("Nanos", false, |f| {
            f.uint_field("lo", lo as u64);
            f.uint_field("hi", hi as u64);
        })
    }
}

#[cfg(feature = "wfslab")]
impl<T: Serialize, A: Allocator + Clone, const N: usize> Serialize for wfslab::SlabArray<T, A, N> {
    fn serialize<FA: Allocator>(&self, f: &mut fmt::Formatter<FA>) {
        use core::fmt::Write;

        f.dictionary(true, |f| {
            for (index, item) in self.iter() {
                let mut buf: InlineString<256> = InlineString::new();
                write!(buf, "{index}").unwrap();
                f.field(&buf, |f| item.serialize(f))
            }
        });
    }
}

#[derive(Debug, PartialEq)]
pub enum DeserializeError {
    Parse(ParseError),
    Kind { expected: Kind, got: Kind },
    Type { expected: &'static str, got: &'static str },
    StringCapacity { expected_up_to: usize, got: usize },
    ArrayLength { expected: usize, got: usize },
    ArrayCapacity { expected_up_to: usize, got: usize },
    TupleLength { expected: usize, got: usize },
    MissingStructField { ident: &'static str },
    IntConversion,
    TypeConstraint(&'static str),
    UnexpectedVersionInfo { ident: &'static str, version: u64 },
    Other,
}

impl From<ParseError> for DeserializeError {
    fn from(err: ParseError) -> DeserializeError {
        DeserializeError::Parse(err)
    }
}

impl core::fmt::Display for DeserializeError {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        write!(f, "{self:?}")
    }
}

pub trait Deserialize<A: Allocator>: Sized {
    fn deserialize<NA: Allocator + Clone>(node: &Node<NA>, allocator: A) -> Result<Self, DeserializeError>;
}

impl<A: Allocator> Deserialize<A> for bool {
    fn deserialize<NA>(node: &Node<NA>, _: A) -> Result<Self, DeserializeError>
    where
        NA: Allocator + Clone,
    {
        match node {
            Node::Boolean(true) => Ok(true),
            Node::Boolean(false) => Ok(false),
            other => Err(DeserializeError::Kind {
                expected: Kind::Boolean,
                got: other.kind(),
            }),
        }
    }
}

impl<A: Allocator> Deserialize<A> for i8 {
    fn deserialize<NA>(node: &Node<NA>, _: A) -> Result<Self, DeserializeError>
    where
        NA: Allocator + Clone,
    {
        match node {
            Node::Int(int) => i8::try_from(*int).map_err(|_| DeserializeError::IntConversion),
            Node::Uint(uint) => i8::try_from(*uint).map_err(|_| DeserializeError::IntConversion),
            other => Err(DeserializeError::Kind {
                expected: Kind::Int,
                got: other.kind(),
            }),
        }
    }
}

impl<A: Allocator> Deserialize<A> for i16 {
    fn deserialize<NA>(node: &Node<NA>, _: A) -> Result<Self, DeserializeError>
    where
        NA: Allocator + Clone,
    {
        match node {
            Node::Int(int) => i16::try_from(*int).map_err(|_| DeserializeError::IntConversion),
            Node::Uint(uint) => i16::try_from(*uint).map_err(|_| DeserializeError::IntConversion),
            other => Err(DeserializeError::Kind {
                expected: Kind::Int,
                got: other.kind(),
            }),
        }
    }
}

impl<A: Allocator> Deserialize<A> for i32 {
    fn deserialize<NA>(node: &Node<NA>, _: A) -> Result<Self, DeserializeError>
    where
        NA: Allocator + Clone,
    {
        match node {
            Node::Int(int) => i32::try_from(*int).map_err(|_| DeserializeError::IntConversion),
            Node::Uint(uint) => i32::try_from(*uint).map_err(|_| DeserializeError::IntConversion),
            other => Err(DeserializeError::Kind {
                expected: Kind::Int,
                got: other.kind(),
            }),
        }
    }
}

impl<A: Allocator> Deserialize<A> for i64 {
    fn deserialize<NA>(node: &Node<NA>, _: A) -> Result<Self, DeserializeError>
    where
        NA: Allocator + Clone,
    {
        match node {
            Node::Int(int) => Ok(*int),
            Node::Uint(uint) => i64::try_from(*uint).map_err(|_| DeserializeError::IntConversion),
            other => Err(DeserializeError::Kind {
                expected: Kind::Int,
                got: other.kind(),
            }),
        }
    }
}

impl<A: Allocator> Deserialize<A> for u8 {
    fn deserialize<NA>(node: &Node<NA>, _: A) -> Result<Self, DeserializeError>
    where
        NA: Allocator + Clone,
    {
        match node {
            Node::Int(int) => u8::try_from(*int).map_err(|_| DeserializeError::IntConversion),
            Node::Uint(uint) => u8::try_from(*uint).map_err(|_| DeserializeError::IntConversion),
            other => Err(DeserializeError::Kind {
                expected: Kind::Uint,
                got: other.kind(),
            }),
        }
    }
}

impl<A: Allocator> Deserialize<A> for u16 {
    fn deserialize<NA>(node: &Node<NA>, _: A) -> Result<Self, DeserializeError>
    where
        NA: Allocator + Clone,
    {
        match node {
            Node::Int(int) => u16::try_from(*int).map_err(|_| DeserializeError::IntConversion),
            Node::Uint(uint) => u16::try_from(*uint).map_err(|_| DeserializeError::IntConversion),
            other => Err(DeserializeError::Kind {
                expected: Kind::Uint,
                got: other.kind(),
            }),
        }
    }
}

impl<A: Allocator> Deserialize<A> for u32 {
    fn deserialize<NA>(node: &Node<NA>, _: A) -> Result<Self, DeserializeError>
    where
        NA: Allocator + Clone,
    {
        match node {
            Node::Int(int) => u32::try_from(*int).map_err(|_| DeserializeError::IntConversion),
            Node::Uint(uint) => u32::try_from(*uint).map_err(|_| DeserializeError::IntConversion),
            other => Err(DeserializeError::Kind {
                expected: Kind::Uint,
                got: other.kind(),
            }),
        }
    }
}

impl<A: Allocator> Deserialize<A> for u64 {
    fn deserialize<NA>(node: &Node<NA>, _: A) -> Result<Self, DeserializeError>
    where
        NA: Allocator + Clone,
    {
        match node {
            Node::Int(int) => u64::try_from(*int).map_err(|_| DeserializeError::IntConversion),
            Node::Uint(uint) => Ok(*uint),
            other => Err(DeserializeError::Kind {
                expected: Kind::Uint,
                got: other.kind(),
            }),
        }
    }
}

impl<A: Allocator> Deserialize<A> for usize {
    fn deserialize<NA>(node: &Node<NA>, _: A) -> Result<Self, DeserializeError>
    where
        NA: Allocator + Clone,
    {
        match node {
            Node::Int(int) => usize::try_from(*int).map_err(|_| DeserializeError::IntConversion),
            Node::Uint(uint) => usize::try_from(*uint).map_err(|_| DeserializeError::IntConversion),
            other => Err(DeserializeError::Kind {
                expected: Kind::Uint,
                got: other.kind(),
            }),
        }
    }
}

impl<A: Allocator> Deserialize<A> for f32 {
    fn deserialize<NA>(node: &Node<NA>, _: A) -> Result<Self, DeserializeError>
    where
        NA: Allocator + Clone,
    {
        match node {
            // TODO(yan): Fail if this would lose precision?
            Node::Int(int) => Ok(*int as f32),
            // TODO(yan): Fail if this would lose precision?
            Node::Float(float) => Ok(*float as f32),
            other => Err(DeserializeError::Kind {
                expected: Kind::Float,
                got: other.kind(),
            }),
        }
    }
}

impl<A: Allocator> Deserialize<A> for f64 {
    fn deserialize<NA>(node: &Node<NA>, _: A) -> Result<Self, DeserializeError>
    where
        NA: Allocator + Clone,
    {
        match node {
            // TODO(yan): Fail if this would lose precision?
            Node::Int(int) => Ok(*int as f64),
            Node::Float(float) => Ok(*float),
            other => Err(DeserializeError::Kind {
                expected: Kind::Float,
                got: other.kind(),
            }),
        }
    }
}

// TODO(yan): Drop Default and Copy bounds and use MaybeUninit internally.
impl<A: Allocator + Clone, T: Deserialize<A> + Default + Copy, const N: usize> Deserialize<A> for [T; N] {
    fn deserialize<NA>(node: &Node<NA>, allocator: A) -> Result<Self, DeserializeError>
    where
        NA: Allocator + Clone,
    {
        match node {
            Node::Array(array) => {
                if array.len() != N {
                    return Err(DeserializeError::ArrayLength {
                        expected: N,
                        got: array.len(),
                    });
                }

                let mut a = [T::default(); N];

                for (i, item_node) in array.iter().enumerate() {
                    let item = T::deserialize(item_node, allocator.clone())?;
                    a[i] = item;
                }

                Ok(a)
            }
            other => Err(DeserializeError::Kind {
                expected: Kind::Array,
                got: other.kind(),
            }),
        }
    }
}

// TODO(yan): Drop Copy bounds and use MaybeUninit internally, if it's even possible.
macro_rules! impl_deserialize_for_tuple {
    ( $length:expr; $( $ty:ident )+ ) => {
        impl<A: Allocator + Clone, $($ty: Deserialize<A> + Copy),+> Deserialize<A> for ($($ty,)+) {
            fn deserialize<NA>(node: &Node<NA>, allocator: A) -> Result<Self, DeserializeError>
            where
                NA: Allocator + Clone,
            {
                #[allow(non_snake_case)]
                #[allow(unused_assignments)]
                match node {
                    Node::Tuple(tuple) => {
                        if tuple.len() != $length {
                            return Err(DeserializeError::TupleLength {
                                expected: $length,
                                got: tuple.len(),
                            });
                        }

                        let mut i: usize = 0;
                        $(let $ty = {
                            let value = $ty::deserialize(&tuple[i], allocator.clone())?;
                            i += 1;

                            value
                        };)+

                        Ok(($($ty,)+))
                    }
                    other => Err(DeserializeError::Kind {
                        expected: Kind::Tuple,
                        got: other.kind(),
                    }),
                }
            }
        }
    };
}

impl_deserialize_for_tuple! { 1; T1 }
impl_deserialize_for_tuple! { 2; T1 T2 }
impl_deserialize_for_tuple! { 3; T1 T2 T3 }
impl_deserialize_for_tuple! { 4; T1 T2 T3 T4 }
impl_deserialize_for_tuple! { 5; T1 T2 T3 T4 T5 }
impl_deserialize_for_tuple! { 6; T1 T2 T3 T4 T5 T6 }
impl_deserialize_for_tuple! { 7; T1 T2 T3 T4 T5 T6 T7 }
impl_deserialize_for_tuple! { 8; T1 T2 T3 T4 T5 T6 T7 T8 }

impl<T: Deserialize<A>, A: Allocator> Deserialize<A> for Option<T> {
    fn deserialize<NA>(node: &Node<NA>, allocator: A) -> Result<Self, DeserializeError>
    where
        NA: Allocator + Clone,
    {
        match node {
            Node::Null => Ok(None),
            node => Ok(Some(T::deserialize(node, allocator)?)),
        }
    }
}

impl<T: Deserialize<A>, A: Allocator + Clone> Deserialize<A> for Vec<T, A> {
    fn deserialize<NA: Allocator + Clone>(node: &Node<NA>, allocator: A) -> Result<Self, DeserializeError> {
        match node {
            Node::Array(array) => {
                let mut vec = Vec::with_capacity_in(array.len(), allocator.clone());

                for item_node in array {
                    let item = T::deserialize(item_node, allocator.clone())?;
                    vec.push(item);
                }

                Ok(vec)
            }
            other => Err(DeserializeError::Kind {
                expected: Kind::Array,
                got: other.kind(),
            }),
        }
    }
}

impl<T: Clone + Copy + Deserialize<A>, A: Allocator + Clone, const N: usize> Deserialize<A> for InlineVec<T, N> {
    fn deserialize<NA>(node: &Node<NA>, allocator: A) -> Result<Self, DeserializeError>
    where
        NA: Allocator + Clone,
    {
        match node {
            Node::Array(array) => {
                if array.len() > N {
                    return Err(DeserializeError::ArrayCapacity {
                        expected_up_to: N,
                        got: array.len(),
                    });
                }

                let mut inlinevec: InlineVec<T, N> = InlineVec::new();

                for item_node in array {
                    let item = T::deserialize(item_node, allocator.clone())?;
                    inlinevec.push(item);
                }

                Ok(inlinevec)
            }
            other => Err(DeserializeError::Kind {
                expected: Kind::Array,
                got: other.kind(),
            }),
        }
    }
}

impl<A: Allocator + Clone, const N: usize> Deserialize<A> for InlineString<N> {
    fn deserialize<NA>(node: &Node<NA>, _: A) -> Result<Self, DeserializeError>
    where
        NA: Allocator + Clone,
    {
        match node {
            Node::String(string) => {
                if string.len() > N {
                    return Err(DeserializeError::StringCapacity {
                        expected_up_to: N,
                        got: string.len(),
                    });
                }

                let mut s = InlineString::new();
                s.push_str(unsafe { str::from_utf8_unchecked(string) });

                Ok(s)
            }
            other => Err(DeserializeError::Kind {
                expected: Kind::String,
                got: other.kind(),
            }),
        }
    }
}

#[cfg(feature = "wfmath")]
impl<A: Allocator + Clone> Deserialize<A> for wfmath::IVec2 {
    fn deserialize<NA>(node: &Node<NA>, allocator: A) -> Result<Self, DeserializeError>
    where
        NA: Allocator + Clone,
    {
        match node {
            Node::Array(array) => {
                if array.len() == Self::AXES.len() {
                    let x = i32::deserialize(&array[0], allocator.clone())?;
                    let y = i32::deserialize(&array[1], allocator)?;

                    Ok(Self::new(x, y))
                } else {
                    Err(DeserializeError::ArrayLength {
                        expected: 2,
                        got: array.len(),
                    })
                }
            }
            other => Err(DeserializeError::Kind {
                expected: Kind::Array,
                got: other.kind(),
            }),
        }
    }
}

#[cfg(feature = "wfmath")]
impl<A: Allocator + Clone> Deserialize<A> for wfmath::IVec3 {
    fn deserialize<NA>(node: &Node<NA>, allocator: A) -> Result<Self, DeserializeError>
    where
        NA: Allocator + Clone,
    {
        match node {
            Node::Array(array) => {
                if array.len() == Self::AXES.len() {
                    let x = i32::deserialize(&array[0], allocator.clone())?;
                    let y = i32::deserialize(&array[1], allocator.clone())?;
                    let z = i32::deserialize(&array[2], allocator)?;

                    Ok(Self::new(x, y, z))
                } else {
                    Err(DeserializeError::ArrayLength {
                        expected: 3,
                        got: array.len(),
                    })
                }
            }
            other => Err(DeserializeError::Kind {
                expected: Kind::Array,
                got: other.kind(),
            }),
        }
    }
}

#[cfg(feature = "wfmath")]
impl<A: Allocator + Clone> Deserialize<A> for wfmath::IVec4 {
    fn deserialize<NA>(node: &Node<NA>, allocator: A) -> Result<Self, DeserializeError>
    where
        NA: Allocator + Clone,
    {
        match node {
            Node::Array(array) => {
                if array.len() == Self::AXES.len() {
                    let x = i32::deserialize(&array[0], allocator.clone())?;
                    let y = i32::deserialize(&array[1], allocator.clone())?;
                    let z = i32::deserialize(&array[2], allocator.clone())?;
                    let w = i32::deserialize(&array[3], allocator)?;

                    Ok(Self::new(x, y, z, w))
                } else {
                    Err(DeserializeError::ArrayLength {
                        expected: 4,
                        got: array.len(),
                    })
                }
            }
            other => Err(DeserializeError::Kind {
                expected: Kind::Array,
                got: other.kind(),
            }),
        }
    }
}

#[cfg(feature = "wfmath")]
impl<A: Allocator + Clone> Deserialize<A> for wfmath::IBox2 {
    fn deserialize<NA>(node: &Node<NA>, allocator: A) -> Result<Self, DeserializeError>
    where
        NA: Allocator + Clone,
    {
        match node {
            Node::Array(array) => {
                if array.len() == 4 {
                    let x = i32::deserialize(&array[0], allocator.clone())?;
                    let y = i32::deserialize(&array[1], allocator.clone())?;
                    let width = i32::deserialize(&array[2], allocator.clone())?;
                    let height = i32::deserialize(&array[3], allocator)?;

                    if width < 0 || height < 0 {
                        Err(DeserializeError::TypeConstraint(
                            "IBox2 width and height must be nonnegative",
                        ))
                    } else {
                        Ok(Self::new(x, y, width, height))
                    }
                } else {
                    Err(DeserializeError::ArrayLength {
                        expected: 4,
                        got: array.len(),
                    })
                }
            }
            other => Err(DeserializeError::Kind {
                expected: Kind::Array,
                got: other.kind(),
            }),
        }
    }
}

#[cfg(feature = "wfmath")]
impl<A: Allocator + Clone> Deserialize<A> for wfmath::Vec2 {
    fn deserialize<NA>(node: &Node<NA>, allocator: A) -> Result<Self, DeserializeError>
    where
        NA: Allocator + Clone,
    {
        match node {
            Node::Array(array) => {
                if array.len() == Self::AXES.len() {
                    let x = f32::deserialize(&array[0], allocator.clone())?;
                    let y = f32::deserialize(&array[1], allocator)?;

                    Ok(Self::new(x, y))
                } else {
                    Err(DeserializeError::ArrayLength {
                        expected: 2,
                        got: array.len(),
                    })
                }
            }
            other => Err(DeserializeError::Kind {
                expected: Kind::Array,
                got: other.kind(),
            }),
        }
    }
}

#[cfg(feature = "wfmath")]
impl<A: Allocator + Clone> Deserialize<A> for wfmath::Vec3 {
    fn deserialize<NA>(node: &Node<NA>, allocator: A) -> Result<Self, DeserializeError>
    where
        NA: Allocator + Clone,
    {
        match node {
            Node::Array(array) => {
                if array.len() == Self::AXES.len() {
                    let x = f32::deserialize(&array[0], allocator.clone())?;
                    let y = f32::deserialize(&array[1], allocator.clone())?;
                    let z = f32::deserialize(&array[2], allocator)?;

                    Ok(Self::new(x, y, z))
                } else {
                    Err(DeserializeError::ArrayLength {
                        expected: 3,
                        got: array.len(),
                    })
                }
            }
            other => Err(DeserializeError::Kind {
                expected: Kind::Array,
                got: other.kind(),
            }),
        }
    }
}

#[cfg(feature = "wfmath")]
impl<A: Allocator + Clone> Deserialize<A> for wfmath::Vec4 {
    fn deserialize<NA>(node: &Node<NA>, allocator: A) -> Result<Self, DeserializeError>
    where
        NA: Allocator + Clone,
    {
        match node {
            Node::Array(array) => {
                if array.len() == Self::AXES.len() {
                    let x = f32::deserialize(&array[0], allocator.clone())?;
                    let y = f32::deserialize(&array[1], allocator.clone())?;
                    let z = f32::deserialize(&array[2], allocator.clone())?;
                    let w = f32::deserialize(&array[3], allocator)?;

                    Ok(Self::new(x, y, z, w))
                } else {
                    Err(DeserializeError::ArrayLength {
                        expected: 4,
                        got: array.len(),
                    })
                }
            }
            other => Err(DeserializeError::Kind {
                expected: Kind::Array,
                got: other.kind(),
            }),
        }
    }
}

#[cfg(feature = "wftime")]
impl<A: Allocator> Deserialize<A> for wftime::Nanos {
    fn deserialize<NA>(node: &Node<NA>, _: A) -> Result<Self, DeserializeError>
    where
        NA: Allocator + Clone,
    {
        match node {
            Node::DictionaryStruct(ty, _, fields) => {
                if ty != "Nanos".as_bytes() {
                    return Err(DeserializeError::Type {
                        expected: "Nanos",
                        got: "<unavailable>",
                    });
                }

                let lo_field = fields
                    .get("lo".as_bytes())
                    .ok_or(DeserializeError::MissingStructField { ident: "lo" })?;
                let hi_field = fields
                    .get("hi".as_bytes())
                    .ok_or(DeserializeError::MissingStructField { ident: "hi" })?;

                let lo = lo_field.uint().ok_or(DeserializeError::IntConversion)?;
                let hi = hi_field.uint().ok_or(DeserializeError::IntConversion)?;

                let nanos = u128::from(hi) << 64 | u128::from(lo);

                Ok(Self::from_nanos(nanos))
            }
            other => Err(DeserializeError::Kind {
                expected: Kind::DictionaryStruct,
                got: other.kind(),
            }),
        }
    }
}

#[cfg(feature = "wfslab")]
impl<T: Deserialize<A>, A: Allocator + Clone, const N: usize> Deserialize<A> for wfslab::SlabArray<T, A, N> {
    fn deserialize<NA>(node: &Node<NA>, allocator: A) -> Result<Self, DeserializeError>
    where
        NA: Allocator + Clone,
    {
        use core::str::FromStr;

        match node {
            Node::Dictionary(dictionary) => {
                let mut slab_array = wfslab::SlabArray::new_in(allocator.clone());

                for (k, v) in dictionary {
                    // TODO(jt): @Cleanup The unsafe conversion is unnecessary with String<A>.
                    //
                    // SAFETY: serialize keys are always valid strings
                    let s = unsafe { str::from_utf8_unchecked(k) };
                    // TODO(jt): @Cleanup Better error.
                    let index = usize::from_str(s).map_err(|_| DeserializeError::Other)?;
                    let value = T::deserialize(v, allocator.clone())?;

                    slab_array.reserve_for_index(index);
                    slab_array.insert(index, value);
                }

                Ok(slab_array)
            }
            other => Err(DeserializeError::Kind {
                expected: Kind::DictionaryStruct,
                got: other.kind(),
            }),
        }
    }
}

// This is our less convenient work-around for what Thekla has in JAI, where they can use the struct
// field initialization syntax to mention another field, and it works seemlessly with their
// serialization. Presumably, the serialization metaprogram collects all initializers, applies the
// constant ones immediately, and only applies the ones depending on other fields once all the
// values have been deserialized and poked in.
//
// This trait is called on a mutable reference to the already deserialized struct, and can run any
// code. It has access to the current struct version (new_version), as well as the deserialized one
// (old_version).
//
// This is not derived. Instead, tell Deserialize to call this with #[serialize(migrate)] on the
// struct. If specified, a manual implementation for Migrate is required.
pub trait Migrate {
    fn migrate(&mut self, old_version: Option<u64>, new_version: u64);
}

// TODO(jt): Replace with String<A>. This is only used by wfserialize macros. All strings in
// wfserialize are Vec<u8, A> instead of String<A> temporarily, until String<A> exists (and borrows
// as str).
#[doc(hidden)]
#[inline(always)]
pub fn cast_ident(bytes: &[u8]) -> &str {
    unsafe { core::str::from_utf8_unchecked(bytes) }
}
