use core::ops::Deref;

use glam::IVec2;
use wfcommon::static_assert;

use crate::Box2;

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
#[derive(bytemuck::NoUninit)]
pub struct IBox2 {
    x: i32,
    y: i32,
    width: i32,
    height: i32,
}

impl IBox2 {
    pub const ZERO: Self = Self {
        x: 0,
        y: 0,
        width: 0,
        height: 0,
    };

    pub const ONE: Self = Self {
        x: 0,
        y: 0,
        width: 1,
        height: 1,
    };

    #[inline]
    pub fn new(x: i32, y: i32, width: i32, height: i32) -> Self {
        assert!(width >= 0);
        assert!(height >= 0);

        Self { x, y, width, height }
    }

    #[inline]
    pub fn from_base_size(base: IVec2, size: IVec2) -> Self {
        Self::new(base.x, base.y, size.x, size.y)
    }

    #[inline]
    pub fn from_points(a: IVec2, b: IVec2) -> Self {
        let min_point = a.min(b);
        let max_point = a.max(b);
        let size = max_point - min_point;

        Self {
            x: min_point.x,
            y: min_point.y,
            width: size.x,
            height: size.y,
        }
    }

    #[inline]
    pub fn contains_point(&self, point: IVec2) -> bool {
        let contains_x = self.x <= point.x && self.x + self.width >= point.x;
        let contains_y = self.y <= point.y && self.y + self.width >= point.y;

        contains_x && contains_y
    }

    #[inline]
    pub fn as_rect(&self) -> Box2 {
        Box2::new(self.x as f32, self.y as f32, self.width as f32, self.height as f32)
    }
}

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[derive(bytemuck::AnyBitPattern)]
pub struct IBox2Deref {
    pub x: i32,
    pub y: i32,
    pub width: i32,
    pub height: i32,
}

static_assert!(size_of::<IBox2>() == 16);
static_assert!(size_of::<IBox2Deref>() == 16);

impl Deref for IBox2 {
    type Target = IBox2Deref;

    #[inline]
    fn deref(&self) -> &Self::Target {
        bytemuck::cast_ref(self)
    }
}

// SAFETY: We are not really doing anything unsafe in the strict sense here, instead we are using
// CheckedBitPattern to validate our custom invariants about rect dimensions being nonnegative.
unsafe impl bytemuck::CheckedBitPattern for IBox2 {
    type Bits = IBox2Deref;

    #[inline]
    fn is_valid_bit_pattern(bits: &IBox2Deref) -> bool {
        bits.width >= 0 && bits.height >= 0
    }
}
