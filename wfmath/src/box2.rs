use core::ops::Add;
use core::ops::Deref;
use core::ops::Sub;

use glam::Vec2;
use glam::vec2;
use wfcommon::static_assert;

// TODO(jt): @Cleanup Box2 has a bit too many associated functions. Can we remove some? Whatever we
// keep, also implement for Box3.

// TODO(jt): @Speed Think about RectN/IRectN implementing CheckedBitPattern. While correct, it has
// to check every element individually in slices, which could be a lot slower than just a
// constant-price check for the whole slice. We change it to AnyBitPattern in release builds.

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Default)]
#[derive(bytemuck::NoUninit)]
pub struct Box2 {
    x: f32,
    y: f32,
    width: f32,
    height: f32,
}

impl Box2 {
    pub const ZERO: Self = Self {
        x: 0.0,
        y: 0.0,
        width: 0.0,
        height: 0.0,
    };

    pub const ONE: Self = Self {
        x: 0.0,
        y: 0.0,
        width: 1.0,
        height: 1.0,
    };

    #[inline]
    pub const fn new(x: f32, y: f32, width: f32, height: f32) -> Self {
        assert!(width >= 0.0);
        assert!(height >= 0.0);

        Self { x, y, width, height }
    }

    #[inline]
    pub fn from_base_size(base: Vec2, size: Vec2) -> Self {
        Self::new(base.x, base.y, size.x, size.y)
    }

    #[inline]
    pub fn from_points(a: Vec2, b: Vec2) -> Self {
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
    pub fn extend_by_point(&self, point: Vec2) -> Self {
        let min_point = self.min_point().min(point);
        let max_point = self.max_point().max(point);
        let size = max_point - min_point;

        Self {
            x: min_point.x,
            y: min_point.y,
            width: size.x,
            height: size.y,
        }
    }

    #[inline]
    pub fn extend_by_box(&self, b: Self) -> Self {
        let min_point = self.min_point().min(b.min_point());
        let max_point = self.max_point().max(b.max_point());
        let size = max_point - min_point;

        Self {
            x: min_point.x,
            y: min_point.y,
            width: size.x,
            height: size.y,
        }
    }

    #[inline]
    pub fn inset(&self, amount: f32) -> Self {
        assert!(amount >= 0.0);

        let x = f32::clamp(self.x + amount, self.x, self.max_x());
        let y = f32::clamp(self.y + amount, self.y, self.max_y());
        let width = f32::clamp(self.width - 2.0 * amount, 0.0, self.width);
        let height = f32::clamp(self.height - 2.0 * amount, 0.0, self.height);

        Self::new(x, y, width, height)
    }

    #[inline]
    pub fn offset(&self, amount: f32) -> Self {
        assert!(amount >= 0.0);

        Self::new(
            self.x - amount,
            self.y - amount,
            self.width + 2.0 * amount,
            self.height + 2.0 * amount,
        )
    }

    #[inline]
    pub fn resize(&self, amount: Vec2) -> Self {
        Self {
            x: self.x,
            y: self.y,
            width: f32::max(self.width + amount.x, 0.0),
            height: f32::max(self.height + amount.y, 0.0),
        }
    }

    #[inline]
    pub fn clamp_point(&self, point: Vec2) -> Vec2 {
        point.clamp(self.min_point(), self.max_point())
    }

    #[inline]
    pub fn clamp_box(&self, b: Self) -> Self {
        Self::from_points(self.clamp_point(b.min_point()), self.clamp_point(b.max_point()))
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.width == 0.0 || self.height == 0.0
    }

    #[inline]
    pub fn contains_point(&self, point: Vec2) -> bool {
        let contains_x = self.x <= point.x && self.max_x() >= point.x;
        let contains_y = self.y <= point.y && self.max_y() >= point.y;

        contains_x && contains_y
    }

    #[inline]
    pub fn contains_box(&self, b: Self) -> bool {
        let contains_x = self.x <= b.x && self.max_x() >= b.max_x();
        let contains_y = self.y <= b.y && self.max_y() >= b.max_y();

        contains_x && contains_y
    }

    #[inline]
    pub fn intersects_box(&self, b: Self) -> bool {
        let intersects_x = self.x <= b.max_x() && self.max_x() >= b.x;
        let intersects_y = self.y <= b.max_y() && self.max_y() >= b.y;

        intersects_x && intersects_y
    }

    #[inline]
    pub fn max_x(&self) -> f32 {
        self.x + self.width
    }

    #[inline]
    pub fn max_y(&self) -> f32 {
        self.y + self.height
    }

    #[inline]
    pub fn min_point(&self) -> Vec2 {
        vec2(self.x, self.y)
    }

    #[inline]
    pub fn max_point(&self) -> Vec2 {
        vec2(self.x + self.width, self.y + self.height)
    }

    #[inline]
    pub fn center_point(&self) -> Vec2 {
        vec2(self.x + 0.5 * self.width, self.y + 0.5 * self.height)
    }

    #[inline]
    pub fn size(&self) -> Vec2 {
        vec2(self.width, self.height)
    }
}

impl Add<Vec2> for Box2 {
    type Output = Self;

    #[inline]
    fn add(self, other: Vec2) -> Self {
        Self {
            x: self.x + other.x,
            y: self.y + other.y,
            width: self.width,
            height: self.height,
        }
    }
}

impl Sub<Vec2> for Box2 {
    type Output = Self;

    #[inline]
    fn sub(self, other: Vec2) -> Self {
        Self {
            x: self.x - other.x,
            y: self.y - other.y,
            width: self.width,
            height: self.height,
        }
    }
}

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq)]
#[derive(bytemuck::AnyBitPattern)]
pub struct Box2Deref {
    pub x: f32,
    pub y: f32,
    pub width: f32,
    pub height: f32,
}

static_assert!(size_of::<Box2>() == 16);
static_assert!(size_of::<Box2Deref>() == 16);

impl Deref for Box2 {
    type Target = Box2Deref;

    #[inline]
    fn deref(&self) -> &Self::Target {
        bytemuck::cast_ref(self)
    }
}

// SAFETY: We are not really doing anything unsafe in the strict sense here, instead we are using
// CheckedBitPattern to validate our custom invariants about box dimensions being nonnegative.
unsafe impl bytemuck::CheckedBitPattern for Box2 {
    type Bits = Box2Deref;

    #[inline]
    fn is_valid_bit_pattern(bits: &Box2Deref) -> bool {
        bits.width >= 0.0 && bits.height >= 0.0
    }
}
