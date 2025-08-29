use core::ops::Add;
use core::ops::Deref;
use core::ops::Sub;

use glam::IVec3;
use glam::ivec3;
use wfcommon::static_assert;

use crate::Box3;

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
#[derive(bytemuck::NoUninit)]
pub struct IBox3 {
    x: i32,
    y: i32,
    z: i32,
    xsize: i32,
    ysize: i32,
    zsize: i32,
}

impl IBox3 {
    pub const ZERO: Self = Self {
        x: 0,
        y: 0,
        z: 0,
        xsize: 0,
        ysize: 0,
        zsize: 0,
    };

    pub const ONE: Self = Self {
        x: 0,
        y: 0,
        z: 0,
        xsize: 1,
        ysize: 1,
        zsize: 1,
    };

    #[inline]
    pub fn new(x: i32, y: i32, z: i32, xsize: i32, ysize: i32, zsize: i32) -> Self {
        assert!(xsize >= 0);
        assert!(ysize >= 0);
        assert!(zsize >= 0);

        Self {
            x,
            y,
            z,
            xsize,
            ysize,
            zsize,
        }
    }

    #[inline]
    pub fn from_base_size(base: IVec3, size: IVec3) -> Self {
        Self::new(base.x, base.y, base.z, size.x, size.y, size.z)
    }

    #[inline]
    pub fn from_points(a: IVec3, b: IVec3) -> Self {
        let min_point = a.min(b);
        let max_point = a.max(b);
        let size = max_point - min_point;

        Self {
            x: min_point.x,
            y: min_point.y,
            z: min_point.z,
            xsize: size.x,
            ysize: size.y,
            zsize: size.z,
        }
    }

    #[inline]
    pub fn extend_by_point(&self, point: IVec3) -> Self {
        let min_point = self.min_point().min(point);
        let max_point = self.max_point().max(point);
        let size = max_point - min_point;

        Self {
            x: min_point.x,
            y: min_point.y,
            z: min_point.z,
            xsize: size.x,
            ysize: size.y,
            zsize: size.z,
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
            z: min_point.z,
            xsize: size.x,
            ysize: size.y,
            zsize: size.z,
        }
    }

    #[inline]
    pub fn contains_point(&self, point: IVec3) -> bool {
        let contains_x = self.x <= point.x && self.x + self.xsize >= point.x;
        let contains_y = self.y <= point.y && self.y + self.ysize >= point.y;
        let contains_z = self.z <= point.z && self.z + self.zsize >= point.z;

        contains_x && contains_y && contains_z
    }

    #[inline]
    pub fn min_point(&self) -> IVec3 {
        ivec3(self.x, self.y, self.z)
    }

    #[inline]
    pub fn max_point(&self) -> IVec3 {
        ivec3(self.x + self.xsize, self.y + self.ysize, self.z + self.zsize)
    }

    #[inline]
    pub fn size(&self) -> IVec3 {
        ivec3(self.xsize, self.ysize, self.zsize)
    }

    #[inline]
    pub fn as_box3(&self) -> Box3 {
        Box3::new(
            self.x as f32,
            self.y as f32,
            self.z as f32,
            self.xsize as f32,
            self.ysize as f32,
            self.zsize as f32,
        )
    }
}

impl Add<IVec3> for IBox3 {
    type Output = Self;

    #[inline]
    fn add(self, other: IVec3) -> Self {
        Self {
            x: self.x + other.x,
            y: self.y + other.y,
            z: self.z + other.z,
            xsize: self.xsize,
            ysize: self.ysize,
            zsize: self.zsize,
        }
    }
}

impl Sub<IVec3> for IBox3 {
    type Output = Self;

    #[inline]
    fn sub(self, other: IVec3) -> Self {
        Self {
            x: self.x - other.x,
            y: self.y - other.y,
            z: self.z - other.z,
            xsize: self.xsize,
            ysize: self.ysize,
            zsize: self.zsize,
        }
    }
}

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[derive(bytemuck::AnyBitPattern)]
pub struct IBox3Deref {
    pub x: i32,
    pub y: i32,
    pub z: i32,
    pub xsize: i32,
    pub ysize: i32,
    pub zsize: i32,
}

static_assert!(size_of::<IBox3>() == 24);
static_assert!(size_of::<IBox3Deref>() == 24);

impl Deref for IBox3 {
    type Target = IBox3Deref;

    #[inline]
    fn deref(&self) -> &Self::Target {
        bytemuck::cast_ref(self)
    }
}

// SAFETY: We are not really doing anything unsafe in the strict sense here, instead we are using
// CheckedBitPattern to validate our custom invariants about rect dimensions being nonnegative.
unsafe impl bytemuck::CheckedBitPattern for IBox3 {
    type Bits = IBox3Deref;

    #[inline]
    fn is_valid_bit_pattern(bits: &IBox3Deref) -> bool {
        bits.xsize >= 0 && bits.ysize >= 0 && bits.zsize >= 0
    }
}
