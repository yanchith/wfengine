use core::ops::Add;
use core::ops::Deref;
use core::ops::Sub;

use glam::Vec3;
use glam::vec3;
use wfcommon::static_assert;

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Default)]
#[derive(bytemuck::NoUninit)]
pub struct Box3 {
    x: f32,
    y: f32,
    z: f32,
    xsize: f32,
    ysize: f32,
    zsize: f32,
}

impl Box3 {
    pub const ZERO: Self = Self {
        x: 0.0,
        y: 0.0,
        z: 0.0,
        xsize: 0.0,
        ysize: 0.0,
        zsize: 0.0,
    };

    pub const ONE: Self = Self {
        x: 0.0,
        y: 0.0,
        z: 0.0,
        xsize: 1.0,
        ysize: 1.0,
        zsize: 1.0,
    };

    #[inline]
    pub fn new(x: f32, y: f32, z: f32, xsize: f32, ysize: f32, zsize: f32) -> Self {
        assert!(xsize >= 0.0);
        assert!(ysize >= 0.0);
        assert!(zsize >= 0.0);

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
    pub fn from_base_size(base: Vec3, size: Vec3) -> Self {
        Self::new(base.x, base.y, base.z, size.x, size.y, size.z)
    }

    #[inline]
    pub fn from_points(a: Vec3, b: Vec3) -> Self {
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
    pub fn extend_by_point(&self, point: Vec3) -> Self {
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
    pub fn offset(&self, amount: f32) -> Self {
        assert!(amount >= 0.0);

        Self::new(
            self.x - amount,
            self.y - amount,
            self.z - amount,
            self.xsize + 2.0 * amount,
            self.ysize + 2.0 * amount,
            self.zsize + 2.0 * amount,
        )
    }

    #[inline]
    pub fn contains_point(&self, point: Vec3) -> bool {
        let contains_x = self.x <= point.x && self.x + self.xsize >= point.x;
        let contains_y = self.y <= point.y && self.y + self.ysize >= point.y;
        let contains_z = self.z <= point.z && self.z + self.zsize >= point.z;

        contains_x && contains_y && contains_z
    }

    #[inline]
    pub fn ray_intersection(&self, ray_origin: Vec3, ray_direction: Vec3) -> Option<f32> {
        // This is the slab-based ray-box intersection. It tests one dimension at a time.
        //
        // Source: https://tavianator.com/2011/ray_box.html
        //
        // Normally we'd check for ray_direction_{x,y,z} being zero, but if we let the division go
        // through (or multiplication with ray_direction_inv, ir our case), we get positive and
        // negative infinities, which still behave correctly in the slab tests below.
        assert!(ray_direction != Vec3::ZERO);
        let ray_direction = ray_direction.normalize();
        let ray_direction_inv = 1.0 / ray_direction;

        let min = self.min_point();
        let max = self.max_point();

        let t1 = (min - ray_origin) * ray_direction_inv;
        let t2 = (max - ray_origin) * ray_direction_inv;

        let mut tmin = t1.x.min(t2.x);
        let mut tmax = t1.x.max(t2.x);

        tmin = tmin.max(t1.y.min(t2.y));
        tmax = tmax.min(t1.y.max(t2.y));

        tmin = tmin.max(t1.z.min(t2.z));
        tmax = tmax.min(t1.z.max(t2.z));

        if tmin <= tmax && tmax > 0.0 { Some(tmin) } else { None }
    }

    #[inline]
    pub fn min_point(&self) -> Vec3 {
        vec3(self.x, self.y, self.z)
    }

    #[inline]
    pub fn max_point(&self) -> Vec3 {
        vec3(self.x + self.xsize, self.y + self.ysize, self.z + self.zsize)
    }

    #[inline]
    pub fn center_point(&self) -> Vec3 {
        vec3(self.x, self.y, self.z) + 0.5 * vec3(self.xsize, self.ysize, self.zsize)
    }

    #[inline]
    pub fn size(&self) -> Vec3 {
        vec3(self.xsize, self.ysize, self.zsize)
    }
}

impl Add<Vec3> for Box3 {
    type Output = Self;

    #[inline]
    fn add(self, other: Vec3) -> Self {
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

impl Sub<Vec3> for Box3 {
    type Output = Self;

    #[inline]
    fn sub(self, other: Vec3) -> Self {
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
#[derive(Debug, Clone, Copy, PartialEq)]
#[derive(bytemuck::AnyBitPattern)]
pub struct Box3Deref {
    pub x: f32,
    pub y: f32,
    pub z: f32,
    pub xsize: f32,
    pub ysize: f32,
    pub zsize: f32,
}

static_assert!(size_of::<Box3>() == 24);
static_assert!(size_of::<Box3Deref>() == 24);

impl Deref for Box3 {
    type Target = Box3Deref;

    #[inline]
    fn deref(&self) -> &Self::Target {
        bytemuck::cast_ref(self)
    }
}

// SAFETY: We are not really doing anything unsafe in the strict sense here,
// instead we are using CheckedBitPattern to validate our custom invariants
// about rect dimensions being nonnegative.
unsafe impl bytemuck::CheckedBitPattern for Box3 {
    type Bits = Box3Deref;

    #[inline]
    fn is_valid_bit_pattern(bits: &Box3Deref) -> bool {
        bits.xsize >= 0.0 && bits.ysize >= 0.0 && bits.zsize >= 0.0
    }
}
