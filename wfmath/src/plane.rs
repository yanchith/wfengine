use core::ops::Neg;

use glam::Vec3;
use glam::vec3;

/// A 3D plane represented by `Ax + By + Cz + D = 0`.
///
/// `(A, B, C)` is the plane's normal (not necessarily normalized). `D` is the negative distance of
/// the plane from origin.
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq)]
#[derive(bytemuck::AnyBitPattern, bytemuck::NoUninit)]
pub struct Plane {
    /// The plane normal. Not necessarily normalized.
    pub normal: Vec3,
    pub d: f32,
}

impl Plane {
    pub const XY: Self = Self {
        normal: Vec3::Z,
        d: 0.0,
    };
    pub const YZ: Self = Self {
        normal: Vec3::X,
        d: 0.0,
    };
    pub const ZX: Self = Self {
        normal: Vec3::Y,
        d: 0.0,
    };

    #[inline]
    pub fn new(a: f32, b: f32, c: f32, d: f32) -> Self {
        Self {
            normal: vec3(a, b, c),
            d,
        }
    }

    #[inline]
    pub fn from_normal_and_point(normal: Vec3, point: Vec3) -> Self {
        Self {
            normal,
            d: -normal.dot(point),
        }
    }

    #[inline]
    pub fn normalize(&mut self) {
        debug_assert!(self.normal != Vec3::ZERO);

        let length_inv = 1.0 / self.normal.length();
        self.normal *= length_inv;
        self.d *= length_inv;
    }

    #[inline]
    pub fn is_normalized(&self) -> bool {
        libm::fabsf(self.normal.length_squared() - 1.0) < f32::EPSILON
    }

    /// Dot product where the vector is extended to 4D, with the w component being 1.
    ///
    /// The result is the signed distance of point to the plane.
    #[inline]
    pub fn plane_dot(&self, point: Vec3) -> f32 {
        self.normal.x * point.x + self.normal.y * point.y + self.normal.z * point.z + self.d
    }

    #[inline]
    pub fn ray_intersection(&self, ray_origin: Vec3, ray_direction: Vec3) -> Option<f32> {
        if let Some(distance) = self.line_intersection(ray_origin, ray_direction) {
            if distance >= 0.0 { Some(distance) } else { None }
        } else {
            None
        }
    }

    #[inline]
    pub fn line_intersection(&self, line_point: Vec3, line_direction: Vec3) -> Option<f32> {
        debug_assert!(self.normal != Vec3::ZERO);
        debug_assert!(line_direction != Vec3::ZERO);
        debug_assert!(line_direction.is_normalized());

        assert!(self.is_normalized());

        let cos = line_direction.dot(self.normal);
        if cos < f32::EPSILON && cos > -f32::EPSILON {
            // The ray/line is parallel to our plane.
            return None;
        }

        let signed_distance_to_plane = Plane::plane_dot(self, line_point);
        let signed_distance_to_intersection = signed_distance_to_plane / cos;

        //
        // If the ray/line hits the plane, the signed distance to intersection is the negative of the
        // actual distance the ray needs to travel, because either:
        //
        // - ray/line origin is above the plane (dot(O, N) is positive) and cos(theta) is negative
        // - ray/line origin is below the plane (dot(O, N) is negative) and cos(theta) is positive
        //
        //    Ray/Line
        //      \        ^ Normal
        //       \       |
        //        \      |
        //  -------x----------------------
        //
        let distance = -signed_distance_to_intersection;

        Some(distance)
    }
}

impl Neg for Plane {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Self {
            normal: -self.normal,
            d: -self.d,
        }
    }
}
