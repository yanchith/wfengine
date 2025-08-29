#![no_std]

mod box2;
mod box3;
mod color;
mod ibox2;
mod ibox3;
mod plane;

pub use glam::*;
pub use libm;

pub use crate::box2::*;
pub use crate::box3::*;
pub use crate::color::srgb_to_linear;
pub use crate::ibox2::*;
pub use crate::ibox3::*;
pub use crate::plane::*;

// XXX: Do the TODOs below.
//
// TODO(jt): Think about boxes and iboxes being plane data (deserializable), represented with with
// min and max, and flipping them on demand, if needed.
//
// Yes, this can potentially mean we pay the price of conditioning the data many times, but it also
// has upsides:
//
// - All fields could be public
// - We wouldn't have to worry about checking invariants when deserializing.
//
// TODO(jt): Rename Box2 -> Rect (or AaRect?)
// TODO(jt): Rename Box3 -> Box (or if that proves a terrble idea in Rust, then perhaps AaBox?)

#[inline]
pub fn lerp(a: f32, b: f32, t: f32) -> f32 {
    a + t * (b - a)
}

#[inline]
pub fn move_towards(mut a: f32, b: f32, d: f32) -> f32 {
    if a > b {
        a -= d;
        if a < b {
            a = b;
        }
    } else if a < b {
        a += d;
        if a > b {
            a = b;
        }
    }

    a
}

#[inline]
pub fn dampen(a: f32, b: f32, smoothness: f32, dt: f32) -> f32 {
    let t = 1.0 - libm::powf(f32::clamp(smoothness, 0.0, 1.0), dt);
    lerp(a, b, t)
}

// Port and simplification of https://github.com/gre/bezier-easing
#[inline]
pub fn cubic_bezier(p1: Vec2, p2: Vec2, t: f32) -> f32 {
    #[inline]
    fn a(a1: f32, a2: f32) -> f32 {
        1.0 - 3.0 * a2 + 3.0 * a1
    }

    #[inline]
    fn b(a1: f32, a2: f32) -> f32 {
        3.0 * a2 - 6.0 * a1
    }

    #[inline]
    fn c(a1: f32) -> f32 {
        a1 * 3.0
    }

    // Return x(t) given t, x1, and x2, or y(t) given t, y1, and y2.
    #[inline]
    fn bezier(t: f32, a1: f32, a2: f32) -> f32 {
        ((a(a1, a2) * t + b(a1, a2)) * t + c(a1)) * t
    }

    // Return dx/dt given t, x1, and x2, or dy/dt given t, y1, and y2.
    #[inline]
    fn slope(t: f32, a1: f32, a2: f32) -> f32 {
        3.0 * a(a1, a2) * t * t + 2.0 * b(a1, a2) * t + c(a1)
    }

    // Iteratively find approximation for parameter t along a cubic
    // bezier curve for x.
    //
    // Newton Raphson iteration
    // https://en.wikipedia.org/wiki/Newton%27s_method
    let t = {
        let x = t;
        let mut t = f32::clamp(t, 0.0, 1.0);

        // The more the prettier, with diminishing returns.
        // 1 iteration already looks very nice
        const N: usize = 3;
        for _ in 0..N {
            let slope = slope(t, p1[0], p2[0]);
            if slope == 0.0 {
                break;
            }
            let current_x = bezier(t, p1[0], p2[0]) - x;
            t -= current_x / slope;
        }

        t
    };

    bezier(t, p1[1], p2[1])
}
