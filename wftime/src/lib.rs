use core::cmp::Ordering;
use core::cmp::PartialOrd;
use core::ops::Add;
use core::ops::AddAssign;
use core::ops::Div;
use core::ops::DivAssign;
use core::ops::Mul;
use core::ops::MulAssign;
use core::ops::Rem;
use core::ops::RemAssign;
use core::ops::Sub;
use core::ops::SubAssign;
use core::time::Duration;

const NANOS_PER_SEC: u32 = 1_000_000_000;
const NANOS_PER_MILLI: u32 = 1_000_000;

#[derive(Debug)]
pub enum NanosFromFloatError {
    Negative,
    OverflowOrNan,
}

// TODO(jt): @Cleanup Simplify! This is taken from Rust's Duration code in libcore, with very little
// modification. Analogous code in Apollo_Time is about 1/8 of the length and complexity. Perhaps we
// want to use that as a starting point?
macro_rules! try_from_secs {
    (
        secs = $secs: expr,
        mantissa_bits = $mant_bits: literal,
        exponent_bits = $exp_bits: literal,
        offset = $offset: literal,
        bits_ty = $bits_ty:ty,
        double_ty = $double_ty:ty,
    ) => {{
        //
        // This part is the original code from libcore.
        //

        const MIN_EXP: i16 = 1 - (1i16 << $exp_bits) / 2;
        const MANT_MASK: $bits_ty = (1 << $mant_bits) - 1;
        const EXP_MASK: $bits_ty = (1 << $exp_bits) - 1;

        if $secs < 0.0 {
            return Err(NanosFromFloatError::Negative);
        }

        let bits = $secs.to_bits();
        let mant = (bits & MANT_MASK) | (MANT_MASK + 1);
        let exp = ((bits >> $mant_bits) & EXP_MASK) as i16 + MIN_EXP;

        let (secs, nanos) = if exp < -31 {
            // the input represents less than 1ns and can not be rounded to it
            (0u64, 0u32)
        } else if exp < 0 {
            // the input is less than 1 second
            let t = <$double_ty>::from(mant) << ($offset + exp);
            let nanos_offset = $mant_bits + $offset;
            let nanos_tmp = u128::from(NANOS_PER_SEC) * u128::from(t);
            let nanos = (nanos_tmp >> nanos_offset) as u32;

            let rem_mask = (1 << nanos_offset) - 1;
            let rem_msb_mask = 1 << (nanos_offset - 1);
            let rem = nanos_tmp & rem_mask;
            let is_tie = rem == rem_msb_mask;
            let is_even = (nanos & 1) == 0;
            let rem_msb = nanos_tmp & rem_msb_mask == 0;
            let add_ns = !(rem_msb || (is_even && is_tie));

            // f32 does not have enough precision to trigger the second branch
            // since it can not represent numbers between 0.999_999_940_395 and 1.0.
            let nanos = nanos + add_ns as u32;
            if ($mant_bits == 23) || (nanos != NANOS_PER_SEC) {
                (0, nanos)
            } else {
                (1, 0)
            }
        } else if exp < $mant_bits {
            let secs = u64::from(mant >> ($mant_bits - exp));
            let t = <$double_ty>::from((mant << exp) & MANT_MASK);
            let nanos_offset = $mant_bits;
            let nanos_tmp = <$double_ty>::from(NANOS_PER_SEC) * t;
            let nanos = (nanos_tmp >> nanos_offset) as u32;

            let rem_mask = (1 << nanos_offset) - 1;
            let rem_msb_mask = 1 << (nanos_offset - 1);
            let rem = nanos_tmp & rem_mask;
            let is_tie = rem == rem_msb_mask;
            let is_even = (nanos & 1) == 0;
            let rem_msb = nanos_tmp & rem_msb_mask == 0;
            let add_ns = !(rem_msb || (is_even && is_tie));

            // f32 does not have enough precision to trigger the second branch.
            // For example, it can not represent numbers between 1.999_999_880...
            // and 2.0. Bigger values result in even smaller precision of the
            // fractional part.
            let nanos = nanos + add_ns as u32;
            if ($mant_bits == 23) || (nanos != NANOS_PER_SEC) {
                (secs, nanos)
            } else {
                (secs + 1, 0)
            }
        } else if exp < 64 {
            // the input has no fractional part
            let secs = u64::from(mant) << (exp - $mant_bits);
            (secs, 0)
        } else {
            return Err(NanosFromFloatError::OverflowOrNan);
        };

        //
        // This is our modification. Instead of changing the libcore code, we just convert to u128 now.
        //
        let n = u128::from(secs) * u128::from(NANOS_PER_SEC) + u128::from(nanos);

        Ok(Nanos(n))
    }};
}

/// A duration measured in nanoseconds.
///
/// This basically serves the same purpose of [`core::time::Duration`], but it
/// has no niches, so it can be safely byte-cast without angering the
/// compiler. Because it has no niches, it doesn't get the same enum
/// discriminant elision treatment that [`core::time::Duration`] does, so if
/// that matters for your usecase, don't put it in [`Option`], or convert
/// to [`core::time::Duration`] first.
///
/// Precision is in nanoseconds, because that's what [`core::time::Duration`]
/// uses (most likely because that's what the hardware of this era gives us),
/// and we want good interop with [`core::time::Duration`].
// On aarch64 (or at least on Apple M1), u128 is aligned to 16, but it used to be aligned to 8 on
// x64. We make this aligned the same everywhere, so we don't accidentally break macOS when working
// on Windows, or vice-versa.
#[repr(C, align(16))]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
#[cfg_attr(feature = "bytemuck", derive(bytemuck::AnyBitPattern, bytemuck::NoUninit))]
pub struct Nanos(u128);

impl Nanos {
    pub const ZERO: Self = Self(0);
    pub const MAX: Self = Self(u128::MAX);

    #[inline]
    pub const fn from_nanos(nanos: u128) -> Self {
        Self(nanos)
    }

    #[inline]
    pub const fn from_millis(millis: u64) -> Self {
        // TODO(jt): @Cleanup This used to use u128::from, but no longer, because trait impls can't
        // be const (temporarily) as of April 2023. We can either convert it back to u128::from once
        // rustc re-adds it, or we can make our own widen_u128
        let millis = millis as u128;
        let nanos_per_milli = NANOS_PER_MILLI as u128;
        Self(millis * nanos_per_milli)
    }

    #[inline]
    pub fn from_secs_f32(secs: f32) -> Self {
        Self::try_from_secs_f32(secs).unwrap()
    }

    #[inline]
    pub fn from_secs_f64(secs: f64) -> Self {
        Self::try_from_secs_f64(secs).unwrap()
    }

    #[inline]
    pub fn try_from_secs_f32(secs: f32) -> Result<Self, NanosFromFloatError> {
        try_from_secs!(
            secs = secs,
            mantissa_bits = 23,
            exponent_bits = 8,
            offset = 41,
            bits_ty = u32,
            double_ty = u64,
        )
    }

    #[inline]
    pub fn try_from_secs_f64(secs: f64) -> Result<Self, NanosFromFloatError> {
        try_from_secs!(
            secs = secs,
            mantissa_bits = 52,
            exponent_bits = 11,
            offset = 44,
            bits_ty = u64,
            double_ty = u128,
        )
    }

    #[inline]
    pub const fn as_nanos(&self) -> u128 {
        self.0
    }

    #[inline]
    pub fn as_secs_f32(&self) -> f32 {
        // TODO(jt): @Correctness Doesn't this loose too much precision? Can we do better if we do
        // something manually?
        self.0 as f32 / NANOS_PER_SEC as f32
    }

    #[inline]
    pub fn as_secs_f64(&self) -> f64 {
        // TODO(jt): @Correctness Doesn't this loose too much precision? Can we do better if we do
        // something manually?
        self.0 as f64 / NANOS_PER_SEC as f64
    }

    #[inline]
    pub fn mul_f32(self, other: f32) -> Self {
        // This could convert self to f32, but that would loose too much precision. This is unlikely
        // to be autovectorized anyway, so let's keep precision over speed as the default.
        Self::from_secs_f64(self.as_secs_f64() * other as f64)
    }

    #[inline]
    pub fn mul_f64(self, other: f64) -> Self {
        Self::from_secs_f64(self.as_secs_f64() * other)
    }

    #[inline]
    pub fn saturating_sub(self, other: Self) -> Self {
        Self(self.0.saturating_sub(other.0))
    }
}

impl PartialEq<Duration> for Nanos {
    #[inline]
    fn eq(&self, other: &Duration) -> bool {
        self.eq(&Nanos::from(*other))
    }
}

impl PartialEq<Nanos> for Duration {
    #[inline]
    fn eq(&self, other: &Nanos) -> bool {
        Nanos::from(*self).eq(other)
    }
}

impl PartialOrd<Duration> for Nanos {
    #[inline]
    fn partial_cmp(&self, other: &Duration) -> Option<Ordering> {
        self.partial_cmp(&Nanos::from(*other))
    }
}

impl PartialOrd<Nanos> for Duration {
    #[inline]
    fn partial_cmp(&self, other: &Nanos) -> Option<Ordering> {
        Nanos::from(*self).partial_cmp(other)
    }
}

impl Add for Nanos {
    type Output = Nanos;

    #[inline]
    fn add(self, other: Self) -> Self {
        let n = self.0 + other.0;
        Self(n)
    }
}

impl AddAssign for Nanos {
    #[inline]
    fn add_assign(&mut self, other: Self) {
        self.0 += other.0
    }
}

impl Sub for Nanos {
    type Output = Nanos;

    #[inline]
    fn sub(self, other: Self) -> Self {
        let n = self.0 - other.0;
        Self(n)
    }
}

impl SubAssign for Nanos {
    #[inline]
    fn sub_assign(&mut self, other: Self) {
        self.0 -= other.0
    }
}

impl Mul for Nanos {
    type Output = Nanos;

    #[inline]
    fn mul(self, other: Self) -> Self {
        let n = self.0 * other.0;
        Self(n)
    }
}

impl MulAssign for Nanos {
    #[inline]
    fn mul_assign(&mut self, other: Self) {
        self.0 *= other.0
    }
}

impl Div for Nanos {
    type Output = Nanos;

    #[inline]
    fn div(self, other: Self) -> Self {
        let n = self.0 / other.0;
        Self(n)
    }
}

impl DivAssign for Nanos {
    #[inline]
    fn div_assign(&mut self, other: Self) {
        self.0 /= other.0
    }
}

impl Rem for Nanos {
    type Output = Nanos;

    #[inline]
    fn rem(self, other: Self) -> Self {
        let n = self.0 % other.0;
        Self(n)
    }
}

impl RemAssign for Nanos {
    #[inline]
    fn rem_assign(&mut self, other: Self) {
        self.0 %= other.0
    }
}

impl From<Duration> for Nanos {
    #[inline]
    fn from(duration: Duration) -> Self {
        Self(duration.as_nanos())
    }
}

pub struct DurationFromNanosError;

impl TryFrom<Nanos> for Duration {
    type Error = DurationFromNanosError;

    #[inline]
    fn try_from(nanos: Nanos) -> Result<Self, Self::Error> {
        const MAX_NANOS: u128 = u64::MAX as u128;

        let n = nanos.as_nanos();

        if n > MAX_NANOS {
            Err(DurationFromNanosError)
        } else {
            Ok(Self::from_nanos(n as u64))
        }
    }
}
