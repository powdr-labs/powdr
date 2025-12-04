use std::fmt::LowerHex;
use std::ops::{Add, AddAssign, Div, Mul, MulAssign, Neg, Not, Sub, SubAssign};
use std::str::FromStr;

use ark_ff::{One, Zero};

use num_traits::{ConstOne, ConstZero};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use core::fmt::{self, Debug, Formatter};
use core::hash::Hash;
#[cfg(target_arch = "x86_64")]
use core::hint::unreachable_unchecked;

use crate::{BigUint, FieldElement, KnownField, LargeInt};

// This implementation is adapted from plonky2. The main change is that we ensure that the stored
// value is always less than the field modulus, since we do conversions from and to canonical
// integers all the time.

const EPSILON: u64 = (1 << 32) - 1;

#[derive(
    Clone,
    Copy,
    PartialEq,
    Eq,
    Debug,
    Default,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    JsonSchema,
    derive_more::Display,
)]
#[repr(transparent)]
pub struct GoldilocksField(u64);

impl GoldilocksField {
    const ORDER: u64 = 0xFFFFFFFF00000001;

    /// Returns the inverse of the field element, using Fermat's little theorem.
    /// The inverse of `a` is computed as `a^(p-2)`, where `p` is the prime order of the field.
    ///
    /// Mathematically, this is equivalent to:
    ///                $a^(p-1)     = 1 (mod p)$
    ///                $a^(p-2) * a = 1 (mod p)$
    /// Therefore      $a^(p-2)     = a^-1 (mod p)$
    ///
    /// The following code has been adapted from winterfell/math/src/field/f64/mod.rs
    /// located at <https://github.com/facebook/winterfell>.
    fn try_inverse(&self) -> Option<Self> {
        if self.is_zero() {
            return None;
        }

        // compute base^(P - 2) using 72 multiplications
        // The exponent P - 2 is represented in binary as:
        // 0b1111111111111111111111111111111011111111111111111111111111111111

        // compute base^11
        let t2 = self.square() * *self;

        // compute base^111
        let t3 = t2.square() * *self;

        // compute base^111111 (6 ones)
        // repeatedly square t3 3 times and multiply by t3
        let t6 = exp_acc::<3>(t3, t3);

        // compute base^111111111111 (12 ones)
        // repeatedly square t6 6 times and multiply by t6
        let t12 = exp_acc::<6>(t6, t6);

        // compute base^111111111111111111111111 (24 ones)
        // repeatedly square t12 12 times and multiply by t12
        let t24 = exp_acc::<12>(t12, t12);

        // compute base^1111111111111111111111111111111 (31 ones)
        // repeatedly square t24 6 times and multiply by t6 first. then square t30 and
        // multiply by base
        let t30 = exp_acc::<6>(t24, t6);
        let t31 = t30.square() * *self;

        // compute base^111111111111111111111111111111101111111111111111111111111111111
        // repeatedly square t31 32 times and multiply by t31
        let t63 = exp_acc::<32>(t31, t31);

        // compute base^1111111111111111111111111111111011111111111111111111111111111111
        Some(t63.square() * *self)
    }

    fn square(&self) -> Self {
        *self * *self
    }

    fn exp_power_of_2(&self, power_log: usize) -> Self {
        let mut res = *self;
        for _ in 0..power_log {
            res = res.square();
        }
        res
    }

    #[inline(always)]
    fn from_canonical_u64(n: u64) -> Self {
        debug_assert!(n < Self::ORDER);
        Self(n)
    }

    #[inline]
    fn from_noncanonical_i64(n: i64) -> Self {
        Self::from_canonical_u64(if n < 0 {
            // If n < 0, then this is guaranteed to overflow since
            // both arguments have their high bit set, so the result
            // is in the canonical range.
            Self::ORDER.wrapping_add(n as u64)
        } else {
            n as u64
        })
    }

    #[inline]
    fn to_canonical_u64(self) -> u64 {
        self.0
    }
}

#[inline]
fn wrap(x: u64) -> u64 {
    if x >= GoldilocksField::ORDER {
        x - GoldilocksField::ORDER
    } else {
        x
    }
}

impl Neg for GoldilocksField {
    type Output = Self;

    #[inline]
    fn neg(self) -> Self {
        if self.is_zero() {
            Self::ZERO
        } else {
            Self(Self::ORDER - self.to_canonical_u64())
        }
    }
}

impl Add for GoldilocksField {
    type Output = Self;

    #[inline]
    #[allow(clippy::suspicious_arithmetic_impl)]
    fn add(self, rhs: Self) -> Self {
        let (sum, over) = self.0.overflowing_add(rhs.0);
        let (sum, over) = sum.overflowing_add((over as u64) * EPSILON);
        debug_assert!(!over);
        Self(wrap(sum))
    }
}

impl AddAssign for GoldilocksField {
    #[inline]
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs
    }
}

impl Sub for GoldilocksField {
    type Output = Self;

    #[inline]
    #[allow(clippy::suspicious_arithmetic_impl)]
    fn sub(self, rhs: Self) -> Self {
        let (diff, under) = self.0.overflowing_sub(rhs.0);
        let (diff, under) = diff.overflowing_sub((under as u64) * EPSILON);
        debug_assert!(!under);
        Self(wrap(diff))
    }
}

impl SubAssign for GoldilocksField {
    #[inline]
    fn sub_assign(&mut self, rhs: Self) {
        *self = *self - rhs
    }
}

impl Mul for GoldilocksField {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self {
        reduce128((self.0 as u128) * (rhs.0 as u128))
    }
}

impl MulAssign for GoldilocksField {
    fn mul_assign(&mut self, rhs: Self) {
        *self = *self * rhs
    }
}

impl Div for GoldilocksField {
    type Output = Self;

    #[allow(clippy::suspicious_arithmetic_impl)]
    fn div(self, rhs: Self) -> Self::Output {
        self * rhs.try_inverse().unwrap()
    }
}

/// Fast addition modulo ORDER for x86-64.
/// This function is marked unsafe for the following reasons:
///   - It is only correct if x + y < 2**64 + ORDER = 0x1ffffffff00000001.
///   - It is only faster in some circumstances. In particular, on x86 it overwrites both inputs in
///     the registers, so its use is not recommended when either input will be used again.
#[inline(always)]
#[cfg(target_arch = "x86_64")]
unsafe fn add_no_canonicalize_trashing_input(x: u64, y: u64) -> u64 {
    let res_wrapped: u64;
    let adjustment: u64;
    core::arch::asm!(
        "add {0}, {1}",
        // Trick. The carry flag is set iff the addition overflowed.
        // sbb x, y does x := x - y - CF. In our case, x and y are both {1:e}, so it simply does
        // {1:e} := 0xffffffff on overflow and {1:e} := 0 otherwise. {1:e} is the low 32 bits of
        // {1}; the high 32-bits are zeroed on write. In the end, we end up with 0xffffffff in {1}
        // on overflow; this happens be EPSILON.
        // Note that the CPU does not realize that the result of sbb x, x does not actually depend
        // on x. We must write the result to a register that we know to be ready. We have a
        // dependency on {1} anyway, so let's use it.
        "sbb {1:e}, {1:e}",
        inlateout(reg) x => res_wrapped,
        inlateout(reg) y => adjustment,
        options(pure, nomem, nostack),
    );
    assume(x != 0 || (res_wrapped == y && adjustment == 0));
    assume(y != 0 || (res_wrapped == x && adjustment == 0));
    // Add EPSILON == subtract ORDER.
    // Cannot overflow unless the assumption if x + y < 2**64 + ORDER is incorrect.
    res_wrapped + adjustment
}

#[inline(always)]
#[cfg(not(target_arch = "x86_64"))]
const unsafe fn add_no_canonicalize_trashing_input(x: u64, y: u64) -> u64 {
    let (res_wrapped, carry) = x.overflowing_add(y);
    // Below cannot overflow unless the assumption if x + y < 2**64 + ORDER is incorrect.
    res_wrapped + EPSILON * (carry as u64)
}

/// Reduces to a 64-bit value. The result is in canonical form.
#[inline]
fn reduce128(x: u128) -> GoldilocksField {
    let (x_lo, x_hi) = split(x); // This is a no-op
    let x_hi_hi = x_hi >> 32;
    let x_hi_lo = x_hi & EPSILON;

    let (mut t0, borrow) = x_lo.overflowing_sub(x_hi_hi);
    if borrow {
        branch_hint(); // A borrow is exceedingly rare. It is faster to branch.
        t0 -= EPSILON; // Cannot underflow.
    }
    let t1 = x_hi_lo * EPSILON;
    let t2 = unsafe { add_no_canonicalize_trashing_input(t0, t1) };

    GoldilocksField(wrap(t2))
}

/// Squares the base N number of times and multiplies the result by the tail value.
#[inline(always)]
fn exp_acc<const N: usize>(base: GoldilocksField, tail: GoldilocksField) -> GoldilocksField {
    base.exp_power_of_2(N) * tail
}

#[inline]
const fn split(x: u128) -> (u64, u64) {
    (x as u64, (x >> 64) as u64)
}

#[inline(always)]
#[cfg(target_arch = "x86_64")]
pub fn assume(p: bool) {
    debug_assert!(p);
    if !p {
        unsafe {
            unreachable_unchecked();
        }
    }
}

/// Try to force Rust to emit a branch. Example:
///     if x > 2 {
///         y = foo();
///         branch_hint();
///     } else {
///         y = bar();
///     }
/// This function has no semantics. It is a hint only.
#[inline(always)]
pub fn branch_hint() {
    // NOTE: These are the currently supported assembly architectures. See the
    // [nightly reference](https://doc.rust-lang.org/nightly/reference/inline-assembly.html) for
    // the most up-to-date list.
    #[cfg(any(
        target_arch = "aarch64",
        target_arch = "arm",
        target_arch = "riscv32",
        target_arch = "riscv64",
        target_arch = "x86",
        target_arch = "x86_64",
    ))]
    unsafe {
        core::arch::asm!("", options(nomem, nostack, preserves_flags));
    }
}

impl FieldElement for GoldilocksField {
    type Integer = GLLargeInt;

    const BITS: u32 = 64;

    fn to_degree(&self) -> crate::DegreeType {
        self.to_canonical_u64()
    }

    fn to_integer(&self) -> Self::Integer {
        self.to_canonical_u64().into()
    }

    #[inline]
    fn modulus() -> Self::Integer {
        Self::ORDER.into()
    }

    fn pow(self, exp: Self::Integer) -> Self {
        let mut exp = exp.0;
        if exp == 0 {
            return 1.into();
        } else if exp == 1 {
            return self;
        }
        let mut x = self;
        let mut r: Self = 1.into();
        while exp >= 2 {
            if exp & 1 != 0 {
                r *= x;
            }
            x = x.square();
            exp >>= 1;
        }
        r * x
    }

    fn to_bytes_le(&self) -> Vec<u8> {
        self.to_canonical_u64().to_le_bytes().to_vec()
    }

    fn from_bytes_le(bytes: &[u8]) -> Self {
        wrap(u64::try_from(BigUint::from_le_bytes(bytes)).unwrap()).into()
    }

    fn from_str_radix(s: &str, radix: u32) -> Result<Self, String> {
        let n = u64::from_str_radix(s, radix).map_err(|e| e.to_string())?;
        if n < Self::ORDER {
            Ok(Self::from_canonical_u64(n))
        } else {
            Err(format!("Number \"{s}\" too large for Goldilocks field."))
        }
    }

    fn checked_from(value: ibig::UBig) -> Option<Self> {
        if value < Self::modulus().to_arbitrary_integer() {
            Some(u64::try_from(value).unwrap().into())
        } else {
            None
        }
    }

    fn is_in_lower_half(&self) -> bool {
        self.to_canonical_u64() <= (Self::ORDER - 1) / 2
    }

    fn known_field() -> Option<crate::KnownField> {
        Some(KnownField::GoldilocksField)
    }

    fn has_direct_repr() -> bool {
        true
    }
}

impl LowerHex for GoldilocksField {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        LowerHex::fmt(&self.to_canonical_u64(), f)
    }
}

impl From<bool> for GoldilocksField {
    fn from(b: bool) -> Self {
        Self(b as u64)
    }
}

impl From<i64> for GoldilocksField {
    fn from(n: i64) -> Self {
        Self::from_noncanonical_i64(n)
    }
}

impl From<i32> for GoldilocksField {
    fn from(n: i32) -> Self {
        From::<i64>::from(n as i64)
    }
}

impl From<u32> for GoldilocksField {
    fn from(n: u32) -> Self {
        Self::from_canonical_u64(n as u64)
    }
}

impl From<u64> for GoldilocksField {
    #[inline]
    fn from(n: u64) -> Self {
        Self(wrap(n))
    }
}

impl From<crate::BigUint> for GoldilocksField {
    fn from(n: crate::BigUint) -> Self {
        u64::try_from(n).unwrap().into()
    }
}

impl From<GLLargeInt> for GoldilocksField {
    #[inline]
    fn from(n: GLLargeInt) -> Self {
        Self(wrap(n.0))
    }
}

impl ConstZero for GoldilocksField {
    const ZERO: Self = Self(0);
}

impl Zero for GoldilocksField {
    fn zero() -> Self {
        Self::ZERO
    }

    fn is_zero(&self) -> bool {
        self.0 == 0
    }
}

impl ConstOne for GoldilocksField {
    const ONE: Self = Self(1);
}

impl One for GoldilocksField {
    fn one() -> Self {
        Self::ONE
    }

    fn is_one(&self) -> bool {
        self.to_canonical_u64() == 1
    }
}

impl FromStr for GoldilocksField {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let n = BigUint::from_str(s).map_err(|e| e.to_string())?;
        let modulus = Self::modulus();
        if n >= modulus.to_arbitrary_integer() {
            Err(format!("Decimal number \"{s}\" too large for field."))
        } else {
            Ok(n.into())
        }
    }
}

#[derive(
    Clone,
    Copy,
    PartialEq,
    Eq,
    Debug,
    Default,
    PartialOrd,
    Ord,
    Hash,
    derive_more::Display,
    Serialize,
    Deserialize,
    JsonSchema,
    derive_more::Mul,
    derive_more::Add,
    derive_more::Sub,
    derive_more::AddAssign,
    derive_more::SubAssign,
    derive_more::MulAssign,
    derive_more::Shr,
    derive_more::Shl,
    derive_more::BitAnd,
    derive_more::BitOr,
    derive_more::BitXor,
    derive_more::BitAndAssign,
    derive_more::BitOrAssign,
    derive_more::BitXorAssign,
)]
pub struct GLLargeInt(u64);

impl LargeInt for GLLargeInt {
    const MAX: Self = Self(u64::MAX);
    const NUM_BITS: usize = 64;

    fn to_arbitrary_integer(self) -> ibig::UBig {
        self.0.into()
    }

    fn num_bits(&self) -> usize {
        Self::NUM_BITS - self.0.leading_zeros() as usize
    }

    fn one() -> Self {
        Self(1)
    }

    fn is_one(&self) -> bool {
        self.0 == 1
    }

    fn try_into_u64(&self) -> Option<u64> {
        Some(self.0)
    }

    fn try_into_u32(&self) -> Option<u32> {
        u32::try_from(self.0).ok()
    }

    fn from_hex(s: &str) -> Self {
        Self(u64::from_str_radix(s, 16).unwrap())
    }
}

impl From<u32> for GLLargeInt {
    fn from(value: u32) -> Self {
        Self(value as u64)
    }
}

impl From<u64> for GLLargeInt {
    fn from(value: u64) -> Self {
        Self(value)
    }
}

impl Zero for GLLargeInt {
    fn zero() -> Self {
        Self(0)
    }

    fn is_zero(&self) -> bool {
        self.0 == 0
    }
}

impl ConstZero for GLLargeInt {
    const ZERO: Self = Self(0);
}

impl LowerHex for GLLargeInt {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        LowerHex::fmt(&self.0, f)
    }
}

impl Not for GLLargeInt {
    type Output = Self;

    fn not(self) -> Self::Output {
        Self(!self.0)
    }
}

#[cfg(test)]
mod test {
    use crate::traits::int_from_hex_str;
    use test_log::test;

    use super::*;

    #[test]
    fn bitwise() {
        let n = int_from_hex_str::<GoldilocksField>("00ff00ff00ff00ff");
        let p = int_from_hex_str::<GoldilocksField>("000ff00ff00ff00f");
        let not_n = int_from_hex_str::<GoldilocksField>("ff00ff00ff00ff00");
        let n_shr_4 = int_from_hex_str::<GoldilocksField>("000ff00ff00ff00f");
        let n_shl_4 = int_from_hex_str::<GoldilocksField>("0ff00ff00ff00ff0");
        let n_or_p = int_from_hex_str::<GoldilocksField>("00fff0fff0fff0ff");
        let n_and_p = int_from_hex_str::<GoldilocksField>("000f000f000f000f");
        let n_xor_p = int_from_hex_str::<GoldilocksField>("00f0f0f0f0f0f0f0");

        assert_eq!(n.not().not(), n);
        assert_eq!(n.not(), not_n);
        assert_eq!(n >> 4, n_shr_4);
        assert_eq!(n << 4, n_shl_4);
        assert_eq!(n & p, n_and_p);
        assert_eq!(n | p, n_or_p);
        assert_eq!(n ^ p, n_xor_p);
    }

    #[test]
    fn lower_half() {
        let x = GoldilocksField::from(0);
        assert!(x.is_in_lower_half());
        assert!(!(x - 1.into()).is_in_lower_half());

        let y = GoldilocksField::from_str_radix("7fffffff80000000", 16).unwrap();
        assert!(y.is_in_lower_half());
        assert!(!(y + 1.into()).is_in_lower_half());
    }

    #[test]
    fn from_str_radix_rejects_modulus() {
        // ORDER = 0xffffffff00000001, should be rejected
        assert!(GoldilocksField::from_str_radix("ffffffff00000001", 16).is_err());
    }

    #[test]
    fn from_str_radix_accepts_order_minus_one() {
        // ORDER - 1 = 0xffffffff00000000, should be accepted and equal to the literal value
        let v = GoldilocksField::from_str_radix("ffffffff00000000", 16).unwrap();
        assert_eq!(v.to_canonical_u64(), 0xffff_ffff_0000_0000);
    }

    #[test]
    #[should_panic]
    fn integer_div_by_zero() {
        let _ = GoldilocksField::from(1).to_arbitrary_integer()
            / GoldilocksField::from(0).to_arbitrary_integer();
    }

    #[test]
    #[should_panic]
    fn div_by_zero() {
        let _ = GoldilocksField::from(1) / GoldilocksField::from(0);
    }
}
