use std::{
    collections::BTreeMap, iter::Sum, ops::{Add, Div, Mul, Sub}
};

use num_traits::{One, Zero};
use powdr_number::FieldElement;

use super::extension_field::ExtensionField;

/// An implementation of Fp4, analogous to `std/math/fp4.asm`.
/// An Fp4 element. The tuple (a, b, c, d) represents the polynomial a + b * X + c * X^2 + d * X^3.
/// All computations are done modulo the irreducible polynomial X^4 - 11.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Fp4<T>(pub T, pub T, pub T, pub T);

impl<T: FieldElement> Fp4<T> {
    pub fn new(a: T, b: T, c: T, d: T) -> Self {
        Fp4(a, b, c, d)
    }
}

impl<T: FieldElement> Zero for Fp4<T> {
    fn zero() -> Self {
        Fp4(T::zero(), T::zero(), T::zero(), T::zero())
    }

    fn is_zero(&self) -> bool {
        self.0.is_zero() && self.1.is_zero() && self.2.is_zero() && self.3.is_zero()
    }
}

impl<T: FieldElement> One for Fp4<T> {
    fn one() -> Self {
        Fp4(T::one(), T::zero(), T::zero(), T::zero())
    }

    fn is_one(&self) -> bool {
        self.0.is_one() && self.1.is_zero() && self.2.is_zero() && self.3.is_zero()
    }
}

impl<T: FieldElement> From<T> for Fp4<T> {
    fn from(a: T) -> Self {
        Fp4(a, T::zero(), T::zero(), T::zero())
    }
}

impl<T: FieldElement> Add for Fp4<T> {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Fp4(
            self.0 + other.0,
            self.1 + other.1,
            self.2 + other.2,
            self.3 + other.3,
        )
    }
}

impl<T: FieldElement> Sum for Fp4<T> {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        iter.fold(Self::zero(), Add::add)
    }
}

impl<T: FieldElement> Sub for Fp4<T> {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        Fp4(
            self.0 - other.0,
            self.1 - other.1,
            self.2 - other.2,
            self.3 - other.3,
        )
    }
}

impl<T: FieldElement> Mul for Fp4<T> {
    type Output = Self;

    fn mul(self, other: Self) -> Self {
        Fp4(
            self.0 * other.0
                + T::from(11) * (self.1 * other.3 + self.2 * other.2 + self.3 * other.1),
            self.0 * other.1
                + self.1 * other.0
                + T::from(11) * (self.2 * other.3 + self.3 * other.2),
            self.0 * other.2 + self.1 * other.1 + self.2 * other.0 + T::from(11) * self.3 * other.3,
            self.0 * other.3 + self.1 * other.2 + self.2 * other.1 + self.3 * other.0,
        )
    }
}

impl<T: FieldElement> Mul<T> for Fp4<T> {
    type Output = Self;

    fn mul(self, other: T) -> Self {
        Fp4(
            self.0 * other,
            self.1 * other,
            self.2 * other,
            self.3 * other,
        )
    }
}

impl<T: FieldElement> Fp4<T> {
    pub fn inverse(self) -> Self {
        let b0 = self.0 * self.0 - T::from(11) * (self.1 * T::from(2) * self.3 - self.2 * self.2);
        let b2 = T::from(2) * self.0 * self.2 - self.1 * self.1 - T::from(11) * self.3 * self.3;
        let c = b0 * b0 - T::from(11) * b2 * b2;
        let ic = T::from(1) / c;

        let b0_ic = b0 * ic;
        let b2_ic = b2 * ic;

        Fp4(
            self.0 * b0_ic - T::from(11) * self.2 * b2_ic,
            -self.1 * b0_ic + T::from(11) * self.3 * b2_ic,
            -self.0 * b2_ic + self.2 * b0_ic,
            self.1 * b2_ic - self.3 * b0_ic,
        )
    }
}

impl<T: FieldElement> Div for Fp4<T> {
    type Output = Self;

    #[allow(clippy::suspicious_arithmetic_impl)]
    fn div(self, other: Self) -> Self {
        self * other.inverse()
    }
}

impl<T: FieldElement> ExtensionField<T> for Fp4<T> {
    fn get_challenge(challenges: &BTreeMap<u64, T>, index: u64) -> Self {
        Fp4::new(
            challenges[&(index * 4 + 1)],
            challenges[&(index * 4 + 2)],
            challenges[&(index * 4 + 3)],
            challenges[&(index * 4 + 4)],
        )
    }
    fn size() -> usize {
        4
    }
    fn inverse(self) -> Self {
        self.inverse()
    }
    fn to_vec(self) -> Vec<T> {
        vec![self.0, self.1, self.2, self.3]
    }
}

#[cfg(test)]
mod tests {
    use powdr_number::GoldilocksField;

    use super::*;

    fn new(a: i64, b: i64, c: i64, d: i64) -> Fp4<GoldilocksField> {
        Fp4(
            GoldilocksField::from(a),
            GoldilocksField::from(b),
            GoldilocksField::from(c),
            GoldilocksField::from(d),
        )
    }

    fn from_base(x: i64) -> Fp4<GoldilocksField> {
        Fp4::from(GoldilocksField::from(x))
    }

    #[test]
    fn test_add() {
        assert_eq!(from_base(0) + from_base(0), from_base(0));
        assert_eq!(new(1, 2, 3, 4) + new(5, 6, 7, 8), new(6, 8, 10, 12));
    }

    #[test]
    fn test_sub() {
        assert_eq!(new(1, 2, 3, 4) - new(5, 6, 7, 8), new(-4, -4, -4, -4));
    }

    #[test]
    fn test_mul() {
        assert_eq!(new(1, 2, 3, 4) * new(5, 6, 7, 8), new(676, 588, 386, 60));
    }

    #[test]
    fn test_inverse() {
        let x = new(1, 2, 3, 4);
        assert_eq!(x * x.inverse(), Fp4::one());
    }
}
