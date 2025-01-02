use std::{
    iter::Sum,
    ops::{Add, Div, Mul, Sub},
};

use num_traits::{One, Zero};
use powdr_number::FieldElement;

/// An implementation of Fp2, analogous to `std/math/fp2.asm`.
/// An Fp2 element. The tuple (a, b) represents the polynomial a + b * X.
/// All computations are done modulo the irreducible polynomial X^2 - 11.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Fp2<T>(pub T, pub T);

impl<T: FieldElement> Fp2<T> {
    pub fn new(a: T, b: T) -> Self {
        Fp2(a, b)
    }
}

impl<T: FieldElement> Zero for Fp2<T> {
    fn zero() -> Self {
        Fp2(T::zero(), T::zero())
    }

    fn is_zero(&self) -> bool {
        self.0.is_zero() && self.1.is_zero()
    }
}

impl<T: FieldElement> One for Fp2<T> {
    fn one() -> Self {
        Fp2(T::one(), T::zero())
    }

    fn is_one(&self) -> bool {
        self.0.is_one() && self.1.is_zero()
    }
}

impl<T: FieldElement> From<T> for Fp2<T> {
    fn from(a: T) -> Self {
        Fp2(a, T::zero())
    }
}

impl<T: FieldElement> Add for Fp2<T> {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Fp2(self.0 + other.0, self.1 + other.1)
    }
}

impl<T: FieldElement> Sum for Fp2<T> {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        iter.fold(Self::zero(), Add::add)
    }
}

impl<T: FieldElement> Sub for Fp2<T> {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        Fp2(self.0 - other.0, self.1 - other.1)
    }
}

impl<T: FieldElement> Mul for Fp2<T> {
    type Output = Self;

    fn mul(self, other: Self) -> Self {
        Fp2(
            self.0 * other.0 + self.1 * other.1 * T::from(11),
            self.1 * other.0 + self.0 * other.1,
        )
    }
}

impl<T: FieldElement> Mul<T> for Fp2<T> {
    type Output = Self;

    fn mul(self, other: T) -> Self {
        Fp2(self.0 * other, self.1 * other)
    }
}

impl<T: FieldElement> Fp2<T> {
    pub fn inverse(self) -> Self {
        let inv = T::from(1) / (self.0 * self.0 - self.1 * self.1 * T::from(11));
        Fp2(self.0 * inv, -self.1 * inv)
    }
}

impl<T: FieldElement> Div for Fp2<T> {
    type Output = Self;

    #[allow(clippy::suspicious_arithmetic_impl)]
    fn div(self, other: Self) -> Self {
        self * other.inverse()
    }
}

#[cfg(test)]
mod tests {
    use powdr_number::GoldilocksField;

    use super::*;

    fn new(a: i64, b: i64) -> Fp2<GoldilocksField> {
        Fp2(GoldilocksField::from(a), GoldilocksField::from(b))
    }

    fn from_base(x: i64) -> Fp2<GoldilocksField> {
        GoldilocksField::from(x).into()
    }

    #[test]
    fn test_add() {
        // Test adding 0
        assert_eq!(from_base(0) + from_base(0), from_base(0));
        assert_eq!(new(123, 1234) + from_base(0), new(123, 1234));
        assert_eq!(from_base(0) + new(123, 1234), new(123, 1234));

        // Add arbitrary elements
        assert_eq!(new(123, 1234) + new(567, 5678), new(690, 6912));
        assert_eq!(new(-1, -1) + new(3, 4), new(2, 3));
    }

    #[test]
    fn test_sub() {
        // Test subtracting 0
        assert_eq!(from_base(0) - from_base(0), from_base(0));
        assert_eq!(new(123, 1234) - from_base(0), new(123, 1234));

        // Subtract arbitrary elements
        assert_eq!(new(123, 1234) - new(567, 5678), new(123 - 567, 1234 - 5678));
        assert_eq!(new(-1, -1) - new(0x78000000, 1), new(-0x78000000 - 1, -2));
    }

    #[test]
    fn test_mul() {
        // Test multiplication by 1
        assert_eq!(from_base(1) * from_base(1), from_base(1));
        assert_eq!(new(123, 1234) * from_base(1), new(123, 1234));
        assert_eq!(from_base(1) * new(123, 1234), new(123, 1234));

        // Test multiplication by 0
        assert_eq!(new(123, 1234) * from_base(0), from_base(0));
        assert_eq!(from_base(0) * new(123, 1234), from_base(0));

        // Multiply arbitrary elements
        assert_eq!(new(123, 1234) * new(567, 5678), new(77142913, 1398072));

        // Multiplication with field overflow
        assert_eq!(new(-1, -2) * new(-3, 4), new(3 - 11 * 8, 6 - 4));
    }

    #[test]
    fn test_inverse() {
        let test_elements = [from_base(1), new(123, 1234), new(-1, 500)];

        for x in test_elements.iter() {
            let mul_with_inverse = *x * x.inverse();
            assert_eq!(mul_with_inverse, from_base(1));
        }
    }
}
