use std::ops::{Add, AddAssign, Mul, MulAssign, Neg, Sub};

use num_traits::{One, Zero};
use powdr_number::FieldElement;

use crate::range_constraint::RangeConstraint;

/// Represents a run-time constant in the constraint solver, built over
/// a base field type.
/// The base field type itself (i.e. any T: FieldElement) represents a run-time constant
/// (which is also a compile-time constant), but the trait lets us represent run-time
/// constants symbolically as well.
pub trait RuntimeConstant:
    Sized
    + Neg<Output = Self>
    + Clone
    + From<Self::FieldType>
    + Add<Output = Self>
    + AddAssign<Self>
    + Sub<Output = Self>
    + Mul<Output = Self>
    + MulAssign<Self>
    + PartialEq
    + Eq
    + Zero
    + One
{
    type FieldType: FieldElement;

    /// Tries to convert the constant to a single number. This always works for compile-time constants.
    fn try_to_number(&self) -> Option<Self::FieldType>;

    /// Returns the range constraint for this constant. For compile-time constants,
    /// this will be a single value range constraint.
    fn range_constraint(&self) -> RangeConstraint<Self::FieldType>;

    /// Divides this constant by another constant, returning a new constant.
    fn field_div(&self, other: &Self) -> Self {
        self.clone() * other.field_inverse()
    }

    /// Returns the multiplicative inverse of this constant.
    fn field_inverse(&self) -> Self;

    /// Converts a u64 to a run-time constant.
    fn from_u64(k: u64) -> Self {
        Self::from(Self::FieldType::from(k))
    }

    /// Returns whether this constant is known to be zero at compile time.
    fn is_known_zero(&self) -> bool {
        self.try_to_number().is_some_and(|n| n.is_zero())
    }

    /// Returns whether this constant is known to be one at compile time.
    fn is_known_one(&self) -> bool {
        self.try_to_number().is_some_and(|n| n.is_one())
    }

    /// Returns whether this constant is known to be -1 at compile time.
    fn is_known_minus_one(&self) -> bool {
        self.try_to_number()
            .is_some_and(|n| n == -Self::FieldType::from(1))
    }

    /// Returns whether this constant is known to be non-zero at compile time.
    /// Note that this could return true even if the constant is not known fully
    /// at compile time, but it is guaranteed that the constant is not zero.
    fn is_known_nonzero(&self) -> bool {
        // Only checking range constraint is enough since if this is a known
        // fixed value, we will get a range constraint with just a single value.
        !self.range_constraint().allows_value(0.into())
    }
}

pub trait ReferencedSymbols<V> {
    /// Returns an iterator over all referenced symbols in this constant.
    fn referenced_symbols<'a>(&'a self) -> impl Iterator<Item = &'a V> + 'a
    where
        V: 'a;
}

pub trait Substitutable<V> {
    /// Substitutes a variable with another constant.
    fn substitute(&mut self, variable: &V, substitution: &Self);
}

/// Provides a function to transform the type of variables in an expression.
/// The expectation is that the variable transformation function is injective, i.e.
/// two different variables cannot become equal through the transformation.
pub trait VarTransformable<V1, V2> {
    type Transformed;

    /// Transforms `self` by applying the `var_transform` function to all variables.
    fn transform_var_type(&self, var_transform: &mut impl FnMut(&V1) -> V2) -> Self::Transformed;
}

impl<T: FieldElement> RuntimeConstant for T {
    type FieldType = T;

    fn try_to_number(&self) -> Option<Self> {
        Some(*self)
    }

    fn range_constraint(&self) -> RangeConstraint<Self::FieldType> {
        RangeConstraint::from_value(*self)
    }

    fn field_div(&self, other: &Self) -> Self {
        *self / *other
    }

    fn field_inverse(&self) -> Self {
        T::from(1) / *self
    }
}

impl<T: FieldElement, V> ReferencedSymbols<V> for T {
    fn referenced_symbols<'a>(&'a self) -> impl Iterator<Item = &'a V> + 'a
    where
        V: 'a,
    {
        // No symbols in numbers.
        std::iter::empty()
    }
}

impl<T: FieldElement, V> Substitutable<V> for T {
    fn substitute(&mut self, _variable: &V, _substitution: &Self) {
        // No-op for numbers.
    }
}

impl<T: FieldElement, V1, V2> VarTransformable<V1, V2> for T {
    type Transformed = T;

    fn transform_var_type(&self, _var_transform: &mut impl FnMut(&V1) -> V2) -> Self::Transformed {
        // No variables to transform.
        *self
    }
}
