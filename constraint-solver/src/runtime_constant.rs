use std::ops::{Add, AddAssign, Mul, MulAssign, Neg, Sub};

use num_traits::{One, Zero};
use powdr_number::FieldElement;

use crate::{
    quadratic_symbolic_expression::QuadraticSymbolicExpressionImpl,
    range_constraint::RangeConstraint,
};

/// Represents a run-time constant in the constraint solver.
/// Any T: FieldElement can represent a run-time constant (which is also a compile-time constant),
/// but the trait lets us represent run-time constants symbolically as well.
pub trait RuntimeConstant<V>:
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
    + TryInto<QuadraticSymbolicExpressionImpl<Self, V>>
{
    type FieldType: FieldElement;

    /// Tries to convert the constant to a single number. This works for compile-time constants.
    fn try_to_number(&self) -> Option<Self::FieldType>;

    /// Returns the range constraint for this constant. For compile-time constants,
    /// this will be a single value range constraint.
    fn range_constraint(&self) -> RangeConstraint<Self::FieldType>;

    /// Substitutes a variable with another constant.
    fn substitute(&mut self, variable: &V, substitution: &Self);

    /// Returns an iterator over all referenced symbols in this constant.
    fn referenced_symbols<'a>(&'a self) -> impl Iterator<Item = &'a V> + 'a
    where
        V: 'a;

    /// Divides this constant by another constant, returning a new constant.
    fn field_div(&self, other: &Self) -> Self;

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

impl<T: FieldElement, V> RuntimeConstant<V> for T {
    type FieldType = T;

    fn try_to_number(&self) -> Option<Self> {
        Some(*self)
    }

    fn range_constraint(&self) -> RangeConstraint<Self::FieldType> {
        RangeConstraint::from_value(*self)
    }

    fn substitute(&mut self, _variable: &V, _substitution: &Self) {
        // No-op for numbers.
    }

    fn field_div(&self, other: &Self) -> Self {
        *self / *other
    }

    fn referenced_symbols<'a>(&'a self) -> impl Iterator<Item = &'a V> + 'a
    where
        V: 'a,
    {
        // No symbols in numbers.
        std::iter::empty()
    }
}
