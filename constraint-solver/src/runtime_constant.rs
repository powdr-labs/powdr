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

    /// Creates a run-time constant from a variable.
    fn from_symbol(symbol: V, rc: RangeConstraint<Self::FieldType>) -> Self;
    fn try_to_number(&self) -> Option<Self::FieldType>;
    fn range_constraint(&self) -> RangeConstraint<Self::FieldType>;
    fn substitute(&mut self, variable: &V, substitution: &Self);
    fn referenced_symbols<'a>(&'a self) -> impl Iterator<Item = &'a V> + 'a
    where
        V: 'a;
    fn field_div(&self, other: &Self) -> Self;

    fn from_u64(k: u64) -> Self {
        Self::from(Self::FieldType::from(k))
    }

    fn is_known_zero(&self) -> bool {
        self.try_to_number().is_some_and(|n| n.is_zero())
    }

    fn is_known_one(&self) -> bool {
        self.try_to_number().is_some_and(|n| n.is_one())
    }

    fn is_known_minus_one(&self) -> bool {
        self.try_to_number()
            .is_some_and(|n| n == -Self::FieldType::from(1))
    }

    fn is_known_nonzero(&self) -> bool {
        // Only checking range constraint is enough since if this is a known
        // fixed value, we will get a range constraint with just a single value.
        !self.range_constraint().allows_value(0.into())
    }
}

impl<T: FieldElement, V> RuntimeConstant<V> for T {
    type FieldType = T;

    fn from_symbol(_symbol: V, _rc: RangeConstraint<Self::FieldType>) -> Self {
        // This is the only thing that a generic field element does not support.
        panic!("This type does not support a symbolic representation.")
    }

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
