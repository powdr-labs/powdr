use std::ops::{Add, AddAssign, Mul, Neg, Sub};

use num_traits::{One, Zero};
use powdr_number::FieldElement;

use crate::range_constraint::RangeConstraint;

pub trait RuntimeConstant<V>:
    Sized
    + Neg<Output = Self>
    + Clone
    + From<Self::FieldType>
    + Add<Output = Self>
    + AddAssign<Self>
    + Sub<Output = Self>
    + Mul<Output = Self>
    + PartialEq
    + Eq
    + Zero
{
    type FieldType: FieldElement;

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
        unimplemented!()
    }

    fn try_to_number(&self) -> Option<Self> {
        Some(self.clone())
    }

    fn range_constraint(&self) -> RangeConstraint<Self::FieldType> {
        RangeConstraint::from_value(self.clone())
    }

    fn substitute(&mut self, _variable: &V, _substitution: &Self) {
        // No-op for numbers.
    }

    fn field_div(&self, other: &Self) -> Self {
        self.clone() / other.clone()
    }

    fn referenced_symbols<'a>(&'a self) -> impl Iterator<Item = &'a V> + 'a
    where
        V: 'a,
    {
        // No symbols in numbers.
        std::iter::empty()
    }
}
