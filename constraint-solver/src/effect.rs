use crate::{range_constraint::RangeConstraint, runtime_constant::RuntimeConstant};

/// The effect of solving a symbolic equation.
#[derive(Clone, PartialEq, Eq)]
pub enum Effect<T: RuntimeConstant, V> {
    /// Variable can be assigned a value.
    Assignment(V, T),
    /// We learnt a new range constraint on variable.
    RangeConstraint(V, RangeConstraint<T::FieldType>),
    /// A run-time assertion. If this fails, we have conflicting constraints.
    Assertion(Assertion<T>),
    /// A variable is assigned one of two alternative expressions, depending on a condition.
    ConditionalAssignment {
        variable: V,
        condition: Condition<T>,
        in_range_value: T,
        out_of_range_value: T,
    },
}

/// A run-time assertion. If this fails, we have conflicting constraints.
#[derive(Clone, PartialEq, Eq)]
pub struct Assertion<T: RuntimeConstant> {
    pub lhs: T,
    pub rhs: T,
    /// If this is true, we assert that both sides are equal.
    /// Otherwise, we assert that they are different.
    pub expected_equal: bool,
}

impl<T: RuntimeConstant> Assertion<T> {
    pub fn assert_is_zero<V>(condition: T) -> Effect<T, V> {
        Self::assert_eq(condition, T::from_u64(0))
    }
    pub fn assert_is_nonzero<V>(condition: T) -> Effect<T, V> {
        Self::assert_neq(condition, T::from_u64(0))
    }
    pub fn assert_eq<V>(lhs: T, rhs: T) -> Effect<T, V> {
        Effect::Assertion(Assertion {
            lhs,
            rhs,
            expected_equal: true,
        })
    }
    pub fn assert_neq<V>(lhs: T, rhs: T) -> Effect<T, V> {
        Effect::Assertion(Assertion {
            lhs,
            rhs,
            expected_equal: false,
        })
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct Condition<T: RuntimeConstant> {
    pub value: T,
    pub condition: RangeConstraint<T::FieldType>,
}
