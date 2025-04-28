use std::fmt::{self, Display, Formatter};

use itertools::Itertools;
use powdr_number::FieldElement;

use crate::{range_constraint::RangeConstraint, symbolic_expression::SymbolicExpression};

/// The effect of solving a symbolic equation.
#[derive(Clone, PartialEq, Eq)]
pub enum Effect<T: FieldElement, V> {
    /// Variable can be assigned a value.
    Assignment(V, SymbolicExpression<T, V>),
    /// Perform a bit decomposition of a known value, and assign multiple variables.
    BitDecomposition(BitDecomposition<T, V>),
    /// We learnt a new range constraint on variable.
    RangeConstraint(V, RangeConstraint<T>),
    /// A run-time assertion. If this fails, we have conflicting constraints.
    Assertion(Assertion<T, V>),
    /// A variable is assigned one of two alterantive expressions, depending on a condition.
    ConditionalAssignment {
        variable: V,
        condition: Condition<T, V>,
        in_range_value: SymbolicExpression<T, V>,
        out_of_range_value: SymbolicExpression<T, V>,
    },
}

/// A bit decomposition of a value.
/// Executing this effect solves the following equation:
/// value = sum_{i=0}^{components.len() - 1} (-1)**components[i].negative * 2**components[i].exponent * components[i].variable
///
/// This effect can only be created if the equation has a unique solution.
/// It might be that it leads to a contradiction, which should result in an assertion failure.
#[derive(Clone, PartialEq, Eq)]
pub struct BitDecomposition<T: FieldElement, V> {
    /// The value that is decomposed.
    pub value: SymbolicExpression<T, V>,
    /// The components of the decomposition.
    pub components: Vec<BitDecompositionComponent<T, V>>,
}

impl<T: FieldElement, V: Display> Display for BitDecomposition<T, V> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let BitDecomposition { value, components } = self;
        write!(f, "{} := {value};", components.iter().format(" + "))
    }
}

/// A component in the bit decomposition.
/// In a simplified form, we can solve for `variable` using
/// `(value & bit_mask) >> exponent`.
#[derive(Clone, PartialEq, Eq)]
pub struct BitDecompositionComponent<T: FieldElement, V> {
    /// The variables that will be assigned to.
    pub variable: V,
    /// If the variable occurs negatively in the equation.
    /// Note that the range constraint of the variable itself is always non-negative.
    pub is_negative: bool,
    /// The exponent of two, which forms the coefficient of the variable.
    pub exponent: u64,
    /// The bit mask for this component, relative to the value to be decomposed,
    /// i.e. already scaled by the coefficient.
    pub bit_mask: T::Integer,
}

impl<T: FieldElement, V: Display> Display for BitDecompositionComponent<T, V> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let BitDecompositionComponent {
            variable,
            is_negative,
            exponent,
            bit_mask: _,
        } = self;
        write!(
            f,
            "{}2**{exponent} * {variable}",
            if *is_negative { "-" } else { "" },
        )
    }
}

/// A run-time assertion. If this fails, we have conflicting constraints.
#[derive(Clone, PartialEq, Eq)]
pub struct Assertion<T: FieldElement, V> {
    pub lhs: SymbolicExpression<T, V>,
    pub rhs: SymbolicExpression<T, V>,
    /// If this is true, we assert that both sides are equal.
    /// Otherwise, we assert that they are different.
    pub expected_equal: bool,
}

impl<T: FieldElement, V> Assertion<T, V> {
    pub fn assert_is_zero(condition: SymbolicExpression<T, V>) -> Effect<T, V> {
        Self::assert_eq(condition, SymbolicExpression::from(T::from(0)))
    }
    pub fn assert_is_nonzero(condition: SymbolicExpression<T, V>) -> Effect<T, V> {
        Self::assert_neq(condition, SymbolicExpression::from(T::from(0)))
    }
    pub fn assert_eq(lhs: SymbolicExpression<T, V>, rhs: SymbolicExpression<T, V>) -> Effect<T, V> {
        Effect::Assertion(Assertion {
            lhs,
            rhs,
            expected_equal: true,
        })
    }
    pub fn assert_neq(
        lhs: SymbolicExpression<T, V>,
        rhs: SymbolicExpression<T, V>,
    ) -> Effect<T, V> {
        Effect::Assertion(Assertion {
            lhs,
            rhs,
            expected_equal: false,
        })
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct Condition<T: FieldElement, V> {
    pub value: SymbolicExpression<T, V>,
    pub condition: RangeConstraint<T>,
}
