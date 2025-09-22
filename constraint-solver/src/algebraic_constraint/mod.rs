use std::fmt::Display;

use crate::{
    grouped_expression::GroupedExpression,
    runtime_constant::{ReferencedSymbols, RuntimeConstant, Substitutable},
};

use num_traits::{One, Zero};

pub mod solve;

/// An algebraic constraint
#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq)]
pub struct AlgebraicConstraint<V> {
    /// The expression representing the constraint, which must evaluate to 0 for the constraint to be satisfied.
    pub expression: V,
}

// We implement `From` to make writing tests easier. However, we recommend using `AlgebraicConstraint::assert_zero` for clarity
impl<V> From<V> for AlgebraicConstraint<V> {
    fn from(expression: V) -> Self {
        AlgebraicConstraint::assert_zero(expression)
    }
}

impl<V> AlgebraicConstraint<V> {
    /// Create a constraint which asserts that the expression evaluates to 0.
    pub fn assert_zero(expression: V) -> Self {
        AlgebraicConstraint { expression }
    }

    /// Returns a constraint over a reference to the expression. This is useful to interact with the solver.
    pub fn as_ref(&self) -> AlgebraicConstraint<&V> {
        AlgebraicConstraint {
            expression: &self.expression,
        }
    }
}

impl<V: Clone> AlgebraicConstraint<&V> {
    pub(crate) fn cloned(&self) -> AlgebraicConstraint<V> {
        AlgebraicConstraint {
            expression: self.expression.clone(),
        }
    }
}

impl<T: RuntimeConstant, V: Clone + Ord> AlgebraicConstraint<GroupedExpression<T, V>> {
    /// Returns a constraint which asserts that the two expressions are equal.
    pub fn assert_eq(expression: GroupedExpression<T, V>, other: GroupedExpression<T, V>) -> Self {
        Self::assert_zero(expression - other)
    }

    /// Returns a constraint which asserts that the expression is a boolean.
    pub fn assert_bool(expression: GroupedExpression<T, V>) -> Self {
        Self::assert_zero(expression.clone() * (expression - GroupedExpression::one()))
    }
}

impl<V: Zero> AlgebraicConstraint<V> {
    pub fn is_redundant(&self) -> bool {
        self.expression.is_zero()
    }
}

impl<T: RuntimeConstant + Substitutable<V>, V: Clone + Eq + Ord>
    AlgebraicConstraint<GroupedExpression<T, V>>
{
    /// Substitute a variable by a symbolically known expression. The variable can be known or unknown.
    /// If it was already known, it will be substituted in the known expressions.
    pub fn substitute_by_known(&mut self, variable: &V, substitution: &T) {
        self.expression.substitute_by_known(variable, substitution);
    }

    pub fn degree(&self) -> usize {
        self.expression.degree()
    }
}

impl<V: Display> Display for AlgebraicConstraint<V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} = 0", self.expression)
    }
}

impl<T, V> AlgebraicConstraint<GroupedExpression<T, V>> {
    /// Returns the referenced unknown variables. Might contain repetitions.
    pub fn referenced_unknown_variables(&self) -> Box<dyn Iterator<Item = &V> + '_> {
        self.expression.referenced_unknown_variables()
    }
}

impl<T, V> AlgebraicConstraint<&GroupedExpression<T, V>> {
    /// Returns the referenced unknown variables. Might contain repetitions.
    pub fn referenced_unknown_variables(&self) -> Box<dyn Iterator<Item = &V> + '_> {
        self.expression.referenced_unknown_variables()
    }
}

impl<T: ReferencedSymbols<V>, V> AlgebraicConstraint<GroupedExpression<T, V>> {
    /// Returns the set of referenced variables, both know and unknown.
    pub fn referenced_variables(&self) -> Box<dyn Iterator<Item = &V> + '_> {
        self.expression.referenced_variables()
    }
}

impl<T: ReferencedSymbols<V>, V> AlgebraicConstraint<&GroupedExpression<T, V>> {
    /// Returns the set of referenced variables, both know and unknown.
    pub fn referenced_variables(&self) -> Box<dyn Iterator<Item = &V> + '_> {
        self.expression.referenced_variables()
    }
}
