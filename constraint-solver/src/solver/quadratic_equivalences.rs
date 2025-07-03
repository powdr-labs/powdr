//! Module to help find certain pairs of equivalent variables in a system of quadratic constraints.

use std::{collections::HashSet, fmt::Display, hash::Hash};

use itertools::Itertools;

use crate::{
    grouped_expression::{GroupedExpression, RangeConstraintProvider},
    range_constraint::RangeConstraint,
    runtime_constant::RuntimeConstant,
};

/// Given a list of constraints in the form of grouped expressions, tries to determine
/// pairs of equivalent variables.
pub fn find_quadratic_equalities<T: RuntimeConstant, V: Ord + Clone + Hash + Eq + Display>(
    constraints: &[GroupedExpression<T, V>],
    range_constraints: impl RangeConstraintProvider<T::FieldType, V>,
) -> Vec<(V, V)> {
    let candidates = constraints
        .iter()
        .filter_map(QuadraticEqualityCandidate::try_from_grouped_expression)
        .filter(|c| c.variables.len() >= 2)
        .collect::<Vec<_>>();
    candidates
        .iter()
        .tuple_combinations()
        .flat_map(|(c1, c2)| process_quadratic_equality_candidate_pair(c1, c2, &range_constraints))
        .collect()
}

/// If we have two constraints of the form
/// `(X + expr) * (X + expr + offset) = 0` and
/// `(Y + expr) * (Y + expr + offset) = 0`
/// with the same range constraints on `X` and `Y` and
/// offset is a known value such that `X` and `Y` can be
/// determined by a (the same) conditional assignment from
/// `expr` or `expr + offset` (see [`QuadraticSymbolicExpression::solve_quadratic`]),
/// then `X` and `Y` must be equal and are returned.
fn process_quadratic_equality_candidate_pair<
    T: RuntimeConstant,
    V: Ord + Clone + Hash + Eq + Display,
>(
    c1: &QuadraticEqualityCandidate<T, V>,
    c2: &QuadraticEqualityCandidate<T, V>,
    range_constraints: &impl RangeConstraintProvider<T::FieldType, V>,
) -> Option<(V, V)> {
    if c1.variables.len() != c2.variables.len() || c1.variables.len() < 2 {
        return None;
    }
    let c1_var = c1.variables.difference(&c2.variables).exactly_one().ok()?;
    let c2_var = c2.variables.difference(&c1.variables).exactly_one().ok()?;
    // The expressions differ in exactly one variable.
    let rc1 = range_constraints.get(c1_var);
    let rc2 = range_constraints.get(c2_var);

    // And those variables have the same range constraint.
    if rc1 != rc2 {
        return None;
    }

    let c1 = c1.normalized_for_var(c1_var);
    let c2 = c2.normalized_for_var(c2_var);

    let c1_offset = c1.offset.try_to_number()?;
    let c2_offset = c2.offset.try_to_number()?;

    if c1_offset != c2_offset {
        return None;
    }

    // And the offset (the difference between the two alternatives) determines if
    // we are inside the range constraint or not.
    if !rc1.is_disjoint(&rc1.combine_sum(&RangeConstraint::from_value(c1_offset))) {
        return None;
    }

    // Now the only remaining check is to see if the affine expressions are the same.
    // This could have been the first step, but it is rather expensive, so we do it last.
    if c1.expr - GroupedExpression::from_unknown_variable(c1_var.clone())
        != c2.expr - GroupedExpression::from_unknown_variable(c2_var.clone())
    {
        return None;
    }

    // Now we have `(X + A) * (X + A + offset) = 0` and `(Y + A) * (Y + A + offset) = 0`
    // Furthermore, the range constraints of `X` and `Y` are such that for both identities,
    // the two alternatives can never be satisfied at the same time. Since both variables
    // have the same range constraint, depending on the value of `A`, we either have
    // - X = -A and Y = -A, or
    // - X = -A - offset and Y = -A - offset
    // Since `A` has to have some value, we can conclude `X = Y`.

    Some((c1_var.clone(), c2_var.clone()))
}

/// This represents an identity `expr * (expr + offset) = 0`,
/// where `expr` is an affine expression and `offset` is a runtime constant.
///
/// All unknown variables appearing in `expr` are stored in `variables`.
struct QuadraticEqualityCandidate<T: RuntimeConstant, V: Ord + Clone + Hash + Eq> {
    expr: GroupedExpression<T, V>,
    offset: T,
    /// All unknown variables in `expr`.
    variables: HashSet<V>,
}

impl<T: RuntimeConstant, V: Ord + Clone + Hash + Eq> QuadraticEqualityCandidate<T, V> {
    fn try_from_grouped_expression(constr: &GroupedExpression<T, V>) -> Option<Self> {
        let (left, right) = constr.try_as_single_product()?;
        if !left.is_affine() || !right.is_affine() {
            return None;
        }
        // `constr = 0` is equivalent to `left * right = 0`
        let offset = (left - right).try_to_known()?.try_to_number()?;
        // `offset + right = left`
        // `constr = 0` is equivalent to `right * (right + offset) = 0`
        let variables = right
            .referenced_unknown_variables()
            .cloned()
            .collect::<HashSet<_>>();
        Some(Self {
            expr: right.clone(),
            offset: offset.into(),
            variables,
        })
    }

    /// Returns an equivalent candidate that is normalized
    /// such that `var` has a coefficient of `1`.
    fn normalized_for_var(&self, var: &V) -> Self {
        let coefficient = self.expr.coefficient_of_variable(var).unwrap();

        // self represents
        // `(coeff * var + X) * (coeff * var + X + offset) = 0`
        // Dividing by `coeff` twice results in
        // `(var + X / coeff) * (var + X / coeff + offset / coeff) = 0`
        let offset = self.offset.field_div(coefficient);
        let expr = self.expr.clone() * coefficient.field_inverse();
        Self {
            expr,
            offset,
            variables: self.variables.clone(),
        }
    }
}
