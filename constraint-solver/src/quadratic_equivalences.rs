//! Module to help find certain pairs of equivalent variables in a system of quadratic constraints.

use std::{collections::HashSet, fmt::Display, hash::Hash};

use itertools::Itertools;
use powdr_number::FieldElement;

use crate::{
    quadratic_symbolic_expression::{QuadraticSymbolicExpression, RangeConstraintProvider},
    range_constraint::RangeConstraint,
};

/// Given a list of constraints in the form of quadratic symbolic expressions, tries to determine
/// pairs of equivalent variables.
pub fn find_quadratic_equalities<T: FieldElement, V: Ord + Clone + Hash + Eq + Display>(
    constraints: &[QuadraticSymbolicExpression<T, V>],
    range_constraints: impl RangeConstraintProvider<T, V>,
) -> Vec<(V, V)> {
    let candidates = constraints
        .iter()
        .filter_map(QuadraticEqualityCandidate::try_from_qse)
        .filter(|c| c.variables.len() >= 2)
        .collect::<Vec<_>>();
    println!("Found {} candidates", candidates.len());
    let mut result = vec![];
    for (i, c1) in candidates.iter().enumerate() {
        //        println!("Processing candidate {i}: {}  ++ {}", c1.expr, c1.offset);

        for c2 in &candidates[(i + 1)..] {
            result.extend(process_quadratic_equality_candidate_pair(
                c1,
                c2,
                &range_constraints,
            ))
        }
    }

    result
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
    T: FieldElement,
    V: Ord + Clone + Hash + Eq + Display,
>(
    c1: &QuadraticEqualityCandidate<T, V>,
    c2: &QuadraticEqualityCandidate<T, V>,
    range_constraints: &impl RangeConstraintProvider<T, V>,
) -> Option<(V, V)> {
    if c1.offset != c2.offset {
        return None;
    }
    if !(c1.variables.len() == c2.variables.len() && c1.variables.len() >= 2) {
        return None;
    }
    let c1_var = c1.variables.difference(&c2.variables).exactly_one().ok()?;
    let c2_var = c2.variables.difference(&c1.variables).exactly_one().ok()?;
    println!("Found candidate pair:\n  {}\n  {}", &c1.expr, &c2.expr);
    // The expressions differ in exactly one variable.
    let rc1 = range_constraints.get(c1_var);
    let rc2 = range_constraints.get(c2_var);
    if rc1 != rc2 {
        println!("  Range constraints are not equal: {rc1}   --- {rc2}");
        return None;
    }

    // TODO at this point we should normalize the two candidates WRT the coefficients
    // of the variables. I think it only works with coefficients of 1 and -1, but I need to check.
    // Note that this could influence the offset above.

    // And those variables have the same range constraint.
    if !rc1.is_disjoint(&rc1.combine_sum(&RangeConstraint::from_value(c1.offset))) {
        println!(
            "  Range constraints are not disjoint (offset is {}): {rc1}   --- {}",
            c1.offset,
            rc1.combine_sum(&RangeConstraint::from_value(c1.offset))
        );
        return None;
    }
    // And the offset (the difference between the two alternatives) determines if we are inside the range constraint or not.

    // Now the only remaining check is to see if the affine expressions are the same.
    // This could have been the first step, but it is rather expensive, so we do it last.

    if c1.expr.clone() - QuadraticSymbolicExpression::from_unknown_variable(c1_var.clone())
        != c2.expr.clone() - QuadraticSymbolicExpression::from_unknown_variable(c2_var.clone())
    {
        println!("  Expressions are not equal: {}   --- {}", c1.expr, c2.expr);
        return None;
    }
    println!("  Found equivalent variables: {c1_var} and {c2_var}");

    Some((c1_var.clone(), c2_var.clone()))
}

/// This represents an identity `expr * (expr + offset) = 0`,
/// where `expr` is an affine expression and `offset` is a known number.
///
/// All unknown variables appearing in `expr` are stored in `variables`.
struct QuadraticEqualityCandidate<T: FieldElement, V: Ord + Clone + Hash + Eq> {
    expr: QuadraticSymbolicExpression<T, V>,
    offset: T,
    variables: HashSet<V>,
}

impl<T: FieldElement, V: Ord + Clone + Hash + Eq> QuadraticEqualityCandidate<T, V> {
    fn try_from_qse(constr: &QuadraticSymbolicExpression<T, V>) -> Option<Self> {
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
            offset,
            variables,
        })
    }
}
