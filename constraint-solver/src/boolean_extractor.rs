use std::collections::BTreeMap;
use std::fmt::Display;
use std::hash::Hash;

use crate::effect::Effect;
use crate::quadratic_symbolic_expression::QuadraticSymbolicExpression;
use crate::quadratic_symbolic_expression::{NoRangeConstraints, RangeConstraintProvider};
use crate::range_constraint::RangeConstraint;
use itertools::Itertools;
use powdr_number::FieldElement;

/// Tries to simplify a quadratic constraint by transforming it into an affine
/// constraint that makes use of a new boolean variable.
///
/// @param constraint The quadratic constraint to transform.
/// @param var_dispenser A function that returns a new variable that is assumed to be boolean-constrained.
/// It will only be called if the transformation is performed.
pub fn extract_boolean<T: FieldElement, V: Ord + Clone + Hash + Eq>(
    constraint: &QuadraticSymbolicExpression<T, V>,
    mut var_dispenser: impl FnMut() -> V,
) -> Option<QuadraticSymbolicExpression<T, V>> {
    let (left, right) = constraint.try_as_single_product()?;
    // `constr = 0` is equivalent to `left * right = 0`
    let offset = left - right;
    // We only do the transformation if `offset` is known, because
    // otherwise the constraint stays quadratic.
    offset.try_to_known()?;
    // `offset + right = left`
    // `constr = 0` is equivalent to `right * (right + offset) = 0`

    let z = var_dispenser();

    // We return `right + z * offset == 0`, which is equivalent to the original constraint.
    Some(right + &(QuadraticSymbolicExpression::from_unknown_variable(z) * offset))
}

/// Performs the inverse of `extract_boolean` but on a full system of constraints.
pub fn unextract_boolean<T: FieldElement, V: Ord + Clone + Hash + Eq + Display>(
    constraints: Vec<QuadraticSymbolicExpression<T, V>>,
    range_constraints: &impl RangeConstraintProvider<T, V>,
) -> Vec<QuadraticSymbolicExpression<T, V>> {
    // TODO does not take into account range constraints from bus interactions.

    // Boolean constraints by variable
    let mut boolean_constraints = BTreeMap::new();
    // Other constraints
    let mut constraints = constraints
        .into_iter()
        .filter_map(|constr| {
            if let Some(var) = is_boolean_constraint(&constr) {
                boolean_constraints.insert(var.clone(), constr.clone());
                None
            } else {
                Some(constr)
            }
        })
        .collect_vec();

    let constraints_by_variable = constraints
        .iter()
        .enumerate()
        .flat_map(|(i, constr)| {
            constr
                .referenced_unknown_variables()
                .map(move |var| (var.clone(), i))
        })
        .into_group_map();

    for (var, constrs) in constraints_by_variable.iter().sorted() {
        if !boolean_constraints.contains_key(var)
            && range_constraints.get(var) != RangeConstraint::from_mask(1u64)
        {
            continue;
        }
        // If there is only one constraint (apart from boolean constraints) containing this variable, inline it.
        let [index] = constrs.as_slice() else {
            continue;
        };
        let mut zero = constraints[*index].clone();
        zero.substitute_by_known(var, &T::from(0).into());
        let mut one = constraints[*index].clone();
        one.substitute_by_known(var, &T::from(1).into());
        constraints[*index] = zero * one;
        boolean_constraints.remove(var).unwrap();
    }
    constraints.extend(boolean_constraints.values().cloned());
    constraints
}

fn is_boolean_constraint<T: FieldElement, V: Ord + Clone + Hash + Eq + Display>(
    constr: &QuadraticSymbolicExpression<T, V>,
) -> Option<V> {
    if constr
        .referenced_unknown_variables()
        .unique()
        .exactly_one()
        .is_err()
    {
        return None;
    }
    let result = constr.solve(&NoRangeConstraints).ok()?;
    if !result.complete {
        return None;
    }
    match &result.effects[..] {
        [Effect::RangeConstraint(var, range_constraint)] => {
            if *range_constraint == RangeConstraint::from_mask(1u64) {
                Some(var.clone())
            } else {
                None
            }
        }
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        quadratic_symbolic_expression::NoRangeConstraints,
        test_utils::{constant, var},
    };

    use super::*;

    #[test]
    fn test_extract_boolean() {
        let mut var_dispenser = || "z";
        let expr = (var("a") + var("b")) * (var("a") + var("b") + constant(10));
        let result = extract_boolean(expr, &mut var_dispenser);
        assert!(result.is_some());
        let result = result.unwrap();
        assert_eq!(result.to_string(), "a + b + -10 * z + 10");
    }

    #[test]
    fn test_unextract_boolean() {
        let constraints = vec![
            var("a") + var("b") - constant(10) * var("z") + constant(10),
            var("z") * (constant(1) - var("z")),
        ];
        let result = unextract_boolean(constraints, &NoRangeConstraints);
        assert_eq!(result.len(), 1);
        assert_eq!(result[0].to_string(), "(a + b + 10) * (a + b)");
    }
}
