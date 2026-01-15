use std::collections::BTreeMap;

use powdr_number::{FieldElement, LargeInt};

use crate::{
    empirical_constraints::{EqualityConstraint, EqualityExpression},
    execution::{OptimisticConstraint, OptimisticExpression, OptimisticLiteral},
    expression::AlgebraicReference,
};

/// Converts a list of equality constraints into optimistic execution constraints.
/// Only works for constraints between numbers and algebraic references that have
/// corresponding optimistic literal, otherwise panics.
pub fn generate_execution_constraints<T: FieldElement>(
    equality_constraints: &[EqualityConstraint<T>],
    optimistic_literals: &BTreeMap<AlgebraicReference, OptimisticLiteral<Vec<T>>>,
) -> Vec<OptimisticConstraint<Vec<T>, u32>> {
    equality_constraints
        .iter()
        .map(|constraint| OptimisticConstraint {
            left: get_optimistic_expression(optimistic_literals, &constraint.left).unwrap(),
            right: get_optimistic_expression(optimistic_literals, &constraint.right).unwrap(),
        })
        .collect()
}

fn get_optimistic_expression<T: FieldElement>(
    optimistic_literals: &BTreeMap<AlgebraicReference, OptimisticLiteral<Vec<T>>>,
    algebraic_expression: &EqualityExpression<T>,
) -> Option<OptimisticExpression<Vec<T>, u32>> {
    match algebraic_expression {
        EqualityExpression::Number(n) => Some(OptimisticExpression::Number(
            n.to_integer().try_into_u32().unwrap(),
        )),
        EqualityExpression::Reference(r) => {
            let optimistic_literal = optimistic_literals.get(r)?;
            Some(OptimisticExpression::Literal(optimistic_literal.clone()))
        }
    }
}
