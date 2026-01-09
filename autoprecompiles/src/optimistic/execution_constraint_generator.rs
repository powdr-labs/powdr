use std::collections::BTreeMap;

use powdr_number::{FieldElement, LargeInt};

use crate::{
    adapter::Adapter,
    empirical_constraints::EqualityConstraint,
    execution::{OptimisticConstraint, OptimisticExpression},
    expression::{AlgebraicExpression, AlgebraicReference},
    optimistic::AdapterOptimisticLiteral,
};

/// Converts a list of equality constraints into optimistic execution constraints.
/// Only works for constraints between numbers and algebraic references that have
/// corresponding optimistic literal, otherwise panics.
pub fn generate_execution_constraints<A: Adapter>(
    equality_constraints: &[EqualityConstraint<A>],
    optimistic_literals: &BTreeMap<AlgebraicReference, AdapterOptimisticLiteral<A>>,
) -> Vec<OptimisticConstraint<Vec<<A as Adapter>::PowdrField>, u32>> {
    equality_constraints
        .iter()
        .map(|constraint| OptimisticConstraint {
            left: get_optimistic_expression::<A>(optimistic_literals, &constraint.left).unwrap(),
            right: get_optimistic_expression::<A>(optimistic_literals, &constraint.right).unwrap(),
        })
        .collect()
}

fn get_optimistic_expression<A: Adapter>(
    optimistic_literals: &BTreeMap<AlgebraicReference, AdapterOptimisticLiteral<A>>,
    algebraic_expression: &AlgebraicExpression<<A as Adapter>::PowdrField>,
) -> Option<OptimisticExpression<Vec<<A as Adapter>::PowdrField>, u32>> {
    match algebraic_expression {
        AlgebraicExpression::Number(n) => Some(OptimisticExpression::Number(
            n.to_integer().try_into_u32().unwrap(),
        )),
        AlgebraicExpression::Reference(r) => {
            let optimistic_literal = optimistic_literals.get(r)?;
            Some(OptimisticExpression::Literal(optimistic_literal.clone()))
        }
        _ => unreachable!(),
    }
}
