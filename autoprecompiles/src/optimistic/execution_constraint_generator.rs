use std::collections::BTreeMap;


use crate::{
    adapter::Adapter,
    empirical_constraints::{EqualityConstraint, EqualityExpression},
    execution::{
        ExecutionState, OptimisticConstraint, OptimisticExpression,
        OptimisticLiteral,
    },
    expression::AlgebraicReference,
};

/// Converts a list of equality constraints into optimistic execution constraints.
/// Only works for constraints between numbers and algebraic references that have
/// corresponding optimistic literal, otherwise panics.
pub fn generate_execution_constraints<A: Adapter>(
    equality_constraints: &[EqualityConstraint<A::PowdrField>],
    optimistic_literals: &BTreeMap<
        AlgebraicReference,
        OptimisticLiteral<<A::ExecutionState as ExecutionState>::RegisterAddress>,
    >,
) -> Vec<
    OptimisticConstraint<
        <A::ExecutionState as ExecutionState>::RegisterAddress,
        <A::ExecutionState as ExecutionState>::Value,
    >,
> {
    equality_constraints
        .iter()
        .map(|constraint| OptimisticConstraint {
            left: get_optimistic_expression::<A>(optimistic_literals, &constraint.left).unwrap(),
            right: get_optimistic_expression::<A>(optimistic_literals, &constraint.right).unwrap(),
        })
        .collect()
}

fn get_optimistic_expression<A: Adapter>(
    optimistic_literals: &BTreeMap<
        AlgebraicReference,
        OptimisticLiteral<<A::ExecutionState as ExecutionState>::RegisterAddress>,
    >,
    algebraic_expression: &EqualityExpression<A::PowdrField>,
) -> Option<
    OptimisticExpression<
        <A::ExecutionState as ExecutionState>::RegisterAddress,
        <A::ExecutionState as ExecutionState>::Value,
    >,
> {
    match algebraic_expression {
        EqualityExpression::Number(n) => {
            Some(OptimisticExpression::Number(A::try_into_value(*n).unwrap()))
        }
        EqualityExpression::Reference(r) => {
            let optimistic_literal = optimistic_literals.get(r)?;
            Some(OptimisticExpression::Literal(optimistic_literal.clone()))
        }
    }
}
