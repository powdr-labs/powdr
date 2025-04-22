use std::hash::Hash;

use powdr_number::FieldElement;

use crate::{
    quadratic_symbolic_expression::QuadraticSymbolicExpression,
    symbolic_expression::{BinaryOperator, SymbolicExpression, UnaryOperator},
};

pub fn symbolic_expression_to_quadratic_symbolic_expression<
    T: FieldElement,
    V: Clone + Ord + Hash,
>(
    e: &SymbolicExpression<T, V>,
) -> Option<QuadraticSymbolicExpression<T, V>> {
    Some(match e {
        SymbolicExpression::Concrete(value) => (*value).into(),
        SymbolicExpression::Symbol(var, _) => {
            QuadraticSymbolicExpression::from_unknown_variable(var.clone())
        }
        SymbolicExpression::BinaryOperation(left, op, right, _) => {
            let left = symbolic_expression_to_quadratic_symbolic_expression(left)?;
            let right = symbolic_expression_to_quadratic_symbolic_expression(right)?;
            match op {
                BinaryOperator::Add => left + right,
                BinaryOperator::Sub => left - right,
                BinaryOperator::Mul => left * right,
                BinaryOperator::Div => return None, // TODO
            }
        }
        SymbolicExpression::UnaryOperation(op, inner, _) => {
            let inner = symbolic_expression_to_quadratic_symbolic_expression(inner)?;
            match op {
                UnaryOperator::Neg => -inner,
            }
        }
    })
}
