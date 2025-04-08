use powdr_ast::analyzed::{
    AlgebraicBinaryOperation, AlgebraicBinaryOperator, AlgebraicExpression,
    AlgebraicUnaryOperation, AlgebraicUnaryOperator,
};
use powdr_number::FieldElement;

use super::{
    quadratic_symbolic_expression::{QuadraticSymbolicExpression, RangeConstraintProvider},
    variable::Variable,
};

pub trait KnownProvider {
    fn is_known(&self, variable: &Variable) -> bool;
}


pub fn algebraic_expression_to_quadratic_symbolic_expression<T: FieldElement>(
    expr: &AlgebraicExpression<T>,
    row_offset: i32,
    require_concretely_known: bool,
    range_constraints: &impl RangeConstraintProvider<T, Variable>,
    known_provider: &impl KnownProvider,
) -> QuadraticSymbolicExpression<T, Variable> {
    match expr {
        AlgebraicExpression::Reference(r) => {
            let variable = Variable::from_reference(r, row_offset);
            let rc = range_constraints.get(&variable);
            let known = if require_concretely_known {
                rc.try_to_single_value().is_some()
            } else {
                known_provider.is_known(&variable)
            };
            if known {
                QuadraticSymbolicExpression::from_known_symbol(variable, rc)
            } else {
                QuadraticSymbolicExpression::from_unknown_variable(variable)
            }
        }
        AlgebraicExpression::PublicReference(_) | AlgebraicExpression::Challenge(_) => todo!(),
        AlgebraicExpression::Number(n) => (*n).into(),
        AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation { left, op, right }) => {
            let left = algebraic_expression_to_quadratic_symbolic_expression(
                left,
                row_offset,
                require_concretely_known,
                range_constraints,
                known_provider,
            );
            let right = algebraic_expression_to_quadratic_symbolic_expression(
                right,
                row_offset,
                require_concretely_known,
                range_constraints,
                known_provider,
            );
            match op {
                AlgebraicBinaryOperator::Add => left + right,
                AlgebraicBinaryOperator::Sub => left - right,
                AlgebraicBinaryOperator::Mul => left * right,
                AlgebraicBinaryOperator::Pow => {
                    todo!()
                }
            }
        }
        AlgebraicExpression::UnaryOperation(AlgebraicUnaryOperation { op, expr }) => {
            let expr = algebraic_expression_to_quadratic_symbolic_expression(
                expr,
                row_offset,
                require_concretely_known,
                range_constraints,
                known_provider,
            );
            match op {
                AlgebraicUnaryOperator::Minus => -expr,
            }
        }
    }
}
