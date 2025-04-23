use powdr_ast::analyzed::{
    AlgebraicBinaryOperation, AlgebraicBinaryOperator, AlgebraicExpression, AlgebraicReference,
    AlgebraicUnaryOperation, AlgebraicUnaryOperator, Analyzed, Identity, PolynomialIdentity,
};
use powdr_constraint_solver::{
    quadratic_symbolic_expression::QuadraticSymbolicExpression, solver::Solver,
    symbolic_expression::SymbolicExpression,
};
use powdr_number::FieldElement;

pub fn run_compile_time_solver<T: FieldElement>(pil_file: &mut Analyzed<T>) {
    let constraints = pil_file
        .identities
        .iter()
        .filter_map(|identity| match identity {
            Identity::Polynomial(PolynomialIdentity { expression, .. }) => {
                Some(algebraic_to_quadratic_symbolic_expression(expression))
            }
            _ => None,
        })
        .collect();

    let solver_result = Solver::new(constraints).solve().unwrap();

    for (var, expr) in solver_result.into_iter() {
        if let SymbolicExpression::Concrete(v) = expr {
            pil_file.append_polynomial_identity(
                AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
                    left: Box::new(AlgebraicExpression::Reference(var)),
                    op: AlgebraicBinaryOperator::Sub,
                    right: Box::new(AlgebraicExpression::Number(v)),
                }),
                Default::default(),
            );
        }
    }
}

// TODO: Copied from https://github.com/powdr-labs/powdr/pull/2661
/// Turns an algebraic expression into a quadratic symbolic expression,
/// assuming all [`AlgebraicReference`]s are unknown variables.
fn algebraic_to_quadratic_symbolic_expression<T: FieldElement>(
    expr: &AlgebraicExpression<T>,
) -> QuadraticSymbolicExpression<T, AlgebraicReference> {
    match expr {
        AlgebraicExpression::Reference(r) => {
            QuadraticSymbolicExpression::from_unknown_variable(r.clone())
        }
        AlgebraicExpression::Number(n) => (*n).into(),
        AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation { left, op, right }) => {
            let left = algebraic_to_quadratic_symbolic_expression(left);
            let right = algebraic_to_quadratic_symbolic_expression(right);
            match op {
                AlgebraicBinaryOperator::Add => left + right,
                AlgebraicBinaryOperator::Sub => left - right,
                AlgebraicBinaryOperator::Mul => left * right,
                AlgebraicBinaryOperator::Pow => {
                    todo!()
                }
            }
        }
        AlgebraicExpression::UnaryOperation(AlgebraicUnaryOperation { op, expr }) => match op {
            AlgebraicUnaryOperator::Minus => -algebraic_to_quadratic_symbolic_expression(expr),
        },
        AlgebraicExpression::PublicReference(_) | AlgebraicExpression::Challenge(_) => todo!(),
    }
}
