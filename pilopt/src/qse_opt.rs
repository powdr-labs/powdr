use std::iter;

use itertools::Itertools;
use powdr_ast::analyzed::{
    AlgebraicBinaryOperation, AlgebraicBinaryOperator, AlgebraicExpression, AlgebraicReference,
    AlgebraicUnaryOperation, AlgebraicUnaryOperator, Analyzed, Identity, PolynomialIdentity,
};
use powdr_constraint_solver::{
    quadratic_symbolic_expression::QuadraticSymbolicExpression,
    solver::{self, SolveResult},
    symbolic_expression::{BinaryOperator, SymbolicExpression, UnaryOperator},
};
use powdr_number::FieldElement;

pub fn run_qse_optimization<T: FieldElement>(pil_file: &mut Analyzed<T>) {
    let identities = pil_file
        .identities
        .iter()
        .filter_map(|identity| match identity {
            Identity::Polynomial(PolynomialIdentity { expression, .. }) => {
                try_algebraic_to_quadratic_symbolic_expression(expression)
            }
            _ => None,
        })
        .collect_vec();

    match solver::Solver::new(identities).solve() {
        Err(_) => {
            log::error!("Error while QSE-optimizing. This is usually the case when the constraints are inconsistent.");
        }
        Ok(SolveResult {
            simplified_algebraic_constraints,
            ..
        }) => {
            let mut replacements = 0;
            // TODO this is not correct if the are constraints with publics and challenges, because we skip them.
            for identity in &mut pil_file.identities {
                if let Identity::Polynomial(PolynomialIdentity { expression, .. }) = identity {
                    if let Some(constraint) = simplified_algebraic_constraints.get(replacements) {
                        *expression = quadratic_symbolic_expression_to_algebraic(constraint);
                    } else {
                        *expression = AlgebraicExpression::Number(0.into());
                    }
                    replacements += 1;
                }
            }
            // If not, we have more simplified algebraic constraints than original
            // and would have to append.
            assert!(replacements >= simplified_algebraic_constraints.len());
        }
    }
}

// Turns an algebraic expression into a quadratic symbolic expression,
/// assuming all [`AlgebraicReference`]s are unknown variables.
///
/// Fails (returns None) if public references or challenges are used.
fn try_algebraic_to_quadratic_symbolic_expression<T: FieldElement>(
    expr: &AlgebraicExpression<T>,
) -> Option<QuadraticSymbolicExpression<T, AlgebraicReference>> {
    Some(match expr {
        AlgebraicExpression::Reference(r) => {
            QuadraticSymbolicExpression::from_unknown_variable(r.clone())
        }
        AlgebraicExpression::Number(n) => (*n).into(),
        AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation { left, op, right }) => {
            let left = try_algebraic_to_quadratic_symbolic_expression(left)?;
            let right = try_algebraic_to_quadratic_symbolic_expression(right)?;
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
            AlgebraicUnaryOperator::Minus => -try_algebraic_to_quadratic_symbolic_expression(expr)?,
        },
        AlgebraicExpression::PublicReference(_) | AlgebraicExpression::Challenge(_) => return None,
    })
}

fn quadratic_symbolic_expression_to_algebraic<T: FieldElement>(
    expr: &QuadraticSymbolicExpression<T, AlgebraicReference>,
) -> AlgebraicExpression<T> {
    let (quadratic, linear, constant) = expr.elements();
    quadratic
        .iter()
        .map(|(l, r)| {
            quadratic_symbolic_expression_to_algebraic(l)
                * quadratic_symbolic_expression_to_algebraic(r)
        })
        .chain(linear.iter().map(|(v, c)| {
            AlgebraicExpression::Reference((*v).clone()) * symbolic_expression_to_algebraic(c)
        }))
        .chain(iter::once(symbolic_expression_to_algebraic(constant)))
        .reduce(|acc, x| acc + x)
        .unwrap()
}

fn symbolic_expression_to_algebraic<T: FieldElement>(
    e: &SymbolicExpression<T, AlgebraicReference>,
) -> AlgebraicExpression<T> {
    // Problem: div does not work!
    match e {
        SymbolicExpression::Concrete(v) => AlgebraicExpression::Number(*v),
        SymbolicExpression::Symbol(var, _) => AlgebraicExpression::Reference((*var).clone()),
        SymbolicExpression::BinaryOperation(left, op, right, _) => {
            let left = Box::new(symbolic_expression_to_algebraic(left));
            let right = Box::new(symbolic_expression_to_algebraic(right));
            let op = symbolic_op_to_algebraic(*op);
            AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation { left, op, right })
        }
        SymbolicExpression::UnaryOperation(op, inner, _) => match op {
            UnaryOperator::Neg => {
                let expr = Box::new(symbolic_expression_to_algebraic(inner));
                AlgebraicExpression::UnaryOperation(AlgebraicUnaryOperation {
                    expr,
                    op: AlgebraicUnaryOperator::Minus,
                })
            }
        },
    }
}

fn symbolic_op_to_algebraic(op: BinaryOperator) -> AlgebraicBinaryOperator {
    match op {
        BinaryOperator::Add => AlgebraicBinaryOperator::Add,
        BinaryOperator::Sub => AlgebraicBinaryOperator::Sub,
        BinaryOperator::Mul => AlgebraicBinaryOperator::Mul,
        BinaryOperator::Div => todo!(),
    }
}
