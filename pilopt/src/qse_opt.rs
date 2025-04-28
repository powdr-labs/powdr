use std::fmt::{self, Display};

use itertools::Itertools;
use powdr_ast::analyzed::{
    AlgebraicBinaryOperation, AlgebraicBinaryOperator, AlgebraicExpression, AlgebraicReference,
    AlgebraicUnaryOperation, AlgebraicUnaryOperator, Analyzed, Challenge, Identity,
    PolynomialIdentity,
};
use powdr_constraint_solver::{
    quadratic_symbolic_expression::QuadraticSymbolicExpression,
    solver::{self, SolveResult},
    symbolic_expression::{BinaryOperator, SymbolicExpression, UnaryOperator},
};
use powdr_number::FieldElement;

/// Performs optimizations on the polynomial constraints in `pil_file`
/// by utilizing quadratic symbolic expressions, and the constraint solver
/// based on those.
///
/// This fully parses the constraints into an internal representation,
/// optimizes on that representation and converts them back into the
/// original representation.
///
/// This means the syntactic structure (order of additions, etc) is not
/// preserved.
pub fn run_qse_optimization<T: FieldElement>(pil_file: &mut Analyzed<T>) {
    let identities = pil_file
        .identities
        .iter()
        .filter_map(|identity| match identity {
            Identity::Polynomial(PolynomialIdentity { expression, .. }) => {
                Some(algebraic_to_quadratic_symbolic_expression(expression))
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
            pil_file
                .identities
                .iter_mut()
                .filter_map(|identity| {
                    if let Identity::Polynomial(PolynomialIdentity { expression, .. }) = identity {
                        Some(expression)
                    } else {
                        None
                    }
                })
                .zip_eq(simplified_algebraic_constraints)
                .for_each(|(identity, simplified)| {
                    *identity = quadratic_symbolic_expression_to_algebraic(&simplified);
                });
        }
    }
}

#[derive(Clone, PartialOrd, Ord, PartialEq, Eq, Hash, Debug)]
enum Variable {
    Reference(AlgebraicReference),
    PublicReference(String),
    Challenge(Challenge),
}

impl Display for Variable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Variable::Reference(r) => write!(f, "{r}"),
            Variable::PublicReference(r) => write!(f, "{r}"),
            Variable::Challenge(c) => write!(f, "{c}"),
        }
    }
}

/// Turns an algebraic expression into a quadratic symbolic expression,
/// assuming all [`AlgebraicReference`]s, public references and challenges
/// are unknown variables.
fn algebraic_to_quadratic_symbolic_expression<T: FieldElement>(
    expr: &AlgebraicExpression<T>,
) -> QuadraticSymbolicExpression<T, Variable> {
    match expr {
        AlgebraicExpression::Reference(r) => {
            QuadraticSymbolicExpression::from_unknown_variable(Variable::Reference(r.clone()))
        }
        AlgebraicExpression::PublicReference(r) => {
            QuadraticSymbolicExpression::from_unknown_variable(Variable::PublicReference(r.clone()))
        }
        AlgebraicExpression::Challenge(c) => {
            QuadraticSymbolicExpression::from_unknown_variable(Variable::Challenge(*c))
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
    }
}

/// Turns a quadratic symbolic expression back into an algebraic expression.
/// Tries to simplify the expression wrt negation and constant factors
/// to aid human readability.
fn quadratic_symbolic_expression_to_algebraic<T: FieldElement>(
    expr: &QuadraticSymbolicExpression<T, Variable>,
) -> AlgebraicExpression<T> {
    // Turn the expression into a list of to-be-summed items and try to
    // simplify on the way.
    let (quadratic, linear, constant) = expr.components();
    let items = quadratic
        .iter()
        .map(|(l, r)| {
            let l = quadratic_symbolic_expression_to_algebraic(l);
            let (l, l_negated) = extract_negation_if_possible(l);
            let r = quadratic_symbolic_expression_to_algebraic(r);
            let (r, r_negated) = extract_negation_if_possible(r);
            if l_negated == r_negated {
                l * r
            } else {
                -(l * r)
            }
        })
        .chain(linear.iter().map(|(v, c)| {
            if let Some(c) = c.try_to_number() {
                if c.is_one() {
                    return variable_to_algebraic_expression(v.clone());
                } else if (-c).is_one() {
                    return -variable_to_algebraic_expression(v.clone());
                }
            }
            let (c, negated) = extract_negation_if_possible(symbolic_expression_to_algebraic(c));
            if negated {
                -(c * variable_to_algebraic_expression(v.clone()))
            } else {
                c * variable_to_algebraic_expression(v.clone())
            }
        }))
        .chain((!constant.is_known_zero()).then(|| symbolic_expression_to_algebraic(constant)));

    // Now order the items by negated and non-negated.
    let mut positive = vec![];
    let mut negated = vec![];
    for item in items {
        let (item, item_negated) = extract_negation_if_possible(item);
        if item_negated {
            negated.push(item);
        } else {
            positive.push(item);
        }
    }
    let positive = positive.into_iter().reduce(|acc, item| acc + item);
    let negated = negated.into_iter().reduce(|acc, item| acc + item);
    match (positive, negated) {
        (Some(positive), Some(negated)) => positive - negated,
        (Some(positive), None) => positive,
        (None, Some(negated)) => -negated,
        (None, None) => AlgebraicExpression::from(T::zero()),
    }
}

fn symbolic_expression_to_algebraic<T: FieldElement>(
    e: &SymbolicExpression<T, Variable>,
) -> AlgebraicExpression<T> {
    match e {
        SymbolicExpression::Concrete(v) => {
            if v.is_in_lower_half() {
                AlgebraicExpression::from(*v)
            } else {
                -AlgebraicExpression::from(-*v)
            }
        }
        SymbolicExpression::Symbol(var, _) => variable_to_algebraic_expression(var.clone()),
        SymbolicExpression::BinaryOperation(left, op, right, _) => {
            let left = Box::new(symbolic_expression_to_algebraic(left));
            let right = Box::new(symbolic_expression_to_algebraic(right));
            let op = symbolic_op_to_algebraic(*op);
            AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation { left, op, right })
        }
        SymbolicExpression::UnaryOperation(op, inner, _) => match op {
            UnaryOperator::Neg => AlgebraicExpression::UnaryOperation(AlgebraicUnaryOperation {
                expr: Box::new(symbolic_expression_to_algebraic(inner)),
                op: AlgebraicUnaryOperator::Minus,
            }),
        },
    }
}

fn symbolic_op_to_algebraic(op: BinaryOperator) -> AlgebraicBinaryOperator {
    match op {
        BinaryOperator::Add => AlgebraicBinaryOperator::Add,
        BinaryOperator::Sub => AlgebraicBinaryOperator::Sub,
        BinaryOperator::Mul => AlgebraicBinaryOperator::Mul,
        BinaryOperator::Div => unreachable!(),
    }
}

fn variable_to_algebraic_expression<T>(var: Variable) -> AlgebraicExpression<T> {
    match var {
        Variable::Reference(r) => AlgebraicExpression::Reference(r),
        Variable::PublicReference(r) => AlgebraicExpression::PublicReference(r),
        Variable::Challenge(c) => AlgebraicExpression::Challenge(c),
    }
}

/// If `e` is negated, returns the expression without negation and `true`,
/// otherwise returns the un-modified expression and `false`.
fn extract_negation_if_possible<T>(e: AlgebraicExpression<T>) -> (AlgebraicExpression<T>, bool) {
    match e {
        AlgebraicExpression::UnaryOperation(AlgebraicUnaryOperation {
            op: AlgebraicUnaryOperator::Minus,
            expr,
        }) => (*expr, true),
        _ => (e, false),
    }
}
