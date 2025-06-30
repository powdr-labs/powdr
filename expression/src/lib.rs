use std::{
    iter,
    ops::{self, Add, Mul, Neg, Sub},
};

use powdr_number::ExpressionConvertible;
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

pub mod display;
pub mod visitors;

#[derive(
    Debug,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Clone,
    Serialize,
    Deserialize,
    JsonSchema,
    Hash,
    derive_more::Display,
)]
pub enum AlgebraicExpression<T, R> {
    Reference(R),
    Number(T),
    BinaryOperation(AlgebraicBinaryOperation<T, R>),
    UnaryOperation(AlgebraicUnaryOperation<T, R>),
}

#[derive(
    Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Serialize, Deserialize, JsonSchema, Hash,
)]
pub struct AlgebraicBinaryOperation<T, R> {
    pub left: Box<AlgebraicExpression<T, R>>,
    pub op: AlgebraicBinaryOperator,
    pub right: Box<AlgebraicExpression<T, R>>,
}

#[derive(
    Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Serialize, Deserialize, JsonSchema, Hash,
)]
pub enum AlgebraicBinaryOperator {
    Add,
    Sub,
    Mul,
}

#[derive(
    Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Serialize, Deserialize, JsonSchema, Hash,
)]
pub struct AlgebraicUnaryOperation<T, R> {
    pub op: AlgebraicUnaryOperator,
    pub expr: Box<AlgebraicExpression<T, R>>,
}

#[derive(
    Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Serialize, Deserialize, JsonSchema, Hash,
)]
pub enum AlgebraicUnaryOperator {
    Minus,
}

impl<T, R> AlgebraicExpression<T, R> {
    /// Returns an iterator over all (top-level) expressions in this expression.
    /// This specifically does not implement the Children trait because otherwise it
    /// would have a wrong implementation of ExpressionVisitable (which is implemented
    /// generically for all types that implement Children<Expr>).
    fn children(&self) -> Box<dyn Iterator<Item = &AlgebraicExpression<T, R>> + '_> {
        match self {
            AlgebraicExpression::Reference(_) | AlgebraicExpression::Number(_) => {
                Box::new(iter::empty())
            }
            AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
                left, right, ..
            }) => Box::new([left.as_ref(), right.as_ref()].into_iter()),
            AlgebraicExpression::UnaryOperation(AlgebraicUnaryOperation { expr: e, .. }) => {
                Box::new([e.as_ref()].into_iter())
            }
        }
    }
    /// Returns an iterator over all (top-level) expressions in this expression.
    /// This specifically does not implement the Children trait because otherwise it
    /// would have a wrong implementation of ExpressionVisitable (which is implemented
    /// generically for all types that implement Children<Expr>).
    fn children_mut(&mut self) -> Box<dyn Iterator<Item = &mut AlgebraicExpression<T, R>> + '_> {
        match self {
            AlgebraicExpression::Reference(_) | AlgebraicExpression::Number(_) => {
                Box::new(iter::empty())
            }
            AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
                left, right, ..
            }) => Box::new([left.as_mut(), right.as_mut()].into_iter()),
            AlgebraicExpression::UnaryOperation(AlgebraicUnaryOperation { expr: e, .. }) => {
                Box::new([e.as_mut()].into_iter())
            }
        }
    }

    /// Returns the degree of the expressions
    pub fn degree(&self) -> usize {
        match self {
            AlgebraicExpression::Reference(..) => 1,
            // Multiplying two expressions adds their degrees
            AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
                op: AlgebraicBinaryOperator::Mul,
                left,
                right,
            }) => left.degree() + right.degree(),
            // In all other cases, we take the maximum of the degrees of the children
            _ => self.children().map(|e| e.degree()).max().unwrap_or(0),
        }
    }

    pub fn new_binary(left: Self, op: AlgebraicBinaryOperator, right: Self) -> Self {
        AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
            left: Box::new(left),
            op,
            right: Box::new(right),
        })
    }

    pub fn new_unary(op: AlgebraicUnaryOperator, expr: Self) -> Self {
        AlgebraicExpression::UnaryOperation(AlgebraicUnaryOperation {
            op,
            expr: Box::new(expr),
        })
    }
}

impl<T: Clone + Ord, R: Clone + Ord> AlgebraicExpression<T, R> {
    /// Get a canonical form by expanding the expression and reordering operands.
    pub fn normalize(self) -> Self {
        normalize(expand(self))
    }
}

impl<T, R> ops::Add for AlgebraicExpression<T, R> {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self::new_binary(self, AlgebraicBinaryOperator::Add, rhs)
    }
}

impl<T, R> ops::Sub for AlgebraicExpression<T, R> {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self::new_binary(self, AlgebraicBinaryOperator::Sub, rhs)
    }
}

impl<T, R> ops::Neg for AlgebraicExpression<T, R> {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Self::new_unary(AlgebraicUnaryOperator::Minus, self)
    }
}

impl<T, R> ops::Mul for AlgebraicExpression<T, R> {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        Self::new_binary(self, AlgebraicBinaryOperator::Mul, rhs)
    }
}

impl<T, R> From<T> for AlgebraicExpression<T, R> {
    fn from(value: T) -> Self {
        AlgebraicExpression::Number(value)
    }
}

// Helper to flatten Add trees
fn flatten_add<T: Ord + Clone, R: Ord + Clone>(
    expr: AlgebraicExpression<T, R>,
    acc: &mut Vec<AlgebraicExpression<T, R>>,
) {
    match expr {
        AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
            left,
            op: AlgebraicBinaryOperator::Add,
            right,
        }) => {
            flatten_add(*left, acc);
            flatten_add(*right, acc);
        }
        _ => acc.push(normalize(expr)),
    }
}

// Helper to flatten Mul trees
fn flatten_mul<T: Ord + Clone, R: Ord + Clone>(
    expr: AlgebraicExpression<T, R>,
    acc: &mut Vec<AlgebraicExpression<T, R>>,
) {
    match expr {
        AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
            left,
            op: AlgebraicBinaryOperator::Mul,
            right,
        }) => {
            flatten_mul(*left, acc);
            flatten_mul(*right, acc);
        }
        _ => acc.push(normalize(expr)),
    }
}

fn normalize<T: Ord + Clone, R: Ord + Clone>(
    expr: AlgebraicExpression<T, R>,
) -> AlgebraicExpression<T, R> {
    match expr {
        AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
            left,
            op: AlgebraicBinaryOperator::Add,
            right,
        }) => {
            let mut terms = Vec::new();
            flatten_add(*left, &mut terms);
            flatten_add(*right, &mut terms);
            terms.sort();
            terms.into_iter().reduce(|a, b| a + b).unwrap()
        }
        AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
            left,
            op: AlgebraicBinaryOperator::Mul,
            right,
        }) => {
            let mut factors = Vec::new();
            flatten_mul(*left, &mut factors);
            flatten_mul(*right, &mut factors);
            factors.sort();
            factors.into_iter().reduce(|a, b| a * b).unwrap()
        }
        AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
            left,
            op: AlgebraicBinaryOperator::Sub,
            right,
        }) => normalize(*left) - normalize(*right),
        _ => expr,
    }
}

fn expand<T: Ord + Clone, R: Ord + Clone>(
    expr: AlgebraicExpression<T, R>,
) -> AlgebraicExpression<T, R> {
    match expr {
        AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
            left,
            op: AlgebraicBinaryOperator::Mul,
            right,
        }) => {
            let left = expand(*left);
            let right = expand(*right);
            match (left, right) {
                // (a + b)*c ==> a*c + b*c
                (
                    AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
                        left: a,
                        op: AlgebraicBinaryOperator::Add,
                        right: b,
                    }),
                    c,
                ) => expand(*a * c.clone()) + expand(*b * c),
                // a*(b + c) ==> a*b + a*c
                (
                    a,
                    AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
                        left: b,
                        op: AlgebraicBinaryOperator::Add,
                        right: c,
                    }),
                ) => expand(a.clone() * *b) + expand(a.clone() * *c),
                (left, right) => left * right,
            }
        }
        AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
            left,
            op: AlgebraicBinaryOperator::Add,
            right,
        }) => expand(*left) + expand(*right),
        AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
            left,
            op: AlgebraicBinaryOperator::Sub,
            right,
        }) => expand(*left) - expand(*right),
        _ => expr,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    type Expr = AlgebraicExpression<u32, String>;

    fn v(name: &str) -> Expr {
        Expr::Reference(name.to_string())
    }

    #[test]
    fn test_normalize() {
        let expr1 = normalize(v("c") + v("b") + v("a"));
        let expr2 = normalize(v("a") + v("b") + v("c"));
        assert_eq!(expr1, expr2);

        let expr1 = normalize(v("c") * v("b") * v("a"));
        let expr2 = normalize(v("a") * v("b") * v("c"));
        assert_eq!(expr1, expr2);

        let expr1 = normalize(v("a") + v("b") * v("c"));
        let expr2 = normalize(v("c") * v("b") + v("a"));
        assert_eq!(expr1, expr2);
    }

    #[test]
    fn test_expand() {
        let e1 = expand(v("a") * (v("b") + v("c")));
        let e2 = v("a") * v("b") + v("a") * v("c");
        assert_eq!(e1, e2);

        let e1 = expand((v("a") + v("b")) * v("c"));
        let e2 = v("a") * v("c") + v("b") * v("c");
        assert_eq!(e1, e2);

        // (a + b) * (c + d)
        let e1 = expand((v("a") + v("b")) * (v("c") + v("d")));
        let e2 = v("a") * v("c") + v("a") * v("d") + v("b") * v("c") + v("b") * v("d");
        assert_eq!(normalize(e1), normalize(e2));

        // (a + b) * (c + d) * (e + f)
        let e1 = expand((v("a") + v("b")) * (v("c") + v("d")) * (v("e") + v("f")));
        let e2 = v("a") * v("c") * v("e")
            + v("a") * v("c") * v("f")
            + v("a") * v("d") * v("e")
            + v("a") * v("d") * v("f")
            + v("b") * v("c") * v("e")
            + v("b") * v("c") * v("f")
            + v("b") * v("d") * v("e")
            + v("b") * v("d") * v("f");
        assert_eq!(normalize(e1), normalize(e2));

        // a*(b + c) + d + b*(a + b)
        let expr1 = normalize(expand(
            v("a") * (v("b") + v("c")) + v("d") + v("b") * (v("a") + v("b")),
        ));
        // a*b + a*c + (d + b*a + b*b)
        let expr2 = normalize(expand(
            v("a") * v("b") + v("a") * v("c") + (v("d") + v("b") * v("a") + v("b") * v("b")),
        ));
        // (d + b*a) + b*b + (a*b + a*c)
        let expr3 = normalize(expand(
            (v("d") + v("b") * v("a")) + v("b") * v("b") + (v("a") * v("b") + v("a") * v("c")),
        ));
        assert_eq!(expr1, expr2);
        assert_eq!(expr1, expr3);
    }
}

impl<T, R> ExpressionConvertible<T, R> for AlgebraicExpression<T, R> {
    fn to_expression<
        E: Add<E, Output = E> + Sub<E, Output = E> + Mul<E, Output = E> + Neg<Output = E>,
    >(
        &self,
        number_converter: &impl Fn(&T) -> E,
        var_converter: &impl Fn(&R) -> E,
    ) -> E {
        match self {
            AlgebraicExpression::Reference(r) => var_converter(r),
            AlgebraicExpression::Number(n) => number_converter(n),
            AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation { left, op, right }) => {
                let left = left.to_expression(number_converter, var_converter);
                let right = right.to_expression(number_converter, var_converter);

                match op {
                    AlgebraicBinaryOperator::Add => left + right,
                    AlgebraicBinaryOperator::Sub => left - right,
                    AlgebraicBinaryOperator::Mul => left * right,
                }
            }
            AlgebraicExpression::UnaryOperation(AlgebraicUnaryOperation { op, expr }) => match op {
                AlgebraicUnaryOperator::Minus => {
                    -expr.to_expression(number_converter, var_converter)
                }
            },
        }
    }
}
