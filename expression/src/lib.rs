use std::{
    fmt::{self, Display, Formatter},
    iter, ops,
};

use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

pub mod conversion;
pub mod visitors;

#[derive(
    Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Serialize, Deserialize, JsonSchema, Hash,
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
    /// This specifically does not implement Children because otherwise it would
    /// have a wrong implementation of ExpressionVisitable (which is implemented
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
    /// This specifically does not implement Children because otherwise it would
    /// have a wrong implementation of ExpressionVisitable (which is implemented
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

pub type ExpressionPrecedence = u64;
trait Precedence {
    fn precedence(&self) -> Option<ExpressionPrecedence>;
}

impl Precedence for AlgebraicUnaryOperator {
    fn precedence(&self) -> Option<ExpressionPrecedence> {
        use AlgebraicUnaryOperator::*;
        let precedence = match self {
            Minus => 1,
        };

        Some(precedence)
    }
}

impl Precedence for AlgebraicBinaryOperator {
    fn precedence(&self) -> Option<ExpressionPrecedence> {
        let precedence = match self {
            Self::Mul => 3,
            Self::Add | Self::Sub => 4,
        };

        Some(precedence)
    }
}

impl<T, R> Precedence for AlgebraicExpression<T, R> {
    fn precedence(&self) -> Option<ExpressionPrecedence> {
        match self {
            AlgebraicExpression::UnaryOperation(operation) => operation.op.precedence(),
            AlgebraicExpression::BinaryOperation(operation) => operation.op.precedence(),
            _ => None,
        }
    }
}

impl<T: Display, R: Display> Display for AlgebraicExpression<T, R> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            AlgebraicExpression::Reference(reference) => write!(f, "{reference}"),
            AlgebraicExpression::Number(value) => write!(f, "{value}"),
            AlgebraicExpression::BinaryOperation(o) => {
                write!(f, "{o}")
            }
            AlgebraicExpression::UnaryOperation(o) => write!(f, "{o}"),
        }
    }
}

impl<T: Display, R: Display> Display for AlgebraicBinaryOperation<T, R> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let op_precedence = self.op.precedence().unwrap();
        let use_left_parentheses = match self.left.precedence() {
            Some(left_precedence) => left_precedence > op_precedence,
            None => false,
        };

        let use_right_parentheses = match self.right.precedence() {
            Some(right_precedence) => right_precedence >= op_precedence,
            None => false,
        };

        let left_string = if use_left_parentheses {
            format!("({})", self.left)
        } else {
            format!("{}", self.left)
        };
        let right_string = if use_right_parentheses {
            format!("({})", self.right)
        } else {
            format!("{}", self.right)
        };

        write!(f, "{left_string} {} {right_string}", self.op)
    }
}

impl<T: Display, R: Display> Display for AlgebraicUnaryOperation<T, R> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let exp_string = match (self.op.precedence(), self.expr.precedence()) {
            (Some(precedence), Some(inner_precedence)) if precedence < inner_precedence => {
                format!("({})", self.expr)
            }
            _ => {
                format!("{}", self.expr)
            }
        };

        write!(f, "{}{exp_string}", self.op)
    }
}

impl Display for AlgebraicUnaryOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            AlgebraicUnaryOperator::Minus => write!(f, "-"),
        }
    }
}

impl Display for AlgebraicBinaryOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            AlgebraicBinaryOperator::Add => write!(f, "+"),
            AlgebraicBinaryOperator::Sub => write!(f, "-"),
            AlgebraicBinaryOperator::Mul => write!(f, "*"),
        }
    }
}
