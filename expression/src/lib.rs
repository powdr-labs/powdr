use std::{
    iter,
    ops::{self, Add, Mul, Neg, Sub},
};

use powdr_number::ExpressionConvertible;
use schemars::JsonSchema;
use serde::{ser::SerializeTuple, Deserialize, Serialize};

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
    #[serde(untagged)]
    Reference(R),
    #[serde(untagged)]
    Number(T),
    #[serde(untagged, serialize_with = "serialize_binary_operation")]
    BinaryOperation(AlgebraicBinaryOperation<T, R>),
    #[serde(untagged, serialize_with = "serialize_unary_operation")]
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
    #[serde(rename = "+")]
    Add,
    #[serde(rename = "-")]
    Sub,
    #[serde(rename = "*")]
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
    #[serde(rename = "-")]
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

fn serialize_unary_operation<S, T, R>(
    un_op: &AlgebraicUnaryOperation<T, R>,
    serializer: S,
) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
    T: Serialize,
    R: Serialize,
{
    (&un_op.op, un_op.expr.as_ref()).serialize(serializer)
}

fn serialize_binary_operation<S, T, R>(
    bin_op: &AlgebraicBinaryOperation<T, R>,
    serializer: S,
) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
    T: Serialize,
    R: Serialize,
{
    (bin_op.left.as_ref(), &bin_op.op, bin_op.right.as_ref()).serialize(serializer)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_serde() {
        let x: AlgebraicExpression<u32, &'static str> = AlgebraicExpression::from(5)
            * AlgebraicExpression::Reference("x")
            - AlgebraicExpression::from(3);
        let serialized = serde_json::to_string(&x).unwrap();
        assert_eq!(serialized, r#"[[5,"*","x"],"-",3]"#);
        let deserialized = serde_json::from_str(&serialized).unwrap();
        assert_eq!(x, deserialized);
    }
}
