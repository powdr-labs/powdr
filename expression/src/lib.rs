use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

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
