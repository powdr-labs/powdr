use std::{
    hash::Hash,
    ops::{Add, Div, Mul, Neg, Sub},
};

use powdr_number::FieldElement;

use crate::{
    quadratic_symbolic_expression::QuadraticSymbolicExpression,
    symbolic_expression::{BinaryOperator, SymbolicExpression, UnaryOperator},
};
