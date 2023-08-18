use crate::parsed::{BinaryOperator, Expression};

use super::PolynomialReference;

pub fn direct_reference<S: Into<String>, T>(name: S) -> Expression<T> {
    PolynomialReference::new(name)
        .single()
        .local()
        .current()
        .into()
}

pub fn namespaced_reference<S: Into<String>, T>(namespace: String, name: S) -> Expression<T> {
    PolynomialReference::new(name)
        .single()
        .namespaced(namespace)
        .current()
        .into()
}

pub fn next_reference<T>(name: &str) -> Expression<T> {
    PolynomialReference::new(name)
        .single()
        .local()
        .next()
        .into()
}

pub fn build_mul<T>(left: Expression<T>, right: Expression<T>) -> Expression<T> {
    build_binary_expr(left, BinaryOperator::Mul, right)
}

pub fn build_sub<T>(left: Expression<T>, right: Expression<T>) -> Expression<T> {
    build_binary_expr(left, BinaryOperator::Sub, right)
}

pub fn build_add<T>(left: Expression<T>, right: Expression<T>) -> Expression<T> {
    build_binary_expr(left, BinaryOperator::Add, right)
}

pub fn build_binary_expr<T>(
    left: Expression<T>,
    op: BinaryOperator,
    right: Expression<T>,
) -> Expression<T> {
    Expression::BinaryOperation(Box::new(left), op, Box::new(right))
}

pub fn build_number<T, V: Into<T>>(value: V) -> Expression<T> {
    Expression::Number(value.into())
}
