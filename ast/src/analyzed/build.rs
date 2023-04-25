use super::{BinaryOperator, Expression};

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
