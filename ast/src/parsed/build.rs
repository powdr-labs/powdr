use powdr_number::BigUint;

use crate::parsed::Expression;

use super::{
    asm::{parse_absolute_path, Part, SymbolPath},
    BinaryOperation, BinaryOperator, IndexAccess, NamespacedPolynomialReference, UnaryOperator,
};

pub fn absolute_reference(name: &str) -> Expression {
    NamespacedPolynomialReference::from(parse_absolute_path(name).relative_to(&Default::default()))
        .into()
}

pub fn direct_reference<S: Into<String>>(name: S) -> Expression {
    NamespacedPolynomialReference::from(SymbolPath::from_identifier(name.into())).into()
}

pub fn namespaced_reference<S: Into<String>>(namespace: String, name: S) -> Expression {
    NamespacedPolynomialReference::from(SymbolPath::from_parts(vec![
        Part::Named(namespace),
        Part::Named(name.into()),
    ]))
    .into()
}

pub fn next_reference<S: Into<String>>(name: S) -> Expression {
    Expression::UnaryOperation(UnaryOperator::Next, Box::new(direct_reference(name)))
}

/// Returns an index access operation to expr if the index is Some, otherwise returns expr itself.
pub fn index_access(expr: Expression, index: Option<BigUint>) -> Expression {
    match index {
        Some(i) => Expression::IndexAccess(IndexAccess {
            array: Box::new(expr),
            index: Box::new(Expression::Number(i, None)),
        }),
        None => expr,
    }
}

pub fn identity(lhs: Expression, rhs: Expression) -> Expression {
    BinaryOperation {
        left: Box::new(lhs),
        op: BinaryOperator::Identity,
        right: Box::new(rhs),
    }
    .into()
}
