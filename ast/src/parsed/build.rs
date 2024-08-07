use powdr_number::BigUint;

use crate::parsed::{Expression, SourceReference};

use super::{
    asm::{parse_absolute_path, Part, SymbolPath},
    BinaryOperation, BinaryOperator, IndexAccess, NamespacedPolynomialReference, UnaryOperation,
    UnaryOperator,
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
    UnaryOperation {
        op: UnaryOperator::Next,
        expr: Box::new(direct_reference(name)),
    }
    .into()
}

/// Returns an index access operation to expr if the index is Some, otherwise returns expr itself.
pub fn index_access(expr: Expression, index: Option<BigUint>) -> Expression {
    match index {
        Some(i) => Expression::IndexAccess(
            expr.source_reference().clone(),
            IndexAccess {
                array: Box::new(expr),
                index: Box::new(i.into()),
            },
        ),
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
