use powdr_number::BigUint;

use crate::{parsed::Expression, SourceRef};

use super::{
    asm::{parse_absolute_path, Part, SymbolPath},
    BinaryOperation, BinaryOperator, IndexAccess, NamespacedPolynomialReference, SourceInfo,
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
    Expression::UnaryOperation(
        SourceRef::unknown(),
        super::UnaryOperation {
            op: UnaryOperator::Next,
            e: Box::new(direct_reference(name)),
        },
    )
}

/// Returns an index access operation to expr if the index is Some, otherwise returns expr itself.
pub fn index_access(expr: Expression, index: Option<BigUint>) -> Expression {
    match index {
        Some(i) => Expression::IndexAccess(
            expr.get_source().clone(),
            IndexAccess {
                array: Box::new(expr.clone()),
                index: Box::new(Expression::Number(
                    SourceRef::unknown(),
                    super::Number {
                        value: i,
                        type_: None,
                    },
                )),
            },
        ),
        None => expr,
    }
}

pub fn identity(lhs: Expression, rhs: Expression) -> Expression {
    Expression::BinaryOperation(
        lhs.get_source().clone(),
        BinaryOperation {
            left: Box::new(lhs.clone()),
            op: BinaryOperator::Identity,
            right: Box::new(rhs),
        },
    )
}
