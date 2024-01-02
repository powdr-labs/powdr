use number::FieldElement;

use crate::parsed::Expression;

use super::{
    asm::{Part, SymbolPath},
    IndexAccess, NamespacedPolynomialReference, UnaryOperator,
};

pub fn direct_reference<S: Into<String>, T>(name: S) -> Expression<T> {
    NamespacedPolynomialReference::from(SymbolPath {
        parts: vec![Part::Named(name.into())],
    })
    .into()
}

pub fn namespaced_reference<S: Into<String>, T>(namespace: String, name: S) -> Expression<T> {
    NamespacedPolynomialReference::from(SymbolPath {
        parts: vec![Part::Named(namespace), Part::Named(name.into())],
    })
    .into()
}

pub fn next_reference<T>(name: &str) -> Expression<T> {
    Expression::UnaryOperation(UnaryOperator::Next, Box::new(direct_reference(name)))
}

/// Returns an index access operation to expr if the index is Some, otherwise returns expr itself.
pub fn index_access<T: FieldElement>(expr: Expression<T>, index: Option<T>) -> Expression<T> {
    match index {
        Some(i) => Expression::IndexAccess(IndexAccess {
            array: Box::new(expr),
            index: Box::new(i.into()),
        }),
        None => expr,
    }
}
