use crate::parsed::Expression;

use super::{NamespacedPolynomialReference, UnaryOperator};

pub fn direct_reference<S: Into<String>, T>(name: S) -> Expression<T> {
    NamespacedPolynomialReference {
        namespace: None,
        name: name.into(),
    }
    .into()
}

pub fn namespaced_reference<S: Into<String>, T>(namespace: String, name: S) -> Expression<T> {
    NamespacedPolynomialReference {
        namespace: Some(namespace),
        name: name.into(),
    }
    .into()
}

pub fn next_reference<T>(name: &str) -> Expression<T> {
    Expression::UnaryOperation(UnaryOperator::Next, Box::new(direct_reference(name)))
}
