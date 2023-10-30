use crate::parsed::Expression;

use super::{PolynomialReference, UnaryOperator};

pub fn direct_reference<S: Into<String>, T>(name: S) -> Expression<T> {
    PolynomialReference::new(name).single().local().into()
}

pub fn namespaced_reference<S: Into<String>, T>(namespace: String, name: S) -> Expression<T> {
    PolynomialReference::new(name)
        .single()
        .namespaced(namespace)
        .into()
}

pub fn next_reference<T>(name: &str) -> Expression<T> {
    Expression::UnaryOperation(UnaryOperator::Next, Box::new(direct_reference(name)))
}
