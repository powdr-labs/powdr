use crate::parsed::Expression;

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
