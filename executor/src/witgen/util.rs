use powdr_ast::analyzed::{AlgebraicExpression as Expression, AlgebraicReference};

/// Checks if an expression is
/// - a polynomial
/// - not part of a polynomial array
/// - not shifted with `'`
///
/// and return the polynomial if so
pub fn try_to_simple_poly<T>(expr: &Expression<T>) -> Option<&AlgebraicReference> {
    if let Expression::Reference(p @ AlgebraicReference { next: false, .. }) = expr {
        Some(p)
    } else {
        None
    }
}

pub fn try_to_simple_poly_ref<T>(expr: &Expression<T>) -> Option<&AlgebraicReference> {
    if let Expression::Reference(poly_ref) = expr {
        (!poly_ref.next).then_some(poly_ref)
    } else {
        None
    }
}
