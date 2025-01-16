use powdr_ast::analyzed::{AlgebraicExpression, AlgebraicReference};

/// Checks if an algebraic expression just a polynomial / column reference without "next"
/// and returns the polynomial if so
pub fn try_to_simple_poly<T>(expr: &AlgebraicExpression<T>) -> Option<&AlgebraicReference> {
    if let AlgebraicExpression::Reference(p @ AlgebraicReference { next: false, .. }) = expr {
        Some(p)
    } else {
        None
    }
}
