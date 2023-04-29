use pil_analyzer::{util::expr_any, Expression, PolynomialReference};

use super::FixedData;

pub trait WitnessColumnNamer<K = usize> {
    fn name(&self, i: K) -> String;
}

/// @returns true if the expression contains a reference to a next value of a
/// (witness or fixed) column
pub fn contains_next_ref(expr: &Expression) -> bool {
    expr_any(expr, |e| match e {
        Expression::PolynomialReference(poly) => poly.next,
        _ => false,
    })
}

/// @returns true if the expression contains a reference to a next value of a witness column.
pub fn contains_next_witness_ref(expr: &Expression, fixed_data: &FixedData) -> bool {
    expr_any(expr, |e| match e {
        Expression::PolynomialReference(poly) => {
            poly.next && fixed_data.witness_ids.contains_key(poly.name.as_str())
        }
        _ => false,
    })
}

/// @returns true if the expression contains a reference to a witness column.
pub fn contains_witness_ref(expr: &Expression, fixed_data: &FixedData) -> bool {
    expr_any(expr, |e| match e {
        Expression::PolynomialReference(poly) => {
            fixed_data.witness_ids.contains_key(poly.name.as_str())
        }
        _ => false,
    })
}

/// Checks if an expression is
/// - a polynomial
/// - not part of a polynomial array
/// - not shifted with `'`
/// and return the polynomial's name if so
pub fn is_simple_poly(expr: &Expression) -> Option<&str> {
    // TODO return the ID and not the str
    if let Expression::PolynomialReference(PolynomialReference {
        name,
        index: None,
        next: false,
        ..
    }) = expr
    {
        Some(name)
    } else {
        None
    }
}

pub fn is_simple_poly_of_name(expr: &Expression, poly_name: &str) -> bool {
    if let Expression::PolynomialReference(PolynomialReference {
        name,
        index: None,
        next: false,
        ..
    }) = expr
    {
        name == poly_name
    } else {
        false
    }
}
