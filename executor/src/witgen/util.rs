use std::collections::HashMap;

use number::FieldElement;
use pil_analyzer::util::{expr_any, previsit_expressions_in_identity_mut};
use pil_analyzer::{Expression, Identity, PolyID, PolynomialReference};

/// @returns true if the expression contains a reference to a next value of a
/// (witness or fixed) column
pub fn contains_next_ref(expr: &Expression) -> bool {
    expr_any(expr, |e| match e {
        Expression::PolynomialReference(poly) => poly.next,
        _ => false,
    })
}

/// @returns true if the expression contains a reference to a next value of a witness column.
pub fn contains_next_witness_ref(expr: &Expression) -> bool {
    expr_any(expr, |e| match e {
        Expression::PolynomialReference(poly) => poly.next && poly.is_witness(),
        _ => false,
    })
}

/// @returns true if the expression contains a reference to a witness column.
pub fn contains_witness_ref(expr: &Expression) -> bool {
    expr_any(expr, |e| match e {
        Expression::PolynomialReference(poly) => poly.is_witness(),
        _ => false,
    })
}

/// Checks if an expression is
/// - a polynomial
/// - not part of a polynomial array
/// - not shifted with `'`
/// and return the polynomial if so
pub fn try_to_simple_poly(expr: &Expression) -> Option<&PolynomialReference> {
    if let Expression::PolynomialReference(
        p @ PolynomialReference {
            index: None,
            next: false,
            ..
        },
    ) = expr
    {
        Some(p)
    } else {
        None
    }
}

pub fn try_to_simple_poly_ref(expr: &Expression) -> Option<PolyID> {
    if let Expression::PolynomialReference(PolynomialReference {
        poly_id,
        index: None,
        next: false,
        ..
    }) = expr
    {
        Some(poly_id.unwrap())
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

pub fn substitute_constants(
    identities: &[Identity],
    constants: &HashMap<String, FieldElement>,
) -> Vec<Identity> {
    identities
        .iter()
        .cloned()
        .map(|mut identity| {
            previsit_expressions_in_identity_mut(&mut identity, &mut |e| {
                if let Expression::Constant(name) = e {
                    *e = Expression::Number(constants[name])
                }
                std::ops::ControlFlow::Continue::<()>(())
            });
            identity
        })
        .collect()
}
