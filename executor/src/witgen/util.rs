use std::{collections::HashSet, ops::ControlFlow};

use pil_analyzer::{
    util::{expr_any, previsit_expression},
    Expression, Identity, IdentityKind, PolynomialReference,
};

use super::FixedData;

pub trait WitnessColumnNamer {
    fn name(&self, i: usize) -> String;
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

pub fn all_witness_refs<'a>(
    expr: &'a Expression,
    fixed_data: &FixedData,
) -> HashSet<&'a PolynomialReference> {
    let mut references: HashSet<&'a PolynomialReference> = Default::default();
    previsit_expression(expr, &mut |e| {
        if let Expression::PolynomialReference(poly) = e {
            if fixed_data.witness_ids.contains_key(poly.name.as_str()) {
                references.insert(poly);
            }
        };
        ControlFlow::Continue::<()>(())
    });
    references
}

/// Checks if the identity is a polynomial identity with exactly one "next" reference.
pub fn is_propagating_identity(identity: &Identity, fixed_data: &FixedData) -> bool {
    if identity.kind != IdentityKind::Polynomial {
        return false;
    }
    let refs = all_witness_refs(identity.left.selector.as_ref().unwrap(), fixed_data);
    refs.iter().filter(|r| r.next).count() == 1
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

pub fn is_simple_poly_ref(expr: &Expression) -> Option<PolyID> {
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
