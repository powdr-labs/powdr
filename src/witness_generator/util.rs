use crate::analyzer::{Expression, PolynomialReference};

use super::FixedData;

pub trait WitnessColumnNamer {
    fn name(&self, i: usize) -> String;
}

/// @returns true if the expression contains a reference to a next value of a
/// (witness or fixed) column
pub fn contains_next_ref(expr: &Expression) -> bool {
    expr_any(expr, &mut |e| match e {
        Expression::PolynomialReference(poly) => poly.next,
        _ => false,
    })
}

/// @returns true if the expression contains a reference to a next value of a witness column.
pub fn contains_next_witness_ref(expr: &Expression, fixed_data: &FixedData) -> bool {
    expr_any(expr, &mut |e| match e {
        Expression::PolynomialReference(poly) => {
            poly.next && fixed_data.witness_ids.contains_key(poly.name.as_str())
        }
        _ => false,
    })
}

/// @returns true if the expression contains a reference to a witness column.
pub fn contains_witness_ref(expr: &Expression, fixed_data: &FixedData) -> bool {
    expr_any(expr, &mut |e| match e {
        Expression::PolynomialReference(poly) => {
            fixed_data.witness_ids.contains_key(poly.name.as_str())
        }
        _ => false,
    })
}

pub fn expr_any(expr: &Expression, f: &mut impl FnMut(&Expression) -> bool) -> bool {
    if f(expr) {
        true
    } else {
        match expr {
            Expression::Tuple(items) => items.iter().any(|e| expr_any(e, f)),
            Expression::BinaryOperation(l, _, r) => expr_any(l, f) || expr_any(r, f),
            Expression::UnaryOperation(_, e) => expr_any(e, f),
            Expression::FunctionCall(_, args) => args.iter().any(|e| expr_any(e, f)),
            Expression::Constant(_)
            | Expression::PolynomialReference(_)
            | Expression::LocalVariableReference(_)
            | Expression::PublicReference(_)
            | Expression::Number(_)
            | Expression::String(_) => false,
        }
    }
}

/// Returns the name of the polynomial if the expression is just a polynomial
/// reference (without next).
pub fn is_simple_poly(expr: &Expression) -> Option<&str> {
    if let Expression::PolynomialReference(PolynomialReference {
        name,
        index: None,
        next: false,
    }) = expr
    {
        Some(name)
    } else {
        None
    }
}
