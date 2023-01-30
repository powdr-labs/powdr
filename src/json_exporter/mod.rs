use std::collections::BTreeMap;

use json::{object, JsonValue};

use crate::{
    analyzer::{Analyzed, PolynomialType},
    parser::ast::Expression,
};

pub fn export(analyzed: &Analyzed) -> JsonValue {
    let references = analyzed
        .declarations
        .iter()
        .map(|(name, poly)| {
            let mut out = object! {
                type: polynomial_type_to_json_string(poly.poly_type),
                id: poly.id,
                polDeg: poly.degree as i64,
                isArray: poly.is_array(),
            };
            if poly.is_array() {
                out["len"] = JsonValue::from(poly.length.unwrap() as i64);
            }
            (name.clone(), out)
        })
        .collect::<BTreeMap<String, JsonValue>>();
    let mut expressions: Vec<JsonValue> = Vec::new();
    let pol_identities = analyzed
        .polynomial_identities
        .iter()
        .map(|expr| {
            object! {
                e: extract_expression(expr, &mut expressions)
            }
        })
        .collect::<Vec<JsonValue>>();
    object! {
        nCommitments: analyzed.commitment_count(),
        nQ: 0, // number of expressions with degree == 2
        nIm: analyzed.intermediate_count(),
        nConstants: analyzed.constant_count(),
        publics: [],
        references: references,
        expressions: [],
        polIdentities: pol_identities,
        plookupIdentities: [],
        permutationIdentities: [],
        connectionIdentities: []
    }
}

fn polynomial_type_to_json_string(t: PolynomialType) -> &'static str {
    match t {
        PolynomialType::Committed => "cmP",
        PolynomialType::Constant => "constP",
        PolynomialType::Intermediate => "imP",
    }
}

fn extract_expression(expr: &Expression, expressions: &mut Vec<JsonValue>) -> usize {
    let id = expressions.len();
    expressions.push(expression_to_json(expr));
    id
}

fn expression_to_json(expr: &Expression) -> JsonValue {
    // TODO apply simplify_expression if possible - do that already during analysis.
    match expr {
        Expression::Constant(_) => todo!(),
        Expression::PolynomialReference(poly) => {
            object! {
                op: "const",
                deg: 1,
                id: 3,
                next: poly.next
            }
        }
        Expression::Number(_) => todo!(),
        Expression::BinaryOperation(_, _, _) => todo!(),
        Expression::UnaryOperation(_, _) => todo!(),
    }
}
