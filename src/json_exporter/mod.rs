use std::{cmp, collections::BTreeMap};

use json::{object, JsonValue};

use crate::analyzer::{Analyzed, BinaryOperator, Expression, PolynomialReference, PolynomialType};

struct Exporter<'a> {
    analyzed: &'a Analyzed,
    expressions: Vec<JsonValue>,
}

pub fn export(analyzed: &Analyzed) -> JsonValue {
    let mut exporter = Exporter::new(analyzed);
    let pol_identities = analyzed
        .polynomial_identities
        .iter()
        .map(|expr| {
            object! {
                e: exporter.extract_expression(expr)
            }
        })
        .collect::<Vec<JsonValue>>();

    let plookup_identities = analyzed
        .plookup_identities
        .iter()
        .map(|identity| {
            let sel_f = exporter.extract_expression_opt(&identity.key.selector);
            let f = exporter.extract_expression_vec(&identity.key.expressions);
            let sel_t = exporter.extract_expression_opt(&identity.haystack.selector);
            let t = exporter.extract_expression_vec(&identity.haystack.expressions);
            object! {
                selF: sel_f,
                f: f,
                selT: sel_t,
                t: t,
            }
        })
        .collect::<Vec<JsonValue>>();
    object! {
        nCommitments: analyzed.commitment_count(),
        nQ: 0, // number of expressions with degree == 2
        nIm: analyzed.intermediate_count(),
        nConstants: analyzed.constant_count(),
        publics: [], // @TODO
        references: exporter.references(),
        expressions: exporter.expressions,
        polIdentities: pol_identities,
        plookupIdentities: plookup_identities,
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

fn polynomial_reference_type_to_json_string(t: PolynomialType) -> &'static str {
    match t {
        PolynomialType::Committed => "cm",
        PolynomialType::Constant => "const",
        PolynomialType::Intermediate => todo!(),
    }
}

impl<'a> Exporter<'a> {
    fn new(analyzed: &'a Analyzed) -> Self {
        Self {
            analyzed,
            expressions: vec![],
        }
    }

    fn references(&self) -> JsonValue {
        self.analyzed
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
            .collect::<BTreeMap<String, JsonValue>>()
            .into()
    }

    fn extract_expression(&mut self, expr: &Expression) -> usize {
        let id = self.expressions.len();
        let json = self.expression_to_json(expr).1;
        self.expressions.push(json);
        id
    }

    fn extract_expression_opt(&mut self, expr: &Option<Expression>) -> Option<usize> {
        expr.as_ref().map(|e| self.extract_expression(e))
    }

    fn extract_expression_vec(&mut self, expr: &[Expression]) -> Vec<usize> {
        expr.iter().map(|e| self.extract_expression(e)).collect()
    }

    fn expression_to_json(&mut self, expr: &Expression) -> (u32, JsonValue) {
        match expr {
            Expression::Constant(name) => (
                1,
                object! {
                    // TODO I think "const" is for constant poly, not a constant value.
                    op: "const",
                    deg: 1,
                    // TODO is it declarations or constants?
                    id: self.analyzed.declarations[name].id,
                    next: false
                },
            ),
            Expression::PolynomialReference(PolynomialReference { name, index, next }) => {
                let poly = &self.analyzed.declarations[name];
                let mut poly_json = object! {
                    id: poly.id,
                    op: polynomial_reference_type_to_json_string(poly.poly_type),
                    deg: 1,
                    next: *next,
                };
                if let Some(index) = *index {
                    // TODO correct?
                    poly_json["index"] = index.into();
                }
                (1, poly_json)
            }
            Expression::Number(value) => (
                0,
                object! {
                    op: "number",
                    deg: 0,
                    value: *value as i64,
                },
            ),
            Expression::BinaryOperation(left, op, right) => {
                let (deg_left, left) = self.expression_to_json(left);
                let (deg_right, right) = self.expression_to_json(right);
                let (op, degree) = match op {
                    BinaryOperator::Add => ("add", cmp::max(deg_left, deg_right)),
                    BinaryOperator::Sub => ("sub", cmp::max(deg_left, deg_right)),
                    BinaryOperator::Mul => ("mul", deg_left + deg_right), // TODO correct?
                    BinaryOperator::Div => ("div", deg_left + deg_right), // TODO correct?
                    BinaryOperator::Pow => ("pow", deg_left + deg_right), // TODO correct?
                };
                (
                    degree,
                    object! {
                        op: op,
                        deg: degree,
                        values: [left, right],
                    },
                )
            }
            Expression::UnaryOperation(_, _) => todo!(),
        }
    }
}
