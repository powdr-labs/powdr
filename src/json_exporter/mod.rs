use std::{
    cmp,
    collections::{BTreeMap, HashMap},
};

use json::{object, JsonValue};

use crate::analyzer::{
    Analyzed, BinaryOperator, Expression, PolynomialReference, PolynomialType, StatementIdentifier,
    UnaryOperator,
};

struct Exporter<'a> {
    analyzed: &'a Analyzed,
    expressions: Vec<JsonValue>,
    /// Translates from polynomial IDs to expression IDs for intermediate
    /// polynomials.
    intermediate_poly_expression_ids: HashMap<u64, u64>,
}

pub fn export(analyzed: &Analyzed) -> JsonValue {
    let mut exporter = Exporter::new(analyzed);
    let mut pol_identities = Vec::new();
    let mut plookup_identities = Vec::new();
    for item in &analyzed.source_order {
        match item {
            StatementIdentifier::Definition(name) => {
                if let (poly, Some(value)) = &analyzed.definitions[name] {
                    let (_, expr, _) = exporter.expression_to_json(value);
                    exporter.expressions.push(expr);
                    assert_eq!(poly.poly_type, PolynomialType::Intermediate);
                    exporter
                        .intermediate_poly_expression_ids
                        .insert(poly.id, (exporter.expressions.len() - 1) as u64);
                }
            }
            StatementIdentifier::Identity(id) => {
                let expr = &analyzed.polynomial_identities[*id];
                pol_identities.push(object! {
                    e: exporter.extract_expression(expr)
                })
            }
            StatementIdentifier::Plookup(id) => {
                let identity = &analyzed.plookups[*id];
                let sel_f = exporter.extract_expression_opt(&identity.key.selector);
                let f = exporter.extract_expression_vec(&identity.key.expressions);
                let sel_t = exporter.extract_expression_opt(&identity.haystack.selector);
                let t = exporter.extract_expression_vec(&identity.haystack.expressions);
                plookup_identities.push(object! {
                    selF: sel_f,
                    f: f,
                    selT: sel_t,
                    t: t,
                });
            }
        }
    }
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
        PolynomialType::Intermediate => "exp",
    }
}

impl<'a> Exporter<'a> {
    fn new(analyzed: &'a Analyzed) -> Self {
        Self {
            analyzed,
            expressions: vec![],
            intermediate_poly_expression_ids: HashMap::new(),
        }
    }

    fn references(&self) -> JsonValue {
        self.analyzed
            .definitions
            .iter()
            .map(|(name, (poly, _value))| {
                let id = if poly.poly_type == PolynomialType::Intermediate {
                    self.intermediate_poly_expression_ids[&poly.id]
                } else {
                    poly.id
                };
                let mut out = object! {
                    type: polynomial_type_to_json_string(poly.poly_type),
                    id: id,
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
        let (_, mut json, dependencies) = self.expression_to_json(expr);
        if !dependencies.is_empty() {
            json["deps"] = dependencies.into();
        }
        self.expressions.push(json);
        id
    }

    fn extract_expression_opt(&mut self, expr: &Option<Expression>) -> Option<usize> {
        expr.as_ref().map(|e| self.extract_expression(e))
    }

    fn extract_expression_vec(&mut self, expr: &[Expression]) -> Vec<usize> {
        expr.iter().map(|e| self.extract_expression(e)).collect()
    }

    /// returns the degree, the json value and the dependencies (intermediate polynomial IDs)
    fn expression_to_json(&mut self, expr: &Expression) -> (u32, JsonValue, Vec<u64>) {
        match expr {
            Expression::Constant(name) => (
                1,
                object! {
                    // TODO I think "const" is for constant poly, not a constant value.
                    op: "const",
                    deg: 1,
                    // TODO is it declarations or constants?
                    id: self.analyzed.definitions[name].0.id,
                    next: false
                },
                Vec::new(),
            ),
            Expression::PolynomialReference(PolynomialReference { name, index, next }) => {
                let poly = &self.analyzed.definitions[name].0;
                let id = if poly.poly_type == PolynomialType::Intermediate {
                    assert!(index.is_none());
                    self.intermediate_poly_expression_ids[&poly.id]
                } else {
                    poly.id + index.unwrap_or_default()
                };
                let poly_json = object! {
                    id: id,
                    op: polynomial_reference_type_to_json_string(poly.poly_type),
                    deg: 1,
                    next: *next,
                };
                let dependencies = if poly.poly_type == PolynomialType::Intermediate {
                    vec![id]
                } else {
                    Vec::new()
                };
                (1, poly_json, dependencies)
            }
            Expression::Number(value) => (
                0,
                object! {
                    op: "number",
                    deg: 0,
                    value: format!("{}", *value as i64),
                },
                Vec::new(),
            ),
            Expression::BinaryOperation(left, op, right) => {
                let (deg_left, left, deps_left) = self.expression_to_json(left);
                let (deg_right, right, deps_right) = self.expression_to_json(right);
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
                    [deps_left, deps_right].concat(),
                )
            }
            Expression::UnaryOperation(op, value) => {
                let (deg, value, deps) = self.expression_to_json(value);
                match op {
                    UnaryOperator::Plus => (deg, value, deps),
                    UnaryOperator::Minus => (
                        deg,
                        object! {
                            op: "neg",
                            deg: deg,
                            values: [value],
                        },
                        deps,
                    ),
                }
            }
        }
    }
}
