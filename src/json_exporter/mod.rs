use std::cmp;
use std::collections::{BTreeMap, HashMap};

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
    number_q: u64,
}

pub fn export(analyzed: &Analyzed) -> JsonValue {
    let mut exporter = Exporter::new(analyzed);
    let mut pol_identities = Vec::new();
    let mut plookup_identities = Vec::new();
    for item in &analyzed.source_order {
        match item {
            StatementIdentifier::Definition(name) => {
                if let (poly, Some(value)) = &analyzed.definitions[name] {
                    let expression_id = exporter.extract_expression(value, 1);
                    assert_eq!(poly.poly_type, PolynomialType::Intermediate);
                    exporter
                        .intermediate_poly_expression_ids
                        .insert(poly.id, expression_id as u64);
                }
            }
            StatementIdentifier::Identity(id) => {
                let (expr, source_ref) = &analyzed.polynomial_identities[*id];
                pol_identities.push(object! {
                    e: exporter.extract_expression(expr, 2),
                    fileName: source_ref.file.clone(),
                    line: source_ref.line,
                })
            }
            StatementIdentifier::Plookup(id) => {
                let plookup = &analyzed.plookups[*id];
                let f = exporter.extract_expression_vec(&plookup.key.expressions, 1);
                let sel_f = exporter.extract_expression_opt(&plookup.key.selector, 1);
                let t = exporter.extract_expression_vec(&plookup.haystack.expressions, 1);
                let sel_t = exporter.extract_expression_opt(&plookup.haystack.selector, 1);
                plookup_identities.push(object! {
                    selF: sel_f,
                    f: f,
                    selT: sel_t,
                    t: t,
                    fileName: plookup.source.file.clone(),
                    line: plookup.source.line,
                });
            }
        }
    }
    object! {
        nCommitments: analyzed.commitment_count(),
        nQ: exporter.number_q,
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
            number_q: 0,
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

    /// Processes the given expression
    /// @returns the expression ID
    fn extract_expression(&mut self, expr: &Expression, max_degree: u32) -> usize {
        let id = self.expressions.len();
        let (degree, mut json, dependencies) = self.expression_to_json(expr);
        if degree > max_degree {
            json["idQ"] = self.number_q.into();
            json["deg"] = 1.into();
            self.number_q += 1;
        }
        if !dependencies.is_empty() && json["op"] != "exp" {
            json["deps"] = dependencies.into();
        }
        self.expressions.push(json);
        id
    }

    fn extract_expression_opt(
        &mut self,
        expr: &Option<Expression>,
        max_degree: u32,
    ) -> Option<usize> {
        expr.as_ref()
            .map(|e| self.extract_expression(e, max_degree))
    }

    fn extract_expression_vec(&mut self, expr: &[Expression], max_degree: u32) -> Vec<usize> {
        expr.iter()
            .map(|e| self.extract_expression(e, max_degree))
            .collect()
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
                    BinaryOperator::Mul => ("mul", deg_left + deg_right),
                    BinaryOperator::Div => panic!("Div is not really allowed"),
                    BinaryOperator::Pow => {
                        assert_eq!(
                            deg_left + deg_right,
                            0,
                            "Exponentiation can only be used on constants."
                        );
                        ("pow", deg_left + deg_right)
                    }
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

#[cfg(test)]
mod test {
    use std::fs;
    use std::path::Path;
    use std::process::Command;

    use crate::analyzer;

    use super::*;

    fn generate_json_pair(file: &str) -> (JsonValue, JsonValue) {
        let analyzed = analyzer::analyze(Path::new(file));
        let json_out = export(&analyzed);

        let pilcom = std::env::var("PILCOM")
            .expect("Please set PILCOM to the path to the pilcom js file 'src/pil.js'.");
        let pilcom_output = Command::new("node")
            .args([pilcom, file.to_string()])
            .output()
            .expect("failed to run pilcom");
        if !pilcom_output.status.success() {
            panic!(
                "Pilcom run was unsuccessful.\nStdout: {}\nStderr: {}\n",
                String::from_utf8_lossy(&pilcom_output.stdout),
                String::from_utf8_lossy(&pilcom_output.stderr)
            );
        }

        let output_file = format!(
            "{}.json",
            Path::new(file)
                .canonicalize()
                .unwrap()
                .file_name()
                .unwrap()
                .to_str()
                .unwrap()
        );
        let pilcom_out = fs::read_to_string(&output_file).unwrap_or_else(|_| {
            panic!("Pilcom did not generate {output_file} at the expected location.")
        });
        let pilcom_parsed = json::parse(&pilcom_out).expect("Invalid json from pilcom.");
        (json_out, pilcom_parsed)
    }

    fn compare_export_file(file: &str) {
        let (json_out, pilcom_parsed) = generate_json_pair(file);
        assert_eq!(json_out, pilcom_parsed);
    }

    fn set_idq_to_99(v: &mut JsonValue) {
        match v {
            JsonValue::Object(obj) => obj.iter_mut().for_each(|(key, value)| {
                if key == "idQ" {
                    *value = 99.into();
                } else {
                    set_idq_to_99(value)
                }
            }),
            JsonValue::Array(arr) => arr.iter_mut().for_each(set_idq_to_99),
            _ => {}
        }
    }

    fn compare_export_file_ignore_idq(file: &str) {
        let (mut json_out, mut pilcom_parsed) = generate_json_pair(file);
        set_idq_to_99(&mut json_out);
        set_idq_to_99(&mut pilcom_parsed);
        assert_eq!(json_out, pilcom_parsed);
    }

    #[test]
    fn export_config() {
        compare_export_file("test_files/config.pil");
    }

    #[test]
    fn export_binary() {
        compare_export_file("test_files/binary.pil");
    }

    #[test]
    fn export_byte4() {
        compare_export_file("test_files/byte4.pil");
    }

    #[test]
    fn export_global() {
        compare_export_file("test_files/global.pil");
    }

    #[test]
    fn export_arith() {
        // We ignore the specific value assigned to idQ.
        // It is just a counter and pilcom assigns it in a weird order.
        compare_export_file_ignore_idq("test_files/arith.pil");
    }

    #[test]
    fn export_mem() {
        compare_export_file_ignore_idq("test_files/mem.pil");
    }
}
