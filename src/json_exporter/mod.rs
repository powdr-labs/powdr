use std::cmp;
use std::collections::{BTreeMap, HashMap};

use json::{object, JsonValue};

use crate::analyzer::{
    Analyzed, BinaryOperator, Expression, FunctionValueDefinition, IdentityKind,
    PolynomialReference, PolynomialType, StatementIdentifier, UnaryOperator,
};

use self::expression_counter::compute_intermediate_expression_ids;

mod expression_counter;

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
    let mut publics = Vec::new();
    let mut pol_identities = Vec::new();
    let mut plookup_identities = Vec::new();
    let mut permutation_identities = Vec::new();
    let mut connection_identities = Vec::new();
    for item in &analyzed.source_order {
        match item {
            StatementIdentifier::Definition(name) => {
                if let (poly, Some(value)) = &analyzed.definitions[name] {
                    if poly.poly_type == PolynomialType::Intermediate {
                        if let FunctionValueDefinition::Mapping(value) = value {
                            let expression_id = exporter.extract_expression(value, 1);
                            assert_eq!(
                                expression_id,
                                exporter.intermediate_poly_expression_ids[&poly.id] as usize
                            );
                        } else {
                            panic!("Expected single value");
                        }
                    }
                }
            }
            StatementIdentifier::PublicDeclaration(name) => {
                let pub_def = &analyzed.public_declarations[name];
                let (_, json, _) = exporter.polynomial_reference_to_json(&pub_def.polynomial);
                let id = publics.len();
                publics.push(object! {
                    name: name.clone(),
                    polId: json["id"].clone(), // This includes the array offset
                    polType: polynomial_reference_type_to_type(json["op"].as_str().unwrap()),
                    idx: pub_def.index,
                    id: id
                });
            }
            StatementIdentifier::Identity(id) => {
                let identity = &analyzed.identities[*id];
                let file_name = identity.source.file.clone();
                let line = identity.source.line;
                let selector_degree = if identity.kind == IdentityKind::Polynomial {
                    2
                } else {
                    1
                };
                let left = exporter.extract_expression_vec(&identity.left.expressions, 1);
                let sel_left =
                    exporter.extract_expression_opt(&identity.left.selector, selector_degree);
                let right = exporter.extract_expression_vec(&identity.right.expressions, 1);
                let sel_right = exporter.extract_expression_opt(&identity.right.selector, 1);
                match identity.kind {
                    IdentityKind::Polynomial => pol_identities.push(object! {
                        e: sel_left.unwrap(),
                        fileName: file_name,
                        line: line
                    }),
                    IdentityKind::Plookup => {
                        plookup_identities.push(object! {
                            selF: sel_left,
                            f: left,
                            selT: sel_right,
                            t: right,
                            fileName: file_name,
                            line: line
                        });
                    }
                    IdentityKind::Permutation => {
                        permutation_identities.push(object! {
                            selF: sel_left,
                            f: left,
                            selT: sel_right,
                            t: right,
                            fileName: file_name,
                            line: line
                        });
                    }
                    IdentityKind::Connect => {
                        connection_identities.push(object! {
                            pols: left,
                            connections: right,
                            fileName: file_name,
                            line: line
                        });
                    }
                }
            }
        }
    }
    object! {
        nCommitments: analyzed.commitment_count(),
        nQ: exporter.number_q,
        nIm: analyzed.intermediate_count(),
        nConstants: analyzed.constant_count(),
        publics: publics,
        references: exporter.references(),
        expressions: exporter.expressions,
        polIdentities: pol_identities,
        plookupIdentities: plookup_identities,
        permutationIdentities: permutation_identities,
        connectionIdentities: connection_identities,
    }
}

fn polynomial_type_to_json_string(t: PolynomialType) -> &'static str {
    polynomial_reference_type_to_type(polynomial_reference_type_to_json_string(t))
}

fn polynomial_reference_type_to_json_string(t: PolynomialType) -> &'static str {
    match t {
        PolynomialType::Committed => "cm",
        PolynomialType::Constant => "const",
        PolynomialType::Intermediate => "exp",
    }
}

fn polynomial_reference_type_to_type(t: &str) -> &'static str {
    match t {
        "cm" => "cmP",
        "const" => "constP",
        "exp" => "imP",
        _ => panic!("Invalid polynomial reference type {t}"),
    }
}

impl<'a> Exporter<'a> {
    fn new(analyzed: &'a Analyzed) -> Self {
        Self {
            analyzed,
            expressions: vec![],
            intermediate_poly_expression_ids: compute_intermediate_expression_ids(analyzed),
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

    /// returns the degree, the JSON value and the dependencies (intermediate polynomial IDs)
    fn expression_to_json(&self, expr: &Expression) -> (u32, JsonValue, Vec<u64>) {
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
            Expression::PolynomialReference(reference) => {
                self.polynomial_reference_to_json(reference)
            }
            Expression::LocalVariableReference(_) => {
                panic!("No local variable references allowed here.")
            }
            Expression::PublicReference(name) => (
                0,
                object! {
                    op: "public",
                    deg: 0,
                    id: self.analyzed.public_declarations[name].id,
                },
                Vec::new(),
            ),
            Expression::Number(value) => (
                0,
                object! {
                    op: "number",
                    deg: 0,
                    value: format!("{value}"),
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
                    BinaryOperator::Mod
                    | BinaryOperator::BinaryAnd
                    | BinaryOperator::BinaryOr
                    | BinaryOperator::BinaryXor
                    | BinaryOperator::ShiftLeft
                    | BinaryOperator::ShiftRight => {
                        panic!("Operator {op:?} not supported on polynomials.")
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
            Expression::FunctionCall(_, _) => {
                panic!("No function calls allowed here.")
            }
            Expression::String(_) => panic!("Strings not allowed here."),
            Expression::Tuple(_) => panic!("Tuples not allowed here"),
        }
    }

    fn polynomial_reference_to_json(
        &self,
        PolynomialReference { name, index, next }: &PolynomialReference,
    ) -> (u32, JsonValue, Vec<u64>) {
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
}

#[cfg(test)]
mod test {
    use std::fs;
    use std::path::Path;
    use std::process::Command;

    use crate::analyzer;

    use super::*;

    fn generate_json_pair(file: &str) -> (JsonValue, JsonValue) {
        let temp_dir = mktemp::Temp::new_dir().unwrap();
        let output_file = temp_dir.join("out.json");

        let analyzed = analyzer::analyze(Path::new(file));
        let json_out = export(&analyzed);

        let pilcom = std::env::var("PILCOM").expect(
            "Please set the PILCOM environment variable to the path to the pilcom repository.",
        );
        let pilcom_output = Command::new("node")
            .args([
                format!("{pilcom}/src/pil.js"),
                file.to_string(),
                "-o".to_string(),
                format!("{}", output_file.to_string_lossy()),
            ])
            .output()
            .expect("failed to run pilcom");
        if !pilcom_output.status.success() {
            panic!(
                "Pilcom run was unsuccessful.\nStdout: {}\nStderr: {}\n",
                String::from_utf8_lossy(&pilcom_output.stdout),
                String::from_utf8_lossy(&pilcom_output.stderr)
            );
        }

        let pilcom_out = fs::read_to_string(&output_file).unwrap_or_else(|_| {
            panic!("Pilcom did not generate {output_file:?} at the expected location.")
        });
        drop(temp_dir);
        let pilcom_parsed = json::parse(&pilcom_out).expect("Invalid json from pilcom.");
        (json_out, pilcom_parsed)
    }

    fn compare_export_file(file: &str) {
        let (json_out, pilcom_parsed) = generate_json_pair(file);
        assert_eq!(json_out, pilcom_parsed);
    }

    /// Normalizes the json in that it replaces all idQ values by "99"
    /// and converts hex numbers to decimal.
    fn normalize_idq_and_hex(v: &mut JsonValue) {
        match v {
            JsonValue::Object(obj) => obj.iter_mut().for_each(|(key, value)| {
                if key == "idQ" {
                    *value = 99.into();
                } else if key == "value" {
                    match value.as_str() {
                        Some(v) if v.starts_with("0x") => {
                            *value =
                                format!("{}", i64::from_str_radix(&v[2..], 16).unwrap()).into();
                        }
                        _ => {}
                    }
                } else {
                    normalize_idq_and_hex(value)
                }
            }),
            JsonValue::Array(arr) => arr.iter_mut().for_each(normalize_idq_and_hex),
            _ => {}
        }
    }

    fn compare_export_file_ignore_idq_hex(file: &str) {
        let (mut json_out, mut pilcom_parsed) = generate_json_pair(file);
        normalize_idq_and_hex(&mut json_out);
        normalize_idq_and_hex(&mut pilcom_parsed);
        assert_eq!(json_out, pilcom_parsed);
    }

    #[test]
    fn export_config() {
        compare_export_file("tests/polygon-hermez/config.pil");
    }

    #[test]
    fn export_binary() {
        compare_export_file("tests/polygon-hermez/binary.pil");
    }

    #[test]
    fn export_byte4() {
        compare_export_file("tests/polygon-hermez/byte4.pil");
    }

    #[test]
    fn export_global() {
        compare_export_file("tests/polygon-hermez/global.pil");
    }

    #[test]
    fn export_arith() {
        // We ignore the specific value assigned to idQ.
        // It is just a counter and pilcom assigns it in a weird order.
        compare_export_file_ignore_idq_hex("tests/polygon-hermez/arith.pil");
    }

    #[test]
    fn export_mem() {
        compare_export_file_ignore_idq_hex("tests/polygon-hermez/mem.pil");
        compare_export_file_ignore_idq_hex("tests/polygon-hermez/mem_align.pil");
    }

    #[test]
    fn export_keccakf() {
        compare_export_file_ignore_idq_hex("tests/polygon-hermez/keccakf.pil");
    }

    #[test]
    fn export_padding() {
        compare_export_file("tests/polygon-hermez/nine2one.pil");
        compare_export_file_ignore_idq_hex("tests/polygon-hermez/padding_kkbit.pil");
        compare_export_file_ignore_idq_hex("tests/polygon-hermez/padding_kk.pil");
        compare_export_file_ignore_idq_hex("tests/polygon-hermez/padding_kk.pil");
    }

    #[test]
    fn export_poseidong() {
        compare_export_file_ignore_idq_hex("tests/polygon-hermez/padding_pg.pil");
        compare_export_file_ignore_idq_hex("tests/polygon-hermez/poseidong.pil");
        compare_export_file_ignore_idq_hex("tests/polygon-hermez/storage.pil");
    }

    #[test]
    fn export_main() {
        compare_export_file_ignore_idq_hex("tests/polygon-hermez/rom.pil");
        compare_export_file_ignore_idq_hex("tests/polygon-hermez/main.pil");
    }
}
