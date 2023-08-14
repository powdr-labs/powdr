use number::FieldElement;
use std::cmp;
use std::collections::HashMap;

use ast::analyzed::{
    Analyzed, BinaryOperator, Expression, FunctionValueDefinition, IdentityKind, PolyID,
    PolynomialReference, PolynomialType, StatementIdentifier, UnaryOperator,
};
use starky::types::{
    ConnectionIdentity, Expression as StarkyExpr, PermutationIdentity, PlookupIdentity,
    PolIdentity, Reference, PIL,
};

use self::expression_counter::compute_intermediate_expression_ids;

mod expression_counter;

const DEFAULT_EXPR: StarkyExpr = StarkyExpr {
    op: String::new(),
    deg: 0,
    id: None,
    next: None,
    value: None,
    values: None,
    keep: None,
    keep2ns: None,
    idQ: None,
    const_: None,
};
struct Exporter<'a, T> {
    analyzed: &'a Analyzed<T>,
    expressions: Vec<StarkyExpr>,
    /// Translates from polynomial IDs to expression IDs for intermediate
    /// polynomials.
    intermediate_poly_expression_ids: HashMap<u64, u64>,
    number_q: u64,
}

pub fn export<T: FieldElement>(analyzed: &Analyzed<T>) -> PIL {
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
                let (_, expr, _) = exporter.polynomial_reference_to_json(&pub_def.polynomial);
                let id = publics.len();
                publics.push(starky::types::Public {
                    polType: polynomial_reference_type_to_type(&expr.op).to_string(),
                    polId: expr.id.unwrap(),
                    idx: pub_def.index as usize,
                    id,
                    name: name.clone(),
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
                    IdentityKind::Polynomial => pol_identities.push(PolIdentity {
                        e: sel_left.unwrap(),
                        fileName: file_name,
                        line,
                    }),
                    IdentityKind::Plookup => {
                        plookup_identities.push(PlookupIdentity {
                            selF: sel_left,
                            f: Some(left),
                            selT: sel_right,
                            t: Some(right),
                            fileName: file_name,
                            line: line,
                        });
                    }
                    IdentityKind::Permutation => {
                        permutation_identities.push(PermutationIdentity {
                            selF: sel_left,
                            f: Some(left),
                            selT: sel_right,
                            t: Some(right),
                            fileName: file_name,
                            line,
                        });
                    }
                    IdentityKind::Connect => {
                        connection_identities.push(ConnectionIdentity {
                            pols: Some(left),
                            connections: Some(right),
                            fileName: file_name,
                            line: line,
                        });
                    }
                }
            }
        }
    }
    PIL {
        nCommitments: analyzed.commitment_count(),
        nQ: exporter.number_q as usize,
        nIm: analyzed.intermediate_count(),
        nConstants: analyzed.constant_count(),
        publics: publics,
        references: exporter.references(),
        expressions: exporter.expressions,
        polIdentities: pol_identities,
        plookupIdentities: plookup_identities,
        permutationIdentities: (!permutation_identities.is_empty())
            .then_some(permutation_identities),
        connectionIdentities: (!connection_identities.is_empty()).then_some(connection_identities),
        cm_dims: Vec::new(),
        q2exp: Vec::new(),
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

impl<'a, T: FieldElement> Exporter<'a, T> {
    fn new(analyzed: &'a Analyzed<T>) -> Self {
        Self {
            analyzed,
            expressions: vec![],
            intermediate_poly_expression_ids: compute_intermediate_expression_ids(analyzed),
            number_q: 0,
        }
    }

    fn references(&self) -> HashMap<String, Reference> {
        self.analyzed
            .definitions
            .iter()
            .map(|(name, (poly, _value))| {
                let id = if poly.poly_type == PolynomialType::Intermediate {
                    self.intermediate_poly_expression_ids[&poly.id]
                } else {
                    poly.id
                };
                let out = Reference {
                    polType: None,
                    type_: polynomial_type_to_json_string(poly.poly_type).to_string(),
                    id: id as usize,
                    polDeg: poly.degree as usize,
                    isArray: poly.is_array(),
                    elementType: None,
                    len: poly.length.map(|l| l as usize),
                };
                (name.clone(), out)
            })
            .collect::<HashMap<String, Reference>>()
    }

    /// Processes the given expression
    /// @returns the expression ID
    fn extract_expression(&mut self, expr: &Expression<T>, max_degree: u32) -> usize {
        let id = self.expressions.len();
        let (degree, mut expr, dependencies) = self.expression_to_json(expr);
        if degree > max_degree {
            expr.idQ = Some(self.number_q as usize);
            expr.deg = 1;
            self.number_q += 1;
        }
        // TODO: decide what to do with dependencies
        /*if !dependencies.is_empty() && expr.op != "exp" {
            expr["deps"] = dependencies.into();
        }*/
        self.expressions.push(expr);
        id
    }

    fn extract_expression_opt(
        &mut self,
        expr: &Option<Expression<T>>,
        max_degree: u32,
    ) -> Option<usize> {
        expr.as_ref()
            .map(|e| self.extract_expression(e, max_degree))
    }

    fn extract_expression_vec(&mut self, expr: &[Expression<T>], max_degree: u32) -> Vec<usize> {
        expr.iter()
            .map(|e| self.extract_expression(e, max_degree))
            .collect()
    }

    /// returns the degree, the JSON value and the dependencies (intermediate polynomial IDs)
    fn expression_to_json(&self, expr: &Expression<T>) -> (u32, StarkyExpr, Vec<u64>) {
        match expr {
            Expression::Constant(name) => (
                0,
                StarkyExpr {
                    op: "number".to_string(),
                    deg: 0,
                    value: Some(format!("{}", self.analyzed.constants[name])),
                    ..DEFAULT_EXPR
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
                StarkyExpr {
                    op: "public".to_string(),
                    deg: 0,
                    id: Some(self.analyzed.public_declarations[name].id as usize),
                    ..DEFAULT_EXPR
                },
                Vec::new(),
            ),
            Expression::Number(value) => (
                0,
                StarkyExpr {
                    op: "number".to_string(),
                    deg: 0,
                    value: Some(format!("{value}")),
                    ..DEFAULT_EXPR
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
                    StarkyExpr {
                        op: op.to_string(),
                        deg: degree as usize,
                        values: Some(vec![left, right]),
                        ..DEFAULT_EXPR
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
                        StarkyExpr {
                            op: "neg".to_string(),
                            deg: deg as usize,
                            values: Some(vec![value]),
                            ..DEFAULT_EXPR
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
            Expression::MatchExpression(_, _) => {
                panic!("No match expressions allowed here.")
            }
        }
    }

    fn polynomial_reference_to_json(
        &self,
        PolynomialReference {
            name: _,
            index,
            poly_id,
            next,
        }: &PolynomialReference,
    ) -> (u32, StarkyExpr, Vec<u64>) {
        let PolyID { id, ptype } = poly_id.unwrap();
        let id = if ptype == PolynomialType::Intermediate {
            assert!(index.is_none());
            self.intermediate_poly_expression_ids[&id]
        } else {
            id + index.unwrap_or_default()
        };
        let poly = StarkyExpr {
            id: Some(id as usize),
            op: polynomial_reference_type_to_json_string(ptype).to_string(),
            deg: 1,
            next: Some(*next),
            ..DEFAULT_EXPR
        };
        let dependencies = if ptype == PolynomialType::Intermediate {
            vec![id]
        } else {
            Vec::new()
        };
        (1, poly, dependencies)
    }
}

#[cfg(test)]
mod test {
    use pil_analyzer::analyze;
    use serde_json::Value as JsonValue;
    use std::fs;
    use std::process::Command;
    use test_log::test;

    use number::GoldilocksField;

    use super::*;

    fn generate_json_pair(file: &str) -> (JsonValue, JsonValue) {
        let temp_dir = mktemp::Temp::new_dir().unwrap();
        let output_file = temp_dir.join("out.json");

        let file = std::path::PathBuf::from("../test_data/polygon-hermez/").join(file);

        let analyzed = analyze::<GoldilocksField>(&file);
        let json_out = export(&analyzed);

        let pilcom = std::env::var("PILCOM").expect(
            "Please set the PILCOM environment variable to the path to the pilcom repository.",
        );
        let pilcom_output = Command::new("node")
            .args([
                format!("{pilcom}/src/pil.js"),
                file.display().to_string(),
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

        let json_out = serde_json::to_value(json_out).unwrap();

        let pilcom_parsed = serde_json::from_str(&pilcom_out).expect("Invalid json from pilcom.");
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
        compare_export_file("config.pil");
    }

    #[test]
    fn export_binary() {
        compare_export_file("binary.pil");
    }

    #[test]
    fn export_byte4() {
        compare_export_file("byte4.pil");
    }

    #[test]
    fn export_global() {
        compare_export_file("global.pil");
    }

    #[test]
    fn export_arith() {
        // We ignore the specific value assigned to idQ.
        // It is just a counter and pilcom assigns it in a weird order.
        compare_export_file_ignore_idq_hex("arith.pil");
    }

    #[test]
    fn export_mem() {
        compare_export_file_ignore_idq_hex("mem.pil");
        compare_export_file_ignore_idq_hex("mem_align.pil");
    }

    #[test]
    fn export_keccakf() {
        compare_export_file_ignore_idq_hex("keccakf.pil");
    }

    #[test]
    fn export_padding() {
        compare_export_file("nine2one.pil");
        compare_export_file_ignore_idq_hex("padding_kkbit.pil");
        compare_export_file_ignore_idq_hex("padding_kk.pil");
        compare_export_file_ignore_idq_hex("padding_kk.pil");
    }

    #[test]
    fn export_poseidong() {
        compare_export_file_ignore_idq_hex("padding_pg.pil");
        compare_export_file_ignore_idq_hex("poseidong.pil");
        compare_export_file_ignore_idq_hex("storage.pil");
    }

    #[test]
    fn export_main() {
        compare_export_file_ignore_idq_hex("rom.pil");
        compare_export_file_ignore_idq_hex("main.pil");
    }
}
