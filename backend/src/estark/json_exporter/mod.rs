use powdr_number::FieldElement;
use std::collections::HashMap;
use std::{cmp, path::PathBuf};

use powdr_ast::analyzed::{
    AlgebraicBinaryOperation, AlgebraicBinaryOperator, AlgebraicExpression as Expression,
    AlgebraicUnaryOperation, AlgebraicUnaryOperator, Analyzed, IdentityKind, PolyID,
    PolynomialType, StatementIdentifier, SymbolKind,
};
use powdr_parser_util::SourceRef;
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
    /// A cache to improve computing the line from a file offset.
    /// Comparison is by raw pointer value because the data comes
    /// from Arcs and we assume the actual data is not cloned.
    line_starts: HashMap<*const u8, Vec<usize>>,
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
                if let Some((poly, value)) = analyzed.intermediate_columns.get(name) {
                    assert_eq!(poly.kind, SymbolKind::Poly(PolynomialType::Intermediate));
                    for ((_, id), value) in poly.array_elements().zip(value) {
                        let expression_id = exporter.extract_expression(value, 1);
                        assert_eq!(
                            expression_id,
                            exporter.intermediate_poly_expression_ids[&id.id] as usize
                        );
                    }
                }
            }
            StatementIdentifier::PublicDeclaration(name) => {
                let pub_def = &analyzed.public_declarations[name];
                let symbol = &analyzed.definitions[&pub_def.polynomial.name].0;
                let (_, poly) = symbol
                    .array_elements()
                    .nth(pub_def.array_index.unwrap_or_default())
                    .unwrap();
                let (_, expr) = exporter.polynomial_reference_to_json(poly, false);
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
                // PILCOM strips the path from filenames, we do the same here for compatibility
                let file_name = identity
                    .source
                    .file_name
                    .as_deref()
                    .and_then(|s| {
                        PathBuf::from(s)
                            .file_name()
                            .and_then(|n| n.to_str())
                            .map(String::from)
                    })
                    .unwrap_or_default();
                let line = exporter.line_of_source_ref(&identity.source);
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
                            line,
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
                            line,
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
        publics,
        references: exporter.references(),
        expressions: exporter.expressions,
        polIdentities: pol_identities,
        plookupIdentities: plookup_identities,
        permutationIdentities: Some(permutation_identities),
        connectionIdentities: Some(connection_identities),
        cm_dims: Vec::new(),
        q2exp: Vec::new(),
    }
}

fn symbol_kind_to_json_string(k: SymbolKind) -> &'static str {
    match k {
        SymbolKind::Poly(poly_type) => polynomial_type_to_json_string(poly_type),
        SymbolKind::Other() => panic!("Cannot translate \"other\" symbol to json."),
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

/// Makes names compatible with estark, which sometimes require that
/// there is exactly one `.` in the name.
fn fixup_name(name: &str) -> String {
    if let Some(last) = name.rfind("::") {
        format!("{}.{}", &name[..last], &name[last + 2..])
    } else {
        panic!("Witness or intermediate column is not inside a namespace: {name}");
    }
}

impl<'a, T: FieldElement> Exporter<'a, T> {
    fn new(analyzed: &'a Analyzed<T>) -> Self {
        Self {
            analyzed,
            expressions: vec![],
            intermediate_poly_expression_ids: compute_intermediate_expression_ids(analyzed),
            number_q: 0,
            line_starts: Default::default(),
        }
    }

    fn references(&self) -> HashMap<String, Reference> {
        self.analyzed
            .definitions
            .iter()
            .filter_map(|(name, (symbol, _value))| {
                let id = match symbol.kind {
                    SymbolKind::Poly(PolynomialType::Intermediate) => {
                        panic!("Should be in intermediates")
                    }
                    SymbolKind::Poly(_) => Some(symbol.id),
                    SymbolKind::Other() => None,
                }?;

                let out = Reference {
                    polType: None,
                    type_: symbol_kind_to_json_string(symbol.kind).to_string(),
                    id: id as usize,
                    polDeg: self.analyzed.degree() as usize,
                    isArray: symbol.is_array(),
                    elementType: None,
                    len: symbol.length.map(|l| l as usize),
                };
                Some((fixup_name(name), out))
            })
            .chain(
                self.analyzed
                    .intermediate_columns
                    .iter()
                    .map(|(name, (symbol, _))| {
                        assert_eq!(symbol.kind, SymbolKind::Poly(PolynomialType::Intermediate));
                        let id = self.intermediate_poly_expression_ids[&symbol.id];

                        let out = Reference {
                            polType: None,
                            type_: symbol_kind_to_json_string(symbol.kind).to_string(),
                            id: id as usize,
                            polDeg: self.analyzed.degree() as usize,
                            isArray: symbol.is_array(),
                            elementType: None,
                            len: symbol.length.map(|l| l as usize),
                        };
                        (fixup_name(name), out)
                    }),
            )
            .collect::<HashMap<String, Reference>>()
    }

    /// Processes the given expression
    /// @returns the expression ID
    fn extract_expression(&mut self, expr: &Expression<T>, max_degree: u32) -> usize {
        let id = self.expressions.len();
        let (degree, mut expr) = self.expression_to_json(expr);
        if degree > max_degree {
            expr.idQ = Some(self.number_q as usize);
            expr.deg = 1;
            self.number_q += 1;
        }
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

    /// returns the degree and the JSON value (intermediate polynomial IDs)
    fn expression_to_json(&self, expr: &Expression<T>) -> (u32, StarkyExpr) {
        match expr {
            Expression::Reference(reference) => {
                self.polynomial_reference_to_json(reference.poly_id, reference.next)
            }
            Expression::PublicReference(name) => (
                0,
                StarkyExpr {
                    op: "public".to_string(),
                    deg: 0,
                    id: Some(self.analyzed.public_declarations[name].id as usize),
                    ..DEFAULT_EXPR
                },
            ),
            Expression::Challenge(challenge) => (
                0,
                StarkyExpr {
                    op: "challenge".to_string(),
                    deg: 0,
                    id: Some(challenge.id as usize),
                    ..DEFAULT_EXPR
                },
            ),
            Expression::Number(value) => (
                0,
                StarkyExpr {
                    op: "number".to_string(),
                    deg: 0,
                    value: Some(format!("{value}")),
                    ..DEFAULT_EXPR
                },
            ),
            Expression::BinaryOperation(AlgebraicBinaryOperation { left, op, right }) => {
                let (deg_left, left) = self.expression_to_json(left);
                let (deg_right, right) = self.expression_to_json(right);
                let (op, degree) = match op {
                    AlgebraicBinaryOperator::Add => ("add", cmp::max(deg_left, deg_right)),
                    AlgebraicBinaryOperator::Sub => ("sub", cmp::max(deg_left, deg_right)),
                    AlgebraicBinaryOperator::Mul => ("mul", deg_left + deg_right),
                    AlgebraicBinaryOperator::Pow => {
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
                    StarkyExpr {
                        op: op.to_string(),
                        deg: degree as usize,
                        values: Some(vec![left, right]),
                        ..DEFAULT_EXPR
                    },
                )
            }
            Expression::UnaryOperation(AlgebraicUnaryOperation { op, expr: value }) => {
                let (deg, value) = self.expression_to_json(value);
                match op {
                    AlgebraicUnaryOperator::Minus => (
                        deg,
                        StarkyExpr {
                            op: "neg".to_string(),
                            deg: deg as usize,
                            values: Some(vec![value]),
                            ..DEFAULT_EXPR
                        },
                    ),
                }
            }
        }
    }

    fn polynomial_reference_to_json(
        &self,
        PolyID { id, ptype }: PolyID,
        next: bool,
    ) -> (u32, StarkyExpr) {
        let id = if ptype == PolynomialType::Intermediate {
            self.intermediate_poly_expression_ids[&id]
        } else {
            id
        };
        let poly = StarkyExpr {
            id: Some(id as usize),
            op: polynomial_reference_type_to_json_string(ptype).to_string(),
            deg: 1,
            next: Some(next),
            ..DEFAULT_EXPR
        };
        (1, poly)
    }

    fn line_of_source_ref(&mut self, source: &SourceRef) -> usize {
        let Some(file_contents) = source.file_contents.as_ref() else {
            return 0;
        };
        let line_starts = self
            .line_starts
            .entry(file_contents.as_ptr())
            .or_insert_with(|| compute_line_starts(file_contents));
        offset_to_line_col(source.start, line_starts).0
    }
}

fn compute_line_starts(source: &str) -> Vec<usize> {
    std::iter::once(0)
        .chain(source.match_indices('\n').map(|(i, _)| i + 1))
        .collect::<Vec<_>>()
}

/// Returns a tuple `(line, col)` given the file offset of line starts.
/// `line` is 1 based and `col` is 0 based.
fn offset_to_line_col(offset: usize, line_starts: &[usize]) -> (usize, usize) {
    let line = match line_starts.binary_search(&offset) {
        Ok(line) => line + 1,
        Err(next_line) => next_line,
    };
    (line, offset - line_starts[line - 1])
}

#[cfg(test)]
mod test {
    use pretty_assertions::assert_eq;
    use test_log::test;

    use super::*;

    #[test]
    fn line_calc() {
        let input = "abc\nde";
        let breaks = compute_line_starts(input);
        let line_col_pairs = (0..input.len())
            .map(|o| offset_to_line_col(o, &breaks))
            .collect::<Vec<_>>();
        assert_eq!(
            line_col_pairs,
            [(1, 0), (1, 1), (1, 2), (1, 3), (2, 0), (2, 1)]
        );
    }

    #[test]
    fn line_calc_empty_start() {
        let input = "\nab\n\nc\nde\n";
        let breaks = compute_line_starts(input);
        let line_col_pairs = (0..input.len())
            .map(|o| offset_to_line_col(o, &breaks))
            .collect::<Vec<_>>();
        assert_eq!(
            line_col_pairs,
            [
                (1, 0),
                (2, 0),
                (2, 1),
                (2, 2),
                (3, 0),
                (4, 0),
                (4, 1),
                (5, 0),
                (5, 1),
                (5, 2)
            ]
        );
    }
}
