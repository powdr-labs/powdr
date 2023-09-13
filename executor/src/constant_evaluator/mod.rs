use std::collections::HashMap;

use ast::analyzed::{Analyzed, Expression, FunctionValueDefinition, Reference};
use ast::parsed::{FunctionCall, MatchArm};
use ast::{evaluate_binary_operation, evaluate_unary_operation};
use itertools::Itertools;
use number::{DegreeType, FieldElement};
use rayon::prelude::{IntoParallelIterator, ParallelIterator};

/// Generates the constant polynomial values for all constant polynomials
/// that are defined (and not just declared).
/// @returns the values (in source order) and the degree of the polynomials.
pub fn generate<T: FieldElement>(analyzed: &Analyzed<T>) -> (Vec<(&str, Vec<T>)>, DegreeType) {
    let mut degree = None;
    let mut other_constants = HashMap::new();
    for (poly, value) in analyzed.constant_polys_in_source_order() {
        if let Some(value) = value {
            if let Some(degree) = degree {
                assert!(degree == poly.degree);
            } else {
                degree = Some(poly.degree);
            }
            let values = generate_values(analyzed, poly.degree, value, &other_constants);
            other_constants.insert(&poly.absolute_name, values);
        }
    }
    let values = other_constants
        .into_iter()
        .sorted_by_key(|(name, _)| analyzed.definitions[&name.to_string()].0.id)
        .collect::<Vec<_>>();
    (values, degree.unwrap_or_default())
}

fn generate_values<T: FieldElement>(
    analyzed: &Analyzed<T>,
    degree: DegreeType,
    body: &FunctionValueDefinition<T>,
    other_constants: &HashMap<&str, Vec<T>>,
) -> Vec<T> {
    match body {
        FunctionValueDefinition::Mapping(body) => (0..degree)
            .into_par_iter()
            .map(|i| {
                Evaluator {
                    analyzed,
                    variables: &[i.into()],
                    other_constants,
                }
                .evaluate(body)
            })
            .collect(),
        FunctionValueDefinition::Array(values) => {
            let evaluator = Evaluator {
                analyzed,
                variables: &[],
                other_constants,
            };
            let values: Vec<_> = values
                .iter()
                .flat_map(|elements| {
                    let items = elements
                        .pattern()
                        .iter()
                        .map(|v| evaluator.evaluate(v))
                        .collect::<Vec<_>>();

                    items
                        .into_iter()
                        .cycle()
                        .take(elements.size() as usize)
                        .collect::<Vec<_>>()
                })
                .collect();
            assert_eq!(values.len(), degree as usize);
            values
        }
        FunctionValueDefinition::Query(_) => panic!("Query used for fixed column."),
        FunctionValueDefinition::Expression(_) => {
            panic!("Expression used for fixed column, only expected for intermediate polynomials")
        }
    }
}

struct Evaluator<'a, T> {
    analyzed: &'a Analyzed<T>,
    other_constants: &'a HashMap<&'a str, Vec<T>>,
    variables: &'a [T],
}

impl<'a, T: FieldElement> Evaluator<'a, T> {
    fn evaluate(&self, expr: &Expression<T>) -> T {
        match expr {
            Expression::Constant(name) => self.analyzed.constants[name],
            Expression::Reference(Reference::LocalVar(i)) => self.variables[*i as usize],
            Expression::Reference(Reference::Poly(_)) => todo!(),
            Expression::PublicReference(_) => todo!(),
            Expression::Number(n) => *n,
            Expression::String(_) => panic!(),
            Expression::Tuple(_) => panic!(),
            Expression::BinaryOperation(left, op, right) => {
                evaluate_binary_operation(self.evaluate(left), *op, self.evaluate(right))
            }
            Expression::UnaryOperation(op, expr) => {
                evaluate_unary_operation(*op, self.evaluate(expr))
            }
            Expression::FunctionCall(FunctionCall { id, arguments }) => {
                let arg_values = arguments
                    .iter()
                    .map(|a| self.evaluate(a))
                    .collect::<Vec<_>>();
                assert!(arg_values.len() == 1);
                let values = &self.other_constants[id.as_str()];
                values[arg_values[0].to_degree() as usize % values.len()]
            }
            Expression::MatchExpression(scrutinee, arms) => {
                let v = self.evaluate(scrutinee);
                arms.iter()
                    .find_map(|MatchArm { pattern, value }| match pattern {
                        Some(p) => (self.evaluate(p) == v).then(|| self.evaluate(value)),
                        None => Some(self.evaluate(value)),
                    })
                    .expect("No arm matched the value {v}")
            }
            Expression::FreeInput(_) => panic!(),
        }
    }
}

#[cfg(test)]
mod test {
    use number::GoldilocksField;
    use pil_analyzer::analyze_string;
    use pretty_assertions::assert_eq;
    use test_log::test;

    use super::*;

    fn convert(input: Vec<i32>) -> Vec<GoldilocksField> {
        input.into_iter().map(|x| x.into()).collect()
    }

    #[test]
    pub fn test_last() {
        let src = r#"
            constant %N = 8;
            namespace F(%N);
            pol constant LAST(i) { match i {
                %N - 1 => 1,
                _ => 0,
            } };
        "#;
        let analyzed = analyze_string(src);
        let (constants, degree) = generate(&analyzed);
        assert_eq!(degree, 8);
        assert_eq!(
            constants,
            vec![("F.LAST", convert(vec![0, 0, 0, 0, 0, 0, 0, 1]))]
        );
    }

    #[test]
    pub fn test_counter() {
        let src = r#"
            constant %N = 8;
            namespace F(%N);
            pol constant EVEN(i) { 2 * (i - 1) };
        "#;
        let analyzed = analyze_string(src);
        let (constants, degree) = generate(&analyzed);
        assert_eq!(degree, 8);
        assert_eq!(
            constants,
            vec![("F.EVEN", convert(vec![-2, 0, 2, 4, 6, 8, 10, 12]))]
        );
    }

    #[test]
    pub fn test_xor() {
        let src = r#"
            constant %N = 8;
            namespace F(%N);
            pol constant X(i) { i ^ (i + 17) | 3 };
        "#;
        let analyzed = analyze_string(src);
        let (constants, degree) = generate(&analyzed);
        assert_eq!(degree, 8);
        assert_eq!(
            constants,
            vec![("F.X", convert((0..8).map(|i| i ^ (i + 17) | 3).collect()))]
        );
    }

    #[test]
    pub fn test_match() {
        let src = r#"
            constant %N = 8;
            namespace F(%N);
            pol constant X(i) { match i {
                0 => 7,
                3 => 9,
                5 => 2,
                _ => 4,
            } + 1 };
        "#;
        let analyzed = analyze_string(src);
        let (constants, degree) = generate(&analyzed);
        assert_eq!(degree, 8);
        assert_eq!(
            constants,
            vec![("F.X", convert(vec![8, 5, 5, 10, 5, 3, 5, 5]))]
        );
    }

    #[test]
    pub fn test_macro() {
        let src = r#"
            constant %N = 8;
            namespace F(%N);
            macro minus_one(X) { X - 1 };
            pol constant EVEN(i) { 2 * minus_one(i) };
        "#;
        let analyzed = analyze_string(src);
        let (constants, degree) = generate(&analyzed);
        assert_eq!(degree, 8);
        assert_eq!(
            constants,
            vec![("F.EVEN", convert(vec![-2, 0, 2, 4, 6, 8, 10, 12]))]
        );
    }

    #[test]
    pub fn test_macro_double() {
        let src = r#"
            constant %N = 12;
            namespace F(%N);
            macro is_nonzero(X) { match X { 0 => 0, _ => 1, } };
            macro is_zero(X) { 1 - is_nonzero(X) };
            macro is_one(X) { is_zero(1 - X) };
            macro is_equal(A, B) { is_zero(A - B) };
            macro ite(C, T, F) { is_one(C) * T + is_zero(C) * F };
            pol constant TEN(i) { ite(is_equal(i, 10), 1, 0) };
        "#;
        let analyzed = analyze_string(src);
        let (constants, degree) = generate(&analyzed);
        assert_eq!(degree, 12);
        assert_eq!(
            constants,
            vec![(
                "F.TEN",
                convert([[0; 10].to_vec(), [1, 0].to_vec()].concat())
            )]
        );
    }

    #[test]
    pub fn test_poly_call() {
        let src = r#"
            constant %N = 10;
            namespace F(%N);
            col fixed seq(i) { i };
            col fixed doub(i) { seq((2 * i) % %N) + 1 };
            col fixed half_nibble(i) { i & 0x7 };
            col fixed doubled_half_nibble(i) { half_nibble(i / 2) };
        "#;
        let analyzed = analyze_string(src);
        let (constants, degree) = generate(&analyzed);
        assert_eq!(degree, 10);
        assert_eq!(constants.len(), 4);
        assert_eq!(
            constants[0],
            ("F.seq", convert((0..=9i32).collect::<Vec<_>>()))
        );
        assert_eq!(
            constants[1],
            (
                "F.doub",
                convert([1i32, 3, 5, 7, 9, 1, 3, 5, 7, 9].to_vec())
            )
        );
        assert_eq!(
            constants[2],
            (
                "F.half_nibble",
                convert([0i32, 1, 2, 3, 4, 5, 6, 7, 0, 1].to_vec())
            )
        );
        assert_eq!(
            constants[3],
            (
                "F.doubled_half_nibble",
                convert([0i32, 0, 1, 1, 2, 2, 3, 3, 4, 4].to_vec())
            )
        );
    }

    #[test]
    pub fn test_arrays() {
        let src = r#"
            constant %N = 10;
            namespace F(%N);
            col fixed alt = [0, 1, 0, 1, 0, 1] + [0]*;
            col fixed empty = [] + [0]*;
            col fixed ref_other = [%N-1, alt(1), 8] + [0]*;
        "#;
        let analyzed = analyze_string(src);
        let (constants, degree) = generate(&analyzed);
        assert_eq!(degree, 10);
        assert_eq!(constants.len(), 3);
        assert_eq!(
            constants[0],
            ("F.alt", convert([0i32, 1, 0, 1, 0, 1, 0, 0, 0, 0].to_vec()))
        );
        assert_eq!(constants[1], ("F.empty", convert([0i32; 10].to_vec())));
        assert_eq!(
            constants[2],
            (
                "F.ref_other",
                convert([9i32, 1, 8, 0, 0, 0, 0, 0, 0, 0].to_vec())
            )
        );
    }

    #[test]
    pub fn repetition_front() {
        let src = r#"
            constant %N = 10;
            namespace F(%N);
            col fixed arr = [0, 1, 2]* + [7];
        "#;
        let analyzed = analyze_string(src);
        let (constants, degree) = generate(&analyzed);
        assert_eq!(degree, 10);
        assert_eq!(constants.len(), 1);
        assert_eq!(
            constants[0],
            ("F.arr", convert([0i32, 1, 2, 0, 1, 2, 0, 1, 2, 7].to_vec()))
        );
    }

    #[test]
    pub fn comparisons() {
        let src = r#"
            constant %N = 6;
            namespace F(%N);
            col fixed id(i) { i };
            col fixed inv(i) { %N - i };
            col fixed a = [0, 1, 0, 1, 2, 1];
            col fixed b = [0, 0, 1, 1, 0, 5];
            col fixed or(i) { a(i) || b(i) };
            col fixed and(i) { a(i) && b(i) };
            col fixed not(i) { !a(i) };
            col fixed less(i) { id(i) < inv(i) };
            col fixed less_eq(i) { id(i) <= inv(i) };
            col fixed eq(i) { id(i) == inv(i) };
            col fixed not_eq(i) { id(i) != inv(i) };
            col fixed greater(i) { id(i) > inv(i) };
            col fixed greater_eq(i) { id(i) >= inv(i) };
        "#;
        let analyzed = analyze_string(src);
        let (constants, degree) = generate(&analyzed);
        assert_eq!(degree, 6);
        assert_eq!(constants[0], ("F.id", convert([0, 1, 2, 3, 4, 5].to_vec())));
        assert_eq!(
            constants[1],
            ("F.inv", convert([6, 5, 4, 3, 2, 1].to_vec()))
        );
        assert_eq!(constants[4], ("F.or", convert([0, 1, 1, 1, 1, 1].to_vec())));
        assert_eq!(
            constants[5],
            ("F.and", convert([0, 0, 0, 1, 0, 1].to_vec()))
        );
        assert_eq!(
            constants[6],
            ("F.not", convert([1, 0, 1, 0, 0, 0].to_vec()))
        );
        assert_eq!(
            constants[7],
            ("F.less", convert([1, 1, 1, 0, 0, 0].to_vec()))
        );
        assert_eq!(
            constants[8],
            ("F.less_eq", convert([1, 1, 1, 1, 0, 0].to_vec()))
        );
        assert_eq!(constants[9], ("F.eq", convert([0, 0, 0, 1, 0, 0].to_vec())));
        assert_eq!(
            constants[10],
            ("F.not_eq", convert([1, 1, 1, 0, 1, 1].to_vec()))
        );
        assert_eq!(
            constants[11],
            ("F.greater", convert([0, 0, 0, 0, 1, 1].to_vec()))
        );
        assert_eq!(
            constants[12],
            ("F.greater_eq", convert([0, 0, 0, 1, 1, 1].to_vec()))
        );
    }
}
