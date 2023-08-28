use std::collections::HashMap;

use ast::analyzed::{Analyzed, Expression, FunctionValueDefinition};
use ast::parsed::{BinaryOperator, UnaryOperator};
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
            Expression::PolynomialReference(_) => todo!(),
            Expression::LocalVariableReference(i) => self.variables[*i as usize],
            Expression::PublicReference(_) => todo!(),
            Expression::Number(n) => *n,
            Expression::String(_) => panic!(),
            Expression::Tuple(_) => panic!(),
            Expression::BinaryOperation(left, op, right) => {
                self.evaluate_binary_operation(left, op, right)
            }
            Expression::UnaryOperation(op, expr) => self.evaluate_unary_operation(op, expr),
            Expression::SumOfProducts(_, _) => todo!(),
            Expression::FunctionCall(name, args) => {
                let arg_values = args.iter().map(|a| self.evaluate(a)).collect::<Vec<_>>();
                assert!(arg_values.len() == 1);
                let values = &self.other_constants[name.as_str()];
                values[arg_values[0].to_degree() as usize % values.len()]
            }
            Expression::MatchExpression(scrutinee, arms) => {
                let v = self.evaluate(scrutinee);
                arms.iter()
                    .find(|(n, _)| n.is_none() || n.as_ref() == Some(&v))
                    .map(|(_, e)| self.evaluate(e))
                    .expect("No arm matched the value {v}")
            }
        }
    }

    fn evaluate_binary_operation(
        &self,
        left: &Expression<T>,
        op: &BinaryOperator,
        right: &Expression<T>,
    ) -> T {
        let left = self.evaluate(left);
        let right = self.evaluate(right);
        match op {
            BinaryOperator::Add => left + right,
            BinaryOperator::Sub => left - right,
            BinaryOperator::Mul => left * right,
            BinaryOperator::Div => left.integer_div(right),
            BinaryOperator::Pow => left.pow(right.to_integer()),
            BinaryOperator::Mod => {
                (left.to_arbitrary_integer() % right.to_arbitrary_integer()).into()
            }
            BinaryOperator::BinaryAnd => (left.to_integer() & right.to_integer()).into(),
            BinaryOperator::BinaryXor => (left.to_integer() ^ right.to_integer()).into(),
            BinaryOperator::BinaryOr => (left.to_integer() | right.to_integer()).into(),
            BinaryOperator::ShiftLeft => (left.to_integer() << right.to_degree()).into(),
            BinaryOperator::ShiftRight => (left.to_integer() >> right.to_degree()).into(),
        }
    }

    fn evaluate_unary_operation(&self, op: &UnaryOperator, expr: &Expression<T>) -> T {
        let v = self.evaluate(expr);
        match op {
            UnaryOperator::Plus => v,
            UnaryOperator::Minus => -v,
        }
    }
}

#[cfg(test)]
mod test {
    use number::GoldilocksField;
    use pil_analyzer::analyze_string;
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
}
