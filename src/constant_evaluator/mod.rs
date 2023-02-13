use crate::analyzer::{Analyzed, BinaryOperator, ConstantNumberType, Expression, UnaryOperator};

/// Generates the constant polynomial values for all constant polynomials
/// that are defined (and not just declared).
/// @returns the values (in source order) and the degree of the polynomials.
pub fn generate(
    analyzed: &Analyzed,
) -> (Vec<(&String, Vec<ConstantNumberType>)>, ConstantNumberType) {
    let mut degree = None;
    let values = analyzed
        .constant_polys_in_source_order()
        .iter()
        .filter_map(|(poly, value)| {
            if let Some(value) = value {
                if let Some(degree) = degree {
                    assert!(degree == poly.degree);
                } else {
                    degree = Some(poly.degree);
                }
                return Some((
                    &poly.absolute_name,
                    generate_values(analyzed, poly.degree, value),
                ));
            }
            None
        })
        .collect();
    (values, degree.unwrap_or_default())
}

fn generate_values(
    analyzed: &Analyzed,
    degree: ConstantNumberType,
    body: &Expression,
) -> Vec<ConstantNumberType> {
    (0..degree)
        .map(|i| {
            Evaluator {
                analyzed,
                variables: &[i],
            }
            .evaluate(body)
        })
        .collect()
}

struct Evaluator<'a> {
    analyzed: &'a Analyzed,
    variables: &'a [ConstantNumberType],
}

impl<'a> Evaluator<'a> {
    fn evaluate(&self, expr: &Expression) -> ConstantNumberType {
        match expr {
            Expression::Constant(name) => self.analyzed.constants[name],
            Expression::PolynomialReference(_) => todo!(),
            Expression::LocalVariableReference(i) => self.variables[*i as usize],
            Expression::PublicReference(_) => todo!(),
            Expression::Number(n) => *n,
            Expression::BinaryOperation(left, op, right) => {
                self.evaluate_binary_operation(left, op, right)
            }
            Expression::UnaryOperation(op, expr) => self.evaluate_unary_operation(op, expr),
        }
    }

    fn evaluate_binary_operation(
        &self,
        left: &Expression,
        op: &BinaryOperator,
        right: &Expression,
    ) -> ConstantNumberType {
        let left = self.evaluate(left);
        let right = self.evaluate(right);
        match op {
            BinaryOperator::Add => left + right,
            BinaryOperator::Sub => left - right,
            BinaryOperator::Mul => left * right,
            BinaryOperator::Div => {
                if left == 0 {
                    0
                } else {
                    left / right
                }
            }
            BinaryOperator::Pow => {
                assert!(right <= u32::MAX.into());
                left.pow(right as u32)
            }
        }
    }

    fn evaluate_unary_operation(
        &self,
        op: &UnaryOperator,
        expr: &Expression,
    ) -> ConstantNumberType {
        let v = self.evaluate(expr);
        match op {
            UnaryOperator::Plus => v,
            UnaryOperator::Minus => -v,
        }
    }
}

#[cfg(test)]
mod test {
    use crate::analyzer::analyze_string;

    use super::generate;

    #[test]
    pub fn test_last() {
        let src = r#"
            constant %N = 8;
            namespace F(%N);
            pol constant LAST(i) { 1 - (i - (%N - 1)) / (i - (%N - 1)) };
        "#;
        let analyzed = analyze_string(src);
        let (constants, degree) = generate(&analyzed);
        assert_eq!(degree, 8);
        assert_eq!(
            constants,
            vec![(&"F.LAST".to_string(), vec![0, 0, 0, 0, 0, 0, 0, 1])]
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
            vec![(&"F.EVEN".to_string(), vec![-2, 0, 2, 4, 6, 8, 10, 12])]
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
            vec![(&"F.EVEN".to_string(), vec![-2, 0, 2, 4, 6, 8, 10, 12])]
        );
    }

    #[test]
    pub fn test_macro_double() {
        let src = r#"
            constant %N = 12;
            namespace F(%N);
            macro is_nonzero(X) { X / X };
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
                &"F.TEN".to_string(),
                [[0; 10].to_vec(), [1, 0].to_vec()].concat()
            )]
        );
    }
}
