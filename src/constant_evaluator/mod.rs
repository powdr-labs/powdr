use crate::analyzer::{Analyzed, BinaryOperator, ConstantNumberType, Expression, UnaryOperator};

/// Generates the constant polynomial values for all constant polynomials
/// that are defined (and not just declared).
/// @returns the values (in source order) and the degree of the polynomials.
pub fn generate(analyzed: &Analyzed) -> (Vec<Vec<ConstantNumberType>>, ConstantNumberType) {
    let mut degree = None;
    let values = analyzed
        .constants_in_source_order()
        .iter()
        .filter_map(|(poly, value)| {
            if let Some(value) = value {
                if let Some(degree) = degree {
                    assert!(degree == poly.degree);
                } else {
                    degree = Some(poly.degree);
                }
                return Some(generate_values(analyzed, poly.degree, value));
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
