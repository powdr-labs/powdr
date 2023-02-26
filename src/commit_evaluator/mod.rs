use std::collections::HashMap;

use crate::analyzer::{
    Analyzed, BinaryOperator, ConstantNumberType, Expression, IdentityKind, UnaryOperator,
};

mod affine_expression;

use affine_expression::AffineExpression;

/// Generates the committed polynomial values
/// @returns the values (in source order) and the degree of the polynomials.
pub fn generate<'a>(
    analyzed: &'a Analyzed,
    degree: &ConstantNumberType,
    constants: &[(&String, Vec<ConstantNumberType>)],
) -> Vec<(&'a String, Vec<ConstantNumberType>)> {
    let polys: Vec<&String> = analyzed
        .committed_polys_in_source_order()
        .iter()
        .map(|(poly, value)| {
            assert!(value.is_none());
            if poly.length.is_some() {
                unimplemented!("Committed arrays not implemented.")
            }
            &poly.absolute_name
        })
        .collect();
    let mut values: Vec<(&String, Vec<i128>)> =
        polys.iter().map(|name| (*name, Vec::new())).collect();
    let mut evaluator = Evaluator::new(analyzed, constants, polys);
    for row in 0..*degree as usize {
        let row_values = evaluator.compute_next_row(row);
        for (col, v) in row_values.into_iter().enumerate() {
            values[col].1.push(v);
        }
    }
    values
}

struct Evaluator<'a> {
    analyzed: &'a Analyzed,
    constants: HashMap<&'a String, &'a Vec<ConstantNumberType>>,
    /// Maps the committed polynomial names to their IDs internal to this component.
    committed: HashMap<&'a String, usize>,
    committed_names: Vec<&'a String>,
    /// Values of the committed polynomials
    current: Vec<Option<ConstantNumberType>>,
    /// Values of the committed polynomials in the next row
    next: Vec<Option<ConstantNumberType>>,
    next_row: usize,
}

impl<'a> Evaluator<'a> {
    pub fn new(
        analyzed: &'a Analyzed,
        constants: &'a [(&String, Vec<ConstantNumberType>)],
        committed: Vec<&'a String>,
    ) -> Self {
        Evaluator {
            analyzed,
            constants: constants
                .iter()
                .map(|(name, values)| (*name, values))
                .collect(),
            committed: committed.iter().enumerate().map(|(i, c)| (*c, i)).collect(),
            committed_names: committed.clone(),
            current: vec![None; committed.len()],
            next: vec![None; committed.len()],
            next_row: 0,
        }
    }

    pub fn compute_next_row(&mut self, next_row: usize) -> Vec<ConstantNumberType> {
        self.next_row = next_row;

        // TODO maybe better to generate a dependency graph than looping multiple times.
        // TODO at least we could cache the affine expressions between loops.
        loop {
            let mut progress = false;
            // TODO also use lookups, not only polynomial identities
            for identity in &self.analyzed.identities {
                if identity.kind == IdentityKind::Polynomial {
                    let expr = identity.left.selector.as_ref().unwrap();
                    if let Some((id, value)) = self.evaluate(expr).and_then(|expr| expr.solve()) {
                        self.next[id] = Some(value);
                        progress = true;
                    }
                }
            }
            if !progress || self.next.iter().all(|v| v.is_some()) {
                break;
            }
        }
        if self.next.iter().any(|v| v.is_none()) {
            eprintln!(
                "Error: Row {next_row}: Unable to derive values for committed polynomials: {}",
                self.next
                    .iter()
                    .enumerate()
                    .filter_map(|(i, v)| if v.is_none() {
                        Some(self.committed_names[i].clone())
                    } else {
                        None
                    })
                    .collect::<Vec<String>>()
                    .join(", ")
            );
            panic!();
        } else {
            std::mem::swap(&mut self.next, &mut self.current);
            self.next = vec![None; self.current.len()];
            self.current.iter().map(|v| v.unwrap()).collect()
        }
    }

    /// Tries to evaluate the expression to an expression affine in the committed polynomials,
    /// taking current values of polynomials into account.
    /// @returns an expression affine in the committed polynomials of the next row.
    fn evaluate(&self, expr: &Expression) -> Option<AffineExpression> {
        // @TODO if we iterate on processing the constraints in the same row,
        // we could store the simplified values.
        match expr {
            Expression::Constant(name) => Some(self.analyzed.constants[name].into()),
            Expression::PolynomialReference(poly) => {
                // TODO arrays
                if let Some(&id) = self.committed.get(&poly.name) {
                    // Committed polynomial
                    if poly.next {
                        Some(if let Some(value) = self.next[id] {
                            // We already computed the concrete value
                            value.into()
                        } else {
                            // We continue with a symbolic value
                            AffineExpression::from_committed_poly_value(id)
                        })
                    } else {
                        self.current[id].map(|value| value.into())
                    }
                } else {
                    // Constant polynomial (or something else)
                    self.constants.get(&poly.name).map(|values| {
                        let degree = values.len();
                        let row = if poly.next {
                            self.next_row
                        } else {
                            (self.next_row + degree - 1) % degree
                        };
                        values[row].into()
                    })
                }
            }
            Expression::Number(n) => Some((*n).into()),
            Expression::BinaryOperation(left, op, right) => {
                self.evaluate_binary_operation(left, op, right)
            }
            Expression::UnaryOperation(op, expr) => self.evaluate_unary_operation(op, expr),
            Expression::Tuple(_) => panic!(),
            Expression::String(_) => panic!(),
            Expression::LocalVariableReference(_) => panic!(),
            Expression::PublicReference(_) => panic!(),
            Expression::FunctionCall(_, _) => panic!(),
        }
    }
    fn evaluate_binary_operation(
        &self,
        left: &Expression,
        op: &BinaryOperator,
        right: &Expression,
    ) -> Option<AffineExpression> {
        if let (Some(left), Some(right)) = (self.evaluate(left), self.evaluate(right)) {
            match op {
                BinaryOperator::Add => Some(left + right),
                BinaryOperator::Sub => Some(left - right),
                BinaryOperator::Mul => {
                    if let Some(f) = left.constant_value() {
                        Some(right.mul(f))
                    } else {
                        right.constant_value().map(|f| left.mul(f))
                    }
                }
                BinaryOperator::Div => {
                    if let (Some(l), Some(r)) = (left.constant_value(), right.constant_value()) {
                        // TODO Maybe warn about division by zero here.
                        if l == 0 {
                            Some(0.into())
                        } else {
                            Some((l / r).into())
                        }
                    } else {
                        None
                    }
                }
                BinaryOperator::Pow => {
                    if let (Some(l), Some(r)) = (left.constant_value(), right.constant_value()) {
                        assert!(r <= u32::MAX.into());
                        Some(l.pow(r as u32).into())
                    } else {
                        None
                    }
                }
                BinaryOperator::Mod
                | BinaryOperator::BinaryAnd
                | BinaryOperator::BinaryOr
                | BinaryOperator::ShiftLeft
                | BinaryOperator::ShiftRight => {
                    if let (Some(left), Some(right)) =
                        (left.constant_value(), right.constant_value())
                    {
                        let result = match op {
                            BinaryOperator::Mod => left % right,
                            BinaryOperator::BinaryAnd => left & right,
                            BinaryOperator::BinaryOr => left | right,
                            BinaryOperator::ShiftLeft => left << right,
                            BinaryOperator::ShiftRight => left >> right,
                            _ => panic!(),
                        };
                        Some(result.into())
                    } else {
                        panic!()
                    }
                }
            }
        } else {
            None
        }
    }

    fn evaluate_unary_operation(
        &self,
        op: &UnaryOperator,
        expr: &Expression,
    ) -> Option<AffineExpression> {
        self.evaluate(expr).map(|v| match op {
            UnaryOperator::Plus => v,
            UnaryOperator::Minus => -v,
        })
    }
}
