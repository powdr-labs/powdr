use std::collections::HashMap;

use crate::analyzer::{
    Analyzed, BinaryOperator, ConstantNumberType, Expression, FunctionValueDefinition,
    IdentityKind, UnaryOperator,
};

mod affine_expression;

use affine_expression::AffineExpression;

/// Generates the committed polynomial values
/// @returns the values (in source order) and the degree of the polynomials.
pub fn generate<'a>(
    analyzed: &'a Analyzed,
    degree: &ConstantNumberType,
    constants: &[(&String, Vec<ConstantNumberType>)],
    query_callback: Option<impl FnMut(&str) -> Option<ConstantNumberType>>,
) -> Vec<(&'a String, Vec<ConstantNumberType>)> {
    let polys: Vec<WitnessColumn> = analyzed
        .committed_polys_in_source_order()
        .iter()
        .enumerate()
        .map(|(i, (poly, value))| {
            if poly.length.is_some() {
                unimplemented!("Committed arrays not implemented.")
            }
            WitnessColumn::new(i, &poly.absolute_name, value)
        })
        .collect();
    let mut values: Vec<(&String, Vec<i128>)> =
        polys.iter().map(|p| (p.name, Vec::new())).collect();
    let mut evaluator = Evaluator::new(analyzed, constants, &polys, query_callback);
    for row in 0..*degree as usize {
        let row_values = evaluator.compute_next_row(row);
        for (col, v) in row_values.into_iter().enumerate() {
            values[col].1.push(v);
        }
    }
    values
}

struct WitnessColumn<'a> {
    id: usize,
    name: &'a String,
    query: Option<&'a Expression>,
}

impl<'a> WitnessColumn<'a> {
    pub fn new(
        id: usize,
        name: &'a String,
        value: &'a Option<FunctionValueDefinition>,
    ) -> WitnessColumn<'a> {
        let query = if let Some(FunctionValueDefinition::Query(query)) = value {
            Some(query)
        } else {
            None
        };
        WitnessColumn { id, name, query }
    }
}

struct Evaluator<'a, QueryCallback>
where
    QueryCallback: FnMut(&'a str) -> Option<ConstantNumberType>,
{
    analyzed: &'a Analyzed,
    constants: HashMap<&'a String, &'a Vec<ConstantNumberType>>,
    query_callback: Option<QueryCallback>,
    /// Maps the committed polynomial names to their IDs internal to this component
    /// and optional parameter and query string.
    committed: HashMap<&'a String, &'a WitnessColumn<'a>>,
    committed_names: Vec<&'a String>,
    /// Values of the committed polynomials
    current: Vec<Option<ConstantNumberType>>,
    /// Values of the committed polynomials in the next row
    next: Vec<Option<ConstantNumberType>>,
    next_row: usize,
}

impl<'a, QueryCallback> Evaluator<'a, QueryCallback>
where
    QueryCallback: FnMut(&str) -> Option<ConstantNumberType>,
{
    pub fn new(
        analyzed: &'a Analyzed,
        constants: &'a [(&String, Vec<ConstantNumberType>)],
        committed: &'a Vec<WitnessColumn<'a>>,
        query_callback: Option<QueryCallback>,
    ) -> Self {
        Evaluator {
            analyzed,
            constants: constants
                .iter()
                .map(|(name, values)| (*name, values))
                .collect(),
            query_callback,
            committed: committed.iter().map(|p| (p.name, p)).collect(),
            committed_names: committed.iter().map(|p| p.name).collect(),
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

            if self.query_callback.is_some() {
                for column in self.committed.values() {
                    if self.next[column.id].is_none() && column.query.is_some() {
                        let query = self.interpolate_query(column.query.unwrap());
                        let result = self.query_callback.as_mut().unwrap()(&query);
                        if let Some(value) = result {
                            self.next[column.id] = Some(value);
                            progress = true;
                        }
                    }
                }
            }
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

    fn interpolate_query(&self, query: &Expression) -> String {
        // TODO combine that with the constant evaluator and the commit evaluator...
        match query {
            Expression::Tuple(items) => items
                .iter()
                .map(|i| self.interpolate_query(i))
                .collect::<Vec<_>>()
                .join(", "),
            Expression::LocalVariableReference(i) => {
                assert!(*i == 0);
                format!("{}", self.next_row)
            }
            Expression::Constant(_) => todo!(),
            Expression::PolynomialReference(_) => todo!(),
            Expression::PublicReference(_) => todo!(),
            Expression::Number(n) => format!("{n}"),
            Expression::String(s) => {
                format!("\"{}\"", s.replace('\\', "\\\\").replace('"', "\\\""))
            }
            Expression::BinaryOperation(_, _, _) => todo!(),
            Expression::UnaryOperation(_, _) => todo!(),
            Expression::FunctionCall(_, _) => todo!(),
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
                if let Some(WitnessColumn { id, .. }) = self.committed.get(&poly.name) {
                    // Committed polynomial
                    if poly.next {
                        Some(if let Some(value) = self.next[*id] {
                            // We already computed the concrete value
                            value.into()
                        } else {
                            // We continue with a symbolic value
                            AffineExpression::from_committed_poly_value(*id)
                        })
                    } else {
                        // TODO make this work in case we have a constraint
                        // that does not contain a "next" on any witness columns.
                        // In that case, we can use this constraint to derive
                        // a value (from constants or already computed witnesses).
                        self.current[*id].map(|value| value.into())
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
