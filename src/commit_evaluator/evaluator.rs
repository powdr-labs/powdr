use crate::analyzer::{BinaryOperator, Expression, Identity, IdentityKind, UnaryOperator};
use crate::number::{abstract_to_degree, is_zero};
use crate::utils::indent;
use std::collections::{BTreeMap, HashMap};
// TODO should use finite field instead of abstract number
use crate::number::{AbstractNumberType, DegreeType};

use super::affine_expression::AffineExpression;
use super::eval_error::{self, EvalError};
use super::machine::{LookupReturn, Machine};
use super::{EvalResult, FixedData, WitnessColumn};

pub struct Evaluator<'a, QueryCallback>
where
    QueryCallback: FnMut(&'a str) -> Option<AbstractNumberType>,
{
    fixed_data: &'a FixedData<'a>,
    identities: Vec<&'a Identity>,
    machines: Vec<Box<dyn Machine>>,
    query_callback: Option<QueryCallback>,
    /// Maps the witness polynomial names to their IDs internal to this component
    /// and optional parameter and query string.
    witness_cols: BTreeMap<&'a String, &'a WitnessColumn<'a>>,
    witness_names: Vec<&'a String>,
    /// Values of the witness polynomials
    current: Vec<Option<AbstractNumberType>>,
    /// Values of the witness polynomials in the next row
    next: Vec<Option<AbstractNumberType>>,
    next_row: DegreeType,
    failure_reasons: Vec<String>,
    progress: bool,
}

#[derive(PartialEq, Eq, Clone, Copy)]
enum EvaluationRow {
    /// p is p[next_row - 1], p' is p[next_row]
    Current,
    /// p is p[next_row], p' is p[next_row + 1]
    Next,
    /// p is p[arg], p' is p[arg + 1]
    Specific(DegreeType),
}

impl<'a, QueryCallback> Evaluator<'a, QueryCallback>
where
    QueryCallback: FnMut(&str) -> Option<AbstractNumberType>,
{
    pub fn new(
        fixed_data: &'a FixedData<'a>,
        identities: Vec<&'a Identity>,
        machines: Vec<Box<dyn Machine>>,
        query_callback: Option<QueryCallback>,
    ) -> Self {
        let witness_cols = fixed_data.witness_cols;

        Evaluator {
            fixed_data,
            identities,
            machines,
            query_callback,
            witness_cols: witness_cols.iter().map(|p| (p.name, p)).collect(),
            witness_names: witness_cols.iter().map(|p| p.name).collect(),
            current: vec![None; witness_cols.len()],
            next: vec![None; witness_cols.len()],
            next_row: 0,
            failure_reasons: vec![],
            progress: true,
        }
    }

    pub fn compute_next_row(&mut self, next_row: DegreeType) -> Vec<AbstractNumberType> {
        self.next_row = next_row;

        // TODO maybe better to generate a dependency graph than looping multiple times.
        // TODO at least we could cache the affine expressions between loops.

        let mut identity_failed;
        loop {
            identity_failed = false;
            self.progress = false;
            self.failure_reasons.clear();

            // TODO avoid clone
            for identity in &self.identities.clone() {
                let result = match identity.kind {
                    IdentityKind::Polynomial => {
                        self.process_polynomial_identity(identity.left.selector.as_ref().unwrap())
                    }
                    IdentityKind::Plookup => self.process_plookup(identity),
                    _ => Err("Unsupported lookup type".to_string().into()),
                }
                .map_err(|err| {
                    format!(
                        "No progress on {identity}:\n{}",
                        indent(&format!("{err}"), "    ")
                    )
                    .into()
                });
                if result.is_err() {
                    identity_failed = true;
                }
                self.handle_eval_result(result);
            }
            if self.query_callback.is_some() {
                // TODO avoid clone
                for column in self.witness_cols.clone().values() {
                    // TOOD we should acutally query even if it is already known, to check
                    // if the value would be different.
                    if !self.has_known_next_value(column.id) && column.query.is_some() {
                        let result = self.process_witness_query(column);
                        self.handle_eval_result(result)
                    }
                }
            }
            if !self.progress {
                break;
            }
            if self.next.iter().all(|v| v.is_some()) {
                break;
            }
        }
        // Identity check failure on the first row is not fatal. We will proceed with
        // "unknown", report zero and re-check the wrap-around against the zero values at the end.
        if identity_failed && next_row != 0 {
            eprintln!(
                "\nError: Row {next_row}: Identity check failer or unable to derive values for witness polynomials: {}\n",
                self.next
                    .iter()
                    .enumerate()
                    .filter_map(|(i, v)| if v.is_none() {
                        Some(self.witness_names[i].clone())
                    } else {
                        None
                    })
                    .collect::<Vec<String>>()
                    .join(", ")
            );
            eprintln!("Reasons:\n{}\n", self.failure_reasons.join("\n\n"));
            eprintln!(
                "Current values:\n{}",
                indent(&self.format_next_values().join("\n"), "    ")
            );
            panic!();
        } else {
            if self.fixed_data.verbose {
                println!(
                    "===== Row {next_row}:\n{}",
                    indent(&self.format_next_values().join("\n"), "    ")
                );
            }
            std::mem::swap(&mut self.next, &mut self.current);
            self.next = vec![None; self.current.len()];
            // TODO check a bit better that "None" values do not
            // violate constraints.
            self.current
                .iter()
                .map(|v| v.clone().unwrap_or_default())
                .collect()
        }
    }

    pub fn machine_witness_col_values(&mut self) -> HashMap<String, Vec<AbstractNumberType>> {
        let mut result: HashMap<_, _> = Default::default();
        for m in &mut self.machines {
            result.extend(m.witness_col_values(self.fixed_data));
        }
        result
    }

    fn format_next_values(&self) -> Vec<String> {
        self.next
            .iter()
            .enumerate()
            .map(|(i, v)| {
                format!(
                    "{} = {}",
                    self.witness_names[i],
                    v.as_ref()
                        .map(format_number)
                        .unwrap_or("<unknown>".to_string())
                )
            })
            .collect()
    }

    fn process_witness_query(
        &mut self,
        column: &&WitnessColumn,
    ) -> Result<Vec<(usize, AbstractNumberType)>, EvalError> {
        let query = self.interpolate_query(column.query.unwrap())?;
        if let Some(value) = self.query_callback.as_mut().and_then(|c| (c)(&query)) {
            Ok(vec![(column.id, value)])
        } else {
            Err(format!(
                "No query answer for {} query: {query}.",
                self.witness_names[column.id]
            )
            .into())
        }
    }

    fn interpolate_query(&self, query: &Expression) -> Result<String, String> {
        if let Ok(v) = self.evaluate(query, EvaluationRow::Next) {
            if v.is_constant() {
                return Ok(self.format_affine_expression(&v));
            }
        }
        // TODO combine that with the constant evaluator and the commit evaluator...
        match query {
            Expression::Tuple(items) => Ok(items
                .iter()
                .map(|i| self.interpolate_query(i))
                .collect::<Result<Vec<_>, _>>()?
                .join(", ")),
            Expression::LocalVariableReference(i) => {
                assert!(*i == 0);
                Ok(format!("{}", self.next_row))
            }
            Expression::String(s) => Ok(format!(
                "\"{}\"",
                s.replace('\\', "\\\\").replace('"', "\\\"")
            )),
            _ => Err(format!("Cannot handle / evaluate {query}")),
        }
    }

    fn process_polynomial_identity(&self, identity: &Expression) -> EvalResult {
        // If there is no "next" reference in the expression,
        // we just evaluate it directly on the "next" row.
        let row = if self.contains_next_ref(identity) {
            EvaluationRow::Current
        } else {
            EvaluationRow::Next
        };
        let evaluated = self.evaluate(identity, row)?;
        if evaluated.constant_value() == Some(0.into()) {
            Ok(vec![])
        } else {
            match evaluated.solve() {
                Some((id, value)) => Ok(vec![(id, value)]),
                None => {
                    let formatted = self.format_affine_expression(&evaluated);
                    Err(if evaluated.is_invalid() {
                        format!("Constraint is invalid ({formatted} != 0).").into()
                    } else {
                        format!("Could not solve expression {formatted} = 0.").into()
                    })
                }
            }
        }
    }

    fn process_plookup(&mut self, identity: &Identity) -> EvalResult {
        if let Some(left_selector) = &identity.left.selector {
            let value = self.evaluate(left_selector, EvaluationRow::Next)?;
            match value.constant_value() {
                Some(v) if v == 0.into() => {
                    return Ok(vec![]);
                }
                Some(v) if v == 1.into() => {}
                _ => {
                    return Err(format!(
                        "Value of the selector on the left hand side unknown or not boolean: {}",
                        self.format_affine_expression(&value)
                    )
                    .into())
                }
            };
        }
        if identity.right.selector.is_some() {
            return Err("Selectors at the RHS not yet supported.".to_string().into());
        }

        let mut reasons = vec![];
        let left = identity
            .left
            .expressions
            .iter()
            .map(|e| self.evaluate(e, EvaluationRow::Next))
            .map(|e| match e {
                Ok(r) => Some(r),
                Err(e) => {
                    reasons.push(e);
                    None
                }
            })
            .collect::<Vec<_>>();

        // TODO: We need to call the machine even if the LHS is already known.
        // But then we also need a way to flag "progress" and we also need
        // always call the machine, even if all witnesses are already known.

        // Try to see if it's a query to a machine.
        for m in &mut self.machines {
            // TODO also consider the reasons above.
            if let LookupReturn::Assignments(assignments) =
                m.process_plookup(self.fixed_data, &left, &identity.right)?
            {
                return Ok(assignments);
            }
        }

        // If we already know the LHS, skip it.
        if left
            .iter()
            .all(|v| v.is_some() && v.as_ref().unwrap().is_constant())
        {
            return Ok(vec![]);
        }

        // TODO turn the rest of this code into a specialized machine that does
        // lookups inside constants.

        // TODO this ignores the error in `reasons` above.
        let left_key = match left[0].clone() {
            Some(v) => match v.constant_value() {
                Some(v) => Ok(v),
                None => Err(format!(
                    "First expression needs to be constant but is not: {}.",
                    self.format_affine_expression(&v)
                )),
            },
            // TODO this ignores the error in `reasons` above.
            None => Err("First expression on the LHS is unknown.".to_string()),
        }?;

        let right_key = identity.right.expressions.first().unwrap();
        let rhs_row = if let Expression::PolynomialReference(poly) = right_key {
            // TODO we really need a search index on this.
            self.fixed_data.fixed_cols
                .get(&poly.name)
                .and_then(|values| values.iter().position(|v| *v == left_key))
                .ok_or_else(|| {
                    format!(
                        "Unable to find matching row on the RHS where the first element is {left_key} - only fixed columns supported there."
                    )
                })
                .map(|i| i as DegreeType)
        } else {
            Err("First item on the RHS must be a polynomial reference.".to_string())
        }?;

        // TODO we only support the following case:
        // - The first component on the LHS has to be known
        // - The first component on the RHS has to be a direct fixed column reference
        // - The first match of those uniquely determines the rest of the RHS.

        //TODO there should be a shortcut to succeed if any of an iterator is "Ok" and combine the errors otherwise.
        let mut result = vec![];
        for (l, r) in identity
            .left
            .expressions
            .iter()
            .zip(&identity.right.expressions)
            .skip(1)
        {
            match self.equate_to_constant_rhs(l, r, rhs_row) {
                Ok(assignments) => result.extend(assignments),
                Err(err) => reasons.push(err),
            }
        }
        if result.is_empty() {
            Err(reasons.into_iter().reduce(eval_error::combine).unwrap())
        } else {
            Ok(result)
        }
    }

    fn equate_to_constant_rhs(
        &self,
        l: &Expression,
        r: &Expression,
        rhs_row: DegreeType,
    ) -> EvalResult {
        // This needs to be a costant because symbolic variables
        // would reference a different row!
        let r = self
            .evaluate(r, EvaluationRow::Specific(rhs_row))
            .and_then(|r| {
                r.constant_value().ok_or_else(|| {
                    format!(
                        "Constant value required: {}",
                        self.format_affine_expression(&r)
                    )
                    .into()
                })
            })?;

        let evaluated = self.evaluate(l, EvaluationRow::Next)? - r.into();
        match evaluated.solve() {
            Some((id, value)) => Ok(vec![(id, value)]),
            None => {
                // TODO somehow also add `l` and `r` to the error message.
                let formatted = self.format_affine_expression(&evaluated);
                Err(if evaluated.is_invalid() {
                    format!("Constraint is invalid ({formatted} != 0).").into()
                } else {
                    format!("Could not solve expression {formatted} = 0.").into()
                })
            }
        }
    }

    fn handle_eval_result(&mut self, result: EvalResult) {
        match result {
            Ok(assignments) => {
                for (id, value) in assignments {
                    self.next[id] = Some(value);
                    self.progress = true;
                }
            }
            Err(reason) => {
                self.failure_reasons.push(format!("{reason}"));
            }
        }
    }

    fn has_known_next_value(&self, id: usize) -> bool {
        self.next[id].is_some()
    }

    /// Tries to evaluate the expression to an expression affine in the witness polynomials,
    /// taking current values of polynomials into account.
    /// @returns an expression affine in the witness polynomials
    fn evaluate(
        &self,
        expr: &Expression,
        row: EvaluationRow,
    ) -> Result<AffineExpression, EvalError> {
        // @TODO if we iterate on processing the constraints in the same row,
        // we could store the simplified values.
        match expr {
            Expression::Constant(name) => Ok(self.fixed_data.constants[name].clone().into()),
            Expression::PolynomialReference(poly) => {
                // TODO arrays
                if let Some(WitnessColumn { id, .. }) = self.witness_cols.get(&poly.name) {
                    // Witness polynomial
                    if !poly.next && row == EvaluationRow::Current {
                        // All values in the "current" row should usually be known.
                        // The exception is when we start the analysis on the first row.
                        self.current[*id]
                            .as_ref()
                            .map(|value| value.clone().into())
                            .ok_or_else(|| EvalError::PreviousValueUnknown(poly.name.clone()))
                    } else if (poly.next && row == EvaluationRow::Current)
                        || (!poly.next && row == EvaluationRow::Next)
                    {
                        Ok(if let Some(value) = self.next[*id].clone() {
                            // We already computed the concrete value
                            value.into()
                        } else {
                            // We continue with a symbolic value
                            AffineExpression::from_wittness_poly_value(*id)
                        })
                    } else {
                        // "double next" or evaluation of a witness on a specific row
                        Err(format!(
                            "{}' references the next-next row when evaluating on the current row.",
                            self.witness_names[*id]
                        )
                        .into())
                    }
                } else {
                    // Constant polynomial (or something else)
                    let values = self.fixed_data.fixed_cols[&poly.name];
                    let degree = values.len() as DegreeType;
                    let mut row = match row {
                        EvaluationRow::Current => (self.next_row + degree - 1) % degree,
                        EvaluationRow::Next => self.next_row,
                        EvaluationRow::Specific(r) => r,
                    };
                    if poly.next {
                        row = (row + 1) % degree;
                    }
                    Ok(values[row as usize].clone().into())
                }
            }
            Expression::Number(n) => Ok(n.clone().into()),
            Expression::BinaryOperation(left, op, right) => {
                self.evaluate_binary_operation(left, op, right, row)
            }
            Expression::UnaryOperation(op, expr) => self.evaluate_unary_operation(op, expr, row),
            Expression::Tuple(_) => Err("Tuple not implemented.".to_string().into()),
            Expression::String(_) => Err("String not implemented.".to_string().into()),
            Expression::LocalVariableReference(_) => {
                Err("Local variable references not implemented."
                    .to_string()
                    .into())
            }
            Expression::PublicReference(_) => {
                Err("Public references not implemented.".to_string().into())
            }
            Expression::FunctionCall(_, _) => {
                Err("Function calls not implemented.".to_string().into())
            }
        }
    }

    fn evaluate_binary_operation(
        &self,
        left: &Expression,
        op: &BinaryOperator,
        right: &Expression,
        row: EvaluationRow,
    ) -> Result<AffineExpression, EvalError> {
        match (self.evaluate(left, row), self.evaluate(right, row)) {
            (Ok(left), Ok(right)) => match op {
                BinaryOperator::Add => Ok(left + right),
                BinaryOperator::Sub => Ok(left - right),
                BinaryOperator::Mul => {
                    if let Some(f) = left.constant_value() {
                        Ok(right.mul(f))
                    } else if let Some(f) = right.constant_value() {
                        Ok(left.mul(f))
                    } else {
                        Err(format!(
                            "Multiplication of two non-constants: ({}) * ({})",
                            self.format_affine_expression(&left),
                            self.format_affine_expression(&right)
                        )
                        .into())
                    }
                }
                BinaryOperator::Div => {
                    if let (Some(l), Some(r)) = (left.constant_value(), right.constant_value()) {
                        // TODO Maybe warn about division by zero here.
                        if l == 0.into() {
                            Ok(0.into())
                        } else {
                            // TODO We have to do division in the proper field.
                            Ok((l / r).into())
                        }
                    } else {
                        Err(format!(
                            "Division of two non-constants: ({}) / ({})",
                            self.format_affine_expression(&left),
                            self.format_affine_expression(&right)
                        )
                        .into())
                    }
                }
                BinaryOperator::Pow => {
                    if let (Some(l), Some(r)) = (left.constant_value(), right.constant_value()) {
                        Ok(l.pow(abstract_to_degree(&r) as u32).into())
                    } else {
                        Err(format!(
                            "Pow of two non-constants: ({}) ** ({})",
                            self.format_affine_expression(&left),
                            self.format_affine_expression(&right)
                        )
                        .into())
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
                            BinaryOperator::ShiftLeft => left << abstract_to_degree(&right),
                            BinaryOperator::ShiftRight => left >> abstract_to_degree(&right),
                            _ => panic!(),
                        };
                        Ok(result.into())
                    } else {
                        panic!()
                    }
                }
            },
            (Ok(_), Err(reason)) | (Err(reason), Ok(_)) => Err(reason),
            (Err(r1), Err(r2)) => Err(eval_error::combine(r1, r2)),
        }
    }

    fn evaluate_unary_operation(
        &self,
        op: &UnaryOperator,
        expr: &Expression,
        row: EvaluationRow,
    ) -> Result<AffineExpression, EvalError> {
        self.evaluate(expr, row).map(|v| match op {
            UnaryOperator::Plus => v,
            UnaryOperator::Minus => -v,
        })
    }

    /// @returns true if the expression contains a reference to a next value of a witness column.
    fn contains_next_ref(&self, expr: &Expression) -> bool {
        match expr {
            Expression::PolynomialReference(poly) => {
                poly.next && self.witness_cols.contains_key(&poly.name)
            }
            Expression::Tuple(items) => items.iter().any(|e| self.contains_next_ref(e)),
            Expression::BinaryOperation(l, _, r) => {
                self.contains_next_ref(l) || self.contains_next_ref(r)
            }
            Expression::UnaryOperation(_, e) => self.contains_next_ref(e),
            Expression::FunctionCall(_, args) => args.iter().any(|e| self.contains_next_ref(e)),
            Expression::Constant(_)
            | Expression::LocalVariableReference(_)
            | Expression::PublicReference(_)
            | Expression::Number(_)
            | Expression::String(_) => false,
        }
    }

    fn format_affine_expression(&self, e: &AffineExpression) -> String {
        e.coefficients
            .iter()
            .enumerate()
            .filter(|(_, c)| !is_zero(c))
            .map(|(i, c)| {
                let name = self.witness_names[i];
                if *c == 1.into() {
                    name.clone()
                } else if *c == (-1).into() {
                    format!("-{name}")
                } else {
                    format!("{} * {name}", format_number(c))
                }
            })
            .chain(e.constant_value().map(|v| format!("{v}")))
            .collect::<Vec<_>>()
            .join(" + ")
    }
}

const GOLDILOCKS_MOD: u64 = 0xffffffff00000001u64;

fn format_number(x: &AbstractNumberType) -> String {
    if *x > (GOLDILOCKS_MOD / 2).into() {
        format!("{}", GOLDILOCKS_MOD - x)
    } else {
        format!("{x}")
    }
}
