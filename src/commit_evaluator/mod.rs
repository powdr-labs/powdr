use std::collections::{BTreeMap, HashMap};

use crate::analyzer::{
    Analyzed, BinaryOperator, Expression, FunctionValueDefinition, Identity, IdentityKind,
    UnaryOperator,
};
// TODO should use finite field instead of abstract number
use crate::number::{abstract_to_degree, is_zero, AbstractNumberType, DegreeType};

mod affine_expression;

use affine_expression::AffineExpression;

/// Generates the committed polynomial values
/// @returns the values (in source order) and the degree of the polynomials.
pub fn generate<'a>(
    analyzed: &'a Analyzed,
    degree: &DegreeType,
    constants: &[(&String, Vec<AbstractNumberType>)],
    query_callback: Option<impl FnMut(&str) -> Option<AbstractNumberType>>,
) -> Vec<(&'a String, Vec<AbstractNumberType>)> {
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
    let mut values: Vec<(&String, Vec<AbstractNumberType>)> =
        polys.iter().map(|p| (p.name, Vec::new())).collect();
    let mut evaluator = Evaluator::new(analyzed, constants, &polys, query_callback);
    for row in 0..*degree as DegreeType {
        let row_values = evaluator.compute_next_row(row);
        for (col, v) in row_values.into_iter().enumerate() {
            values[col].1.push(v);
        }
    }
    for (col, v) in evaluator.compute_next_row(0).into_iter().enumerate() {
        if v != values[col].1[0] {
            eprintln!("Wrap-around value for column {} does not match: {} (wrap-around) vs. {} (first row).",
            polys[col].name, v, values[col].1[0]);
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

type EvalResult = Result<Vec<(usize, AbstractNumberType)>, String>;

struct Evaluator<'a, QueryCallback>
where
    QueryCallback: FnMut(&'a str) -> Option<AbstractNumberType>,
{
    analyzed: &'a Analyzed,
    constants: HashMap<&'a String, &'a Vec<AbstractNumberType>>,
    query_callback: Option<QueryCallback>,
    /// Maps the committed polynomial names to their IDs internal to this component
    /// and optional parameter and query string.
    committed: BTreeMap<&'a String, &'a WitnessColumn<'a>>,
    committed_names: Vec<&'a String>,
    /// Values of the committed polynomials
    current: Vec<Option<AbstractNumberType>>,
    /// Values of the committed polynomials in the next row
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

// This could have more structure in the future.
type EvalError = String;

impl<'a, QueryCallback> Evaluator<'a, QueryCallback>
where
    QueryCallback: FnMut(&str) -> Option<AbstractNumberType>,
{
    pub fn new(
        analyzed: &'a Analyzed,
        constants: &'a [(&String, Vec<AbstractNumberType>)],
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

            if self.query_callback.is_some() {
                // TODO avoid clone
                for column in self.committed.clone().values() {
                    if !self.has_known_next_value(column.id) && column.query.is_some() {
                        let result = self.process_witness_query(column);
                        self.handle_eval_result(result)
                    }
                }
            }
            for identity in &self.analyzed.identities {
                let result = match identity.kind {
                    IdentityKind::Polynomial => {
                        self.process_polynomial_identity(identity.left.selector.as_ref().unwrap())
                    }
                    IdentityKind::Plookup => self.process_plookup(identity),
                    _ => Ok(vec![]),
                }
                .map_err(|err| format!("No progress on {identity}:\n    {err}"));
                if result.is_err() {
                    identity_failed = true;
                }
                self.handle_eval_result(result);
            }
            if !self.progress {
                break;
            }
            if self.next.iter().all(|v| v.is_some()) {
                // let values = self
                //     .next
                //     .iter()
                //     .enumerate()
                //     .map(|(i, v)| format!("{} = {}", self.committed_names[i], v.unwrap()))
                //     .collect::<Vec<_>>()
                //     .join(", ");
                // println!("Row {next_row}: {values}");
                break;
            }
        }
        //println!("\n\n================================\n");
        if identity_failed && self.next.iter().any(|v| v.is_none()) {
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
            eprintln!("Reasons: {}", self.failure_reasons.join("\n\n"));
            eprintln!(
                "Current values:\n{}",
                self.next
                    .iter()
                    .enumerate()
                    .map(|(i, v)| format!(
                        "{} = {}",
                        self.committed_names[i],
                        v.as_ref()
                            .map(format_number)
                            .unwrap_or("<unknown>".to_string())
                    ))
                    .collect::<Vec<_>>()
                    .join("\n")
            );
            panic!();
        } else {
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

    fn process_witness_query(
        &mut self,
        column: &&WitnessColumn,
    ) -> Result<Vec<(usize, AbstractNumberType)>, String> {
        let query = self.interpolate_query(column.query.unwrap())?;
        if let Some(value) = self.query_callback.as_mut().unwrap()(&query) {
            Ok(vec![(column.id, value)])
        } else {
            Err(format!(
                "No query answer for {} query: {query}.",
                self.committed_names[column.id]
            ))
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

    fn process_polynomial_identity(&mut self, identity: &Expression) -> EvalResult {
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
                        format!("Constraint is invalid ({formatted} != 0).")
                    } else {
                        format!("Could not solve expression {formatted} = 0.")
                    })
                }
            }
        }
    }

    fn process_plookup(&mut self, identity: &Identity) -> EvalResult {
        if identity.left.selector.is_some() || identity.right.selector.is_some() {
            return Err("Selectors not yet supported.".to_string());
        }
        let left = identity
            .left
            .expressions
            .iter()
            .map(|e| self.evaluate(e, EvaluationRow::Next))
            .collect::<Vec<_>>();
        // If we already know the LHS, skip it.
        if left
            .iter()
            .all(|v| v.is_ok() && v.as_ref().unwrap().is_constant())
        {
            return Ok(vec![]);
        }

        let left_key = left[0].clone().and_then(|v| match v.constant_value() {
            Some(v) => Ok(v),
            None => Err(format!(
                "First expression needs to be constant but is not: {}.",
                self.format_affine_expression(&v)
            )),
        })?;

        let right_key = identity.right.expressions.first().unwrap();
        let rhs_row = if let Expression::PolynomialReference(poly) = right_key {
            // TODO we really need a search index on this.
            self.constants
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
        let mut reasons = vec![];
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
            Err(reasons.join(", "))
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
        let r = self
            .evaluate(r, EvaluationRow::Specific(rhs_row))
            .and_then(|r| {
                r.constant_value().ok_or_else(|| {
                    format!(
                        "Constant value required: {}",
                        self.format_affine_expression(&r)
                    )
                })
            })?;

        let expr = Expression::BinaryOperation(
            Box::new(l.clone()),
            BinaryOperator::Sub,
            Box::new(Expression::Number(r)),
        );
        let evaluated = self.evaluate(&expr, EvaluationRow::Next)?;
        match evaluated.solve() {
            Some((id, value)) => Ok(vec![(id, value)]),
            None => match evaluated.solve() {
                Some((id, value)) => Ok(vec![(id, value)]),
                None => {
                    // TODO somehow also add `l` and `r` to the error message.
                    let formatted = self.format_affine_expression(&evaluated);
                    Err(if evaluated.is_invalid() {
                        format!("Constraint is invalid ({formatted} != 0).")
                    } else {
                        format!("Could not solve expression {formatted} = 0.")
                    })
                }
            },
        }
    }

    fn handle_eval_result(&mut self, result: EvalResult) {
        match result {
            Ok(assignments) => {
                for (id, value) in assignments {
                    //println!("{} = {value}", self.committed_names[id]);
                    self.next[id] = Some(value);
                    self.progress = true;
                }
            }
            Err(reason) => {
                self.failure_reasons.push(reason);
            }
        }
    }

    fn has_known_next_value(&self, id: usize) -> bool {
        self.next[id].is_some()
    }

    /// Tries to evaluate the expression to an expression affine in the committed polynomials,
    /// taking current values of polynomials into account.
    /// @returns an expression affine in the committed polynomials
    fn evaluate(
        &self,
        expr: &Expression,
        row: EvaluationRow,
    ) -> Result<AffineExpression, EvalError> {
        // @TODO if we iterate on processing the constraints in the same row,
        // we could store the simplified values.
        match expr {
            Expression::Constant(name) => Ok(self.analyzed.constants[name].clone().into()),
            Expression::PolynomialReference(poly) => {
                // TODO arrays
                if let Some(WitnessColumn { id, .. }) = self.committed.get(&poly.name) {
                    // Committed polynomial
                    if !poly.next && row == EvaluationRow::Current {
                        // All values in the "current" row should usually be known.
                        // The exception is when we start the analysis on the first row.
                        self.current[*id]
                            .as_ref()
                            .map(|value| value.clone().into())
                            .ok_or_else(|| {
                                format!("Previous value of column {} not yet known.", poly.name)
                            })
                    } else if (poly.next && row == EvaluationRow::Current)
                        || (!poly.next && row == EvaluationRow::Next)
                    {
                        Ok(if let Some(value) = self.next[*id].clone() {
                            // We already computed the concrete value
                            value.into()
                        } else {
                            // We continue with a symbolic value
                            AffineExpression::from_committed_poly_value(*id)
                        })
                    } else {
                        // "double next" or evaluation of a witness on a specific row
                        Err(format!(
                            "{}' references the next-next row when evaluating on the current row.",
                            self.committed_names[*id]
                        ))
                    }
                } else {
                    // Constant polynomial (or something else)
                    let values = self.constants[&poly.name];
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
            Expression::Tuple(_) => Err("Tuple not implemented.".to_string()),
            Expression::String(_) => Err("String not implemented.".to_string()),
            Expression::LocalVariableReference(_) => {
                Err("Local variable references not implemented.".to_string())
            }
            Expression::PublicReference(_) => Err("Public references not implemented.".to_string()),
            Expression::FunctionCall(_, _) => Err("Function calls not implemented.".to_string()),
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
                        ))
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
                        ))
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
                        ))
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
            (Err(r1), Err(r2)) => Err(format!("{r1}, {r2}")),
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
                poly.next && self.committed.contains_key(&poly.name)
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
                let name = self.committed_names[i];
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
