use crate::analyzer::{Expression, Identity, IdentityKind};
use crate::number::format_number;
use crate::utils::indent;
use std::collections::{BTreeMap, HashMap};
// TODO should use finite field instead of abstract number
use crate::number::{AbstractNumberType, DegreeType};

use super::affine_expression::AffineExpression;
use super::bit_constraints::{BitConstraint, BitConstraintSet};
use super::eval_error::EvalError;
use super::expression_evaluator::ExpressionEvaluator;
use super::machines::{FixedLookup, Machine};
use super::symbolic_witness_evaluator::{SymoblicWitnessEvaluator, WitnessColumnEvaluator};
use super::util::{contains_next_witness_ref, WitnessColumnNamer};
use super::{Constraint, EvalResult, FixedData, WitnessColumn};

pub struct Generator<'a, QueryCallback>
where
    QueryCallback: FnMut(&'a str) -> Option<AbstractNumberType>,
{
    fixed_lookup: &'a mut FixedLookup<'a>,
    identities: Vec<&'a Identity>,
    machines: Vec<Box<dyn Machine>>,
    query_callback: Option<QueryCallback>,
    global_bit_constraints: BTreeMap<&'a str, BitConstraint>,
    /// Maps the witness polynomial names to optional parameter and query string.
    witness_cols: BTreeMap<&'a str, &'a WitnessColumn<'a>>,
    /// Values of the witness polynomials
    current: Vec<Option<AbstractNumberType>>,
    /// Values of the witness polynomials in the next row
    next: Vec<Option<AbstractNumberType>>,
    /// Bit constraints on the witness polynomials in the next row.
    next_bit_constraints: Vec<Option<BitConstraint>>,
    next_row: DegreeType,
    failure_reasons: Vec<String>,
    progress: bool,
    last_report: DegreeType,
}

#[derive(PartialEq, Eq, Clone, Copy)]
enum EvaluationRow {
    /// p is p[next_row - 1], p' is p[next_row]
    Current,
    /// p is p[next_row], p' is p[next_row + 1]
    Next,
}

impl<'a, QueryCallback> Generator<'a, QueryCallback>
where
    QueryCallback: FnMut(&str) -> Option<AbstractNumberType>,
{
    pub fn new(
        fixed_lookup: &'a mut FixedLookup<'a>,
        identities: Vec<&'a Identity>,
        global_bit_constraints: BTreeMap<&'a str, BitConstraint>,
        machines: Vec<Box<dyn Machine>>,
        query_callback: Option<QueryCallback>,
    ) -> Self {
        let witness_cols = fixed_lookup.data.witness_cols;

        Generator {
            fixed_lookup,
            identities,
            machines,
            query_callback,
            global_bit_constraints,
            witness_cols: witness_cols.iter().map(|p| (p.name, p)).collect(),
            current: vec![None; witness_cols.len()],
            next: vec![None; witness_cols.len()],
            next_bit_constraints: vec![None; witness_cols.len()],
            next_row: 0,
            failure_reasons: vec![],
            progress: true,
            last_report: 0,
        }
    }

    pub fn compute_next_row(&mut self, next_row: DegreeType) -> Vec<AbstractNumberType> {
        if next_row >= self.last_report + 1000 {
            println!(
                "{next_row} of {} rows ({} %)",
                self.fixed_lookup.data.degree,
                next_row * 100 / self.fixed_lookup.data.degree
            );
            self.last_report = next_row;
        }
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
                    IdentityKind::Plookup | IdentityKind::Permutation => {
                        self.process_plookup(identity)
                    }
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
                    // TODO we should actually query even if it is already known, to check
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
                "\nError: Row {next_row}: Identity check failed or unable to derive values for witness polynomials: {}\n",
                self.next
                    .iter()
                    .enumerate()
                    .filter_map(|(i, v)| if v.is_none() {
                        Some(self.fixed_lookup.data.witness_cols[i].name.to_string())
                    } else {
                        None
                    })
                    .collect::<Vec<String>>()
                    .join(", ")
            );
            eprintln!("Reasons:\n{}\n", self.failure_reasons.join("\n\n"));
            eprintln!("Known bit constraints:");
            eprintln!("Global:");
            for (name, cons) in &self.global_bit_constraints {
                eprintln!("  {name}: {cons}");
            }
            eprintln!("For this row:");
            for (id, cons) in self.next_bit_constraints.iter().enumerate() {
                if let Some(cons) = cons {
                    eprintln!("  {}: {cons}", self.fixed_lookup.data.witness_cols[id].name);
                }
            }
            eprintln!();
            eprintln!(
                "Current values:\n{}",
                indent(&self.format_next_values().join("\n"), "    ")
            );
            panic!();
        } else {
            if self.fixed_lookup.data.verbose {
                println!(
                    "===== Row {next_row}:\n{}",
                    indent(&self.format_next_values().join("\n"), "    ")
                );
            }
            std::mem::swap(&mut self.next, &mut self.current);
            self.next = vec![None; self.current.len()];
            self.next_bit_constraints = vec![None; self.current.len()];
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
            result.extend(m.witness_col_values(self.fixed_lookup.data));
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
                    AffineExpression::from_witness_poly_value(i).format(self.fixed_lookup.data),
                    v.as_ref()
                        .map(format_number)
                        .unwrap_or("<unknown>".to_string())
                )
            })
            .collect()
    }

    fn process_witness_query(&mut self, column: &&WitnessColumn) -> EvalResult {
        let query = self.interpolate_query(column.query.unwrap())?;
        if let Some(value) = self.query_callback.as_mut().and_then(|c| (c)(&query)) {
            Ok(vec![(column.id, Constraint::Assignment(value))])
        } else {
            Err(format!("No query answer for {} query: {query}.", column.name).into())
        }
    }

    fn interpolate_query(&self, query: &Expression) -> Result<String, String> {
        if let Ok(v) = self.evaluate(query, EvaluationRow::Next) {
            if v.is_constant() {
                return Ok(v.format(self.fixed_lookup.data));
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
        let row = if contains_next_witness_ref(identity, self.fixed_lookup.data) {
            // TODO this is the only situation where we use "current"
            // TODO this is the only that actually uses a window.
            EvaluationRow::Current
        } else {
            EvaluationRow::Next
        };
        let evaluated = self.evaluate(identity, row)?;
        if evaluated.constant_value() == Some(0.into()) {
            Ok(vec![])
        } else {
            evaluated
                .solve_with_bit_constraints(&self.bit_constraint_set())
                .map_err(|e| {
                    let formatted = evaluated.format(self.fixed_lookup.data);
                    if let EvalError::ConstraintUnsatisfiable(_) = e {
                        EvalError::ConstraintUnsatisfiable(format!(
                            "Constraint is invalid ({formatted} != 0)."
                        ))
                    } else {
                        format!("Could not solve expression {formatted} = 0.").into()
                    }
                })
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
                        value.format(self.fixed_lookup.data)
                    )
                    .into())
                }
            };
        }

        let left = identity
            .left
            .expressions
            .iter()
            .map(|e| self.evaluate(e, EvaluationRow::Next))
            .collect::<Vec<_>>();

        // Now query the machines.
        // Note that we should always query all machines that match, because they might
        // update their internal data, even if all values are already known.
        // TODO could it be that multiple machines match?

        // query the fixed lookup "machine"
        if let Some(result) =
            self.fixed_lookup
                .process_plookup(identity.kind, &left, &identity.right)
        {
            return result;
        }

        for m in &mut self.machines {
            // TODO also consider the reasons above.
            if let Some(result) =
                m.process_plookup(self.fixed_lookup, identity.kind, &left, &identity.right)
            {
                return result;
            }
        }

        Err("Could not find a matching machine for the lookup."
            .to_string()
            .into())
    }

    fn handle_eval_result(&mut self, result: EvalResult) {
        match result {
            Ok(constraints) => {
                if !constraints.is_empty() {
                    self.progress = true;
                }
                for (id, c) in constraints {
                    match c {
                        Constraint::Assignment(value) => {
                            self.next[id] = Some(value);
                        }
                        Constraint::BitConstraint(cons) => {
                            self.next_bit_constraints[id] = Some(cons);
                        }
                    }
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
        evaluate_row: EvaluationRow,
    ) -> Result<AffineExpression, EvalError> {
        let degree = self.fixed_lookup.data.degree;
        let fixed_row = match evaluate_row {
            EvaluationRow::Current => (self.next_row + degree - 1) % degree,
            EvaluationRow::Next => self.next_row,
        };

        ExpressionEvaluator::new(SymoblicWitnessEvaluator::new(
            self.fixed_lookup.data,
            fixed_row,
            EvaluationData {
                fixed_data: self.fixed_lookup.data,
                current_witnesses: &self.current,
                next_witnesses: &self.next,
                evaluate_row,
            },
        ))
        .evaluate(expr)
    }

    fn bit_constraint_set(&self) -> WitnessBitConstraintSet {
        WitnessBitConstraintSet {
            fixed_data: self.fixed_lookup.data,
            global_bit_constraints: &self.global_bit_constraints,
            next_bit_constraints: &self.next_bit_constraints,
        }
    }
}

struct WitnessBitConstraintSet<'a> {
    fixed_data: &'a FixedData<'a>,
    /// Global constraints on witness and fixed polynomials.
    global_bit_constraints: &'a BTreeMap<&'a str, BitConstraint>,
    /// Bit constraints on the witness polynomials in the next row.
    next_bit_constraints: &'a Vec<Option<BitConstraint>>,
}

impl<'a> BitConstraintSet for WitnessBitConstraintSet<'a> {
    fn bit_constraint(&self, id: usize) -> Option<BitConstraint> {
        let name = self.fixed_data.witness_cols[id].name;
        self.global_bit_constraints
            .get(name)
            .or_else(|| self.next_bit_constraints[id].as_ref())
            .cloned()
    }
}

struct EvaluationData<'a> {
    pub fixed_data: &'a FixedData<'a>,
    /// Values of the witness polynomials in the current / last row
    pub current_witnesses: &'a Vec<Option<AbstractNumberType>>,
    /// Values of the witness polynomials in the next row
    pub next_witnesses: &'a Vec<Option<AbstractNumberType>>,
    pub evaluate_row: EvaluationRow,
}

impl<'a> WitnessColumnEvaluator for EvaluationData<'a> {
    fn value(&self, name: &str, next: bool) -> Result<AffineExpression, EvalError> {
        let id = self.fixed_data.witness_ids[name];
        match (next, self.evaluate_row) {
            (false, EvaluationRow::Current) => {
                // All values in the "current" row should usually be known.
                // The exception is when we start the analysis on the first row.
                self.current_witnesses[id]
                    .as_ref()
                    .map(|value| value.clone().into())
                    .ok_or_else(|| EvalError::PreviousValueUnknown(name.to_string()))
            }
            (false, EvaluationRow::Next) | (true, EvaluationRow::Current) => {
                Ok(if let Some(value) = &self.next_witnesses[id] {
                    // We already computed the concrete value
                    value.clone().into()
                } else {
                    // We continue with a symbolic value
                    AffineExpression::from_witness_poly_value(id)
                })
            }
            (true, EvaluationRow::Next) => {
                // "double next" or evaluation of a witness on a specific row
                Err(format!(
                    "{name}' references the next-next row when evaluating on the current row.",
                )
                .into())
            }
        }
    }
}

impl<'a> WitnessColumnNamer for EvaluationData<'a> {
    fn name(&self, i: usize) -> String {
        self.fixed_data.name(i)
    }
}
