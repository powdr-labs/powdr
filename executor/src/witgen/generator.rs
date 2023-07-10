use ast::analyzed::{Expression, Identity, IdentityKind, PolynomialReference};
use itertools::Itertools;
use number::{DegreeType, FieldElement};
use parser_util::lines::indent;
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::time::Instant;

use super::affine_expression::{AffineExpression, AffineResult};
use super::range_constraints::{RangeConstraint, RangeConstraintSet};

use super::expression_evaluator::ExpressionEvaluator;
use super::machines::{FixedLookup, Machine};
use super::symbolic_witness_evaluator::{SymoblicWitnessEvaluator, WitnessColumnEvaluator};
use super::{Constraint, EvalResult, EvalValue, FixedData, IncompleteCause, WitnessColumn};

pub struct Generator<'a, T: FieldElement, QueryCallback: Send + Sync> {
    fixed_data: &'a FixedData<'a, T>,
    fixed_lookup: &'a mut FixedLookup<T>,
    identities: &'a [&'a Identity<T>],
    witnesses: BTreeSet<&'a PolynomialReference>,
    machines: Vec<Box<dyn Machine<T>>>,
    query_callback: Option<QueryCallback>,
    global_range_constraints: BTreeMap<&'a PolynomialReference, RangeConstraint<T>>,
    /// Values of the witness polynomials
    current: Vec<Option<T>>,
    /// Values of the witness polynomials in the next row
    next: Vec<Option<T>>,
    /// Range constraints on the witness polynomials in the next row.
    next_range_constraints: Vec<Option<RangeConstraint<T>>>,
    next_row: DegreeType,
    failure_reasons: Vec<String>,
    last_report: DegreeType,
    last_report_time: Instant,
}

#[derive(PartialEq, Eq, Clone, Copy)]
enum EvaluationRow {
    /// p is p[next_row - 1], p' is p[next_row]
    Current,
    /// p is p[next_row], p' is p[next_row + 1]
    Next,
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
enum SolvingStrategy {
    /// Only solve expressions that are affine in a single variable
    /// (and use range constraints).
    SingleVariableAffine,
    /// Assume that all unknown values are zero and check that this does not generate
    /// a conflict (but do not store the values as fixed zero to avoid relying on nondeterminism).
    AssumeZero,
}

#[derive(PartialEq, Eq, Clone, Copy)]
enum EvaluateUnknown {
    Symbolic,
    AssumeZero,
}

impl<'a, T: FieldElement, QueryCallback> Generator<'a, T, QueryCallback>
where
    QueryCallback: FnMut(&str) -> Option<T> + Send + Sync,
{
    pub fn new(
        fixed_data: &'a FixedData<'a, T>,
        fixed_lookup: &'a mut FixedLookup<T>,
        identities: &'a [&'a Identity<T>],
        witnesses: BTreeSet<&'a PolynomialReference>,
        global_range_constraints: BTreeMap<&'a PolynomialReference, RangeConstraint<T>>,
        machines: Vec<Box<dyn Machine<T>>>,
        query_callback: Option<QueryCallback>,
    ) -> Self {
        let witness_cols_len = fixed_data.witness_cols.len();

        Generator {
            fixed_data,
            fixed_lookup,
            identities,
            witnesses,
            machines,
            query_callback,
            global_range_constraints,
            current: vec![None; witness_cols_len],
            next: vec![None; witness_cols_len],
            next_range_constraints: vec![None; witness_cols_len],
            next_row: 0,
            failure_reasons: vec![],
            last_report: 0,
            last_report_time: Instant::now(),
        }
    }

    pub fn compute_next_row(&mut self, next_row: DegreeType) -> Vec<T> {
        self.set_next_row_and_log(next_row);

        let mut complete_identities = vec![false; self.identities.len()];

        log::trace!("Row: {}", next_row);

        let mut identity_failed = false;
        for strategy in [
            SolvingStrategy::SingleVariableAffine,
            SolvingStrategy::AssumeZero,
        ] {
            log::trace!("  Strategy: {:?}", strategy);
            let mut i = 0;
            loop {
                identity_failed = false;
                let mut progress = false;
                self.failure_reasons.clear();

                log::trace!("    Pass {i}");
                i += 1;

                for (identity, complete) in self
                    .identities
                    .iter()
                    .zip(complete_identities.iter_mut())
                    .filter(|(_, complete)| !**complete)
                {
                    log::trace!("      Checking identity: {}", identity);
                    let result = self.process_identity(identity, strategy).map_err(|err| {
                        let msg = match strategy {
                            SolvingStrategy::SingleVariableAffine => "No progress on",
                            SolvingStrategy::AssumeZero => {
                                "Assuming zero for unknown columns failed in"
                            }
                        };
                        format!("{msg} {identity}:\n{}", indent(&format!("{err}"), "    ")).into()
                    });

                    match &result {
                        Ok(e) => {
                            *complete = e.is_complete();
                        }
                        Err(_) => {
                            identity_failed = true;
                        }
                    };

                    progress |= self.handle_eval_result(result, strategy);
                }

                if self.query_callback.is_some()
                    && strategy == SolvingStrategy::SingleVariableAffine
                {
                    for column in self.fixed_data.witness_cols() {
                        if !self.has_known_next_value(column.poly.poly_id() as usize)
                            && column.query.is_some()
                        {
                            let result = self.process_witness_query(&column);
                            progress |= self.handle_eval_result(result, strategy);
                        }
                    }
                }

                if !progress {
                    break;
                }
            }
        }
        if identity_failed {
            let list_undetermined = |values: &Vec<Option<T>>| {
                values
                    .iter()
                    .enumerate()
                    .filter_map(|(i, v)| {
                        if v.is_none() && self.is_relevant_witness(i) {
                            Some(self.fixed_data.witness_cols[i].poly.to_string())
                        } else {
                            None
                        }
                    })
                    .collect::<Vec<String>>()
                    .join(", ")
            };

            log::error!(
                "\nError: Row {next_row}: Identity check failed or unable to derive values for some witness columns.\nSet RUST_LOG=debug for more information.");
            log::debug!(
                "\nThe following columns were undetermined in the previous row and might have been needed to derive this row's values:\n{}",
                list_undetermined(&self.current)
            );
            log::debug!(
                "\nThe following columns are still undetermined in the current row:\n{}",
                list_undetermined(&self.next)
            );
            log::debug!("\nReasons:\n{}\n", self.failure_reasons.join("\n\n"));
            log::debug!(
                "Determined range constraints for this row:\n{}",
                self.next_range_constraints
                    .iter()
                    .enumerate()
                    .filter_map(|(id, cons)| {
                        cons.as_ref().map(|cons| {
                            format!("  {}: {cons}", self.fixed_data.witness_cols[id].poly)
                        })
                    })
                    .join("\n")
            );
            log::debug!(
                "Current values (known nonzero first, then zero, unknown omitted):\n{}",
                indent(&self.format_next_known_values().join("\n"), "    ")
            );
            panic!("Witness generation failed.");
        }

        log::trace!(
            "===== Row {next_row}:\n{}",
            indent(&self.format_next_values().join("\n"), "    ")
        );
        std::mem::swap(&mut self.next, &mut self.current);
        self.next = vec![None; self.current.len()];
        self.next_range_constraints = vec![None; self.current.len()];

        self.current
            .iter()
            .map(|v| (*v).unwrap_or_default())
            .collect()
    }

    /// Verifies the proposed values for the next row.
    /// TODO this is bad for machines because we might introduce rows in the machine that are then
    /// not used.
    pub fn propose_next_row(&mut self, next_row: DegreeType, values: &[T]) -> bool {
        self.set_next_row_and_log(next_row);
        self.next = values.iter().cloned().map(Some).collect();

        for identity in self.identities {
            if self
                .process_identity(identity, SolvingStrategy::AssumeZero)
                .is_err()
            {
                self.next = vec![None; self.current.len()];
                self.next_range_constraints = vec![None; self.current.len()];
                return false;
            }
        }
        std::mem::swap(&mut self.next, &mut self.current);
        self.next = vec![None; self.current.len()];
        self.next_range_constraints = vec![None; self.current.len()];
        true
    }

    pub fn machine_witness_col_values(&mut self) -> HashMap<String, Vec<T>> {
        let mut result: HashMap<_, _> = Default::default();
        for m in &mut self.machines {
            result.extend(m.witness_col_values(self.fixed_data));
        }
        result
    }

    fn set_next_row_and_log(&mut self, next_row: DegreeType) {
        if next_row >= self.last_report + 1000 {
            let duration = self.last_report_time.elapsed();
            self.last_report_time = Instant::now();

            log::info!(
                "{next_row} of {} rows ({} %, {} rows per second)",
                self.fixed_data.degree,
                next_row * 100 / self.fixed_data.degree,
                1000000 / duration.as_millis()
            );
            self.last_report = next_row;
        }
        self.next_row = next_row;
    }

    fn format_next_values(&self) -> Vec<String> {
        self.format_next_values_iter(
            self.next
                .iter()
                .enumerate()
                .filter(|(i, _)| self.is_relevant_witness(*i)),
        )
    }

    fn format_next_known_values(&self) -> Vec<String> {
        self.format_next_values_iter(self.next.iter().enumerate().filter(|(_, v)| v.is_some()))
    }

    fn format_next_values_iter<'b>(
        &self,
        values: impl IntoIterator<Item = (usize, &'b Option<T>)>,
    ) -> Vec<String> {
        let mut values = values.into_iter().collect::<Vec<_>>();
        values.sort_by_key(|(i, v1)| {
            (
                match v1 {
                    Some(v) if v.is_zero() => 1,
                    Some(_) => 0,
                    None => 2,
                },
                *i,
            )
        });
        values
            .into_iter()
            .map(|(i, v)| {
                format!(
                    "{} = {}",
                    self.fixed_data.witness_cols[i].poly,
                    v.as_ref()
                        .map(ToString::to_string)
                        .unwrap_or_else(|| "<unknown>".to_string())
                )
            })
            .collect()
    }

    fn process_witness_query(&mut self, column: &&'a WitnessColumn<T>) -> EvalResult<'a, T> {
        let query = match self.interpolate_query(column.query.unwrap()) {
            Ok(query) => query,
            Err(incomplete) => return Ok(EvalValue::incomplete(incomplete)),
        };
        log::trace!("      Running query: {}", query);
        if let Some(value) = self.query_callback.as_mut().and_then(|c| (c)(&query)) {
            Ok(EvalValue::complete(vec![(
                &column.poly,
                Constraint::Assignment(value),
            )]))
        } else {
            Ok(EvalValue::incomplete(IncompleteCause::NoQueryAnswer(
                query,
                column.poly.name.to_string(),
            )))
        }
    }

    fn interpolate_query<'b>(
        &self,
        query: &'b Expression<T>,
    ) -> Result<String, IncompleteCause<&'b PolynomialReference>> {
        if let Ok(v) = self.evaluate(query, EvaluationRow::Next, EvaluateUnknown::Symbolic) {
            if v.is_constant() {
                return Ok(v.to_string());
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
            Expression::MatchExpression(scrutinee, arms) => {
                self.interpolate_match_expression_for_query(scrutinee.as_ref(), arms)
            }
            query => unimplemented!("Cannot handle / evaluate {query}"),
        }
    }

    fn interpolate_match_expression_for_query<'b>(
        &self,
        scrutinee: &'b Expression<T>,
        arms: &'b [(Option<T>, Expression<T>)],
    ) -> Result<String, IncompleteCause<&'b PolynomialReference>> {
        let v = self
            .evaluate(scrutinee, EvaluationRow::Next, EvaluateUnknown::Symbolic)?
            .constant_value()
            .ok_or(IncompleteCause::NonConstantQueryMatchScrutinee)?;
        let (_, expr) = arms
            .iter()
            .find(|(n, _)| n.is_none() || n.as_ref() == Some(&v))
            .ok_or(IncompleteCause::NoMatchArmFound)?;
        self.interpolate_query(expr)
    }

    fn process_identity<'b>(
        &mut self,
        identity: &'b Identity<T>,
        strategy: SolvingStrategy,
    ) -> EvalResult<'b, T> {
        match identity.kind {
            IdentityKind::Polynomial => {
                self.process_polynomial_identity(identity.expression_for_poly_id(), strategy)
            }
            IdentityKind::Plookup | IdentityKind::Permutation => {
                self.process_plookup(identity, strategy)
            }
            kind => {
                unimplemented!("Identity of kind {kind:?} is not supported in the executor")
            }
        }
    }

    fn process_polynomial_identity<'b>(
        &self,
        identity: &'b Expression<T>,
        strategy: SolvingStrategy,
    ) -> EvalResult<'b, T> {
        // If there is no "next" reference in the expression,
        // we just evaluate it directly on the "next" row.
        let row = if identity.contains_next_witness_ref() {
            // TODO this is the only situation where we use "current"
            // TODO this is the only that actually uses a window.
            EvaluationRow::Current
        } else {
            EvaluationRow::Next
        };
        let evaluate_unknown = if strategy == SolvingStrategy::AssumeZero {
            EvaluateUnknown::AssumeZero
        } else {
            EvaluateUnknown::Symbolic
        };
        let evaluated = match self.evaluate(identity, row, evaluate_unknown) {
            Ok(evaluated) => evaluated,
            Err(cause) => {
                return Ok(EvalValue::incomplete(cause));
            }
        };
        if evaluated.constant_value() == Some(0.into()) {
            Ok(EvalValue::complete(vec![]))
        } else {
            evaluated.solve_with_range_constraints(&self.range_constraint_set())
        }
    }

    fn process_plookup<'b>(
        &mut self,
        identity: &'b Identity<T>,
        strategy: SolvingStrategy,
    ) -> EvalResult<'b, T> {
        let evaluate_unknown = if strategy == SolvingStrategy::AssumeZero {
            EvaluateUnknown::AssumeZero
        } else {
            EvaluateUnknown::Symbolic
        };
        if let Some(left_selector) = &identity.left.selector {
            let value = match self.evaluate(left_selector, EvaluationRow::Next, evaluate_unknown) {
                Ok(value) => value,
                Err(cause) => return Ok(EvalValue::incomplete(cause)),
            };
            match value.constant_value() {
                Some(v) if v.is_zero() => {
                    return Ok(EvalValue::complete(vec![]));
                }
                Some(v) if v.is_one() => {}
                _ => {
                    return Ok(EvalValue::incomplete(
                        IncompleteCause::NonConstantLeftSelector,
                    ))
                }
            };
        }

        let left = identity
            .left
            .expressions
            .iter()
            .map(|e| self.evaluate(e, EvaluationRow::Next, evaluate_unknown))
            .collect::<Vec<_>>();

        // Now query the machines.
        // Note that we should always query all machines that match, because they might
        // update their internal data, even if all values are already known.
        // TODO could it be that multiple machines match?

        // query the fixed lookup "machine"
        if let Some(result) = self.fixed_lookup.process_plookup(
            self.fixed_data,
            identity.kind,
            &left,
            &identity.right,
        ) {
            return result;
        }

        for m in &mut self.machines {
            // TODO also consider the reasons above.
            if let Some(result) = m.process_plookup(
                self.fixed_data,
                self.fixed_lookup,
                identity.kind,
                &left,
                &identity.right,
            ) {
                return result;
            }
        }

        unimplemented!("No executor machine matched identity `{identity}`")
    }

    /// Processes the evaluation result: Stores failure reasons and updates next values.
    /// Returns true if a new value or constraint was determined.
    fn handle_eval_result(&mut self, result: EvalResult<T>, strategy: SolvingStrategy) -> bool {
        match result {
            Ok(constraints) => {
                let progress = !constraints.is_empty();
                // If we assume unknown variables to be zero, we cannot learn anything new.
                if strategy == SolvingStrategy::AssumeZero {
                    assert!(!progress);
                }
                for (id, c) in constraints.constraints {
                    match c {
                        Constraint::Assignment(value) => {
                            log::trace!("        Setting next value for {id} to {value}");
                            self.next[id.poly_id() as usize] = Some(value);
                        }
                        Constraint::RangeConstraint(cons) => {
                            log::trace!("        Adding range constraint for {id}: {cons}");
                            self.next_range_constraints[id.poly_id() as usize] = Some(cons);
                        }
                    }
                }
                progress
            }
            Err(reason) => {
                self.failure_reasons.push(format!("{reason}"));
                false
            }
        }
    }

    fn has_known_next_value(&self, id: usize) -> bool {
        self.next[id].is_some()
    }

    /// Returns true if this is a witness column we care about (instead of a sub-machine witness).
    pub fn is_relevant_witness(&self, id: usize) -> bool {
        self.witnesses
            .contains(&self.fixed_data.witness_cols[id].poly)
    }

    /// Tries to evaluate the expression to an expression affine in the witness polynomials,
    /// taking current values of polynomials into account.
    /// @returns an expression affine in the witness polynomials
    fn evaluate<'b>(
        &self,
        expr: &'b Expression<T>,
        evaluate_row: EvaluationRow,
        evaluate_unknown: EvaluateUnknown,
    ) -> AffineResult<&'b PolynomialReference, T> {
        let degree = self.fixed_data.degree;

        // We want to determine the values of next_row, but if the expression contains
        // references to the next row, we want to evaluate the expression for the current row
        // in order to determine values for next_row.
        // Otherwise, we want to evaluate the expression on next_row directly.
        let fixed_row = match evaluate_row {
            EvaluationRow::Current => (self.next_row + degree - 1) % degree,
            EvaluationRow::Next => self.next_row,
        };

        ExpressionEvaluator::new(SymoblicWitnessEvaluator::new(
            self.fixed_data,
            fixed_row,
            EvaluationData {
                current_witnesses: &self.current,
                next_witnesses: &self.next,
                evaluate_row,
                evaluate_unknown,
            },
        ))
        .evaluate(expr)
    }

    fn range_constraint_set(&'a self) -> WitnessRangeConstraintSet<'a, T> {
        WitnessRangeConstraintSet {
            global_range_constraints: &self.global_range_constraints,
            next_range_constraints: &self.next_range_constraints,
        }
    }
}

struct WitnessRangeConstraintSet<'a, T: FieldElement> {
    /// Global constraints on witness and fixed polynomials.
    global_range_constraints: &'a BTreeMap<&'a PolynomialReference, RangeConstraint<T>>,
    /// Range constraints on the witness polynomials in the next row.
    next_range_constraints: &'a Vec<Option<RangeConstraint<T>>>,
}

impl<'a, T: FieldElement> RangeConstraintSet<&PolynomialReference, T>
    for WitnessRangeConstraintSet<'a, T>
{
    fn range_constraint(&self, poly: &PolynomialReference) -> Option<RangeConstraint<T>> {
        self.global_range_constraints
            .get(poly)
            .or_else(|| {
                poly.is_witness()
                    .then(|| self.next_range_constraints[poly.poly_id() as usize].as_ref())
                    .flatten()
            })
            .cloned()
    }
}

struct EvaluationData<'a, T> {
    /// Values of the witness polynomials in the current / last row
    pub current_witnesses: &'a Vec<Option<T>>,
    /// Values of the witness polynomials in the next row
    pub next_witnesses: &'a Vec<Option<T>>,
    pub evaluate_row: EvaluationRow,
    pub evaluate_unknown: EvaluateUnknown,
}

impl<'a, T: FieldElement> WitnessColumnEvaluator<T> for EvaluationData<'a, T> {
    fn value<'b>(&self, poly: &'b PolynomialReference) -> AffineResult<&'b PolynomialReference, T> {
        let id = poly.poly_id() as usize;
        match (poly.next, self.evaluate_row) {
            (false, EvaluationRow::Current) => {
                // All values in the "current" row should usually be known.
                // The exception is when we start the analysis on the first row.
                self.current_witnesses[id]
                    .as_ref()
                    .map(|value| (*value).into())
                    .ok_or(IncompleteCause::PreviousValueUnknown(poly))
            }
            (false, EvaluationRow::Next) | (true, EvaluationRow::Current) => {
                Ok(if let Some(value) = &self.next_witnesses[id] {
                    // We already computed the concrete value
                    (*value).into()
                } else if self.evaluate_unknown == EvaluateUnknown::AssumeZero {
                    T::from(0).into()
                } else {
                    // We continue with a symbolic value
                    AffineExpression::from_variable_id(poly)
                })
            }
            (true, EvaluationRow::Next) => {
                unimplemented!(
                    "{poly} references the next-next row when evaluating on the current row."
                );
            }
        }
    }
}
