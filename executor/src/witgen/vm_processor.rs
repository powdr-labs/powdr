use indicatif::{ProgressBar, ProgressStyle};
use itertools::Itertools;
use powdr_ast::analyzed::{
    AlgebraicExpression as Expression, AlgebraicReference, Identity, IdentityKind, PolyID,
};
use powdr_ast::indent;
use powdr_number::{DegreeType, FieldElement};
use std::cmp::max;
use std::collections::HashSet;
use std::time::Instant;

use crate::witgen::identity_processor::{self};
use crate::witgen::IncompleteCause;

use super::data_structures::finalizable_data::FinalizableData;
use super::processor::{OuterQuery, Processor};

use super::rows::{Row, RowIndex, UnknownStrategy};
use super::{Constraints, EvalError, EvalValue, FixedData, MutableState, QueryCallback};

/// Maximal period checked during loop detection.
const MAX_PERIOD: usize = 4;

const REPORT_FREQUENCY: u64 = 1_000;

/// A list of identities with a flag whether it is complete.
struct CompletableIdentities<'a, T: FieldElement> {
    identities_with_complete: Vec<(&'a Identity<Expression<T>>, bool)>,
}

impl<'a, T: FieldElement> CompletableIdentities<'a, T> {
    fn new(identities: impl Iterator<Item = &'a Identity<Expression<T>>>) -> Self {
        Self {
            identities_with_complete: identities.map(|identity| (identity, false)).collect(),
        }
    }

    /// Yields immutable references to the identity and mutable references to the complete flag.
    fn iter_mut(&mut self) -> impl Iterator<Item = (&'a Identity<Expression<T>>, &mut bool)> {
        self.identities_with_complete
            .iter_mut()
            .map(|(identity, complete)| (*identity, complete))
    }
}

pub struct VmProcessor<'a, 'b, 'c, T: FieldElement, Q: QueryCallback<T>> {
    /// The global index of the first row of [VmProcessor::data].
    row_offset: DegreeType,
    /// The witness columns belonging to this machine
    witnesses: HashSet<PolyID>,
    fixed_data: &'a FixedData<'a, T>,
    /// The subset of identities that contains a reference to the next row
    /// (precomputed once for performance reasons)
    identities_with_next_ref: Vec<&'a Identity<Expression<T>>>,
    /// The subset of identities that does not contain a reference to the next row
    /// (precomputed once for performance reasons)
    identities_without_next_ref: Vec<&'a Identity<Expression<T>>>,
    last_report: DegreeType,
    last_report_time: Instant,
    processor: Processor<'a, 'b, 'c, T, Q>,
    progress_bar: ProgressBar,
}

impl<'a, 'b, 'c, T: FieldElement, Q: QueryCallback<T>> VmProcessor<'a, 'b, 'c, T, Q> {
    pub fn new(
        row_offset: RowIndex,
        fixed_data: &'a FixedData<'a, T>,
        identities: &[&'a Identity<Expression<T>>],
        witnesses: &'c HashSet<PolyID>,
        data: FinalizableData<'a, T>,
        mutable_state: &'c mut MutableState<'a, 'b, T, Q>,
    ) -> Self {
        let (identities_with_next, identities_without_next): (Vec<_>, Vec<_>) = identities
            .iter()
            .partition(|identity| identity.contains_next_ref());
        let processor = Processor::new(row_offset, data, mutable_state, fixed_data, witnesses);

        let progress_bar = ProgressBar::new(fixed_data.degree);
        progress_bar.set_style(
            ProgressStyle::with_template(
                "[{elapsed_precise} (ETA: {eta_precise})] {bar} {percent}% - {msg}",
            )
            .unwrap(),
        );

        VmProcessor {
            row_offset: row_offset.into(),
            witnesses: witnesses.clone(),
            fixed_data,
            identities_with_next_ref: identities_with_next,
            identities_without_next_ref: identities_without_next,
            last_report: 0,
            last_report_time: Instant::now(),
            processor,
            progress_bar,
        }
    }

    pub fn with_outer_query(self, outer_query: OuterQuery<'a, 'b, T>) -> Self {
        let processor = self.processor.with_outer_query(outer_query);
        Self { processor, ..self }
    }

    pub fn finish(self) -> FinalizableData<'a, T> {
        self.processor.finish()
    }

    /// Starting out with a single row (at a given offset), iteratively append rows
    /// until we have exhausted the rows or the latch expression (if available) evaluates to 1.
    pub fn run(&mut self, is_main_run: bool) -> EvalValue<&'a AlgebraicReference, T> {
        assert!(self.processor.len() == 1);

        if is_main_run {
            log::info!("Running main machine for {} rows", self.fixed_data.degree);
            self.progress_bar.reset();
            self.progress_bar.set_message("Starting...");
            self.progress_bar.tick();
        }

        let mut outer_assignments = vec![];

        // Are we in an infinite loop and can just re-use the old values?
        let mut looping_period = None;
        let mut loop_detection_log_level = if is_main_run {
            log::Level::Info
        } else {
            log::Level::Debug
        };
        let rows_left = self.fixed_data.degree - self.row_offset + 1;
        let mut finalize_start = 1;
        for row_index in 0..rows_left {
            if is_main_run {
                self.maybe_log_performance(row_index);
            }

            if (row_index + 1) % 10000 == 0 {
                // Periodically make sure most rows are finalized.
                // Row 0 and the last MAX_PERIOD rows might be needed later, so they are not finalized.
                let finalize_end = row_index as usize - MAX_PERIOD;
                self.processor.finalize_range(finalize_start..finalize_end);
                finalize_start = finalize_end;
            }

            if row_index >= rows_left - 2 {
                // On th last few rows, it is quite normal for the constraints to be different,
                // so we reduce the log level for loop detection.
                loop_detection_log_level = log::Level::Debug;
            }

            // Check if we are in a loop.
            if looping_period.is_none() && row_index % 100 == 0 && row_index > 0 {
                looping_period = self.rows_are_repeating(row_index);
                if let Some(p) = looping_period {
                    log::log!(
                        loop_detection_log_level,
                        "Found loop with period {p} starting at row {row_index}"
                    );
                }
            }
            if let Some(period) = looping_period {
                let proposed_row = self.processor.row(row_index as usize - period).clone();
                if !self.try_proposed_row(row_index, proposed_row) {
                    log::log!(
                        loop_detection_log_level,
                        "Looping failed. Trying to generate regularly again. (Use RUST_LOG=debug to see whether this happens more often.) {row_index} {rows_left}"
                    );
                    looping_period = None;
                    // For some programs, loop detection will often find loops and then fail.
                    // In this case, we don't want to spam the user with debug messages.
                    loop_detection_log_level = log::Level::Debug;
                }
            }
            // Note that we exit one iteration early in the non-loop case,
            // because ensure_has_next_row() + compute_row() will already
            // add and compute some values for the next row as well.
            if looping_period.is_none() && row_index != rows_left - 1 {
                self.ensure_has_next_row(row_index);
                outer_assignments.extend(self.compute_row(row_index).into_iter());

                // Evaluate latch expression and return if it evaluates to 1.
                if let Some(latch) = self.processor.latch_value(row_index as usize) {
                    if latch {
                        log::trace!("Machine returns!");
                        if self.processor.finished_outer_query() {
                            return EvalValue::complete(outer_assignments);
                        } else {
                            return EvalValue::incomplete_with_constraints(
                                outer_assignments,
                                IncompleteCause::BlockMachineLookupIncomplete,
                            );
                        }
                    }
                } else if self.processor.has_outer_query() {
                    // If we have an outer query (and therefore a latch expression),
                    // its value should be known at this point.
                    // Probably, we don't have all the necessary inputs.
                    return EvalValue::incomplete(IncompleteCause::UnknownLatch);
                }
            };
        }

        assert_eq!(
            self.processor.len() as DegreeType + self.row_offset,
            self.fixed_data.degree + 1
        );

        if is_main_run {
            self.progress_bar.finish();
        }

        EvalValue::complete(outer_assignments)
    }

    /// Checks if the last rows are repeating and returns the period.
    /// Only checks for periods of 1, ..., MAX_PERIOD.
    fn rows_are_repeating(&self, row_index: DegreeType) -> Option<usize> {
        if row_index < MAX_PERIOD as DegreeType {
            return None;
        }

        let row = row_index as usize;
        (1..MAX_PERIOD).find(|&period| {
            (1..=period).all(|i| {
                self.processor
                    .row(row - i - period)
                    .values()
                    .zip(self.processor.row(row - i).values())
                    .all(|(a, b)| a.value == b.value)
            })
        })
    }

    fn ensure_has_next_row(&mut self, row_index: DegreeType) {
        assert!(self.processor.len() as DegreeType > row_index);
        if row_index == self.processor.len() as DegreeType - 1 {
            self.processor.set_row(
                self.processor.len(),
                Row::fresh(
                    self.fixed_data,
                    RowIndex::from_degree(row_index, self.fixed_data.degree) + 1,
                ),
            );
        }
    }

    fn compute_row(&mut self, row_index: DegreeType) -> Constraints<&'a AlgebraicReference, T> {
        log::trace!(
            "===== Starting to process row: {}",
            row_index + self.row_offset
        );

        log::trace!("  Going over all identities until no more progress is made");
        // First, go over identities that don't reference the next row,
        // Second, propagate values to the next row by going over identities that do reference the next row.
        let mut identities_without_next_ref =
            CompletableIdentities::new(self.identities_without_next_ref.iter().cloned());
        let mut identities_with_next_ref =
            CompletableIdentities::new(self.identities_with_next_ref.iter().cloned());
        let outer_assignments = self
            .loop_until_no_progress(row_index, &mut identities_without_next_ref)
            .and_then(|outer_assignments| {
                Ok(outer_assignments
                    .into_iter()
                    .chain(self.loop_until_no_progress(row_index, &mut identities_with_next_ref)?)
                    .collect::<Vec<_>>())
            })
            .map_err(|e| self.report_failure_and_panic_unsatisfiable(row_index, e))
            .unwrap();

        // Check that the computed row is "final" by asserting that all unknown values can
        // be set to 0.
        // This check is only done for the primary machine, as secondary machines might simply
        // not have all the inputs yet and therefore be under-constrained.
        if !self.processor.has_outer_query() {
            log::trace!(
                "  Checking that remaining identities hold when unknown values are set to 0"
            );
            self.process_identities(
                row_index,
                &mut identities_without_next_ref,
                UnknownStrategy::Zero,
            )
            .and_then(|_| {
                self.process_identities(
                    row_index,
                    &mut identities_with_next_ref,
                    UnknownStrategy::Zero,
                )
            })
            .map_err(|e| self.report_failure_and_panic_under_constrained(row_index, e))
            .unwrap();
        }

        log::trace!(
            "{}",
            self.processor.row(row_index as usize).render(
                &format!(
                    "===== Summary for row {}",
                    row_index as DegreeType + self.row_offset
                ),
                true,
                &self.witnesses
            )
        );

        outer_assignments
    }

    /// Loops over all identities and queries, until no further progress is made.
    /// @returns the "incomplete" identities, i.e. identities that contain unknown values.
    fn loop_until_no_progress(
        &mut self,
        row_index: DegreeType,
        identities: &mut CompletableIdentities<'a, T>,
    ) -> Result<Constraints<&'a AlgebraicReference, T>, Vec<EvalError<T>>> {
        let mut outer_assignments = vec![];
        loop {
            let mut progress =
                self.process_identities(row_index, identities, UnknownStrategy::Unknown)?;
            let row_index = row_index as usize;
            if let Some(true) = self.processor.latch_value(row_index) {
                let (outer_query_progress, new_outer_assignments) = self
                    .processor
                    .process_outer_query(row_index)
                    .map_err(|e| vec![e])?;
                progress |= outer_query_progress;
                outer_assignments.extend(new_outer_assignments);
            }

            progress |= self.processor.set_inputs_if_unset(row_index);
            progress |= self
                .processor
                .process_queries(row_index)
                .map_err(|e| vec![e])?;

            if !progress {
                break;
            }
        }
        Ok(outer_assignments)
    }

    /// Loops over all identities once and updates the current row and next row.
    /// Arguments:
    /// * `identities`: Identities to process. Completed identities are removed from the list.
    /// * `unknown_strategy`: How to process unknown variables. Either use zero or keep it symbolic.
    /// Returns:
    /// * `Ok(true)`: If progress was made.
    /// * `Ok(false)`: If no progress was made.
    /// * `Err(errors)`: If an error occurred.
    fn process_identities(
        &mut self,
        row_index: DegreeType,
        identities: &mut CompletableIdentities<'a, T>,
        unknown_strategy: UnknownStrategy,
    ) -> Result<bool, Vec<EvalError<T>>> {
        let mut progress = false;
        let mut errors = vec![];

        for (identity, is_complete) in identities.iter_mut() {
            if *is_complete {
                continue;
            }

            let is_machine_call = matches!(
                identity.kind,
                IdentityKind::Plookup | IdentityKind::Permutation
            );
            if is_machine_call && unknown_strategy == UnknownStrategy::Zero {
                // The fact that we got to the point where we assume 0 for unknown cells, but this identity
                // is still not complete, means that either the inputs or the machine is under-constrained.
                errors.push(format!("{identity}:\n{}",
                    indent("This machine call could not be completed. Either some inputs are missing or the machine is under-constrained.", 1)).into());
                continue;
            }

            let result =
                self.processor
                    .process_identity(row_index as usize, identity, unknown_strategy);

            match result {
                Ok(res) => {
                    *is_complete = res.is_complete;
                    progress |= res.progress;
                }
                Err(e) => {
                    errors.push(e);
                }
            };
        }

        if errors.is_empty() {
            Ok(progress)
        } else {
            Err(errors)
        }
    }

    fn report_failure_and_panic_unsatisfiable(
        &self,
        row_index: DegreeType,
        failures: Vec<EvalError<T>>,
    ) -> ! {
        log::error!(
            "\nError: Row {} failed. Set RUST_LOG=debug for more information.\n",
            row_index + self.row_offset
        );
        let row_index = row_index as usize;
        log::debug!("Some identities where not satisfiable after the following values were uniquely determined (known nonzero first, then zero, unknown omitted):");
        log::debug!(
            "{}",
            self.processor.row(row_index).render(
                &format!("Current row ({row_index})"),
                false,
                &self.witnesses
            )
        );
        log::debug!(
            "{}",
            self.processor.row(row_index + 1).render(
                &format!("Next row ({})", row_index + 1),
                false,
                &self.witnesses
            )
        );
        log::debug!("Set RUST_LOG=trace to understand why these values were chosen.");
        log::error!(
            "Errors:\n{}\n",
            failures.iter().map(|r| indent(r.to_string(), 1)).join("\n")
        );
        panic!("Witness generation failed.");
    }

    fn report_failure_and_panic_under_constrained(
        &self,
        row_index: DegreeType,
        failures: Vec<EvalError<T>>,
    ) -> ! {
        log::error!(
            "\nError: Row {} failed. Set RUST_LOG=debug for more information.\n",
            row_index + self.row_offset
        );
        let row_index = row_index as usize;

        log::debug!("Some columns could not be determined, but setting them to zero does not satisfy the constraints. This typically means that the system is under-constrained!");
        log::debug!(
            "{}",
            self.processor.row(row_index).render(
                &format!("Current row ({row_index})"),
                true,
                &self.witnesses
            )
        );
        log::debug!(
            "{}",
            self.processor.row(row_index + 1).render(
                &format!("Next row ({})", row_index + 1),
                true,
                &self.witnesses
            )
        );
        log::debug!("\nSet RUST_LOG=trace to understand why these values were (not) chosen.");
        log::debug!(
            "Assuming zero for unknown values, the following identities fail:\n{}\n",
            failures.iter().map(|r| indent(r.to_string(), 1)).join("\n")
        );
        panic!("Witness generation failed.");
    }

    /// Verifies the proposed values for the next row.
    /// TODO this is bad for machines because we might introduce rows in the machine that are then
    /// not used.
    fn try_proposed_row(&mut self, row_index: DegreeType, proposed_row: Row<'a, T>) -> bool {
        let constraints_valid = self.identities_with_next_ref.iter().all(|i| {
            self.processor
                .check_row_pair(row_index as usize, &proposed_row, i, true)
        }) && self.identities_without_next_ref.iter().all(|i| {
            self.processor
                .check_row_pair(row_index as usize, &proposed_row, i, false)
        });

        if constraints_valid {
            self.processor.set_row(row_index as usize, proposed_row);
        } else {
            // Note that we never update the next row if proposing a row succeeds (the happy path).
            // If it doesn't, we re-run compute_next_row on the previous row in order to
            // correctly forward-propagate values via next references.
            self.ensure_has_next_row(row_index - 1);
            self.compute_row(row_index - 1);
        }
        constraints_valid
    }

    fn maybe_log_performance(&mut self, row_index: DegreeType) {
        if row_index >= self.last_report + REPORT_FREQUENCY {
            let duration = self.last_report_time.elapsed();
            self.last_report_time = Instant::now();

            let identity_statistics = identity_processor::get_and_reset_solving_statistics();
            let identities_per_sec =
                ((identity_statistics.values().map(|s| s.success).sum::<u64>() as u128 * 1000)
                    / duration.as_micros()) as u64;
            let identities_count = max(identity_statistics.len() as u64, 1);
            let progress_percentage = identity_statistics
                .values()
                .map(|s| s.success * 100 / s.invocations)
                .sum::<u64>()
                / identities_count;

            let row = row_index + self.row_offset;
            self.progress_bar.set_position(row);
            let message = format!(
                "{} rows/s, {identities_per_sec}k identities/s, {progress_percentage}% progress",
                REPORT_FREQUENCY as u128 * 1_000_000 / duration.as_micros()
            );
            self.progress_bar.set_message(message);
            self.last_report = row_index;
        }
    }
}
