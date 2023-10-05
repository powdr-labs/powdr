use ast::analyzed::{Identity, IdentityKind, PolyID, PolynomialReference};
use itertools::Itertools;
use number::{DegreeType, FieldElement};
use parser_util::lines::indent;
use std::cmp::max;
use std::collections::HashSet;
use std::time::Instant;

use crate::witgen::identity_processor::{self, IdentityProcessor};
use crate::witgen::rows::RowUpdater;

use super::query_processor::QueryProcessor;

use super::rows::{Row, RowPair, UnknownStrategy};
use super::{EvalError, EvalResult, EvalValue, FixedData, MutableState};

/// Phase in which [VmProcessor::compute_row] is called.
#[derive(Debug, PartialEq)]
enum ProcessingPhase {
    Initialization,
    Regular,
}

/// A list of identities with a flag whether it is complete.
struct CompletableIdentities<'a, T: FieldElement> {
    identities_with_complete: Vec<(&'a Identity<T>, bool)>,
}

impl<'a, T: FieldElement> CompletableIdentities<'a, T> {
    fn new(identities: impl Iterator<Item = &'a Identity<T>>) -> Self {
        Self {
            identities_with_complete: identities.map(|identity| (identity, false)).collect(),
        }
    }

    /// Yields immutable references to the identity and mutable references to the complete flag.
    fn iter_mut(&mut self) -> impl Iterator<Item = (&'a Identity<T>, &mut bool)> {
        self.identities_with_complete
            .iter_mut()
            .map(|(identity, complete)| (*identity, complete))
    }
}

pub struct VmProcessor<'a, T: FieldElement> {
    /// The witness columns belonging to this machine
    witnesses: HashSet<PolyID>,
    fixed_data: &'a FixedData<'a, T>,
    /// The subset of identities that contains a reference to the next row
    /// (precomputed once for performance reasons)
    identities_with_next_ref: Vec<&'a Identity<T>>,
    /// The subset of identities that does not contain a reference to the next row
    /// (precomputed once for performance reasons)
    identities_without_next_ref: Vec<&'a Identity<T>>,
    data: Vec<Row<'a, T>>,
    last_report: DegreeType,
    last_report_time: Instant,
}

impl<'a, T: FieldElement> VmProcessor<'a, T> {
    pub fn new(
        fixed_data: &'a FixedData<'a, T>,
        identities: &[&'a Identity<T>],
        witnesses: HashSet<PolyID>,
        data: Vec<Row<'a, T>>,
    ) -> Self {
        let (identities_with_next, identities_without_next): (Vec<_>, Vec<_>) = identities
            .iter()
            .partition(|identity| identity.contains_next_ref());

        VmProcessor {
            witnesses,
            fixed_data,
            identities_with_next_ref: identities_with_next,
            identities_without_next_ref: identities_without_next,
            data,
            last_report: 0,
            last_report_time: Instant::now(),
        }
    }

    pub fn finish(self) -> Vec<Row<'a, T>> {
        self.data
    }

    fn last_row(&self) -> DegreeType {
        self.fixed_data.degree - 1
    }

    pub fn run<Q>(&mut self, mutable_state: &mut MutableState<'a, T, Q>)
    where
        Q: FnMut(&str) -> Option<T> + Send + Sync,
    {
        // For identities like `pc' = (1 - first_step') * <...>`, we need to process the last
        // row before processing the first row.
        self.compute_row(
            self.last_row(),
            ProcessingPhase::Initialization,
            mutable_state,
        );

        // Are we in an infinite loop and can just re-use the old values?
        let mut looping_period = None;
        let mut loop_detection_log_level = log::Level::Info;
        for row_index in 0..self.fixed_data.degree as DegreeType {
            self.maybe_log_performance(row_index);
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
                let proposed_row = self.data[row_index as usize - period].clone();
                if !self.try_proposed_row(row_index, proposed_row, mutable_state) {
                    log::log!(
                        loop_detection_log_level,
                        "Looping failed. Trying to generate regularly again. (Use RUST_LOG=debug to see whether this happens more often.)"
                    );
                    looping_period = None;
                    // For some programs, loop detection will often find loops and then fail.
                    // In this case, we don't want to spam the user with debug messages.
                    loop_detection_log_level = log::Level::Debug;
                }
            }
            if looping_period.is_none() {
                self.compute_row(row_index, ProcessingPhase::Regular, mutable_state);
            };
        }
    }

    /// Checks if the last rows are repeating and returns the period.
    /// Only checks for periods of 1, 2, 3 and 4.
    fn rows_are_repeating(&self, row_index: DegreeType) -> Option<usize> {
        if row_index < 4 {
            return None;
        }

        let row = row_index as usize;
        (1..=3).find(|&period| {
            (1..=period).all(|i| {
                self.data[row - i - period]
                    .values()
                    .zip(self.data[row - i].values())
                    .all(|(a, b)| a.value == b.value)
            })
        })
    }

    // Returns a reference to a given row
    fn row(&self, row_index: DegreeType) -> &Row<'a, T> {
        let row_index = (row_index + self.fixed_data.degree) % self.fixed_data.degree;
        &self.data[row_index as usize]
    }

    fn compute_row<Q>(
        &mut self,
        row_index: DegreeType,
        phase: ProcessingPhase,
        mutable_state: &mut MutableState<'a, T, Q>,
    ) where
        Q: FnMut(&str) -> Option<T> + Send + Sync,
    {
        log::trace!("Row: {}", row_index);

        let mut identity_processor = IdentityProcessor::new(
            self.fixed_data,
            &mut mutable_state.fixed_lookup,
            mutable_state.machines.iter_mut().into(),
        );
        let mut query_processor = mutable_state
            .query_callback
            .as_mut()
            .map(|query| QueryProcessor::new(self.fixed_data, query));

        log::trace!("  Going over all identities until no more progress is made");
        // First, go over identities that don't reference the next row,
        // Second, propagate values to the next row by going over identities that do reference the next row.
        let mut identities_without_next_ref =
            CompletableIdentities::new(self.identities_without_next_ref.iter().cloned());
        let mut identities_with_next_ref =
            CompletableIdentities::new(self.identities_with_next_ref.iter().cloned());
        self.loop_until_no_progress(
            row_index,
            &mut identities_without_next_ref,
            &mut identity_processor,
            &mut query_processor,
        )
        .and_then(|_| {
            self.loop_until_no_progress(
                row_index,
                &mut identities_with_next_ref,
                &mut identity_processor,
                &mut query_processor,
            )
        })
        .map_err(|e| self.report_failure_and_panic_unsatisfiable(row_index, e))
        .unwrap();

        // Check that the computed row is "final" by asserting that all unknown values can
        // be set to 0.
        // This check is skipped in the initialization phase (run on the last row),
        // because its only purpose is to transfer values to the first row,
        // not to finalize the last row.
        if phase == ProcessingPhase::Regular {
            log::trace!(
                "  Checking that remaining identities hold when unknown values are set to 0"
            );
            self.process_identities(
                row_index,
                &mut identities_without_next_ref,
                UnknownStrategy::Zero,
                &mut identity_processor,
            )
            .and_then(|_| {
                self.process_identities(
                    row_index,
                    &mut identities_with_next_ref,
                    UnknownStrategy::Zero,
                    &mut identity_processor,
                )
            })
            .map_err(|e| self.report_failure_and_panic_underconstrained(row_index, e))
            .unwrap();
        }

        log::trace!(
            "{}",
            self.row(row_index)
                .render(&format!("===== Row {}", row_index), true, &self.witnesses)
        );
    }

    /// Loops over all identities and queries, until no further progress is made.
    /// @returns the "incomplete" identities, i.e. identities that contain unknown values.
    fn loop_until_no_progress<Q>(
        &mut self,
        row_index: DegreeType,
        identities: &mut CompletableIdentities<'a, T>,
        identity_processor: &mut IdentityProcessor<'a, '_, T>,
        query_processor: &mut Option<QueryProcessor<'a, '_, T, Q>>,
    ) -> Result<(), Vec<EvalError<T>>>
    where
        Q: FnMut(&str) -> Option<T> + Send + Sync,
    {
        loop {
            let mut progress = self.process_identities(
                row_index,
                identities,
                UnknownStrategy::Unknown,
                identity_processor,
            )?;
            if let Some(query_processor) = query_processor.as_mut() {
                let row_pair = RowPair::new(
                    self.row(row_index),
                    self.row(row_index + 1),
                    row_index,
                    self.fixed_data,
                    UnknownStrategy::Unknown,
                );
                let updates = query_processor.process_queries_on_current_row(&row_pair);
                progress |= self.apply_updates(row_index, &updates, || "query".to_string());
            }

            if !progress {
                break;
            }
        }
        Ok(())
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
        identity_processor: &mut IdentityProcessor<'a, '_, T>,
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
                    indent("This machine call could not be completed. Either some inputs are missing or the machine is under-constrained.", "    ")).into());
                continue;
            }

            let row_pair = RowPair::new(
                self.row(row_index),
                self.row(row_index + 1),
                row_index,
                self.fixed_data,
                unknown_strategy,
            );
            let result: EvalResult<'a, T> = identity_processor
                .process_identity(identity, &row_pair)
                .map_err(|err| {
                    format!("{identity}:\n{}", indent(&format!("{err}"), "    ")).into()
                });

            match result {
                Ok(eval_value) => {
                    if unknown_strategy == UnknownStrategy::Zero {
                        assert!(eval_value.constraints.is_empty())
                    } else {
                        *is_complete = eval_value.is_complete();
                        progress |=
                            self.apply_updates(row_index, &eval_value, || format!("{identity}"));
                    }
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

    fn apply_updates(
        &mut self,
        row_index: DegreeType,
        updates: &EvalValue<&PolynomialReference, T>,
        source_name: impl Fn() -> String,
    ) -> bool {
        let (before, after) = if row_index == self.last_row() {
            // Last row is current, first row is next
            let (after, before) = self.data.split_at_mut(row_index as usize);
            (before, after)
        } else {
            self.data.split_at_mut(row_index as usize + 1)
        };
        let current = before.last_mut().unwrap();
        let next = after.first_mut().unwrap();
        let mut row_updater = RowUpdater::new(current, next, row_index);
        row_updater.apply_updates(updates, source_name)
    }

    fn report_failure_and_panic_unsatisfiable(
        &self,
        row_index: DegreeType,
        failures: Vec<EvalError<T>>,
    ) -> ! {
        log::error!(
            "\nError: Row {} failed. Set RUST_LOG=debug for more information.\n",
            row_index
        );
        log::debug!("Some identities where not satisfiable after the following values were uniquely determined (known nonzero first, then zero, unknown omitted):");
        log::debug!(
            "{}",
            self.row(row_index)
                .render("Current Row", false, &self.witnesses)
        );
        log::debug!(
            "{}",
            self.row(row_index + 1)
                .render("Next Row", false, &self.witnesses)
        );
        log::debug!("Set RUST_LOG=trace to understand why these values were chosen.");
        log::debug!(
            "Assuming these values are correct, the following identities fail:\n{}\n",
            failures
                .iter()
                .map(|r| indent(&r.to_string(), "    "))
                .join("\n")
        );
        panic!("Witness generation failed.");
    }

    fn report_failure_and_panic_underconstrained(
        &self,
        row_index: DegreeType,
        failures: Vec<EvalError<T>>,
    ) -> ! {
        log::error!(
            "\nError: Row {} failed. Set RUST_LOG=debug for more information.\n",
            row_index
        );

        log::debug!("Some columns could not be determined, but setting them to zero does not satisfy the constraints. This typically means that the system is underconstrained!");
        log::debug!(
            "{}",
            self.row(row_index)
                .render("Current Row", true, &self.witnesses)
        );
        log::debug!(
            "{}",
            self.row(row_index)
                .render("Next Row", true, &self.witnesses)
        );
        log::debug!("\nSet RUST_LOG=trace to understand why these values were (not) chosen.");
        log::debug!(
            "Assuming zero for unknown values, the following identities fail:\n{}\n",
            failures
                .iter()
                .map(|r| indent(&r.to_string(), "    "))
                .join("\n")
        );
        panic!("Witness generation failed.");
    }

    /// Verifies the proposed values for the next row.
    /// TODO this is bad for machines because we might introduce rows in the machine that are then
    /// not used.
    fn try_proposed_row<Q>(
        &mut self,
        row_index: DegreeType,
        proposed_row: Row<'a, T>,
        mutable_state: &mut MutableState<'a, T, Q>,
    ) -> bool
    where
        Q: FnMut(&str) -> Option<T> + Send + Sync,
    {
        let mut identity_processor = IdentityProcessor::new(
            self.fixed_data,
            &mut mutable_state.fixed_lookup,
            mutable_state.machines.iter_mut().into(),
        );
        let constraints_valid =
            self.check_row_pair(row_index, &proposed_row, false, &mut identity_processor)
                && self.check_row_pair(row_index, &proposed_row, true, &mut identity_processor);

        if constraints_valid {
            self.data[row_index as usize] = proposed_row;
        } else {
            // Note that we never update the next row if proposing a row succeeds (the happy path).
            // If it doesn't, we re-run compute_next_row on the previous row in order to
            // correctly forward-propagate values via next references.
            self.compute_row(row_index - 1, ProcessingPhase::Regular, mutable_state);
        }
        constraints_valid
    }

    fn check_row_pair(
        &self,
        row_index: DegreeType,
        proposed_row: &Row<'a, T>,
        previous: bool,
        identity_processor: &mut IdentityProcessor<'a, '_, T>,
    ) -> bool {
        let row_pair = match previous {
            // Check whether identities with a reference to the next row are satisfied
            // when applied to the previous row and the proposed row.
            true => RowPair::new(
                self.row(row_index - 1),
                proposed_row,
                row_index - 1,
                self.fixed_data,
                UnknownStrategy::Zero,
            ),
            // Check whether identities without a reference to the next row are satisfied
            // when applied to the proposed row.
            // Note that we also provide the next row here, but it is not used.
            false => RowPair::new(
                proposed_row,
                self.row(row_index + 1),
                row_index,
                self.fixed_data,
                UnknownStrategy::Zero,
            ),
        };

        // Check identities depending on whether or not they have a reference to the next row:
        // - Those that do should be checked on the previous and proposed row
        // - All others should be checked on the proposed row
        let identities = match previous {
            true => &self.identities_with_next_ref,
            false => &self.identities_without_next_ref,
        };

        for identity in identities.iter() {
            if identity_processor
                .process_identity(identity, &row_pair)
                .is_err()
            {
                log::debug!("Previous {:?}", self.row(row_index - 1));
                log::debug!("Proposed {:?}", proposed_row);
                log::debug!("Failed on identity: {}", identity);

                return false;
            }
        }
        true
    }

    fn maybe_log_performance(&mut self, row_index: DegreeType) {
        if row_index >= self.last_report + 1000 {
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

            log::info!(
                "{row_index} of {} rows ({}%) - {} rows/s, {identities_per_sec}k identities/s, {progress_percentage}% progress",
                self.fixed_data.degree,
                row_index * 100 / self.fixed_data.degree,
                1_000_000_000 / duration.as_micros()
            );
            self.last_report = row_index;
        }
    }
}
