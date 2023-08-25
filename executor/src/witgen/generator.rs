use ast::analyzed::{Identity, IdentityKind, PolyID, PolynomialReference, SelectedExpressions};
use itertools::Itertools;
use number::{DegreeType, FieldElement};
use parser_util::lines::indent;
use std::cmp::max;
use std::collections::HashSet;
use std::collections::{BTreeSet, HashMap};
use std::time::Instant;

use crate::witgen::identity_processor::{self, IdentityProcessor};
use crate::witgen::rows::RowUpdater;

use super::affine_expression::AffineExpression;
use super::column_map::WitnessColumnMap;
use super::machines::Machine;
use super::query_processor::QueryProcessor;
use super::range_constraints::RangeConstraint;

use super::machines::FixedLookup;
use super::rows::{Row, RowFactory, RowPair, UnknownStrategy};
use super::{EvalError, EvalResult, FixedData, MutableState};

/// Phase in which [Generator::compute_next_row_or_initialize] is called.
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

pub struct Generator<'a, T: FieldElement> {
    /// The witness columns belonging to this machine
    witnesses: BTreeSet<PolyID>,
    row_factory: RowFactory<'a, T>,
    fixed_data: &'a FixedData<'a, T>,
    /// The subset of identities that contains a reference to the next row
    /// (precomputed once for performance reasons)
    identities_with_next_ref: Vec<&'a Identity<T>>,
    /// The subset of identities that does not contain a reference to the next row
    /// (precomputed once for performance reasons)
    identities_without_next_ref: Vec<&'a Identity<T>>,
    /// Values of the witness polynomials in the first row (needed for checking wrap-around)
    first: Row<'a, T>,
    /// Values of the witness polynomials in the previous row (needed to check proposed rows)
    previous: Row<'a, T>,
    /// Values of the witness polynomials
    current: Row<'a, T>,
    /// Values of the witness polynomials in the next row
    next: Row<'a, T>,
    current_row_index: DegreeType,
    last_report: DegreeType,
    last_report_time: Instant,
}

impl<'a, T: FieldElement> Machine<'a, T> for Generator<'a, T> {
    fn process_plookup(
        &mut self,
        _fixed_data: &'a FixedData<T>,
        _fixed_lookup: &mut FixedLookup<T>,
        _kind: IdentityKind,
        _left: &[AffineExpression<&'a PolynomialReference, T>],
        _right: &'a SelectedExpressions<T>,
    ) -> Option<EvalResult<'a, T>> {
        unimplemented!()
    }

    fn take_witness_col_values(
        &mut self,
        _fixed_data: &FixedData<T>,
        _fixed_lookup: &mut FixedLookup<T>,
    ) -> HashMap<String, Vec<T>> {
        unimplemented!()
    }
}

impl<'a, T: FieldElement> Generator<'a, T> {
    pub fn new(
        fixed_data: &'a FixedData<'a, T>,
        identities: &[&'a Identity<T>],
        witnesses: &HashSet<PolyID>,
        global_range_constraints: &WitnessColumnMap<Option<RangeConstraint<T>>>,
    ) -> Self {
        let row_factory = RowFactory::new(fixed_data, global_range_constraints.clone());
        let default_row = row_factory.fresh_row();

        let (identities_with_next, identities_without_next): (Vec<_>, Vec<_>) = identities
            .iter()
            .partition(|identity| identity.contains_next_ref());

        Generator {
            row_factory,
            witnesses: witnesses.clone().into_iter().collect(),
            fixed_data,
            identities_with_next_ref: identities_with_next,
            identities_without_next_ref: identities_without_next,
            first: default_row.clone(),
            previous: default_row.clone(),
            current: default_row.clone(),
            next: default_row,
            current_row_index: fixed_data.degree - 1,
            last_report: 0,
            last_report_time: Instant::now(),
        }
    }

    fn last_row(&self) -> DegreeType {
        self.fixed_data.degree - 1
    }

    pub fn compute_next_row<Q>(
        &mut self,
        next_row: DegreeType,
        mutable_state: &mut MutableState<'a, T, Q>,
    ) -> WitnessColumnMap<T>
    where
        Q: FnMut(&str) -> Option<T> + Send + Sync,
    {
        if next_row == 0 {
            // For identities like `pc' = (1 - first_step') * <...>`, we need to process the last
            // row before processing the first row.
            self.compute_next_row_or_initialize(
                self.last_row(),
                ProcessingPhase::Initialization,
                mutable_state,
            );
        }
        self.compute_next_row_or_initialize(next_row, ProcessingPhase::Regular, mutable_state)
    }

    /// Update the first row for the wrap-around.
    pub fn update_first_row(&mut self) -> WitnessColumnMap<T> {
        assert_eq!(self.current_row_index, self.last_row());
        WitnessColumnMap::<T>::from(self.first.values().zip(self.current.values()).map(
            |(first, new_first)| {
                let first = first.value.clone();
                let new_first = new_first.value.clone();
                match (
                    (first.is_known(), first.unwrap_or_default()),
                    (new_first.is_known(), new_first.unwrap_or_default()),
                ) {
                    ((true, x), (true, y)) => {
                        // TODO we should probably print a proper error.
                        assert_eq!(x, y);
                        x
                    }
                    ((false, _), (true, y)) => y,
                    ((_, x), (_, _)) => x,
                }
            },
        ))
    }

    fn compute_next_row_or_initialize<Q>(
        &mut self,
        next_row: DegreeType,
        phase: ProcessingPhase,
        mutable_state: &mut MutableState<'a, T, Q>,
    ) -> WitnessColumnMap<T>
    where
        Q: FnMut(&str) -> Option<T> + Send + Sync,
    {
        if phase == ProcessingPhase::Initialization {
            assert_eq!(next_row, self.last_row());
            self.current_row_index = next_row;
        } else {
            self.set_next_row_and_log(next_row);
        }

        log::trace!("Row: {}", self.current_row_index);

        let mut identity_processor = IdentityProcessor::new(
            self.fixed_data,
            &mut mutable_state.fixed_lookup,
            &mut mutable_state.machines,
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
            &mut identities_without_next_ref,
            &mut identity_processor,
            &mut query_processor,
        )
        .and_then(|_| {
            self.loop_until_no_progress(
                &mut identities_with_next_ref,
                &mut identity_processor,
                &mut query_processor,
            )
        })
        .map_err(|e| self.report_failure_and_panic_unsatisfiable(e))
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
                &mut identities_without_next_ref,
                UnknownStrategy::Zero,
                &mut identity_processor,
            )
            .and_then(|_| {
                self.process_identities(
                    &mut identities_with_next_ref,
                    UnknownStrategy::Zero,
                    &mut identity_processor,
                )
            })
            .map_err(|e| self.report_failure_and_panic_underconstrained(e))
            .unwrap();
        }

        log::trace!(
            "{}",
            self.current
                .render(&format!("===== Row {}", self.current_row_index), true)
        );

        self.shift_rows();

        if next_row == 0 {
            self.first = self.previous.clone()
        }

        self.previous.clone().into()
    }

    /// Loops over all identities and queries, until no further progress is made.
    /// @returns the "incomplete" identities, i.e. identities that contain unknown values.
    fn loop_until_no_progress<Q>(
        &mut self,
        identities: &mut CompletableIdentities<'a, T>,
        identity_processor: &mut IdentityProcessor<'a, '_, T>,
        query_processor: &mut Option<QueryProcessor<'a, '_, T, Q>>,
    ) -> Result<(), Vec<EvalError<T>>>
    where
        Q: FnMut(&str) -> Option<T> + Send + Sync,
    {
        loop {
            let mut progress =
                self.process_identities(identities, UnknownStrategy::Unknown, identity_processor)?;
            if let Some(query_processor) = query_processor.as_mut() {
                let row_pair = RowPair::new(
                    &self.current,
                    &self.next,
                    self.current_row_index,
                    self.fixed_data,
                    UnknownStrategy::Unknown,
                );
                let updates = query_processor.process_queries_on_current_row(&row_pair);
                let mut row_updater =
                    RowUpdater::new(&mut self.current, &mut self.next, self.current_row_index);
                progress |= row_updater.apply_updates(&updates, || "query".to_string());
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

            let row_pair = RowPair::new(
                &self.current,
                &self.next,
                self.current_row_index,
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
                        let mut row_updater = RowUpdater::new(
                            &mut self.current,
                            &mut self.next,
                            self.current_row_index,
                        );
                        progress |=
                            row_updater.apply_updates(&eval_value, || format!("{identity}"));
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

    /// Shifts rows: fresh row -> next -> current -> previous
    fn shift_rows(&mut self) {
        let mut fresh_row = self.row_factory.fresh_row();
        std::mem::swap(&mut self.previous, &mut fresh_row);
        std::mem::swap(&mut self.current, &mut self.previous);
        std::mem::swap(&mut self.next, &mut self.current);
    }

    fn report_failure_and_panic_unsatisfiable(&self, failures: Vec<EvalError<T>>) -> ! {
        log::error!(
            "\nError: Row {} failed. Set RUST_LOG=debug for more information.\n",
            self.current_row_index
        );
        log::debug!("Some identities where not satisfiable after the following values were uniquely determined (known nonzero first, then zero, unknown omitted):");
        log::debug!("{}", self.current.render("Current Row", false));
        log::debug!("{}", self.next.render("Next Row", false));
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

    fn report_failure_and_panic_underconstrained(&self, failures: Vec<EvalError<T>>) -> ! {
        log::error!(
            "\nError: Row {} failed. Set RUST_LOG=debug for more information.\n",
            self.current_row_index
        );

        log::debug!("Some columns could not be determined, but setting them to zero does not satisfy the constraints. This typically means that the system is underconstrained!");
        log::debug!("{}", self.current.render("Current Row", true));
        log::debug!("{}", self.next.render("Next Row", true));
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
    pub fn propose_next_row<Q>(
        &mut self,
        next_row: DegreeType,
        values: &WitnessColumnMap<T>,
        mutable_state: &mut MutableState<'a, T, Q>,
    ) -> bool
    where
        Q: FnMut(&str) -> Option<T> + Send + Sync,
    {
        self.set_next_row_and_log(next_row);

        let proposed_row = self.row_factory.row_from_known_values_dense(values);

        let mut identity_processor = IdentityProcessor::new(
            self.fixed_data,
            &mut mutable_state.fixed_lookup,
            &mut mutable_state.machines,
        );
        let constraints_valid = self.check_row_pair(&proposed_row, false, &mut identity_processor)
            && self.check_row_pair(&proposed_row, true, &mut identity_processor);

        if constraints_valid {
            self.previous = proposed_row;
        } else {
            // Note that we never update `current` if proposing a row succeeds (the happy path).
            // If it doesn't, we re-run compute_next_row on the previous row in order to
            // correctly forward-propagate values via next references.
            std::mem::swap(&mut self.current, &mut self.previous);
            self.next = self.row_factory.fresh_row();
            self.compute_next_row(next_row - 1, mutable_state);
        }
        constraints_valid
    }

    fn check_row_pair(
        &mut self,
        proposed_row: &Row<'a, T>,
        previous: bool,
        identity_processor: &mut IdentityProcessor<'a, '_, T>,
    ) -> bool {
        let row_pair = match previous {
            // Check whether identities with a reference to the next row are satisfied
            // when applied to the previous row and the proposed row.
            true => RowPair::new(
                &self.previous,
                proposed_row,
                self.current_row_index - 1,
                self.fixed_data,
                UnknownStrategy::Zero,
            ),
            // Check whether identities without a reference to the next row are satisfied
            // when applied to the proposed row.
            // Note that we also provide the next row here, but it is not used.
            false => RowPair::new(
                proposed_row,
                &self.next,
                self.current_row_index,
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
                log::debug!("Previous {:?}", self.previous);
                log::debug!("Proposed {:?}", proposed_row);
                log::debug!("Failed on identity: {}", identity);

                return false;
            }
        }
        true
    }

    fn set_next_row_and_log(&mut self, next_row: DegreeType) {
        if next_row >= self.last_report + 1000 {
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
                "{next_row} of {} rows ({}%) - {} rows/s, {identities_per_sec}k identities/s, {progress_percentage}% progress",
                self.fixed_data.degree,
                next_row * 100 / self.fixed_data.degree,
                1_000_000_000 / duration.as_micros()
            );
            self.last_report = next_row;
        }
        self.current_row_index = next_row;
    }

    /// Returns true if this is a witness column we care about (instead of a sub-machine witness).
    pub fn is_relevant_witness(&self, id: &PolyID) -> bool {
        self.witnesses.contains(id)
    }
}
