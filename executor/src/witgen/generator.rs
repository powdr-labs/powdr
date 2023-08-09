use ast::analyzed::{Identity, PolyID};
use itertools::Itertools;
use number::{DegreeType, FieldElement};
use parser_util::lines::indent;
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::time::Instant;

use crate::witgen::identity_processor::IdentityProcessor;
use crate::witgen::rows::RowUpdater;

use super::column_map::ColumnMap;
use super::query_processor::QueryProcessor;
use super::range_constraints::RangeConstraint;

use super::machines::{FixedLookup, Machine};
use super::rows::{Row, RowFactory, RowPair};
use super::{EvalError, EvalResult, FixedData};

pub struct Generator<'a, T: FieldElement, QueryCallback: Send + Sync> {
    /// The witness columns belonging to this machine
    witnesses: BTreeSet<PolyID>,
    row_factory: RowFactory<'a, T>,
    identity_processor: IdentityProcessor<'a, T>,
    query_processor: Option<QueryProcessor<'a, T, QueryCallback>>,
    fixed_data: &'a FixedData<'a, T>,
    /// The subset of identities that contains a reference to the next row
    /// (precomputed once for performance reasons)
    identities_with_next_ref: Vec<&'a Identity<T>>,
    /// The subset of identities that does not contain a reference to the next row
    /// (precomputed once for performance reasons)
    identities_without_next_ref: Vec<&'a Identity<T>>,
    /// Values of the witness polynomials in the previous row (needed to check proposed rows)
    previous: Row<T>,
    /// Values of the witness polynomials
    current: Row<T>,
    /// Values of the witness polynomials in the next row
    next: Row<T>,
    current_row_index: DegreeType,
    last_report: DegreeType,
    last_report_time: Instant,
}

impl<'a, T: FieldElement, QueryCallback> Generator<'a, T, QueryCallback>
where
    QueryCallback: FnMut(&str) -> Option<T> + Send + Sync,
{
    pub fn new(
        fixed_data: &'a FixedData<'a, T>,
        fixed_lookup: &'a mut FixedLookup<T>,
        identities: &'a [&'a Identity<T>],
        witnesses: BTreeSet<PolyID>,
        global_range_constraints: ColumnMap<Option<RangeConstraint<T>>>,
        machines: Vec<Box<dyn Machine<T>>>,
        query_callback: Option<QueryCallback>,
    ) -> Self {
        let query_processor =
            query_callback.map(|query_callback| QueryProcessor::new(fixed_data, query_callback));
        let identity_processor = IdentityProcessor::new(fixed_data, fixed_lookup, machines);
        let row_factory = RowFactory::new(fixed_data, global_range_constraints);
        let default_row = row_factory.fresh_row();

        let (identities_with_next, identities_without_next): (Vec<_>, Vec<_>) = identities
            .iter()
            .partition(|identity| identity.contains_next_ref());

        let mut generator = Generator {
            row_factory,
            witnesses,
            query_processor,
            identity_processor,
            fixed_data,
            identities_with_next_ref: identities_with_next,
            identities_without_next_ref: identities_without_next,
            previous: default_row.clone(),
            current: default_row.clone(),
            next: default_row,
            current_row_index: fixed_data.degree - 1,
            last_report: 0,
            last_report_time: Instant::now(),
        };
        // For identities like `pc' = (1 - first_step') * <...>`, we need to process the last
        // row before processing the first row.
        // We set `with_check` to false so that it won't check that all identities hold assuming
        // zero for unknown values.
        generator.compute_next_row_or_initialize(fixed_data.degree - 1, false);
        generator
    }

    pub fn compute_next_row(&mut self, next_row: DegreeType) -> ColumnMap<T> {
        self.compute_next_row_or_initialize(next_row, true)
    }

    fn compute_next_row_or_initialize(
        &mut self,
        next_row: DegreeType,
        with_check: bool,
    ) -> ColumnMap<T> {
        self.set_next_row_and_log(next_row);
        log::trace!("Row: {}", self.current_row_index);

        log::trace!("  Going over all identities until no more progress is made");
        // First, go over identities that don't reference the next row,
        // Second, propagate values to the next row by going over identities that do reference the next row.
        let mut incomplete_identities = self
            .loop_until_no_progress(self.identities_without_next_ref.clone())
            .and_then(|mut incomplete_identities| {
                let more_incomplete_identities =
                    self.loop_until_no_progress(self.identities_with_next_ref.clone())?;
                incomplete_identities.extend(more_incomplete_identities);
                Ok(incomplete_identities)
            })
            .map_err(|e| self.report_failure_and_panic_unsatisfiable(e))
            .unwrap();

        if with_check && !incomplete_identities.is_empty() {
            log::trace!(
                "  Checking that remaining {} identities hold when unknown values are set to 0",
                incomplete_identities.len()
            );
            self.process_identities(&mut incomplete_identities, true)
                .map_err(|e| self.report_failure_and_panic_underconstrained(e))
                .unwrap();
        }

        log::trace!(
            "{}",
            self.current
                .render(&format!("===== Row {}", self.current_row_index), true)
        );

        self.shift_rows();

        self.previous.clone().into()
    }

    /// Loops over all identities and queries, until no further progress is made.
    /// @returns the "incomplete" identities, i.e. identities that contain unknown values.
    fn loop_until_no_progress(
        &mut self,
        mut identities: Vec<&'a Identity<T>>,
    ) -> Result<Vec<&'a Identity<T>>, Vec<EvalError<T>>> {
        loop {
            let mut progress = self.process_identities(&mut identities, false)?;
            if let Some(ref mut query_processor) = self.query_processor {
                let row_pair = RowPair::new(
                    &self.current,
                    &self.next,
                    self.current_row_index,
                    self.fixed_data,
                    false,
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
        Ok(identities)
    }

    /// Loops over all identities once and updates the current row and next row.
    /// Arguments:
    /// * `identities`: Identities to process. Completed identities are removed from the list.
    /// * `frozen`: If true, the identities are processed assuming that all unknown values are 0.
    ///             Also, no updates are applied to the current row.
    /// Returns:
    /// * `Ok(true)`: If progress was made.
    /// * `Ok(false)`: If no progress was made.
    /// * `Err(errors)`: If an error occurred.
    fn process_identities(
        &mut self,
        identities: &mut Vec<&'a Identity<T>>,
        frozen: bool,
    ) -> Result<bool, Vec<EvalError<T>>> {
        let mut progress = false;
        let mut errors = vec![];
        let mut incomplete_identities = vec![];

        for identity in identities.iter() {
            let row_pair = RowPair::new(
                &self.current,
                &self.next,
                self.current_row_index,
                self.fixed_data,
                frozen,
            );
            let result: EvalResult<'a, T> = self
                .identity_processor
                .process_identity(*identity, &row_pair)
                .map_err(|err| {
                    format!("{identity}:\n{}", indent(&format!("{err}"), "    ")).into()
                });

            match result {
                Ok(eval_value) => {
                    if frozen {
                        assert!(eval_value.constraints.is_empty())
                    } else {
                        if !eval_value.is_complete() {
                            incomplete_identities.push(*identity);
                        }
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

        *identities = incomplete_identities;

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

    fn report_failure_and_panic_unsatisfiable(&self, failures: Vec<EvalError<T>>) {
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

    fn report_failure_and_panic_underconstrained(&self, failures: Vec<EvalError<T>>) {
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
    pub fn propose_next_row(&mut self, next_row: DegreeType, values: &ColumnMap<T>) -> bool {
        self.set_next_row_and_log(next_row);

        let proposed_row = self.row_factory.row_from_known_values(values);

        let constraints_valid =
            self.check_row_pair(&proposed_row, false) && self.check_row_pair(&proposed_row, true);

        if constraints_valid {
            self.previous = proposed_row;
        } else {
            // Note that we never update `current` if proposing a row succeeds (the happy path).
            // If it doesn't, we re-run compute_next_row on the previous row in order to
            // correctly forward-propagate values via next references.
            std::mem::swap(&mut self.current, &mut self.previous);
            self.next = self.row_factory.fresh_row();
            self.compute_next_row(next_row - 1);
        }
        constraints_valid
    }

    fn check_row_pair(&mut self, proposed_row: &Row<T>, previous: bool) -> bool {
        let row_pair = match previous {
            // Check whether identities with a reference to the next row are satisfied
            // when applied to the previous row and the proposed row.
            true => RowPair::new(
                &self.previous,
                proposed_row,
                self.current_row_index - 1,
                self.fixed_data,
                true,
            ),
            // Check whether identities without a reference to the next row are satisfied
            // when applied to the proposed row.
            // Note that we also provide the next row here, but it is not used.
            false => RowPair::new(
                proposed_row,
                &self.next,
                self.current_row_index,
                self.fixed_data,
                true,
            ),
        };

        // Split identities into whether or not they have a reference to the next row:
        // - Those that do should be checked on the previous and proposed row
        // - All others should be checked on the proposed row
        let identities = match previous {
            true => &self.identities_with_next_ref,
            false => &self.identities_without_next_ref,
        };

        for identity in identities.iter() {
            if self
                .identity_processor
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

    pub fn machine_witness_col_values(&mut self) -> HashMap<PolyID, Vec<T>> {
        let mut result: HashMap<_, _> = Default::default();
        let name_to_id = self
            .fixed_data
            .witness_column_names
            .iter()
            .map(|(poly_id, &name)| (name, poly_id))
            .collect::<BTreeMap<_, _>>();
        for m in &mut self.identity_processor.machines {
            for (col_name, col) in m.witness_col_values(self.fixed_data) {
                result.insert(*name_to_id.get(col_name.as_str()).unwrap(), col);
            }
        }
        result
    }

    fn set_next_row_and_log(&mut self, next_row: DegreeType) {
        if next_row != self.fixed_data.degree - 1 && next_row >= self.last_report + 1000 {
            let duration = self.last_report_time.elapsed();
            self.last_report_time = Instant::now();

            log::info!(
                "{next_row} of {} rows ({} %, {} rows per second)",
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
