use powdr_ast::analyzed::AlgebraicExpression as Expression;
use powdr_number::{DegreeType, FieldElement};
use std::collections::{BTreeMap, HashMap};

use crate::witgen::data_structures::finalizable_data::FinalizableData;
use crate::witgen::data_structures::mutable_state::MutableState;
use crate::witgen::machines::profiling::{record_end, record_start};
use crate::witgen::processor::OuterQuery;
use crate::witgen::EvalValue;

use super::affine_expression::AlgebraicVariable;
use super::block_processor::BlockProcessor;
use super::data_structures::multiplicity_counter::MultiplicityCounter;
use super::machines::{Machine, MachineParts};
use super::processor::SolverState;
use super::rows::{Row, RowIndex, RowPair};
use super::sequence_iterator::{DefaultSequenceIterator, ProcessingSequenceIterator};
use super::vm_processor::VmProcessor;
use super::{EvalResult, FixedData, QueryCallback};

struct ProcessResult<'a, T: FieldElement> {
    eval_value: EvalValue<AlgebraicVariable<'a>, T>,
    updated_data: SolverState<'a, T>,
}

pub struct Generator<'a, T: FieldElement> {
    fixed_data: &'a FixedData<'a, T>,
    parts: MachineParts<'a, T>,
    data: FinalizableData<T>,
    publics: BTreeMap<&'a str, T>,
    latch: Option<Expression<T>>,
    name: String,
    degree: DegreeType,
    multiplicity_counter: MultiplicityCounter,
}

impl<'a, T: FieldElement> Machine<'a, T> for Generator<'a, T> {
    fn identity_ids(&self) -> Vec<u64> {
        self.parts.identity_ids()
    }

    fn name(&self) -> &str {
        &self.name
    }

    fn process_plookup<'b, Q: QueryCallback<T>>(
        &mut self,
        mutable_state: &MutableState<'a, T, Q>,
        identity_id: u64,
        caller_rows: &'b RowPair<'b, 'a, T>,
    ) -> EvalResult<'a, T> {
        let identity = *self.parts.connections.get(&identity_id).unwrap();
        let outer_query = OuterQuery::new(caller_rows, identity);

        log::trace!("Start processing secondary VM '{}'", self.name());
        log::trace!("Arguments:");
        for (r, l) in identity.right.expressions.iter().zip(&outer_query.left) {
            log::trace!("  {r} = {l}");
        }

        let first_row = self
            .data
            .last()
            .cloned()
            .unwrap_or_else(|| self.compute_partial_first_row(mutable_state));

        let ProcessResult {
            eval_value,
            updated_data,
        } = self.process(first_row, 0, mutable_state, Some(outer_query), false);

        let eval_value = if eval_value.is_complete() {
            log::trace!("End processing VM '{}' (successfully)", self.name());
            // Remove the last row of the previous block, if it exists,
            // as it is the first row of the current block.
            self.data.pop();
            self.data.extend(updated_data.block);
            self.publics.extend(updated_data.publics);

            let latch_row = self.data.len() - 1;
            self.multiplicity_counter
                .increment_at_row(identity_id, latch_row);

            eval_value.report_side_effect()
        } else {
            log::trace!("End processing VM '{}' (incomplete)", self.name());
            eval_value
        };
        Ok(eval_value)
    }

    fn take_witness_col_values<'b, Q: QueryCallback<T>>(
        &mut self,
        mutable_state: &'b MutableState<'a, T, Q>,
    ) -> HashMap<String, Vec<T>> {
        log::debug!("Finalizing VM: {}", self.name());

        self.fill_remaining_rows(mutable_state);
        self.fix_first_row();

        self.data
            .take_transposed()
            .map(|(id, (values, _))| (id, values))
            .chain(
                self.multiplicity_counter
                    .generate_columns_single_size(self.degree),
            )
            .map(|(id, values)| (self.fixed_data.column_name(&id).to_string(), values))
            .collect()
    }
}

impl<'a, T: FieldElement> Generator<'a, T> {
    pub fn new(
        name: String,
        fixed_data: &'a FixedData<'a, T>,
        parts: MachineParts<'a, T>,
        latch: Option<Expression<T>>,
    ) -> Self {
        let data = FinalizableData::new(&parts.witnesses);
        let multiplicity_counter = MultiplicityCounter::new(&parts.connections);

        Self {
            degree: parts.common_degree_range().max,
            name,
            fixed_data,
            parts,
            data,
            publics: Default::default(),
            latch,
            multiplicity_counter,
        }
    }

    /// Runs the machine without any arguments from the first row.
    pub fn run<Q: QueryCallback<T>>(&mut self, mutable_state: &MutableState<'a, T, Q>) {
        record_start(self.name());
        assert!(self.data.is_empty());
        let first_row = self.compute_partial_first_row(mutable_state);
        self.data = self
            .process(first_row, 0, mutable_state, None, true)
            .updated_data
            .block;
        record_end(self.name());
    }

    fn fill_remaining_rows<Q: QueryCallback<T>>(&mut self, mutable_state: &MutableState<'a, T, Q>) {
        if self.data.len() < self.degree as usize + 1 {
            assert!(self.latch.is_some());

            let first_row = self.data.pop().unwrap();
            let ProcessResult {
                updated_data,
                eval_value,
            } = self.process(
                first_row,
                self.data.len() as DegreeType,
                mutable_state,
                None,
                false,
            );
            assert!(eval_value.is_complete());

            self.data.extend(updated_data.block);
            self.publics.extend(updated_data.publics);
        }
    }

    /// Runs the solver on the row pair (degree - 1, 0) in order to partially compute the first
    /// row from identities like `pc' = (1 - first_step') * <...>`.
    fn compute_partial_first_row<Q: QueryCallback<T>>(
        &self,
        mutable_state: &MutableState<'a, T, Q>,
    ) -> Row<T> {
        // Use `BlockProcessor` + `DefaultSequenceIterator` using a "block size" of 0. Because `BlockProcessor`
        // expects `data` to include the row before and after the block, this means we'll run the
        // solver on exactly one row pair.
        // Note that using `BlockProcessor` instead of `VmProcessor` is more convenient here because
        // it does not assert that the row is "complete" afterwards (i.e., that all identities
        // are satisfied assuming 0 for unknown values).
        let data = FinalizableData::with_initial_rows_in_progress(
            &self.parts.witnesses,
            [
                Row::fresh(self.fixed_data, RowIndex::from_i64(-1, self.degree)),
                Row::fresh(self.fixed_data, RowIndex::from_i64(0, self.degree)),
            ]
            .into_iter(),
        );

        // We're only interested in the first row anyway, so identities without a next reference
        // are irrelevant.
        // Also, they can lead to problems in the case where some witness columns are provided
        // externally, e.g. if the last row happens to call into a stateful machine like memory.
        let next_parts = self.parts.restricted_to_identities_with_next_references();
        let mut processor = BlockProcessor::new(
            RowIndex::from_i64(-1, self.degree),
            // Shouldn't need any publics at this point
            SolverState::without_publics(data),
            mutable_state,
            self.fixed_data,
            &next_parts,
            self.degree,
        );
        let mut sequence_iterator = ProcessingSequenceIterator::Default(
            DefaultSequenceIterator::new(0, next_parts.identities.len(), None),
        );
        processor.solve(&mut sequence_iterator).unwrap();

        // Ignore any updates to the publics at this point, as we'll re-visit the last row again.
        let mut block = processor.finish().block;
        assert!(block.len() == 2);
        block.pop().unwrap()
    }

    fn process<'c, Q: QueryCallback<T>>(
        &mut self,
        first_row: Row<T>,
        row_offset: DegreeType,
        mutable_state: &MutableState<'a, T, Q>,
        outer_query: Option<OuterQuery<'a, 'c, T>>,
        is_main_run: bool,
    ) -> ProcessResult<'a, T> {
        log::trace!(
            "Running main machine from row {row_offset} with the following initial values in the first row:\n{}",
            first_row.render_values(false, &self.parts)
        );
        let data = FinalizableData::with_initial_rows_in_progress(
            &self.parts.witnesses,
            [first_row].into_iter(),
        );

        let mut processor = VmProcessor::new(
            self.name().to_string(),
            RowIndex::from_degree(row_offset, self.degree),
            self.fixed_data,
            &self.parts,
            SolverState::new(data, self.publics.clone()),
            mutable_state,
        );
        if let Some(outer_query) = outer_query {
            processor = processor.with_outer_query(outer_query);
        }
        let eval_value = processor.run(is_main_run);
        let (updated_data, degree) = processor.finish();

        // The processor might have detected a loop, in which case the degree has changed
        self.degree = degree;

        ProcessResult {
            eval_value,
            updated_data,
        }
    }

    /// At the end of the solving algorithm, we'll have computed the first row twice
    /// (as row 0 and as row <degree>). This function merges the two versions.
    fn fix_first_row(&mut self) {
        assert_eq!(self.data.len() as DegreeType, self.degree + 1);

        let last_row = self.data.pop().unwrap();
        if self.data[0].merge_with(&last_row).is_err() {
            log::error!("{}", self.data[0].render("First row", false, &self.parts));
            log::error!("{}", last_row.render("Last row", false, &self.parts));
            panic!(
                "Failed to merge the first and last row of the VM '{}'",
                self.name()
            );
        }
    }
}
