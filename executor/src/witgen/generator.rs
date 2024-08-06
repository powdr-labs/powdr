use powdr_ast::analyzed::{AlgebraicExpression as Expression, AlgebraicReference, PolyID};
use powdr_number::{DegreeType, FieldElement};
use std::collections::{BTreeMap, HashMap, HashSet};

use crate::witgen::data_structures::finalizable_data::FinalizableData;
use crate::witgen::machines::profiling::{record_end, record_start};
use crate::witgen::processor::OuterQuery;
use crate::witgen::EvalValue;
use crate::Identity;

use super::block_processor::BlockProcessor;
use super::machines::Machine;
use super::rows::{Row, RowIndex, RowPair};
use super::sequence_iterator::{DefaultSequenceIterator, ProcessingSequenceIterator};
use super::vm_processor::VmProcessor;
use super::{EvalResult, FixedData, MutableState, QueryCallback};

struct ProcessResult<'a, T: FieldElement> {
    eval_value: EvalValue<&'a AlgebraicReference, T>,
    block: FinalizableData<T>,
}

pub struct Generator<'a, T: FieldElement> {
    connecting_identities: BTreeMap<u64, &'a Identity<T>>,
    fixed_data: &'a FixedData<'a, T>,
    identities: Vec<&'a Identity<T>>,
    witnesses: HashSet<PolyID>,
    data: FinalizableData<T>,
    latch: Option<Expression<T>>,
    name: String,
    degree: DegreeType,
}

impl<'a, T: FieldElement> Machine<'a, T> for Generator<'a, T> {
    fn identity_ids(&self) -> Vec<u64> {
        self.connecting_identities.keys().cloned().collect()
    }

    fn degree(&self) -> DegreeType {
        self.degree
    }

    fn name(&self) -> &str {
        &self.name
    }

    fn process_plookup<'b, Q: QueryCallback<T>>(
        &mut self,
        mutable_state: &mut MutableState<'a, 'b, T, Q>,
        identity_id: u64,
        caller_rows: &'b RowPair<'b, 'a, T>,
    ) -> EvalResult<'a, T> {
        let identity = self.connecting_identities.get(&identity_id).unwrap();
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

        let ProcessResult { eval_value, block } =
            self.process(first_row, 0, mutable_state, Some(outer_query), false);

        let eval_value = if eval_value.is_complete() {
            log::trace!("End processing VM '{}' (successfully)", self.name());
            // Remove the last row of the previous block, as it is the first row of the current
            // block.
            self.data.pop();
            self.data.extend(block);

            eval_value.report_side_effect()
        } else {
            log::trace!("End processing VM '{}' (incomplete)", self.name());
            eval_value
        };
        Ok(eval_value)
    }

    fn take_witness_col_values<'b, Q: QueryCallback<T>>(
        &mut self,
        mutable_state: &'b mut MutableState<'a, 'b, T, Q>,
    ) -> HashMap<String, Vec<T>> {
        log::debug!("Finalizing VM: {}", self.name());

        self.fill_remaining_rows(mutable_state);
        self.fix_first_row();

        self.data
            .take_transposed()
            .map(|(id, (values, _))| (self.fixed_data.column_name(&id).to_string(), values))
            .collect()
    }
}

impl<'a, T: FieldElement> Generator<'a, T> {
    pub fn new(
        name: String,
        fixed_data: &'a FixedData<'a, T>,
        connecting_identities: &BTreeMap<u64, &'a Identity<T>>,
        identities: Vec<&'a Identity<T>>,
        witnesses: HashSet<PolyID>,
        latch: Option<Expression<T>>,
    ) -> Self {
        let data = FinalizableData::new(&witnesses);

        Self {
            degree: fixed_data.common_degree(&witnesses),
            connecting_identities: connecting_identities.clone(),
            name,
            fixed_data,
            identities,
            witnesses,
            data,
            latch,
        }
    }

    /// Runs the machine without any arguments from the first row.
    pub fn run<'b, Q: QueryCallback<T>>(&mut self, mutable_state: &mut MutableState<'a, 'b, T, Q>) {
        record_start(self.name());
        assert!(self.data.is_empty());
        let first_row = self.compute_partial_first_row(mutable_state);
        self.data = self.process(first_row, 0, mutable_state, None, true).block;
        record_end(self.name());
    }

    fn fill_remaining_rows<Q: QueryCallback<T>>(
        &mut self,
        mutable_state: &mut MutableState<'a, '_, T, Q>,
    ) {
        if self.data.len() < self.degree() as usize + 1 {
            assert!(self.latch.is_some());

            let first_row = self.data.pop().unwrap();
            let ProcessResult { block, eval_value } = self.process(
                first_row,
                self.data.len() as DegreeType,
                mutable_state,
                None,
                false,
            );
            assert!(eval_value.is_complete());

            self.data.extend(block);
        }
    }

    /// Runs the solver on the row pair (degree - 1, 0) in order to partially compute the first
    /// row from identities like `pc' = (1 - first_step') * <...>`.
    fn compute_partial_first_row<Q: QueryCallback<T>>(
        &self,
        mutable_state: &mut MutableState<'a, '_, T, Q>,
    ) -> Row<T> {
        // Use `BlockProcessor` + `DefaultSequenceIterator` using a "block size" of 0. Because `BlockProcessor`
        // expects `data` to include the row before and after the block, this means we'll run the
        // solver on exactly one row pair.
        // Note that using `BlockProcessor` instead of `VmProcessor` is more convenient here because
        // it does not assert that the row is "complete" afterwards (i.e., that all identities
        // are satisfied assuming 0 for unknown values).
        let data = FinalizableData::with_initial_rows_in_progress(
            &self.witnesses,
            [
                Row::fresh(self.fixed_data, RowIndex::from_i64(-1, self.degree())),
                Row::fresh(self.fixed_data, RowIndex::from_i64(0, self.degree())),
            ]
            .into_iter(),
        );

        // We're only interested in the first row anyway, so identities without a next reference
        // are irrelevant.
        // Also, they can lead to problems in the case where some witness columns are provided
        // externally, e.g. if the last row happens to call into a stateful machine like memory.
        let identities_with_next_reference = self
            .identities
            .iter()
            .filter_map(|identity| identity.contains_next_ref().then_some(*identity))
            .collect::<Vec<_>>();
        let mut processor = BlockProcessor::new(
            RowIndex::from_i64(-1, self.degree()),
            data,
            mutable_state,
            &identities_with_next_reference,
            self.fixed_data,
            &self.witnesses,
            self.degree,
        );
        let mut sequence_iterator = ProcessingSequenceIterator::Default(
            DefaultSequenceIterator::new(0, identities_with_next_reference.len(), None),
        );
        processor.solve(&mut sequence_iterator).unwrap();

        processor.finish().remove(1)
    }

    fn process<'b, Q: QueryCallback<T>>(
        &self,
        first_row: Row<T>,
        row_offset: DegreeType,
        mutable_state: &mut MutableState<'a, 'b, T, Q>,
        outer_query: Option<OuterQuery<'a, 'b, T>>,
        is_main_run: bool,
    ) -> ProcessResult<'a, T> {
        log::trace!(
            "Running main machine from row {row_offset} with the following initial values in the first row:\n{}",
            first_row.render_values(false, None, self.fixed_data)
        );
        let data = FinalizableData::with_initial_rows_in_progress(
            &self.witnesses,
            [first_row].into_iter(),
        );

        let degree = self.degree();

        let mut processor = VmProcessor::new(
            RowIndex::from_degree(row_offset, degree),
            self.fixed_data,
            &self.identities,
            &self.witnesses,
            data,
            mutable_state,
        );
        if let Some(outer_query) = outer_query {
            processor = processor.with_outer_query(outer_query);
        }
        let eval_value = processor.run(is_main_run);
        let block = processor.finish();
        ProcessResult { eval_value, block }
    }

    /// At the end of the solving algorithm, we'll have computed the first row twice
    /// (as row 0 and as row <degree>). This function merges the two versions.
    fn fix_first_row(&mut self) {
        assert_eq!(self.data.len() as DegreeType, self.degree() + 1);

        let last_row = self.data.pop().unwrap();
        self.data[0].merge_with(&last_row).unwrap();
    }
}
