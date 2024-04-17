use powdr_ast::analyzed::{
    AlgebraicExpression as Expression, AlgebraicReference, Identity, PolyID,
};
use powdr_ast::parsed::SelectedExpressions;
use powdr_number::{DegreeType, FieldElement};
use std::collections::{BTreeMap, HashMap, HashSet};

use crate::witgen::data_structures::finalizable_data::FinalizableData;
use crate::witgen::machines::profiling::{record_end, record_start};
use crate::witgen::processor::OuterQuery;
use crate::witgen::rows::CellValue;
use crate::witgen::EvalValue;

use super::affine_expression::AffineExpression;
use super::block_processor::BlockProcessor;
use super::data_structures::column_map::WitnessColumnMap;
use super::machines::{FixedLookup, Machine};
use super::rows::{Row, RowIndex};
use super::sequence_iterator::{DefaultSequenceIterator, ProcessingSequenceIterator};
use super::vm_processor::VmProcessor;
use super::{EvalResult, FixedData, MutableState, QueryCallback};

struct ProcessResult<'a, T: FieldElement> {
    eval_value: EvalValue<&'a AlgebraicReference, T>,
    block: FinalizableData<'a, T>,
}

pub struct Generator<'a, T: FieldElement> {
    connecting_rhs: BTreeMap<u64, &'a SelectedExpressions<Expression<T>>>,
    fixed_data: &'a FixedData<'a, T>,
    identities: Vec<&'a Identity<Expression<T>>>,
    witnesses: HashSet<PolyID>,
    data: FinalizableData<'a, T>,
    latch: Option<Expression<T>>,
    name: String,
}

impl<'a, T: FieldElement> Machine<'a, T> for Generator<'a, T> {
    fn identity_ids(&self) -> Vec<u64> {
        self.connecting_rhs.keys().cloned().collect()
    }

    fn name(&self) -> &str {
        &self.name
    }

    fn process_plookup<Q: QueryCallback<T>>(
        &mut self,
        mutable_state: &mut MutableState<'a, '_, T, Q>,
        identity_id: u64,
        args: &[AffineExpression<&'a AlgebraicReference, T>],
    ) -> EvalResult<'a, T> {
        log::trace!("Start processing secondary VM '{}'", self.name());
        log::trace!("Arguments:");
        let right = &self.connecting_rhs.get(&identity_id).unwrap();
        for (r, l) in right.expressions.iter().zip(args) {
            log::trace!("  {r} = {l}");
        }

        let first_row = self
            .data
            .last()
            .cloned()
            .unwrap_or_else(|| self.compute_partial_first_row(mutable_state));

        let outer_query = OuterQuery {
            left: args.to_vec(),
            right,
        };
        let ProcessResult { eval_value, block } =
            self.process(first_row, 0, mutable_state, Some(outer_query), false);

        if eval_value.is_complete() {
            log::trace!("End processing VM '{}' (successfully)", self.name());
            // Remove the last row of the previous block, as it is the first row of the current
            // block.
            self.data.pop();
            self.data.extend(block);
        } else {
            log::trace!("End processing VM '{}' (incomplete)", self.name());
        }
        Ok(eval_value)
    }

    fn take_witness_col_values<'b, Q: QueryCallback<T>>(
        &mut self,
        fixed_lookup: &'b mut FixedLookup<T>,
        query_callback: &'b mut Q,
    ) -> HashMap<String, Vec<T>> {
        log::debug!("Finalizing VM: {}", self.name());

        // In this stage, we don't have access to other machines, as they might already be finalized.
        let mut mutable_state_no_machines = MutableState {
            fixed_lookup,
            machines: [].into_iter().into(),
            query_callback,
        };

        self.fill_remaining_rows(&mut mutable_state_no_machines);
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
        connecting_identities: &[&'a Identity<Expression<T>>],
        identities: Vec<&'a Identity<Expression<T>>>,
        witnesses: HashSet<PolyID>,
        latch: Option<Expression<T>>,
    ) -> Self {
        let data = FinalizableData::new(&witnesses);
        Self {
            connecting_rhs: connecting_identities
                .iter()
                .map(|&identity| (identity.id, &identity.right))
                .collect(),
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
        if self.data.len() < self.fixed_data.degree as usize + 1 {
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
    ) -> Row<'a, T> {
        // Use `BlockProcessor` + `DefaultSequenceIterator` using a "block size" of 0. Because `BlockProcessor`
        // expects `data` to include the row before and after the block, this means we'll run the
        // solver on exactly one row pair.
        // Note that using `BlockProcessor` instead of `VmProcessor` is more convenient here because
        // it does not assert that the row is "complete" afterwards (i.e., that all identities
        // are satisfied assuming 0 for unknown values).
        let data = FinalizableData::with_initial_rows_in_progress(
            &self.witnesses,
            [
                Row::fresh(
                    self.fixed_data,
                    RowIndex::from_i64(-1, self.fixed_data.degree),
                ),
                Row::fresh(
                    self.fixed_data,
                    RowIndex::from_i64(0, self.fixed_data.degree),
                ),
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
            RowIndex::from_i64(-1, self.fixed_data.degree),
            data,
            mutable_state,
            &identities_with_next_reference,
            self.fixed_data,
            &self.witnesses,
        );
        let mut sequence_iterator = ProcessingSequenceIterator::Default(
            DefaultSequenceIterator::new(0, identities_with_next_reference.len(), None),
        );
        processor.solve(&mut sequence_iterator).unwrap();
        let first_row = processor.finish().remove(1);

        first_row
    }

    fn process<Q: QueryCallback<T>>(
        &self,
        first_row: Row<'a, T>,
        row_offset: DegreeType,
        mutable_state: &mut MutableState<'a, '_, T, Q>,
        outer_query: Option<OuterQuery<'a, T>>,
        is_main_run: bool,
    ) -> ProcessResult<'a, T> {
        log::trace!(
            "Running main machine from row {row_offset} with the following initial values in the first row:\n{}", first_row.render_values(false, None)
        );
        let data = FinalizableData::with_initial_rows_in_progress(
            &self.witnesses,
            [first_row].into_iter(),
        );
        let mut processor = VmProcessor::new(
            RowIndex::from_degree(row_offset, self.fixed_data.degree),
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
        assert_eq!(self.data.len() as DegreeType, self.fixed_data.degree + 1);

        let last_row = self.data.pop().unwrap();
        self.data[0] = WitnessColumnMap::from(self.data[0].values().zip(last_row.values()).map(
            |(cell1, cell2)| match (&cell1.value, &cell2.value) {
                (CellValue::Known(v1), CellValue::Known(v2)) => {
                    assert_eq!(v1, v2);
                    cell1.clone()
                }
                (CellValue::Known(_), _) => cell1.clone(),
                _ => cell2.clone(),
            },
        ));
    }
}
