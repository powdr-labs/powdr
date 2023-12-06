use ast::analyzed::{
    AlgebraicExpression as Expression, AlgebraicReference, Identity, IdentityKind, PolyID,
};
use ast::parsed::SelectedExpressions;
use number::{DegreeType, FieldElement};
use std::collections::{HashMap, HashSet};

use crate::witgen::data_structures::finalizable_data::FinalizableData;
use crate::witgen::processor::OuterQuery;
use crate::witgen::rows::CellValue;
use crate::witgen::EvalValue;

use super::affine_expression::AffineExpression;
use super::block_processor::BlockProcessor;
use super::data_structures::column_map::WitnessColumnMap;
use super::global_constraints::GlobalConstraints;
use super::machines::{FixedLookup, Machine};
use super::rows::{Row, RowFactory};
use super::sequence_iterator::{DefaultSequenceIterator, ProcessingSequenceIterator};
use super::vm_processor::VmProcessor;
use super::{EvalResult, FixedData, MutableState, QueryCallback};

struct ProcessResult<'a, T: FieldElement> {
    eval_value: EvalValue<&'a AlgebraicReference, T>,
    block: FinalizableData<'a, T>,
}

pub struct Generator<'a, T: FieldElement> {
    fixed_data: &'a FixedData<'a, T>,
    identities: Vec<&'a Identity<Expression<T>>>,
    witnesses: HashSet<PolyID>,
    global_range_constraints: GlobalConstraints<T>,
    data: FinalizableData<'a, T>,
    latch: Option<Expression<T>>,
}

impl<'a, T: FieldElement> Machine<'a, T> for Generator<'a, T> {
    fn process_plookup<Q: QueryCallback<T>>(
        &mut self,
        mutable_state: &mut MutableState<'a, '_, T, Q>,
        _kind: IdentityKind,
        left: &[AffineExpression<&'a AlgebraicReference, T>],
        right: &'a SelectedExpressions<Expression<T>>,
    ) -> Option<EvalResult<'a, T>> {
        if right.selector != self.latch {
            None
        } else {
            log::trace!("Start processing secondary VM '{}'", self.name());
            log::trace!("Arguments:");
            for (r, l) in right.expressions.iter().zip(left) {
                log::trace!("  {r} = {l}");
            }

            let first_row = self
                .data
                .last()
                .cloned()
                .unwrap_or_else(|| self.compute_partial_first_row(mutable_state));

            let outer_query = OuterQuery {
                left: left.to_vec(),
                right,
            };
            let ProcessResult { eval_value, block } =
                self.process(first_row, 0, mutable_state, Some(outer_query));

            if eval_value.is_complete() {
                log::trace!("End processing VM '{}' (successfully)", self.name());
                // Remove the last row of the previous block, as it is the first row of the current
                // block.
                self.data.pop();
                self.data.extend(block);
            } else {
                log::trace!("End processing VM '{}' (incomplete)", self.name());
            }
            Some(Ok(eval_value))
        }
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
        fixed_data: &'a FixedData<'a, T>,
        identities: &[&'a Identity<Expression<T>>],
        witnesses: HashSet<PolyID>,
        global_range_constraints: &GlobalConstraints<T>,
        latch: Option<Expression<T>>,
    ) -> Self {
        let data = FinalizableData::new(&witnesses);
        Self {
            fixed_data,
            identities: identities.to_vec(),
            witnesses,
            global_range_constraints: global_range_constraints.clone(),
            data,
            latch,
        }
    }

    pub fn name(&self) -> &str {
        let first_witness = self.witnesses.iter().next().unwrap();
        let first_witness_name = self.fixed_data.column_name(first_witness);
        let namespace = first_witness_name
            .rfind('.')
            .map(|idx| &first_witness_name[..idx]);

        // For machines compiled using Powdr ASM we'll always have a namespace, but as a last
        // resort we'll use the first witness name.
        namespace.unwrap_or(first_witness_name)
    }

    /// Runs the machine without any arguments from the first row.
    pub fn run<'b, Q: QueryCallback<T>>(&mut self, mutable_state: &mut MutableState<'a, 'b, T, Q>) {
        assert!(self.data.is_empty());
        let first_row = self.compute_partial_first_row(mutable_state);
        self.data = self.process(first_row, 0, mutable_state, None).block;
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
        let row_factory = RowFactory::new(self.fixed_data, self.global_range_constraints.clone());
        let data = FinalizableData::with_initial_rows_in_progress(
            &self.witnesses,
            [
                row_factory.fresh_row(self.fixed_data.degree - 1),
                row_factory.fresh_row(0),
            ]
            .into_iter(),
        );
        let mut processor = BlockProcessor::new(
            self.fixed_data.degree - 1,
            data,
            mutable_state,
            &self.identities,
            self.fixed_data,
            &self.witnesses,
        );
        let mut sequence_iterator = ProcessingSequenceIterator::Default(
            DefaultSequenceIterator::new(0, self.identities.len(), None),
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
    ) -> ProcessResult<'a, T> {
        log::trace!(
            "Running main machine from row {row_offset} with the following initial values in the first row:\n{}", first_row.render_values(false, None)
        );
        let row_factory = RowFactory::new(self.fixed_data, self.global_range_constraints.clone());
        let data = FinalizableData::with_initial_rows_in_progress(
            &self.witnesses,
            [first_row].into_iter(),
        );
        let mut processor = VmProcessor::new(
            row_offset,
            self.fixed_data,
            &self.identities,
            &self.witnesses,
            data,
            row_factory,
            mutable_state,
        );
        if let Some(outer_query) = outer_query {
            processor = processor.with_outer_query(outer_query);
        }
        let eval_value = processor.run();
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
