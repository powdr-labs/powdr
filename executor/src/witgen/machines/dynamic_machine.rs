use powdr_ast::analyzed::AlgebraicExpression as Expression;
use powdr_number::{DegreeType, FieldElement};
use std::collections::{BTreeMap, HashMap};

use crate::witgen::block_processor::BlockProcessor;
use crate::witgen::data_structures::finalizable_data::FinalizableData;
use crate::witgen::data_structures::mutable_state::MutableState;
use crate::witgen::global_constraints::RangeConstraintSet;
use crate::witgen::machines::{Machine, MachineParts};
use crate::witgen::processor::{OuterQuery, SolverState};
use crate::witgen::rows::{Row, RowIndex};
use crate::witgen::sequence_iterator::{DefaultSequenceIterator, ProcessingSequenceIterator};
use crate::witgen::vm_processor::VmProcessor;
use crate::witgen::{
    AffineExpression, AlgebraicVariable, EvalError, EvalResult, EvalValue, FixedData, QueryCallback,
};

use super::LookupCell;

struct ProcessResult<'a, T: FieldElement> {
    eval_value: EvalValue<AlgebraicVariable<'a>, T>,
    updated_data: SolverState<'a, T>,
}

/// A machine is generic and can handle lookups that generate a dynamic number of rows.
pub struct DynamicMachine<'a, T: FieldElement> {
    fixed_data: &'a FixedData<'a, T>,
    parts: MachineParts<'a, T>,
    data: FinalizableData<'a, T>,
    publics: BTreeMap<&'a str, T>,
    latch: Option<Expression<T>>,
    name: String,
    degree: DegreeType,
}

impl<'a, T: FieldElement> Machine<'a, T> for DynamicMachine<'a, T> {
    fn process_lookup_direct<'b, 'c, Q: QueryCallback<T>>(
        &mut self,
        _mutable_state: &'b MutableState<'a, T, Q>,
        _identity_id: u64,
        _values: &mut [LookupCell<'c, T>],
    ) -> Result<bool, EvalError<T>> {
        unimplemented!("Direct lookup not supported by machine {}.", self.name())
    }

    fn identity_ids(&self) -> Vec<u64> {
        self.parts.identity_ids()
    }

    fn name(&self) -> &str {
        &self.name
    }

    /// Runs the machine without any arguments from the first row.
    fn run<Q: QueryCallback<T>>(&mut self, mutable_state: &MutableState<'a, T, Q>) {
        assert!(self.data.is_empty());
        let first_row = self.compute_partial_first_row(mutable_state);
        let process_result = self
            .process(first_row, 0, mutable_state, None, true)
            .updated_data;
        self.data = process_result.block;
        self.publics.extend(process_result.publics); // at least for the fibonacci example, this is required here to pass the publics to witgen results (i think it might be that the main machine is the dynamic machine and it's not called via a lookup, so we have to also extend publics here as well if it's called via run(), which means the first machine called i think, typically the main)
    }

    fn process_plookup<'b, Q: QueryCallback<T>>(
        &mut self,
        mutable_state: &MutableState<'a, T, Q>,
        identity_id: u64,
        arguments: &[AffineExpression<AlgebraicVariable<'a>, T>],
        range_constraints: &dyn RangeConstraintSet<AlgebraicVariable<'a>, T>,
    ) -> EvalResult<'a, T> {
        let identity = *self.parts.connections.get(&identity_id).unwrap();
        let outer_query = OuterQuery::new(arguments, range_constraints, identity);

        log::trace!("Start processing secondary VM '{}'", self.name());
        log::trace!("Arguments:");
        for (r, l) in identity
            .right
            .expressions
            .iter()
            .zip(&outer_query.arguments)
        {
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
            .map(|(id, values)| (self.fixed_data.column_name(&id).to_string(), values))
            .collect()
    }

    fn take_public_values(&mut self) -> BTreeMap<String, T> {
        println!("dynamic macine take public values: {:?}", self.publics);
        std::mem::take(&mut self.publics)
            .into_iter()
            .map(|(key, value)| (key.to_string(), value))
            .collect()
    }
}

impl<'a, T: FieldElement> DynamicMachine<'a, T> {
    pub fn new(
        name: String,
        fixed_data: &'a FixedData<'a, T>,
        parts: MachineParts<'a, T>,
        latch: Option<Expression<T>>,
    ) -> Self {
        let data = FinalizableData::new(&parts.witnesses, fixed_data);

        Self {
            degree: parts.common_degree_range().max,
            name,
            fixed_data,
            parts,
            data,
            publics: Default::default(),
            latch,
        }
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
        // We're only interested in the first row, so identities without a next reference
        // are irrelevant.
        // Also, they can lead to problems in the case where some witness columns are provided
        // externally, e.g. if the last row happens to call into a stateful machine like memory.
        let mut block = self.compute_row_block(mutable_state, -1, vec![None, None], true);
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
            self.fixed_data,
        );

        let mut processor = VmProcessor::new(
            self.name().to_string(),
            RowIndex::from_degree(row_offset, self.degree),
            self.fixed_data,
            &self.parts,
            SolverState::new(data, self.publics.clone()),
            mutable_state,
            self.degree,
            true,
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

    /// Solves a block of `rows.len()` rows using runtime block machine processor with
    /// a block size set to `rows.len() - 2`.
    /// Some preliminary data can be provided in `rows`.
    /// If `only_identities_with_next` is true, only identities with a next reference are considered.
    fn compute_row_block<Q: QueryCallback<T>>(
        &self,
        mutable_state: &MutableState<'a, T, Q>,
        first_row: i64,
        rows: Vec<Option<Row<T>>>,
        only_identities_with_next: bool,
    ) -> FinalizableData<'a, T> {
        assert!(rows.len() >= 2);
        let block_size = rows.len() - 2;
        let rows = rows.into_iter().enumerate().map(|(i, r)| match r {
            Some(r) => r,
            None => Row::fresh(
                self.fixed_data,
                RowIndex::from_i64(first_row + i as i64, self.degree),
            ),
        });
        let data = FinalizableData::with_initial_rows_in_progress(
            &self.parts.witnesses,
            rows,
            self.fixed_data,
        );

        let parts = if only_identities_with_next {
            &self.parts.restricted_to_identities_with_next_references()
        } else {
            &self.parts
        };

        // Use `BlockProcessor` + `DefaultSequenceIterator`.
        // Note that using `BlockProcessor` instead of `VmProcessor` is more convenient here because
        // it does not assert that the row is "complete" afterwards (i.e., that all identities
        // are satisfied assuming 0 for unknown values).
        let mut processor = BlockProcessor::new(
            RowIndex::from_i64(first_row, self.degree),
            // Shouldn't need any publics at this point
            SolverState::without_publics(data),
            mutable_state,
            self.fixed_data,
            parts,
            self.degree,
        );
        let mut sequence_iterator = ProcessingSequenceIterator::Default(
            DefaultSequenceIterator::new(block_size, parts.identities.len(), None),
        );
        processor.solve(&mut sequence_iterator).unwrap();

        // Ignore any updates to the publics at this point, as we'll re-visit the last row again.
        processor.finish().block
    }

    /// At the end of the solving algorithm, we'll have computed the first row twice
    /// (as row 0 and as row <degree>). This function merges the two versions.
    fn fix_first_row(&mut self) {
        assert_eq!(self.data.len() as DegreeType, self.degree + 1);

        let mut first_row = self.data.get_in_progress_row(0);
        let last_row = self.data.pop().unwrap();
        if first_row.merge_with(&last_row).is_err() {
            log::error!("{}", first_row.render("First row", false, &self.parts));
            log::error!("{}", last_row.render("Last row", false, &self.parts));
            panic!(
                "Failed to merge the first and last row of the VM '{}'",
                self.name()
            );
        }
        self.data.set(0, first_row);
    }

    #[cfg(test)]
    pub fn machine_parts(&self) -> &MachineParts<'a, T> {
        &self.parts
    }
}
