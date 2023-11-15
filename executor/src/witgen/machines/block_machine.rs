use std::collections::{HashMap, HashSet};

use super::{EvalResult, FixedData, FixedLookup};
use crate::witgen::affine_expression::AffineExpression;

use crate::witgen::block_processor::BlockProcessor;
use crate::witgen::data_structures::finalizable_data::FinalizableData;
use crate::witgen::global_constraints::GlobalConstraints;
use crate::witgen::identity_processor::IdentityProcessor;
use crate::witgen::processor::OuterQuery;
use crate::witgen::rows::{CellValue, RowFactory, RowPair, UnknownStrategy};
use crate::witgen::sequence_iterator::{ProcessingSequenceCache, ProcessingSequenceIterator};
use crate::witgen::util::try_to_simple_poly;
use crate::witgen::{machines::Machine, EvalError, EvalValue, IncompleteCause};
use crate::witgen::{MutableState, QueryCallback};
use ast::analyzed::{
    AlgebraicExpression as Expression, AlgebraicReference, Identity, IdentityKind, PolyID,
};
use ast::parsed::SelectedExpressions;
use number::{DegreeType, FieldElement};

enum ProcessResult<'a, T: FieldElement> {
    Success(FinalizableData<'a, T>, EvalValue<&'a AlgebraicReference, T>),
    Incomplete(EvalValue<&'a AlgebraicReference, T>),
}

impl<'a, T: FieldElement> ProcessResult<'a, T> {
    fn new(data: FinalizableData<'a, T>, updates: EvalValue<&'a AlgebraicReference, T>) -> Self {
        match updates.is_complete() {
            true => ProcessResult::Success(data, updates),
            false => ProcessResult::Incomplete(updates),
        }
    }

    fn is_success(&self) -> bool {
        match self {
            ProcessResult::Success(_, _) => true,
            ProcessResult::Incomplete(_) => false,
        }
    }
}

/// A machine that produces multiple rows (one block) per query.
/// TODO we do not actually "detect" the machine yet, we just check if
/// the lookup has a binary selector that is 1 every k rows for some k
pub struct BlockMachine<'a, T: FieldElement> {
    /// Block size, the period of the selector.
    block_size: usize,
    /// The right-hand side of the connecting identity, needed to identify
    /// when this machine is responsible.
    selected_expressions: SelectedExpressions<Expression<T>>,
    /// The internal identities
    identities: Vec<&'a Identity<Expression<T>>>,
    /// The row factory
    row_factory: RowFactory<'a, T>,
    /// The data of the machine.
    data: FinalizableData<'a, T>,
    /// The set of witness columns that are actually part of this machine.
    witness_cols: HashSet<PolyID>,
    /// Cache that states the order in which to evaluate identities
    /// to make progress most quickly.
    processing_sequence_cache: ProcessingSequenceCache,
    fixed_data: &'a FixedData<'a, T>,
}

impl<'a, T: FieldElement> BlockMachine<'a, T> {
    pub fn try_new(
        fixed_data: &'a FixedData<'a, T>,
        connecting_identities: &[&'a Identity<Expression<T>>],
        identities: &[&'a Identity<Expression<T>>],
        witness_cols: &HashSet<PolyID>,
        global_range_constraints: &GlobalConstraints<T>,
    ) -> Option<Self> {
        for id in connecting_identities {
            // TODO we should check that the other constraints/fixed columns are also periodic.
            if let Some(block_size) = try_to_period(&id.right.selector, fixed_data) {
                assert!(block_size <= fixed_data.degree as usize);
                let row_factory = RowFactory::new(fixed_data, global_range_constraints.clone());
                // Start out with a block filled with unknown values so that we do not have to deal with wrap-around
                // when storing machine witness data.
                // This will be filled with the default block in `take_witness_col_values`
                let data = FinalizableData::with_initial_rows_in_progress(
                    witness_cols,
                    (0..block_size).map(|i| row_factory.fresh_row(i as DegreeType)),
                );
                return Some(BlockMachine {
                    block_size,
                    selected_expressions: id.right.clone(),
                    identities: identities.to_vec(),
                    data,
                    row_factory,
                    witness_cols: witness_cols.clone(),
                    processing_sequence_cache: ProcessingSequenceCache::new(
                        block_size,
                        identities.len(),
                    ),
                    fixed_data,
                });
            }
        }

        None
    }
}

/// Check if `expr` is a reference to a function of the form
/// f(i) { if (i + 1) % k == 0 { 1 } else { 0 } }
/// for some k
/// TODO we could make this more generic and only detect the period
/// but not enforce the offset.
fn try_to_period<T: FieldElement>(
    expr: &Option<Expression<T>>,
    fixed_data: &FixedData<T>,
) -> Option<usize> {
    match expr {
        Some(expr) => {
            if let Expression::Number(ref n) = expr {
                if *n == T::one() {
                    return Some(1);
                }
            }

            let poly = try_to_simple_poly(expr)?;
            if !poly.is_fixed() {
                return None;
            }

            let values = fixed_data.fixed_cols[&poly.poly_id].values;

            let period = 1 + values.iter().position(|v| v.is_one())?;
            values
                .iter()
                .enumerate()
                .all(|(i, v)| {
                    let expected = if (i + 1) % period == 0 {
                        1.into()
                    } else {
                        0.into()
                    };
                    *v == expected
                })
                .then_some(period)
        }
        None => Some(1),
    }
}

impl<'a, T: FieldElement> Machine<'a, T> for BlockMachine<'a, T> {
    fn process_plookup<'b, Q: QueryCallback<T>>(
        &mut self,
        mutable_state: &'b mut MutableState<'a, 'b, T, Q>,
        kind: IdentityKind,
        left: &[AffineExpression<&'a AlgebraicReference, T>],
        right: &'a SelectedExpressions<Expression<T>>,
    ) -> Option<EvalResult<'a, T>> {
        if *right != self.selected_expressions || kind != IdentityKind::Plookup {
            return None;
        }
        let previous_len = self.rows() as usize;
        Some({
            let result = self.process_plookup_internal(mutable_state, left, right);
            if let Ok(assignments) = &result {
                if !assignments.is_complete() {
                    // rollback the changes.
                    self.data.truncate(previous_len);
                }
            }
            result
        })
    }

    fn take_witness_col_values<'b, Q: QueryCallback<T>>(
        &mut self,
        _fixed_lookup: &'b mut FixedLookup<T>,
        _query_callback: &'b mut Q,
    ) -> HashMap<String, Vec<T>> {
        if self.data.len() < 2 * self.block_size {
            log::warn!(
                "Filling empty blocks with zeros, because the block machine is never used. \
                 This might violate some internal constraints."
            );
        }
        let mut data = self
            .data
            .take_transposed()
            .map(|(id, (values, known_cells))| {
                // Materialize column as Vec<Option<T>>
                let mut values = values
                    .into_iter()
                    .zip(known_cells)
                    .map(|(v, known)| known.then_some(v))
                    .collect::<Vec<_>>();

                // For all constraints to be satisfied, unused cells have to be filled with valid values.
                // We do this, we construct a default block, by repeating the first input to the block machine.
                values.resize(self.fixed_data.degree as usize, None);

                let second_block_values = values.iter().skip(self.block_size).take(self.block_size);

                // The first block is a dummy block (filled mostly with None), the second block is the first block
                // resulting of an actual evaluation.
                // However, if the block machine already sets some registers in the last row of the previous block,
                // they will be set in the "dummy block". In this case, we want to use these values.
                // As a result, the default block consists of values of the first block if they are set, otherwise
                // the values of the second block.
                // TODO: Determine the row-extend per column
                let default_block = values
                    .iter()
                    .take(self.block_size)
                    .zip(second_block_values)
                    .map(|(first_block, second_block)| {
                        first_block.or(*second_block).unwrap_or_default()
                    })
                    .collect::<Vec<_>>();

                let values = values
                    .into_iter()
                    .enumerate()
                    .map(|(i, v)| v.unwrap_or(default_block[i % self.block_size]))
                    .collect::<Vec<_>>();

                (id, values)
            })
            .collect();
        self.handle_last_row(&mut data);
        data.into_iter()
            .map(|(id, values)| (self.fixed_data.column_name(&id).to_string(), values))
            .collect()
    }
}

impl<'a, T: FieldElement> BlockMachine<'a, T> {
    /// The characteristic of a block machine is that that all fixed columns are
    /// periodic. However, there are exceptions to handle wrapping.
    /// This becomes a problem when a witness polynomial depends on a fixed column
    /// that is not periodic, because values of committed polynomials are copy-pasted
    /// from the default block.
    /// This is the case for the _operation_id_no_change column, generated when
    /// compiling a block machine from Posdr ASM and constrained as:
    /// _operation_id_no_change = ((1 - _block_enforcer_last_step) * (1 - <Latch>));
    /// This function fixes this exception by setting _operation_id_no_change to 0.
    fn handle_last_row(&self, data: &mut HashMap<PolyID, Vec<T>>) {
        for (poly_id, col) in data.iter_mut() {
            if self
                .fixed_data
                .column_name(poly_id)
                .ends_with("_operation_id_no_change")
            {
                log::trace!("Setting _operation_id_no_change to 0.");
                col[self.fixed_data.degree as usize - 1] = T::zero();
            }
        }
    }

    fn rows(&self) -> DegreeType {
        self.data.len() as DegreeType
    }

    fn name(&self) -> &str {
        let first_witness = self.witness_cols.iter().next().unwrap();
        let first_witness_name = self.fixed_data.column_name(first_witness);
        let namespace = first_witness_name
            .rfind('.')
            .map(|idx| &first_witness_name[..idx]);

        // For machines compiled using Powdr ASM we'll always have a namespace, but as a last
        // resort we'll use the first witness name.
        namespace.unwrap_or(first_witness_name)
    }

    fn process_plookup_internal<'b, Q: QueryCallback<T>>(
        &mut self,
        mutable_state: &mut MutableState<'a, 'b, T, Q>,
        left: &[AffineExpression<&'a AlgebraicReference, T>],
        right: &'a SelectedExpressions<Expression<T>>,
    ) -> EvalResult<'a, T> {
        log::trace!("Start processing block machine '{}'", self.name());
        log::trace!("Left values of lookup:");
        for l in left {
            log::trace!("  {}", l);
        }

        // First check if we already store the value.
        // This can happen in the loop detection case, where this function is just called
        // to validate the constraints.
        if left.iter().all(|v| v.is_constant()) && self.rows() > 0 {
            // All values on the left hand side are known, check if this is a query
            // to the last row.
            let row = self.rows() - 1;

            let current = &self.data[row as usize];
            // We don't have the next row, because it would be the first row of the next block.
            // We'll use a fresh row instead.
            let next = self.row_factory.fresh_row(row + 1);
            let row_pair = RowPair::new(
                current,
                &next,
                row,
                self.fixed_data,
                UnknownStrategy::Unknown,
            );

            let mut identity_processor = IdentityProcessor::new(self.fixed_data, mutable_state);
            if let Ok(result) = identity_processor.process_link(left, right, &row_pair) {
                if result.is_complete() && result.constraints.is_empty() {
                    log::trace!(
                        "End processing block machine '{}' (already solved)",
                        self.name()
                    );
                    return Ok(result);
                }
            }
        }

        // TODO this assumes we are always using the same lookup for this machine.
        let mut sequence_iterator = self.processing_sequence_cache.get_processing_sequence(left);

        if !sequence_iterator.has_steps() {
            // Shortcut, no need to do anything.
            log::trace!(
                "Abort processing block machine '{}' (inputs incomplete according to cache)",
                self.name()
            );
            return Ok(EvalValue::incomplete(
                IncompleteCause::BlockMachineLookupIncomplete,
            ));
        }

        let process_result = self.process(mutable_state, left, right, &mut sequence_iterator)?;

        let process_result = if sequence_iterator.is_cached() && !process_result.is_success() {
            log::debug!("The cached sequence did not complete the block machine. \
                         This can happen if the machine's execution steps depend on the input or constant values. \
                         We'll try again with the default sequence.");
            let mut sequence_iterator = self
                .processing_sequence_cache
                .get_default_sequence_iterator();
            self.process(mutable_state, left, right, &mut sequence_iterator)?
        } else {
            process_result
        };

        match process_result {
            ProcessResult::Success(new_block, updates) => {
                log::trace!(
                    "End processing block machine '{}' (successfully)",
                    self.name()
                );
                self.append_block(new_block)?;

                // We solved the query, so report it to the cache.
                self.processing_sequence_cache
                    .report_processing_sequence(left, sequence_iterator);
                Ok(updates)
            }
            ProcessResult::Incomplete(updates) => {
                log::trace!(
                    "End processing block machine '{}' (incomplete)",
                    self.name()
                );
                self.processing_sequence_cache.report_incomplete(left);
                Ok(updates)
            }
        }
    }

    fn process<'b, Q: QueryCallback<T>>(
        &self,
        mutable_state: &mut MutableState<'a, 'b, T, Q>,
        left: &[AffineExpression<&'a AlgebraicReference, T>],
        right: &'a SelectedExpressions<Expression<T>>,
        sequence_iterator: &mut ProcessingSequenceIterator,
    ) -> Result<ProcessResult<'a, T>, EvalError<T>> {
        // We start at the last row of the previous block.
        let row_offset = self.rows() - 1;
        // Make the block two rows larger than the block size, it includes the last row of the previous block
        // and the first row of the next block.
        let block = FinalizableData::with_initial_rows_in_progress(
            &self.witness_cols,
            (0..(self.block_size + 2))
                .map(|i| self.row_factory.fresh_row(i as DegreeType + row_offset)),
        );
        let mut processor = BlockProcessor::new(
            row_offset,
            block,
            mutable_state,
            &self.identities,
            self.fixed_data,
            self.row_factory.clone(),
            &self.witness_cols,
        )
        .with_outer_query(OuterQuery::new(left.to_vec(), right));

        let outer_assignments = processor.solve(sequence_iterator)?;
        let new_block = processor.finish();

        Ok(ProcessResult::new(new_block, outer_assignments))
    }

    /// Takes a block of rows, which contains the last row of its previous block
    /// and the first row of its next block. The first row of its next block is ignored,
    /// the last row of its previous block is merged with the one we have already.
    /// This is necessary to handle non-rectangular block machines, which already use
    /// unused cells in the previous block.
    fn append_block(&mut self, mut new_block: FinalizableData<'a, T>) -> Result<(), EvalError<T>> {
        if self.rows() + self.block_size as DegreeType >= self.fixed_data.degree {
            return Err(EvalError::RowsExhausted);
        }

        assert_eq!(new_block.len(), self.block_size + 2);

        // 1. Ignore the first row of the next block:
        new_block.pop();
        // 2. Merge the last row of the previous block
        let last_row_index = self.rows() as usize - 1;
        let updated_last_row = new_block.get_mut(0).unwrap();
        for (poly_id, existing_value) in self.data[last_row_index].iter() {
            if let CellValue::Known(v) = existing_value.value {
                if updated_last_row[&poly_id].value.is_known()
                    && updated_last_row[&poly_id].value != existing_value.value
                {
                    return Err(EvalError::Generic(
                        "Block machine overwrites existing value with different value!".to_string(),
                    ));
                }
                updated_last_row[&poly_id].value = CellValue::Known(v);
            }
        }

        // 3. Remove the last row of the previous block from data
        self.data.pop();

        // 4. Finalize most of the block
        // The last row might be needed later, so we do not finalize it yet.
        new_block.finalize_range(0..self.block_size);

        // 5. Append the new block (including the merged last row of the previous block)
        self.data.extend(new_block);

        Ok(())
    }
}
