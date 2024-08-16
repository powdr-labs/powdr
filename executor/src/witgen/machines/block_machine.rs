use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::fmt::Display;
use std::iter::{self, once};

use super::{EvalResult, FixedData};

use crate::witgen::block_processor::BlockProcessor;
use crate::witgen::data_structures::finalizable_data::FinalizableData;
use crate::witgen::processor::{OuterQuery, Processor};
use crate::witgen::rows::{Row, RowIndex, RowPair};
use crate::witgen::sequence_iterator::{
    DefaultSequenceIterator, ProcessingSequenceCache, ProcessingSequenceIterator,
};
use crate::witgen::util::try_to_simple_poly;
use crate::witgen::{machines::Machine, EvalError, EvalValue, IncompleteCause};
use crate::witgen::{MutableState, QueryCallback};
use crate::Identity;
use itertools::Itertools;
use powdr_ast::analyzed::{
    AlgebraicExpression as Expression, AlgebraicReference, DegreeRange, IdentityKind, PolyID,
    PolynomialType,
};
use powdr_ast::parsed::visitor::ExpressionVisitable;
use powdr_number::{DegreeType, FieldElement};

enum ProcessResult<'a, T: FieldElement> {
    Success(FinalizableData<T>, EvalValue<&'a AlgebraicReference, T>),
    Incomplete(EvalValue<&'a AlgebraicReference, T>),
}

impl<'a, T: FieldElement> ProcessResult<'a, T> {
    fn new(data: FinalizableData<T>, updates: EvalValue<&'a AlgebraicReference, T>) -> Self {
        match updates.is_complete() {
            true => ProcessResult::Success(data, updates),
            false => ProcessResult::Incomplete(updates),
        }
    }
}

fn collect_fixed_cols<T: FieldElement>(
    expression: &Expression<T>,
    result: &mut BTreeSet<Option<Expression<T>>>,
) {
    expression.pre_visit_expressions(&mut |e| {
        if let Expression::Reference(r) = e {
            if r.is_fixed() {
                result.insert(Some(e.clone()));
            }
        }
    });
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum ConnectionType {
    Permutation,
    Lookup,
}

impl From<ConnectionType> for IdentityKind {
    fn from(value: ConnectionType) -> Self {
        match value {
            ConnectionType::Permutation => IdentityKind::Permutation,
            ConnectionType::Lookup => IdentityKind::Plookup,
        }
    }
}

impl TryFrom<IdentityKind> for ConnectionType {
    type Error = ();

    fn try_from(value: IdentityKind) -> Result<Self, Self::Error> {
        match value {
            IdentityKind::Permutation => Ok(ConnectionType::Permutation),
            IdentityKind::Plookup => Ok(ConnectionType::Lookup),
            _ => Err(()),
        }
    }
}

impl<'a, T: FieldElement> Display for BlockMachine<'a, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} (block_size: {}, latch_row: {})",
            self.name, self.block_size, self.latch_row
        )
    }
}

/// A machine that produces multiple rows (one block) per query.
/// TODO we do not actually "detect" the machine yet, we just check if
/// the lookup has a binary selector that is 1 every k rows for some k
pub struct BlockMachine<'a, T: FieldElement> {
    /// The degree range of all columns in this machine
    degree_range: DegreeRange,
    /// The current degree of all columns in this machine
    degree: DegreeType,
    /// Block size, the period of the selector.
    block_size: usize,
    /// The row index (within the block) of the latch row
    latch_row: usize,
    /// Connecting identities, indexed by their ID.
    connecting_identities: BTreeMap<u64, &'a Identity<T>>,
    /// The type of constraint used to connect this machine to its caller.
    connection_type: ConnectionType,
    /// The internal identities
    identities: Vec<&'a Identity<T>>,
    /// The data of the machine.
    data: FinalizableData<T>,
    /// The index of the first row that has not been finalized yet.
    /// At all times, all rows in the range [block_size..first_in_progress_row) are finalized.
    first_in_progress_row: usize,
    /// The set of witness columns that are actually part of this machine.
    witness_cols: HashSet<PolyID>,
    /// Cache that states the order in which to evaluate identities
    /// to make progress most quickly.
    processing_sequence_cache: ProcessingSequenceCache,
    fixed_data: &'a FixedData<'a, T>,
    name: String,
}

impl<'a, T: FieldElement> BlockMachine<'a, T> {
    pub fn try_new(
        name: String,
        fixed_data: &'a FixedData<'a, T>,
        connecting_identities: &BTreeMap<u64, &'a Identity<T>>,
        identities: &[&'a Identity<T>],
        witness_cols: &HashSet<PolyID>,
    ) -> Option<Self> {
        let degree_range = fixed_data.common_degree_range(witness_cols);

        // start from the max degree
        let degree = degree_range.max;

        let (is_permutation, block_size, latch_row) =
            detect_connection_type_and_block_size(fixed_data, connecting_identities)?;

        for id in connecting_identities.values() {
            for r in id.right.expressions.iter() {
                if let Some(poly) = try_to_simple_poly(r) {
                    if poly.poly_id.ptype == PolynomialType::Constant {
                        // It does not really make sense to have constant polynomials on the RHS
                        // of a block machine lookup, as all constant polynomials are periodic, so
                        // it would always return the same value.
                        return None;
                    }
                }
            }
        }

        assert!(block_size <= degree as usize);
        // Because block shapes are not always rectangular, we add the last block to the data at the
        // beginning. It starts out with unknown values. Should the first block decide to write to
        // rows < 0, they will be written to this block.
        // In `take_witness_col_values()`, this block will be removed and its values will be used to
        // construct the "default" block used to fill up unused rows.
        let start_index = RowIndex::from_i64(-(block_size as i64), degree);
        let data = FinalizableData::with_initial_rows_in_progress(
            witness_cols,
            (0..block_size).map(|i| Row::fresh(fixed_data, start_index + i)),
        );
        Some(BlockMachine {
            name,
            degree_range,
            degree,
            block_size,
            latch_row,
            connecting_identities: connecting_identities.clone(),
            connection_type: is_permutation,
            identities: identities.to_vec(),
            data,
            first_in_progress_row: block_size,
            witness_cols: witness_cols.clone(),
            processing_sequence_cache: ProcessingSequenceCache::new(
                block_size,
                latch_row,
                identities.len(),
            ),
            fixed_data,
        })
    }
}

fn detect_connection_type_and_block_size<'a, T: FieldElement>(
    fixed_data: &'a FixedData<'a, T>,
    connecting_identities: &BTreeMap<u64, &'a Identity<T>>,
) -> Option<(ConnectionType, usize, usize)> {
    // TODO we should check that the other constraints/fixed columns are also periodic.

    // Connecting identities should either all be permutations or all lookups.
    let connection_type = connecting_identities
        .values()
        .map(|id| id.kind.try_into())
        .unique()
        .exactly_one()
        .ok()?
        .ok()?;

    // Detect the block size.
    let (latch_row, block_size) = match connection_type {
        ConnectionType::Lookup => {
            // We'd expect all RHS selectors to be fixed columns of the same period.
            connecting_identities
                .values()
                .map(|id| try_to_period(&id.right.selector, fixed_data))
                .unique()
                .exactly_one()
                .ok()??
        }
        ConnectionType::Permutation => {
            // We check all fixed columns appearing in RHS selectors. If there is none, the block size is 1.

            let find_max_period = |latch_candidates: BTreeSet<Option<Expression<T>>>| {
                latch_candidates
                    .iter()
                    .filter_map(|e| try_to_period(e, fixed_data))
                    // If there is more than one period, the block size is the maximum period.
                    .max_by_key(|&(_, period)| period)
            };
            let mut latch_candidates = BTreeSet::new();
            for id in connecting_identities.values() {
                if let Some(selector) = &id.right.selector {
                    collect_fixed_cols(selector, &mut latch_candidates);
                }
            }
            if latch_candidates.is_empty() {
                (0, 1)
            } else {
                find_max_period(latch_candidates)?
            }
        }
    };
    Some((connection_type, block_size, latch_row))
}

/// Check if `expr` is a reference to a function of the form
/// f(i) { if (i + o) % k == 0 { 1 } else { 0 } }
/// for some k < degree / 2, o.
/// If so, returns (o, k).
fn try_to_period<T: FieldElement>(
    expr: &Option<Expression<T>>,
    fixed_data: &FixedData<T>,
) -> Option<(usize, usize)> {
    match expr {
        Some(expr) => {
            if let Expression::Number(ref n) = expr {
                if *n == T::one() {
                    return Some((0, 1));
                }
            }

            let poly = try_to_simple_poly(expr)?;
            if !poly.is_fixed() {
                return None;
            }

            let degree = fixed_data.common_degree_range(once(&poly.poly_id)).max;

            let values = fixed_data.fixed_cols[&poly.poly_id].values(degree);

            let offset = values.iter().position(|v| v.is_one())?;
            let period = 1 + values.iter().skip(offset + 1).position(|v| v.is_one())?;
            if period > degree as usize / 2 {
                // This filters out columns like [0]* + [1], which might appear in a block machine
                // but shouldn't be detected as the latch.
                return None;
            }
            values
                .iter()
                .enumerate()
                .all(|(i, v)| {
                    let expected = if i % period == offset {
                        1.into()
                    } else {
                        0.into()
                    };
                    *v == expected
                })
                .then_some((offset, period))
        }
        None => Some((0, 1)),
    }
}

impl<'a, T: FieldElement> Machine<'a, T> for BlockMachine<'a, T> {
    fn identity_ids(&self) -> Vec<u64> {
        self.connecting_identities.keys().copied().collect()
    }

    fn process_plookup<'b, Q: QueryCallback<T>>(
        &mut self,
        mutable_state: &'b mut MutableState<'a, 'b, T, Q>,
        identity_id: u64,
        caller_rows: &'b RowPair<'b, 'a, T>,
    ) -> EvalResult<'a, T> {
        let previous_len = self.data.len();
        let result = self.process_plookup_internal(mutable_state, identity_id, caller_rows);
        if let Ok(assignments) = &result {
            if !assignments.is_complete() {
                // rollback the changes.
                self.data.truncate(previous_len);
            }
        }
        result
    }

    fn name(&self) -> &str {
        &self.name
    }

    fn take_witness_col_values<'b, Q: QueryCallback<T>>(
        &mut self,
        mutable_state: &'b mut MutableState<'a, 'b, T, Q>,
    ) -> HashMap<String, Vec<T>> {
        if self.data.len() < 2 * self.block_size {
            log::warn!(
                "Filling empty blocks with zeros, because the block machine is never used. \
                 This might violate some internal constraints."
            );
        }

        let new_degree = self.data.len().next_power_of_two() as DegreeType;
        let new_degree = self.degree_range.fit(new_degree);
        log::info!(
            "Resizing variable length machine '{}': {} -> {} (rounded up from {})",
            self.name,
            self.degree,
            new_degree,
            self.data.len()
        );
        self.degree = new_degree;

        if matches!(self.connection_type, ConnectionType::Permutation) {
            // We have to make sure that *all* selectors are 0 in the dummy block,
            // because otherwise this block won't have a matching block on the LHS.

            // Collect dummy block with rows before and after
            let dummy_block = FinalizableData::with_initial_rows_in_progress(
                &self.witness_cols,
                iter::once(self.block_size - 1)
                    .chain(0..self.block_size)
                    .chain(iter::once(0))
                    .map(|i| self.data[i].clone()),
            );

            // Instantiate a processor
            let row_offset = RowIndex::from_i64(-1, self.degree);
            let mut processor = Processor::new(
                row_offset,
                dummy_block,
                mutable_state,
                self.fixed_data,
                &self.witness_cols,
                self.degree,
            );

            // Set all selectors to 0
            for id in self.connecting_identities.values() {
                processor
                    .set_value(
                        self.latch_row + 1,
                        id.right.selector.as_ref().unwrap(),
                        T::zero(),
                        || "Zero selectors".to_string(),
                    )
                    .unwrap();
            }

            // Run BlockProcessor (to potentially propagate selector values)
            let mut processor = BlockProcessor::from_processor(processor, &self.identities);
            let mut sequence_iterator = ProcessingSequenceIterator::Default(
                DefaultSequenceIterator::new(self.block_size, self.identities.len(), None),
            );
            processor.solve(&mut sequence_iterator).unwrap();
            let mut dummy_block = processor.finish();

            // Replace the dummy block, discarding first and last row
            dummy_block.pop().unwrap();
            for i in (0..self.block_size).rev() {
                self.data[i] = dummy_block.pop().unwrap();
            }
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

                // Remove the "last" block added to the beginning of self.data.
                // It contains the values the first block wrote to it and otherwise unknown values.
                let dummy_block = values.drain(0..self.block_size).collect::<Vec<_>>();

                // For all constraints to be satisfied, unused cells have to be filled with valid values.
                // We do this, we construct a default block, by repeating the first input to the block machine.
                values.resize(self.degree as usize, None);

                // Use the block as the default block. However, it needs to be merged with the dummy block,
                // to handle blocks of non-rectangular shape.
                // For example, let's say, the situation might look like this (block size = 3 in this example):
                //  Row  Latch   C1  C2  C3
                //  -3     0
                //  -2     0
                //  -1     1             X  <- This value belongs to the first block
                //   0     0     X   X   X
                //   1     0     X   X   X
                //   2     1     X   X   X  <- This value belongs to the second block
                //
                // The following code constructs the default block as follows:
                // - All values will come from rows 0-2, EXCEPT
                // - In the last row, the value of C3 is whatever value was written to the dummy block
                //
                // Constructed like this, we can repeat the default block forever.
                //
                // TODO: Determine the row-extend per column
                let first_block_values = values.iter().take(self.block_size);
                let default_block = dummy_block
                    .into_iter()
                    .zip(first_block_values)
                    .map(|(dummy_block, first_block)| {
                        dummy_block.or(*first_block).unwrap_or_default()
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
    /// compiling a block machine from Powdr ASM and constrained as:
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
                col[self.degree as usize - 1] = T::zero();
            }
        }
    }

    /// Returns the current number of rows *not counting the dummy block* inserted at the beginning
    /// (with row numbers (-block_size..0)).
    fn rows(&self) -> DegreeType {
        (self.data.len() - self.block_size) as DegreeType
    }

    fn last_row_index(&self) -> RowIndex {
        RowIndex::from_i64(self.rows() as i64 - 1, self.degree)
    }

    fn get_row(&self, row: RowIndex) -> &Row<T> {
        // The first block is a dummy block corresponding to rows (-block_size, 0),
        // so we have to add the block size to the row index.
        &self.data[(row + self.block_size).into()]
    }

    fn process_plookup_internal<'b, Q: QueryCallback<T>>(
        &mut self,
        mutable_state: &mut MutableState<'a, 'b, T, Q>,
        identity_id: u64,
        caller_rows: &'b RowPair<'b, 'a, T>,
    ) -> EvalResult<'a, T> {
        let outer_query = OuterQuery::new(caller_rows, self.connecting_identities[&identity_id]);

        log::trace!("Start processing block machine '{}'", self.name());
        log::trace!("Left values of lookup:");
        for l in &outer_query.left {
            log::trace!("  {}", l);
        }

        // TODO this assumes we are always using the same lookup for this machine.
        let mut sequence_iterator = self
            .processing_sequence_cache
            .get_processing_sequence(&outer_query.left);

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

        if self.rows() + self.block_size as DegreeType >= self.degree {
            return Err(EvalError::RowsExhausted(self.name.clone()));
        }

        let process_result =
            self.process(mutable_state, &mut sequence_iterator, outer_query.clone())?;

        match process_result {
            ProcessResult::Success(new_block, updates) => {
                log::trace!(
                    "End processing block machine '{}' (successfully)",
                    self.name()
                );
                self.append_block(new_block)?;

                let updates = updates.report_side_effect();

                // We solved the query, so report it to the cache.
                self.processing_sequence_cache
                    .report_processing_sequence(&outer_query.left, sequence_iterator);
                Ok(updates)
            }
            ProcessResult::Incomplete(updates) => {
                log::trace!(
                    "End processing block machine '{}' (incomplete)",
                    self.name()
                );
                self.processing_sequence_cache
                    .report_incomplete(&outer_query.left);
                Ok(updates)
            }
        }
    }

    fn process<'b, Q: QueryCallback<T>>(
        &self,
        mutable_state: &mut MutableState<'a, 'b, T, Q>,
        sequence_iterator: &mut ProcessingSequenceIterator,
        outer_query: OuterQuery<'a, 'b, T>,
    ) -> Result<ProcessResult<'a, T>, EvalError<T>> {
        // We start at the last row of the previous block.
        let row_offset = self.last_row_index();
        // Make the block two rows larger than the block size, it includes the last row of the previous block
        // and the first row of the next block.
        let block = FinalizableData::with_initial_rows_in_progress(
            &self.witness_cols,
            (0..(self.block_size + 2)).map(|i| Row::fresh(self.fixed_data, row_offset + i)),
        );
        let mut processor = BlockProcessor::new(
            row_offset,
            block,
            mutable_state,
            &self.identities,
            self.fixed_data,
            &self.witness_cols,
            self.degree,
        )
        .with_outer_query(outer_query);

        let outer_assignments = processor.solve(sequence_iterator)?;
        let new_block = processor.finish();

        Ok(ProcessResult::new(new_block, outer_assignments))
    }

    /// Takes a block of rows, which contains the last row of its previous block
    /// and the first row of its next block. The first row of its next block is ignored,
    /// the last row of its previous block is merged with the one we have already.
    /// This is necessary to handle non-rectangular block machines, which already use
    /// unused cells in the previous block.
    fn append_block(&mut self, mut new_block: FinalizableData<T>) -> Result<(), EvalError<T>> {
        assert!(
            (self.rows() + self.block_size as DegreeType) < self.degree,
            "Block machine is full (this should have been checked before)"
        );

        assert_eq!(new_block.len(), self.block_size + 2);

        // 1. Ignore the first row of the next block:
        new_block.pop();
        // 2. Merge the last row of the previous block

        new_block
            .get_mut(0)
            .unwrap()
            .merge_with(self.get_row(self.last_row_index()))
            .map_err(|_| {
                EvalError::Generic(
                    "Block machine overwrites existing value with different value!".to_string(),
                )
            })?;
        // 3. Remove the last row of the previous block from data
        self.data.pop();

        // 4. Finalize everything so far (except the dummy block)
        if self.data.len() > self.block_size {
            self.data
                .finalize_range(self.first_in_progress_row..self.data.len());
            self.first_in_progress_row = self.data.len();
        }

        // 5. Append the new block (including the merged last row of the previous block)
        self.data.extend(new_block);

        Ok(())
    }
}
