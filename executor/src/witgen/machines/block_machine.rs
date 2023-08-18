use std::collections::{BTreeMap, HashMap, HashSet};

use super::{EvalResult, FixedData, FixedLookup};
use crate::witgen::column_map::ColumnMap;
use crate::witgen::identity_processor::IdentityProcessor;
use crate::witgen::processor::Processor;
use crate::witgen::rows::{Row, RowFactory, RowPair, RowUpdater, UnknownStrategy};
use crate::witgen::util::try_to_simple_poly;
use crate::witgen::{
    affine_expression::AffineResult, machines::Machine, range_constraints::RangeConstraint,
    EvalError,
};
use crate::witgen::{Constraint, EvalValue, IncompleteCause};
use ast::analyzed::{
    Expression, Identity, IdentityKind, PolyID, PolynomialReference, SelectedExpressions,
};
use number::{DegreeType, FieldElement};

/// Transposes a list of rows into a map from column to a list of values.
/// This is done to match the interface of [Machine::take_witness_col_values].
pub fn transpose_rows<T: FieldElement>(
    rows: Vec<Row<T>>,
    column_set: &HashSet<PolyID>,
) -> BTreeMap<PolyID, Vec<Option<T>>> {
    let mut result = column_set
        .iter()
        .map(|id| (*id, Vec::with_capacity(rows.len())))
        .collect::<BTreeMap<_, _>>();

    for row in rows.into_iter() {
        for poly_id in column_set.iter() {
            result
                .get_mut(poly_id)
                .unwrap()
                .push((&row[poly_id].value).into());
        }
    }
    result
}

/// A machine that produces multiple rows (one block) per query.
/// TODO we do not actually "detect" the machine yet, we just check if
/// the lookup has a binary selector that is 1 every k rows for some k
pub struct BlockMachine<'a, T: FieldElement> {
    /// Block size, the period of the selector.
    block_size: usize,
    /// The right-hand side of the connecting identity, needed to identify
    /// when this machine is responsible.
    selected_expressions: SelectedExpressions<T>,
    /// The internal identities
    identities: Vec<&'a Identity<T>>,
    /// The row factory
    row_factory: RowFactory<'a, T>,
    /// The data of the machine.
    data: Vec<Row<'a, T>>,
    /// The set of witness columns that are actually part of this machine.
    witness_cols: HashSet<PolyID>,
    /// Cache that states the order in which to evaluate identities
    /// to make progress most quickly.
    processing_sequence_cache: ProcessingSequenceCache,
}

impl<'a, T: FieldElement> BlockMachine<'a, T> {
    pub fn try_new(
        fixed_data: &'a FixedData<T>,
        connecting_identities: &[&'a Identity<T>],
        identities: &[&'a Identity<T>],
        witness_cols: &HashSet<PolyID>,
        global_range_constraints: &ColumnMap<Option<RangeConstraint<T>>>,
    ) -> Option<Self> {
        for id in connecting_identities {
            // TODO we should check that the other constraints/fixed columns are also periodic.
            if let Some(period) = try_to_period(&id.right.selector, fixed_data) {
                let row_factory = RowFactory::new(fixed_data, global_range_constraints.clone());
                let mut machine = BlockMachine {
                    block_size: period,
                    selected_expressions: id.right.clone(),
                    identities: identities.to_vec(),
                    data: vec![],
                    row_factory,
                    witness_cols: witness_cols.clone(),
                    processing_sequence_cache: ProcessingSequenceCache::new(
                        period,
                        identities.len(),
                    ),
                };
                // Append a block so that we do not have to deal with wrap-around
                // when storing machine witness data.
                machine.append_new_block(fixed_data.degree).unwrap();

                return Some(machine);
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

            let values = fixed_data.fixed_cols[&poly.poly_id()].values;

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
    fn process_plookup(
        &mut self,
        fixed_data: &'a FixedData<T>,
        fixed_lookup: &mut FixedLookup<T>,
        kind: IdentityKind,
        left: &[AffineResult<&'a PolynomialReference, T>],
        right: &'a SelectedExpressions<T>,
    ) -> Option<EvalResult<'a, T>> {
        if *right != self.selected_expressions || kind != IdentityKind::Plookup {
            return None;
        }
        let previous_len = self.rows() as usize;
        Some({
            let result = self.process_plookup_internal(fixed_data, fixed_lookup, left, right);
            if let Ok(assignments) = &result {
                if !assignments.is_complete() {
                    // rollback the changes.
                    self.data.truncate(previous_len);
                }
            }
            result
        })
    }

    fn take_witness_col_values(
        &mut self,
        fixed_data: &FixedData<T>,
        fixed_lookup: &mut FixedLookup<T>,
    ) -> HashMap<String, Vec<T>> {
        let mut data = transpose_rows(std::mem::take(&mut self.data), &self.witness_cols)
            .into_iter()
            .map(|(id, mut values)| {

                // For all constraints to be satisfied, unused cells have to be filled with valid values.
                // We do this, we construct a default block, by repeating the first input to the block machine.

                if values.len() < 2 * self.block_size {
                    log::warn!("Filling empty blocks with zeros, because the block machine is never used. \
                                This might violate some internal constraints.");
                }

                values.resize(fixed_data.degree as usize, None);

                let second_block_values = values
                    .iter()
                    .skip(self.block_size)
                    .take(self.block_size);

                // The first block is a dummy block (filled mostly with None), the second block is the first block
                // resulting of an actual evaluation.
                // However, if the block machine already sets some registers in the last row of the previous block,
                // they will be set in the "dummy block". In this case, we want to use these values.
                // As a result, the default block consists of values of the first block if they are set, otherwise
                // the values of the second block.
                // TODO: Determine the row-extend per column
                let default_block = values.iter().take(self.block_size).zip(second_block_values).map(
                    |(first_block, second_block)| first_block.or(*second_block).unwrap_or_default()).collect::<Vec<_>>();

                let values = values
                    .into_iter()
                    .enumerate()
                    .map(|(i, v)| v.unwrap_or(default_block[i % self.block_size]))
                    .collect::<Vec<_>>();

                (id, values)
            })
            .collect();
        self.handle_last_row(&mut data, fixed_data, fixed_lookup);
        data.into_iter()
            .map(|(id, values)| (fixed_data.column_name(&id).to_string(), values))
            .collect()
    }
}

impl<'a, T: FieldElement> BlockMachine<'a, T> {
    /// Check if constraints are satisfied on the last row and recompute it if not.
    /// While the characteristic of a block machine is that that all fixed columns
    /// should be periodic (and hence all constraints), the last row might be different,
    /// in order to handle the cyclic nature of the proving system.
    /// This can lead to an invalid last row when a default block is "copy-pasted" until
    /// the end of the table. This function corrects it if it is the case.
    fn handle_last_row(
        &self,
        data: &mut HashMap<PolyID, Vec<T>>,
        fixed_data: &FixedData<T>,
        fixed_lookup: &mut FixedLookup<T>,
    ) {
        // Build a vector of 3 rows: N -2, N - 1 and 0
        let rows = ((fixed_data.degree - 2)..(fixed_data.degree + 1))
            .map(|row| {
                self.row_factory.row_from_known_values_sparse(
                    data.iter()
                        .map(|(col, values)| (*col, values[(row % fixed_data.degree) as usize])),
                )
            })
            .collect();

        // Build the processor. This copies the identities, but it's only done once per block machine instance.
        let mut processor = Processor::new(
            fixed_data.degree - 2,
            rows,
            IdentityProcessor::new(fixed_data, fixed_lookup, vec![]),
            self.identities.clone(),
            fixed_data,
            self.row_factory.clone(),
        );

        // Check if we can accept the last row as is.
        if processor.check_constraints().is_err() {
            log::warn!("Detected error in last row! Will attempt to fix it now.");

            // Clear the last row and run the solver
            processor.clear_row(1);
            processor
                .solve()
                .expect("Some constraints were not satisfiable when solving for the last row.");
            let last_row = processor.finish().remove(1);

            // Copy values into data
            for (poly_id, values) in data.iter_mut() {
                values[fixed_data.degree as usize - 1] =
                    last_row[poly_id].value.unwrap_or_default();
            }
        }
    }

    /// Extends the data with a new block.
    fn append_new_block(&mut self, max_len: DegreeType) -> Result<(), EvalError<T>> {
        if self.rows() + self.block_size as DegreeType >= max_len {
            return Err(EvalError::RowsExhausted);
        }
        self.data
            .resize_with(self.data.len() + self.block_size, || {
                self.row_factory.fresh_row()
            });
        Ok(())
    }

    fn rows(&self) -> DegreeType {
        self.data.len() as DegreeType
    }

    fn process_plookup_internal(
        &mut self,
        fixed_data: &'a FixedData<T>,
        fixed_lookup: &mut FixedLookup<T>,
        left: &[AffineResult<&'a PolynomialReference, T>],
        right: &'a SelectedExpressions<T>,
    ) -> EvalResult<'a, T> {
        log::trace!("Start processing block machine");

        // TODO: Add possibility for machines to call other machines.
        let mut identity_processor = IdentityProcessor::new(fixed_data, fixed_lookup, vec![]);

        // First check if we already store the value.
        // This can happen in the loop detection case, where this function is just called
        // to validate the constraints.
        if left
            .iter()
            .all(|v| v.as_ref().ok().map(|v| v.is_constant()) == Some(true))
            && self.rows() > 0
        {
            // All values on the left hand side are known, check if this is a query
            // to the last row.
            let row = self.rows() - 1;

            let current = &self.data[row as usize];
            // We don't have the next row, because it would be the first row of the next block.
            // We'll use a fresh row instead.
            let next = self.row_factory.fresh_row();
            let row_pair = RowPair::new(current, &next, row, fixed_data, UnknownStrategy::Unknown);

            let result = identity_processor.process_link(left, right, &row_pair)?;

            if result.is_complete() {
                return Ok(result);
            }
        }

        let old_len = self.rows();
        self.append_new_block(fixed_data.degree)?;
        let mut outer_assignments = EvalValue::complete(vec![]);

        // While processing the block, we'll add an additional row which will be
        // removed once the block is done. This is done to prevent infinite loops:
        // If we ignored updates to the last row, they would be computed over
        // and over again.
        self.data.push(self.row_factory.fresh_row());

        // TODO this assumes we are always using the same lookup for this machine.
        let mut processing_sequence_iterator =
            self.processing_sequence_cache.get_processing_sequence(left);

        let mut errors = vec![];
        // TODO The error handling currently does not handle contradictions properly.
        // If we can find an assignment of all LHS variables at the end, we do not return an error,
        // even if there is a conflict.

        // Record the steps where we made progress, so we can report this to the
        // cache later on.
        // TODO: Move this into the processing sequence iterator.
        let mut progress_steps = vec![];

        // A copy of `left` which is mutated by `handle_outer_constraints()`
        let mut left_mut = left.to_vec();

        // Can't use a for loop here, because we need to communicate progress_in_last_step to the
        // default iterator.
        while let Some(step) = processing_sequence_iterator.next() {
            let SequenceStep {
                row_delta,
                identity,
            } = step;
            let row = (old_len as i64 + row_delta + fixed_data.degree as i64) as DegreeType
                % fixed_data.degree;

            let progress = match self.compute_updates(
                row,
                fixed_data,
                &left_mut,
                right,
                identity,
                &mut identity_processor,
            ) {
                Ok(value) => {
                    if !value.is_empty() {
                        errors.clear();

                        let progress = self.apply_updates(row, identity, &value, &mut left_mut);

                        for (poly, constraint) in value.constraints {
                            if !self.witness_cols.contains(&poly.poly_id()) {
                                outer_assignments.constraints.push((poly, constraint));
                            }
                        }

                        progress
                    } else {
                        false
                    }
                }
                Err(e) => {
                    errors.push(format!("In row {}: {e}", row).into());
                    false
                }
            };

            if progress {
                progress_steps.push(step);
            }

            processing_sequence_iterator.report_progress(progress);
        }

        // Remove the extra row we added at the beginning.
        self.data.pop();

        log::trace!("End processing block machine");

        // Only succeed if we can assign everything.
        // Otherwise it is messy because we have to find the correct block again.
        let success = left_mut
            .iter()
            .all(|v| v.as_ref().ok().map(|ae| ae.is_constant()) == Some(true));

        if success {
            // We solved the query, so report it to the cache.
            self.processing_sequence_cache
                .report_processing_sequence(left, progress_steps);
            Ok(outer_assignments)
        } else if !errors.is_empty() {
            Err(errors
                .into_iter()
                .reduce(|x: EvalError<T>, y| x.combine(y))
                .unwrap())
        } else {
            Ok(EvalValue::incomplete(
                IncompleteCause::BlockMachineLookupIncomplete,
            ))
        }
    }

    fn compute_updates(
        &self,
        row: DegreeType,
        fixed_data: &'a FixedData<T>,
        left: &[AffineResult<&'a PolynomialReference, T>],
        right: &'a SelectedExpressions<T>,
        identity: IdentityInSequence,
        identity_processor: &mut IdentityProcessor<'a, '_, T>,
    ) -> EvalResult<'a, T> {
        match identity {
            IdentityInSequence::Internal(i) => identity_processor.process_identity(
                self.identities[i],
                &self.get_current_row_pair(row, fixed_data),
            ),
            IdentityInSequence::OuterQuery => {
                assert!(row as usize % self.block_size == self.block_size - 1);
                identity_processor.process_link(
                    left,
                    right,
                    &self.get_current_row_pair(row, fixed_data),
                )
            }
        }
    }

    fn apply_updates(
        &mut self,
        row: DegreeType,
        identity: IdentityInSequence,
        updates: &EvalValue<&'a PolynomialReference, T>,
        left_mut: &mut [AffineResult<&'a PolynomialReference, T>],
    ) -> bool {
        let mut progress_in_last_step = false;

        let source_name = || match identity {
            IdentityInSequence::Internal(index) => {
                format!("identity {}", self.identities[index])
            }
            IdentityInSequence::OuterQuery => "outer query".to_string(),
        };

        let (before, after) = self.data.split_at_mut(row as usize + 1);
        let current = before.last_mut().unwrap();
        let next = after.first_mut().unwrap();

        let mut row_updater = RowUpdater::new(current, next, row);
        for (poly, c) in &updates.constraints {
            if self.witness_cols.contains(&poly.poly_id()) {
                row_updater.apply_update(poly, c, &source_name);
            } else if let Constraint::Assignment(v) = c {
                for l in left_mut.iter_mut() {
                    if let Ok(l) = l.as_mut() {
                        l.assign(poly, *v);
                    }
                }
            };

            progress_in_last_step = true;
        }

        progress_in_last_step
    }

    fn get_current_row_pair(
        &self,
        row: DegreeType,
        fixed_data: &'a FixedData<T>,
    ) -> RowPair<'_, 'a, T> {
        let current = &self.data[row as usize];
        let next = &self.data[row as usize + 1];
        RowPair::new(current, next, row, fixed_data, UnknownStrategy::Unknown)
    }
}

#[derive(Clone, Debug)]
struct SequenceStep {
    row_delta: i64,
    identity: IdentityInSequence,
}

/// Goes through all rows of the block machine (plus the ones before and after)
/// forward, backward, and forward again.
/// In each row, iterates over all identities until no further progress is made.
struct DefaultSequenceIterator {
    block_size: usize,
    identities_count: usize,
    row_deltas: Vec<i64>,

    /// Whether this is the first time the iterator is called.
    is_first: bool,
    /// Whether any progress was made in the current round.
    progress_in_current_round: bool,
    /// The current row delta index.
    cur_row_delta_index: usize,
    /// The current identity index.
    cur_identity_index: usize,
    /// The number of rounds for the current row delta.
    /// If this number gets too large, we will assume that we're in an infinite loop and exit.
    current_round_count: usize,
}

const MAX_ROUNDS_PER_ROW_DELTA: usize = 100;

impl DefaultSequenceIterator {
    fn new(block_size: usize, identities_count: usize) -> Self {
        let max_row = block_size as i64 - 1;
        DefaultSequenceIterator {
            block_size,
            identities_count,
            row_deltas: (-1..=max_row)
                .chain((-1..max_row).rev())
                .chain(0..=max_row)
                .collect(),
            is_first: true,
            progress_in_current_round: false,
            cur_row_delta_index: 0,
            cur_identity_index: 0,
            current_round_count: 0,
        }
    }

    /// Update the state of the iterator.
    /// If we're not at the last identity in the current row, just moves to the next.
    /// Otherwise, starts with identity 0 and moves to the next row if no progress was made.
    fn update_state(&mut self) {
        if !self.is_first {
            if self.is_last_identity() {
                self.start_next_round();
            } else {
                // Stay at row delta, move to next identity.
                self.cur_identity_index += 1;
            }
        }
        self.is_first = false;
    }

    fn is_last_identity(&self) -> bool {
        let row_delta = self.row_deltas[self.cur_row_delta_index];

        if row_delta + 1 == self.block_size as i64 {
            // In the last row, we want to process one more identity, the outer query.
            self.cur_identity_index == self.identities_count
        } else {
            self.cur_identity_index == self.identities_count - 1
        }
    }

    fn start_next_round(&mut self) {
        if !self.progress_in_current_round || self.current_round_count > MAX_ROUNDS_PER_ROW_DELTA {
            // Move to next row delta, starting with identity 0.
            if self.current_round_count > MAX_ROUNDS_PER_ROW_DELTA {
                panic!("In witness generation for block machine, we have been stuck in the same row for {MAX_ROUNDS_PER_ROW_DELTA} rounds. \
                            This is a bug in the witness generation algorithm.");
            }

            self.cur_row_delta_index += 1;
            self.current_round_count = 0;
        } else {
            // Stay and current row delta, starting with identity 0.
            self.current_round_count += 1;
        }
        self.cur_identity_index = 0;
        self.progress_in_current_round = false;
    }

    pub fn report_progress(&mut self, progress_in_last_step: bool) {
        if !self.is_first {
            self.progress_in_current_round |= progress_in_last_step;
        }
    }

    pub fn next(&mut self) -> Option<SequenceStep> {
        self.update_state();

        if self.cur_row_delta_index == self.row_deltas.len() {
            // Done!
            return None;
        }

        let row_delta = self.row_deltas[self.cur_row_delta_index];
        let identity = if self.cur_identity_index < self.identities_count {
            IdentityInSequence::Internal(self.cur_identity_index)
        } else {
            IdentityInSequence::OuterQuery
        };

        Some(SequenceStep {
            row_delta,
            identity,
        })
    }
}

#[derive(Clone, Copy, Debug)]
enum IdentityInSequence {
    Internal(usize),
    OuterQuery,
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Debug)]
struct SequenceCacheKey {
    /// For each expression on the left-hand side of the lookup, whether it is a constant.
    known_columns: Vec<bool>,
}

impl<K, T> From<&[AffineResult<K, T>]> for SequenceCacheKey
where
    K: Copy + Ord,
    T: FieldElement,
{
    fn from(value: &[AffineResult<K, T>]) -> Self {
        SequenceCacheKey {
            known_columns: value
                .iter()
                .map(|v| {
                    v.as_ref()
                        .ok()
                        .and_then(|ex| ex.is_constant().then_some(true))
                        .is_some()
                })
                .collect(),
        }
    }
}

enum ProcessingSequenceIterator<I: Iterator<Item = SequenceStep>> {
    Default(DefaultSequenceIterator),
    Cached(I),
}

impl<I: Iterator<Item = SequenceStep>> ProcessingSequenceIterator<I> {
    fn report_progress(&mut self, progress_in_last_step: bool) {
        match self {
            ProcessingSequenceIterator::Default(it) => it.report_progress(progress_in_last_step),
            ProcessingSequenceIterator::Cached(_) => {} // Progress is ignored
        }
    }
}

impl<I: Iterator<Item = SequenceStep>> Iterator for ProcessingSequenceIterator<I> {
    type Item = SequenceStep;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            ProcessingSequenceIterator::Default(it) => it.next(),
            ProcessingSequenceIterator::Cached(it) => it.next(),
        }
    }
}

struct ProcessingSequenceCache {
    block_size: usize,
    identities_count: usize,
    cache: BTreeMap<SequenceCacheKey, Vec<SequenceStep>>,
}

impl ProcessingSequenceCache {
    pub fn new(block_size: usize, identities_count: usize) -> Self {
        ProcessingSequenceCache {
            block_size,
            identities_count,
            cache: Default::default(),
        }
    }

    pub fn get_processing_sequence<K, T>(
        &self,
        left: &[AffineResult<K, T>],
    ) -> ProcessingSequenceIterator<impl Iterator<Item = SequenceStep>>
    where
        K: Copy + Ord,
        T: FieldElement,
    {
        match self.cache.get(&left.into()) {
            Some(cached_sequence) => {
                log::trace!("Using cached sequence");
                ProcessingSequenceIterator::Cached(cached_sequence.clone().into_iter())
            }
            None => {
                log::trace!("Using default sequence");
                ProcessingSequenceIterator::Default(DefaultSequenceIterator::new(
                    self.block_size,
                    self.identities_count,
                ))
            }
        }
    }

    pub fn report_processing_sequence<K, T>(
        &mut self,
        left: &[AffineResult<K, T>],
        sequence: Vec<SequenceStep>,
    ) where
        K: Copy + Ord,
        T: FieldElement,
    {
        self.cache.entry(left.into()).or_insert(sequence);
    }
}
