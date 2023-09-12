use std::collections::{BTreeMap, HashMap, HashSet};

use super::{EvalResult, FixedData, FixedLookup};
use crate::witgen::affine_expression::AffineExpression;
use crate::witgen::column_map::WitnessColumnMap;
use crate::witgen::identity_processor::IdentityProcessor;
use crate::witgen::processor::Calldata;
use crate::witgen::processor::Processor;
use crate::witgen::rows::CellValue;
use crate::witgen::rows::{Row, RowFactory, RowPair, UnknownStrategy};
use crate::witgen::sequence_iterator::ProcessingSequenceCache;
use crate::witgen::util::try_to_simple_poly;
use crate::witgen::{machines::Machine, range_constraints::RangeConstraint, EvalError};
use crate::witgen::{EvalValue, IncompleteCause};
use ast::analyzed::{
    Expression, Identity, IdentityKind, PolyID, PolynomialReference, SelectedExpressions,
};
use itertools::Either;
use itertools::Itertools;
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
    fixed_data: &'a FixedData<'a, T>,
}

impl<'a, T: FieldElement> BlockMachine<'a, T> {
    pub fn try_new(
        fixed_data: &'a FixedData<'a, T>,
        connecting_identities: &[&'a Identity<T>],
        identities: &[&'a Identity<T>],
        witness_cols: &HashSet<PolyID>,
        global_range_constraints: &WitnessColumnMap<Option<RangeConstraint<T>>>,
    ) -> Option<Self> {
        for id in connecting_identities {
            // TODO we should check that the other constraints/fixed columns are also periodic.
            if let Some(block_size) = try_to_period(&id.right.selector, fixed_data) {
                assert!(block_size <= fixed_data.degree as usize);
                let row_factory = RowFactory::new(fixed_data, global_range_constraints.clone());
                // Start out with a block filled with unknown values so that we do not have to deal with wrap-around
                // when storing machine witness data.
                // This will be filled with the default block in `take_witness_col_values`
                let data = vec![row_factory.fresh_row(); block_size];
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
        _fixed_data: &'a FixedData<T>,
        fixed_lookup: &mut FixedLookup<T>,
        kind: IdentityKind,
        left: &[AffineExpression<&'a PolynomialReference, T>],
        right: &'a SelectedExpressions<T>,
    ) -> Option<EvalResult<'a, T>> {
        if *right != self.selected_expressions || kind != IdentityKind::Plookup {
            return None;
        }
        let previous_len = self.rows() as usize;
        Some({
            let result = self.process_plookup_internal(fixed_lookup, left, right);
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
        if self.data.len() < 2 * self.block_size {
            log::warn!(
                "Filling empty blocks with zeros, because the block machine is never used. \
                 This might violate some internal constraints."
            );
        }
        let mut data = transpose_rows(std::mem::take(&mut self.data), &self.witness_cols)
            .into_iter()
            .map(|(id, mut values)| {
                // For all constraints to be satisfied, unused cells have to be filled with valid values.
                // We do this, we construct a default block, by repeating the first input to the block machine.
                values.resize(fixed_data.degree as usize, None);

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

        // Build the processor.
        let mut machines = vec![];
        let mut processor = Processor::new(
            fixed_data.degree - 2,
            rows,
            IdentityProcessor::new(fixed_data, fixed_lookup, &mut machines),
            &self.identities,
            fixed_data,
            self.row_factory.clone(),
            &self.witness_cols,
        );

        // Check if we can accept the last row as is.
        if processor.check_constraints().is_err() {
            log::warn!("Detected error in last row! Will attempt to fix it now.");

            // Clear the last row and run the solver
            processor.clear_row(1);
            processor
                .solve_with_default_sequence_iterator()
                .expect("Some constraints were not satisfiable when solving for the last row.");
            let last_row = processor.finish().0.remove(1);

            // Copy values into data
            for (poly_id, values) in data.iter_mut() {
                values[fixed_data.degree as usize - 1] =
                    last_row[poly_id].value.unwrap_or_default();
            }
        }
    }

    fn rows(&self) -> DegreeType {
        self.data.len() as DegreeType
    }

    fn process_plookup_internal(
        &mut self,
        fixed_lookup: &mut FixedLookup<T>,
        left: &[AffineExpression<&'a PolynomialReference, T>],
        right: &'a SelectedExpressions<T>,
    ) -> EvalResult<'a, T> {
        log::trace!("Start processing block machine");
        log::trace!("Left values of lookup:");
        for l in left {
            log::trace!("  {}", l);
        }

        // TODO: Add possibility for machines to call other machines.
        let mut machines = vec![];
        let mut identity_processor =
            IdentityProcessor::new(self.fixed_data, fixed_lookup, &mut machines);

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
            let next = self.row_factory.fresh_row();
            let row_pair = RowPair::new(
                current,
                &next,
                row,
                self.fixed_data,
                UnknownStrategy::Unknown,
            );

            let result = identity_processor.process_link(left, right, &row_pair)?;

            if result.is_complete() {
                return Ok(result);
            }
        }

        // TODO this assumes we are always using the same lookup for this machine.
        let mut sequence_iterator = self.processing_sequence_cache.get_processing_sequence(left);

        if !sequence_iterator.has_steps() {
            // Shortcut, no need to do anything.
            return Ok(EvalValue::incomplete(
                IncompleteCause::BlockMachineLookupIncomplete,
            ));
        }

        // Make the block two larger than the block size, it includes the last row of the previous block
        // and the first row of the next block.
        let block = vec![self.row_factory.fresh_row(); self.block_size + 2];
        let mut processor = Processor::new(
            self.block_size as DegreeType - 1,
            block,
            identity_processor,
            &self.identities,
            self.fixed_data,
            self.row_factory.clone(),
            &self.witness_cols,
        )
        .with_calldata(Calldata::new(left.to_vec(), right));

        let outer_assignments = processor.solve(&mut sequence_iterator)?;
        let (new_block, left_new) = processor.finish();
        let left_new = left_new.unwrap();

        // Only succeed if we can assign everything.
        // Otherwise it is messy because we have to find the correct block again.
        let success = left_new.iter().all(|v| v.is_constant());

        if success {
            log::trace!("End processing block machine (successfully)");
            self.append_block(new_block)?;

            // We solved the query, so report it to the cache.
            self.processing_sequence_cache
                .report_processing_sequence(left, sequence_iterator);
            Ok(EvalValue::complete(outer_assignments))
        } else {
            log::trace!("End processing block machine (incomplete)");
            self.processing_sequence_cache.report_incomplete(left);
            Ok(EvalValue::incomplete(
                IncompleteCause::BlockMachineLookupIncomplete,
            ))
        }
    }

    /// Takes a block of rows, which contains the last row of the previous block
    /// and the first row of the next block. The first row of the next block is ignored,
    /// the last row of the previous block is merged with the first row of the next block.
    /// This is necessary to handle non-rectangular block machines, which already use
    /// unused cells in the previous block.
    fn append_block(&mut self, mut new_block: Vec<Row<'a, T>>) -> Result<(), EvalError<T>> {
        if self.rows() + self.block_size as DegreeType >= self.fixed_data.degree {
            return Err(EvalError::RowsExhausted);
        }

        assert_eq!(new_block.len(), self.block_size + 2);

        // 1. Ignore the first row of the next block:
        new_block.pop();
        // 2. Take out the last row from the previous block
        let (mut last_row, new_block): (Vec<_>, Vec<_>) =
            new_block.into_iter().enumerate().partition_map(|(i, row)| {
                if i == 0 {
                    Either::Left(row)
                } else {
                    Either::Right(row)
                }
            });
        let last_row = last_row.pop().unwrap();
        // 3. Merge the last row of the previous block
        let last_row_index = self.rows() as usize - 1;
        let existing_last_row = self.data.get_mut(last_row_index).unwrap();
        *existing_last_row =
            WitnessColumnMap::from(last_row.into_iter().map(|(k, v)| match &v.value {
                CellValue::Known(_) => v,
                _ => existing_last_row[&k].clone(),
            }));

        // 4. Append the new block
        self.data.extend(new_block);

        Ok(())
    }
}
