use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};

use itertools::Itertools;

use super::{EvalResult, FixedData, FixedLookup};
use crate::witgen::global_constraints::RangeConstraintSet;
use crate::witgen::util::try_to_simple_poly;
use crate::witgen::{
    affine_expression::{AffineExpression, AffineResult},
    expression_evaluator::ExpressionEvaluator,
    machines::Machine,
    range_constraints::RangeConstraint,
    symbolic_witness_evaluator::{SymoblicWitnessEvaluator, WitnessColumnEvaluator},
    Constraint, EvalError,
};
use crate::witgen::{Constraints, EvalValue};
use ast::analyzed::{Expression, Identity, IdentityKind, PolynomialReference, SelectedExpressions};
use number::{DegreeType, FieldElement};

/// A machine that produces multiple rows (one block) per query.
/// TODO we do not actually "detect" the machine yet, we just check if
/// the lookup has a binary selector that is 1 every k rows for some k > 1
pub struct BlockMachine<T: FieldElement> {
    /// Block size, the period of the selector.
    block_size: usize,
    selector: PolynomialReference,
    identities: Vec<Identity<T>>,
    /// One column of values for each witness.
    data: HashMap<usize, Vec<Option<T>>>,
    /// Current row in the machine
    row: DegreeType,
    /// Range constraints, are deleted outside the current block.
    range_constraints: HashMap<usize, HashMap<DegreeType, RangeConstraint<T>>>,
    /// Global range constraints on witness columns.
    global_range_constraints: HashMap<usize, RangeConstraint<T>>,
    /// Poly degree / absolute number of rows
    degree: DegreeType,
    /// Cache that states the order in which to evaluate identities
    /// to make progress most quickly.
    processing_sequence_cache: ProcessingSequenceCache,
}

impl<T: FieldElement> BlockMachine<T> {
    pub fn try_new(
        fixed_data: &FixedData<T>,
        connecting_identities: &[&Identity<T>],
        identities: &[&Identity<T>],
        witness_cols: &HashSet<&PolynomialReference>,
        global_range_constraints: &BTreeMap<&PolynomialReference, RangeConstraint<T>>,
    ) -> Option<Box<Self>> {
        for id in connecting_identities {
            if let Some(sel) = &id.right.selector {
                // TODO we should check that the other constraints/fixed columns are also periodic.
                if let Some((selector, period)) = try_to_boolean_periodic_selector(sel, fixed_data)
                {
                    let mut machine = BlockMachine {
                        block_size: period,
                        selector: selector.clone(),
                        identities: identities.iter().map(|&i| i.clone()).collect(),
                        data: witness_cols
                            .iter()
                            .map(|n| (n.poly_id() as usize, vec![]))
                            .collect(),
                        row: 0,
                        range_constraints: Default::default(),
                        global_range_constraints: global_range_constraints
                            .iter()
                            .filter_map(|(n, c)| {
                                n.is_witness().then_some((n.poly_id() as usize, c.clone()))
                            })
                            .collect(),
                        degree: fixed_data.degree,
                        processing_sequence_cache: ProcessingSequenceCache::new(
                            period,
                            identities.len(),
                        ),
                    };
                    // Append a block so that we do not have to deal with wrap-around
                    // when storing machine witness data.
                    machine.append_new_block(fixed_data.degree).unwrap();

                    return Some(Box::new(machine));
                }
            }
        }

        None
    }
}

/// Check if `expr` is a reference to a function of the form
/// f(i) { if (i + 1) % k == 0 { 1 } else { 0 } }
/// for some k >= 2
/// TODO we could make this more generic and only detect the period
/// but not enforce the offset.
fn try_to_boolean_periodic_selector<'a, T: FieldElement>(
    expr: &'a Expression<T>,
    fixed_data: &FixedData<T>,
) -> Option<(&'a PolynomialReference, usize)> {
    let poly = try_to_simple_poly(expr)?;
    if !poly.is_fixed() {
        return None;
    }

    let values = fixed_data.fixed_col_values[poly.poly_id() as usize];

    let period = 1 + values.iter().position(|v| v.is_one())?;
    if period == 1 {
        return None;
    }
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
        .then_some((poly, period))
}

impl<T: FieldElement> Machine<T> for BlockMachine<T> {
    fn process_plookup<'a>(
        &mut self,
        fixed_data: &FixedData<T>,
        fixed_lookup: &mut FixedLookup<T>,
        kind: IdentityKind,
        left: &[AffineResult<&'a PolynomialReference, T>],
        right: &SelectedExpressions<T>,
    ) -> Option<EvalResult<'a, T>> {
        if *try_to_simple_poly(right.selector.as_ref()?)? != self.selector
            || kind != IdentityKind::Plookup
        {
            return None;
        }
        let previous_len = self.rows() as usize;
        Some({
            let result = self.process_plookup_internal(fixed_data, fixed_lookup, left, right);
            self.range_constraints.clear();
            result.map_err(|e| {
                // rollback the changes.
                for col in self.data.values_mut() {
                    col.truncate(previous_len)
                }
                e
            })
        })
    }

    fn witness_col_values(&mut self, fixed_data: &FixedData<T>) -> HashMap<String, Vec<T>> {
        std::mem::take(&mut self.data)
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

                (fixed_data.witness_cols[id].poly.name.clone(), values)
            })
            .collect()
    }
}

impl<T: FieldElement> BlockMachine<T> {
    /// Extends the data with a new block.
    fn append_new_block(&mut self, max_len: DegreeType) -> Result<(), EvalError> {
        if self.rows() + self.block_size as DegreeType >= max_len {
            return Err(EvalError::RowsExhausted);
        }
        for col in self.data.values_mut() {
            col.resize_with(col.len() + self.block_size, || None);
        }
        Ok(())
    }

    fn rows(&self) -> DegreeType {
        self.data.values().next().unwrap().len() as DegreeType
    }

    fn process_plookup_internal<'b>(
        &mut self,
        fixed_data: &FixedData<T>,
        fixed_lookup: &mut FixedLookup<T>,
        left: &[AffineResult<&'b PolynomialReference, T>],
        right: &SelectedExpressions<T>,
    ) -> EvalResult<'b, T> {
        // First check if we already store the value.
        if left
            .iter()
            .all(|v| v.as_ref().ok().map(|v| v.is_constant()) == Some(true))
            && self.rows() > 0
        {
            // All values on the left hand side are known, check if this is a query
            // to the last row.
            self.row = self.rows() - 1;
            return self
                .process_outer_query(fixed_data, left, right)
                .map(|value| {
                    assert!(value.constraints.is_empty());
                    assert!(value.is_complete());
                    EvalValue::complete(vec![])
                });
        }
        let outer_polys = left
            .iter()
            .flat_map(|r| {
                r.as_ref()
                    .ok()
                    .map(|e| e.nonzero_coefficients().map(|(i, _)| i))
            })
            .flatten()
            .collect::<BTreeSet<_>>();

        let old_len = self.rows();
        self.append_new_block(fixed_data.degree)?;
        let mut outer_assignments = EvalValue::complete(vec![]);

        // TODO this assumes we are always using the same lookup for this machine.
        let cached_sequence = self.processing_sequence_cache.get_processing_sequence(left);

        let mut errors = vec![];
        // TODO The error handling currently does not handle contradictions properly.
        // If we can find an assignment of all LHS variables at the end, we do not return an error,
        // even if there is a conflict.

        // Record the steps where we made progress, so we can report this to the
        // cache later on.
        let mut progress_steps = vec![];
        let mut cached_sequence_iter = cached_sequence.map(|s| s.into_iter());
        let mut default_sequence_iter =
            DefaultSequenceIterator::new(self.block_size, self.identities.len());
        let mut progress_in_last_step = false;
        loop {
            let step = match cached_sequence_iter.as_mut() {
                Some(iter) => iter.next(),
                None => default_sequence_iter.next(progress_in_last_step),
            };

            if step.is_none() {
                break;
            }
            let step = step.unwrap();

            let SequenceStep {
                row_delta,
                identity,
            } = step;
            self.row = (old_len as i64 + row_delta + fixed_data.degree as i64) as DegreeType
                % fixed_data.degree;
            match self.process_identity(fixed_data, fixed_lookup, left, right, identity) {
                Ok(value) => {
                    if !value.is_empty() {
                        progress_in_last_step = true;
                        progress_steps.push(step);
                        errors.clear();
                        let (outer_constraints, inner_value) =
                            self.split_result(value, &outer_polys);
                        outer_assignments.constraints.extend(outer_constraints);
                        for (poly_id, next, constraint) in inner_value
                            .constraints
                            .into_iter()
                            .map(|(poly, constraint)| (poly.poly_id(), poly.next, constraint))
                            .collect::<Vec<_>>()
                        {
                            self.handle_constraint(poly_id as usize, next, constraint);
                        }
                    } else {
                        progress_in_last_step = false;
                    }
                }
                Err(e) => errors.push(format!("In row {}: {e}", self.row).into()),
            }
        }
        // Only succeed if we can assign everything.
        // Otherwise it is messy because we have to find the correct block again.
        let unknown_variables = left
            .iter()
            .filter_map(|l| l.as_ref().ok().map(|l| l.nonzero_variables()))
            .concat()
            .iter()
            .cloned()
            .collect::<BTreeSet<_>>();
        let value_assignments = outer_assignments
            .constraints
            .iter()
            .filter_map(|(var, con)| match con {
                Constraint::Assignment(_) => Some(*var),
                Constraint::RangeConstraint(_) => None,
            })
            .collect::<BTreeSet<_>>();
        if unknown_variables.is_subset(&value_assignments) {
            // We solved the query, so report it to the cache.
            self.processing_sequence_cache
                .report_processing_sequence(left, progress_steps);
            Ok(outer_assignments)
        } else if !errors.is_empty() {
            Err(errors
                .into_iter()
                .reduce(|x: EvalError, y| x.combine(y))
                .unwrap())
        } else {
            Err("Could not assign all variables in the query - maybe the machine does not have enough constraints?".to_string().into())
        }
    }

    // TODO: remove once cleaned up
    #[allow(clippy::type_complexity)]
    fn split_result<'b, 'outer>(
        &self,
        value: EvalValue<&'b PolynomialReference, T>,
        outer_polys: &BTreeSet<&'outer PolynomialReference>,
    ) -> (
        Constraints<&'outer PolynomialReference, T>,
        EvalValue<&'b PolynomialReference, T>,
    ) {
        let mut outer_constraints = vec![];
        let value = EvalValue {
            constraints: value
                .constraints
                .into_iter()
                .filter_map(|(poly, constraint)| {
                    if let Some(outer) = outer_polys.get(poly) {
                        outer_constraints.push((*outer, constraint));
                        None
                    } else {
                        Some((poly, constraint))
                    }
                })
                .collect(),
            ..value
        };
        (outer_constraints, value)
    }

    fn handle_constraint(&mut self, poly: usize, next: bool, constraint: Constraint<T>) {
        let r = (self.row + next as DegreeType) % self.degree;
        match constraint {
            Constraint::Assignment(a) => {
                let values = self.data.get_mut(&poly).unwrap();
                if (r as usize) < values.len() {
                    // do not write to other rows for now
                    values[r as usize] = Some(a);
                }
            }
            Constraint::RangeConstraint(bc) => {
                self.range_constraints
                    .entry(poly)
                    .or_default()
                    .insert(r, bc);
            }
        }
    }

    /// Processes an identity which is either the query or
    /// an identity in the vector of identities.
    fn process_identity<'b>(
        &'b self,
        fixed_data: &FixedData<T>,
        fixed_lookup: &mut FixedLookup<T>,
        left: &[AffineResult<&'b PolynomialReference, T>],
        right: &'b SelectedExpressions<T>,
        identity: IdentityInSequence,
    ) -> EvalResult<'b, T> {
        match identity {
            IdentityInSequence::Internal(index) => {
                let id = &self.identities[index];
                match id.kind {
                    IdentityKind::Polynomial => {
                        self.process_polynomial_identity(fixed_data, id.expression_for_poly_id())
                    }
                    IdentityKind::Plookup | IdentityKind::Permutation => {
                        self.process_plookup(fixed_data, fixed_lookup, id)
                    }
                    _ => Err("Unsupported lookup type".to_string().into()),
                }
            }
            IdentityInSequence::OuterQuery => self.process_outer_query(fixed_data, left, right),
        }
    }

    /// Processes the outer query / the plookup. This function should only be called
    /// on the acutal query row (the last one of the block).
    fn process_outer_query<'b>(
        &self,
        fixed_data: &FixedData<T>,
        left: &[AffineResult<&'b PolynomialReference, T>],
        right: &'b SelectedExpressions<T>,
    ) -> EvalResult<'b, T> {
        assert!(self.row as usize % self.block_size == self.block_size - 1);
        let mut results = EvalValue::complete(vec![]);

        for (l, r) in left.iter().zip(right.expressions.iter()) {
            match (l, self.evaluate(fixed_data, r)) {
                (Ok(l), Ok(r)) => {
                    let result = (l.clone() - r).solve_with_range_constraints(self)?;
                    results.combine(result);
                }
                (Err(e), Ok(_)) => {
                    results.status = results.status.combine(e.clone());
                }
                (Ok(_), Err(e)) => {
                    results.status = results.status.combine(e);
                }
                (Err(e1), Err(e2)) => {
                    results.status = results.status.combine(e1.clone());
                    results.status = results.status.combine(e2);
                }
            }
        }

        Ok(results)
    }

    /// Process a polynomial identity internal no the machine.
    fn process_polynomial_identity<'b>(
        &self,
        fixed_data: &FixedData<T>,
        identity: &'b Expression<T>,
    ) -> EvalResult<'b, T> {
        let evaluated = match self.evaluate(fixed_data, identity) {
            Ok(evaluated) => evaluated,
            Err(cause) => return Ok(EvalValue::incomplete(cause)),
        };
        evaluated.solve_with_range_constraints(self).map_err(|e| {
            let formatted = evaluated.to_string();
            format!("Could not solve expression {formatted} = 0: {e}").into()
        })
    }

    /// Process a plookup internal to the machine against a set of fixed columns.
    fn process_plookup<'b>(
        &self,
        fixed_data: &FixedData<T>,
        fixed_lookup: &mut FixedLookup<T>,
        identity: &'b Identity<T>,
    ) -> EvalResult<'b, T> {
        if identity.left.selector.is_some() || identity.right.selector.is_some() {
            unimplemented!("Selectors not yet implemented.");
        }
        let left = identity
            .left
            .expressions
            .iter()
            .map(|e| self.evaluate(fixed_data, e))
            .collect::<Vec<_>>();
        if let Some(result) =
            fixed_lookup.process_plookup(fixed_data, identity.kind, &left, &identity.right)
        {
            result
        } else {
            Err("Could not find a matching machine for the lookup."
                .to_string()
                .into())
        }
    }

    fn evaluate<'b>(
        &self,
        fixed_data: &FixedData<T>,
        expression: &'b Expression<T>,
    ) -> AffineResult<&'b PolynomialReference, T> {
        ExpressionEvaluator::new(SymoblicWitnessEvaluator::new(
            fixed_data,
            self.row,
            WitnessData {
                fixed_data,
                data: &self.data,
                row: self.row,
            },
        ))
        .evaluate(expression)
    }
}

impl<T: FieldElement> RangeConstraintSet<&PolynomialReference, T> for BlockMachine<T> {
    fn range_constraint(&self, poly: &PolynomialReference) -> Option<RangeConstraint<T>> {
        assert!(poly.is_witness());
        self.global_range_constraints
            .get(&(poly.poly_id() as usize))
            .cloned()
            .or_else(|| {
                let row = (self.row + poly.next as DegreeType) % self.degree;
                self.range_constraints
                    .get(&(poly.poly_id() as usize))?
                    .get(&row)
                    .cloned()
            })
    }
}

#[derive(Clone)]
struct WitnessData<'a, T> {
    pub fixed_data: &'a FixedData<'a, T>,
    pub data: &'a HashMap<usize, Vec<Option<T>>>,
    pub row: DegreeType,
}

impl<'a, T: FieldElement> WitnessColumnEvaluator<T> for WitnessData<'a, T> {
    fn value<'b>(&self, poly: &'b PolynomialReference) -> AffineResult<&'b PolynomialReference, T> {
        let id = poly.poly_id() as usize;
        let row = if poly.next {
            (self.row + 1) % self.fixed_data.degree
        } else {
            self.row
        };
        // It is not an error to access the rows outside the block.
        // We just return a symbolic ID for those.
        match self.data[&id].get(row as usize).cloned().flatten() {
            Some(value) => Ok(value.into()),
            None => Ok(AffineExpression::from_variable_id(poly)),
        }
    }
}

struct ProcessingSequenceCache {
    block_size: usize,
    identities_count: usize,
    cache: BTreeMap<SequenceCacheKey, Vec<SequenceStep>>,
}

#[derive(Clone)]
struct SequenceStep {
    row_delta: i64,
    identity: IdentityInSequence,
}

#[derive(Clone, Copy)]
enum IdentityInSequence {
    Internal(usize),
    OuterQuery,
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Debug)]
struct SequenceCacheKey {
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

struct DefaultSequenceIteratorState {
    progress_in_current_round: bool,
    cur_row_delta_index: usize,
    cur_identity_index: usize,
    is_first: bool,
    current_round_count: usize,
}

impl DefaultSequenceIteratorState {
    fn new() -> Self {
        DefaultSequenceIteratorState {
            progress_in_current_round: false,
            cur_row_delta_index: 0,
            cur_identity_index: 0,
            is_first: true,
            current_round_count: 0,
        }
    }
}

struct DefaultSequenceIterator {
    block_size: usize,
    identities_count: usize,
    row_deltas: Vec<i64>,
    state: DefaultSequenceIteratorState,
}

impl DefaultSequenceIterator {
    fn new(block_size: usize, identities_count: usize) -> Self {
        let block_size_i64 = block_size as i64;
        DefaultSequenceIterator {
            block_size,
            identities_count,
            row_deltas: (-1..=block_size_i64)
                .chain((-1..block_size_i64).rev())
                .chain(0..=block_size_i64)
                .collect(),
            state: DefaultSequenceIteratorState::new(),
        }
    }

    fn next(&mut self, progress_in_last_step: bool) -> Option<SequenceStep> {
        if self.state.cur_row_delta_index == self.row_deltas.len() {
            // Done!
            return None;
        }

        let row_delta = self.row_deltas[self.state.cur_row_delta_index];
        let is_last_identity = if row_delta + 1 == self.block_size as i64 {
            self.state.cur_identity_index == self.identities_count
        } else {
            self.state.cur_identity_index == self.identities_count - 1
        };

        if !self.state.is_first {
            // Update state
            self.state.progress_in_current_round |= progress_in_last_step;
            if is_last_identity {
                if !self.state.progress_in_current_round || self.state.current_round_count > 100 {
                    // Move to next row delta, start with identity 0.
                    self.state.cur_row_delta_index += 1;
                    self.state.current_round_count = 0;
                } else {
                    self.state.current_round_count += 1;
                }
                self.state.cur_identity_index = 0;
                self.state.progress_in_current_round = false;
            } else {
                // Stay at row delta, move to next identity.
                self.state.cur_identity_index += 1;
            }
        }
        self.state.is_first = false;

        let identity = if self.state.cur_identity_index < self.identities_count {
            IdentityInSequence::Internal(self.state.cur_identity_index)
        } else {
            IdentityInSequence::OuterQuery
        };

        Some(SequenceStep {
            row_delta,
            identity,
        })
    }
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
    ) -> Option<Vec<SequenceStep>>
    where
        K: Copy + Ord,
        T: FieldElement,
    {
        self.cache.get(&left.into()).cloned()
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
