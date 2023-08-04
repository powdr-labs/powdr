use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::marker::PhantomData;

use super::{EvalResult, FixedData, FixedLookup};
use crate::witgen::column_map::ColumnMap;
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
use crate::witgen::{Constraints, EvalValue, IncompleteCause};
use ast::analyzed::{
    Expression, Identity, IdentityKind, PolyID, PolynomialReference, SelectedExpressions,
};
use number::{DegreeType, FieldElement};

/// A machine that produces multiple rows (one block) per query.
/// TODO we do not actually "detect" the machine yet, we just check if
/// the lookup has a binary selector that is 1 every k rows for some k
pub struct BlockMachine<T: FieldElement> {
    /// Block size, the period of the selector.
    block_size: usize,
    selected_expressions: SelectedExpressions<T>,
    identities: Vec<Identity<T>>,
    /// One column of values for each witness.
    data: HashMap<PolyID, Vec<Option<T>>>,
    /// Current row in the machine
    row: DegreeType,
    /// Range constraints, are deleted outside the current block.
    range_constraints: HashMap<PolyID, HashMap<DegreeType, RangeConstraint<T>>>,
    /// Range constraints of the outer polynomials, for the current block.
    outer_range_constraints: HashMap<PolyID, RangeConstraint<T>>,
    /// Global range constraints on witness columns.
    global_range_constraints: ColumnMap<Option<RangeConstraint<T>>>,
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
        witness_cols: &HashSet<PolyID>,
        global_range_constraints: &ColumnMap<Option<RangeConstraint<T>>>,
    ) -> Option<Box<Self>> {
        for id in connecting_identities {
            // TODO we should check that the other constraints/fixed columns are also periodic.
            if let Some(period) = try_to_period(&id.right.selector, fixed_data) {
                let mut machine = BlockMachine {
                    block_size: period,
                    selected_expressions: id.right.clone(),
                    identities: identities.iter().map(|&i| i.clone()).collect(),
                    data: witness_cols.iter().map(|&n| (n, vec![])).collect(),
                    row: 0,
                    range_constraints: Default::default(),
                    outer_range_constraints: Default::default(),
                    global_range_constraints: global_range_constraints.clone(),
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

impl<T: FieldElement> Machine<T> for BlockMachine<T> {
    fn process_plookup<'a>(
        &mut self,
        fixed_data: &FixedData<T>,
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
            self.range_constraints.clear();
            self.outer_range_constraints.clear();
            if let Ok(assignments) = &result {
                if !assignments.is_complete() {
                    // rollback the changes.
                    for col in self.data.values_mut() {
                        col.truncate(previous_len)
                    }
                }
            }
            result
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

                (fixed_data.column_name(&id).to_string(), values)
            })
            .collect()
    }
}

struct ChangeLogger<'a, T: FieldElement> {
    first_change: bool,
    identity: &'a IdentityInSequence,
    _marker: PhantomData<T>,
}

impl<'a, T: FieldElement> ChangeLogger<'a, T> {
    fn new(identity: &'a IdentityInSequence) -> Self {
        Self {
            first_change: true,
            identity,
            _marker: PhantomData,
        }
    }

    fn log_constraints(
        &mut self,
        bm: &BlockMachine<T>,
        constraints: &Constraints<&PolynomialReference, T>,
        include_row: bool,
    ) {
        if log::STATIC_MAX_LEVEL >= log::Level::Trace {
            for (poly, constraint) in constraints {
                let row = (bm.row + poly.next as DegreeType) % bm.degree;
                if let Constraint::Assignment(value) = constraint {
                    if self.first_change {
                        let name = match self.identity {
                            IdentityInSequence::Internal(index) => {
                                format!("identity {}", bm.identities[*index])
                            }
                            IdentityInSequence::OuterQuery => "outer query".to_string(),
                        };
                        log::trace!("  Processing {}", name);
                        self.first_change = false;
                    }
                    let row_string = match include_row {
                        true => format!(" (Row {})", row),
                        false => "".to_string(),
                    };
                    log::trace!("    => {}{} = {}", poly.name, row_string, value);
                }
            }
        }
    }
}

impl<T: FieldElement> BlockMachine<T> {
    /// Extends the data with a new block.
    fn append_new_block(&mut self, max_len: DegreeType) -> Result<(), EvalError<T>> {
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
        right: &'b SelectedExpressions<T>,
    ) -> EvalResult<'b, T> {
        log::trace!("Start processing block machine");

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
            let row_before = self.row;
            self.row = self.rows() - 1;
            let result = self.process_outer_query(fixed_data, left, right);

            match result {
                Ok(result) => {
                    if result.is_complete() {
                        return Ok(result);
                    } else {
                        // If the query is not complete, we roll back the row change and continue.
                        // This might happen if the block machine just validates some input (rather
                        // than computing an output) and hasn't been called on that input before.
                        self.row = row_before;
                    }
                }
                Err(e) => {
                    // Non-recoverable error
                    return Err(e);
                }
            }
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
            let mut progress_in_last_step = false;

            let SequenceStep {
                row_delta,
                identity,
            } = step;
            self.row = (old_len as i64 + row_delta + fixed_data.degree as i64) as DegreeType
                % fixed_data.degree;

            let mut change_logger = ChangeLogger::new(&identity);

            match self.process_identity(fixed_data, fixed_lookup, &left_mut, right, identity) {
                Ok(value) => {
                    if !value.is_empty() {
                        errors.clear();

                        let (outer_constraints, inner_value) =
                            self.split_result(value, &outer_polys);

                        change_logger.log_constraints(self, &inner_value.constraints, true);
                        for (poly_id, next, constraint) in inner_value
                            .constraints
                            .into_iter()
                            .map(|(poly, constraint)| (poly.poly_id(), poly.next, constraint))
                            .collect::<Vec<_>>()
                        {
                            progress_in_last_step |=
                                self.handle_constraint(&poly_id, next, constraint);
                        }

                        change_logger.log_constraints(self, &outer_constraints, false);
                        self.handle_outer_constraints(&outer_constraints, &mut left_mut);

                        progress_in_last_step |= !outer_constraints.is_empty();
                        outer_assignments.constraints.extend(outer_constraints);

                        if progress_in_last_step {
                            progress_steps.push(step);
                        }
                    }
                }
                Err(e) => errors.push(format!("In row {}: {e}", self.row).into()),
            }
            processing_sequence_iterator.report_progress(progress_in_last_step);
        }

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

    /// Updates self.data and self.range_constraints for the current constraint.
    /// Returns whether any changed where made.
    fn handle_constraint(&mut self, poly: &PolyID, next: bool, constraint: Constraint<T>) -> bool {
        let r = (self.row + next as DegreeType) % self.degree;
        match constraint {
            Constraint::Assignment(a) => {
                let r = r as usize;
                let values = self.data.get_mut(poly).unwrap();
                if r < values.len() {
                    assert!(values[r].is_none(), "Duplicate assignment");
                    // do not write to other rows for now
                    values[r] = Some(a);
                    true
                } else {
                    false
                }
            }
            Constraint::RangeConstraint(rc) => {
                let poly_map = self.range_constraints.entry(*poly).or_default();
                update_range_constraint(poly_map, r, rc);
                true
            }
        }
    }

    /// Updates self.outer_range_constraints and left for the list of outer constraints.
    fn handle_outer_constraints<'a>(
        &mut self,
        outer_constraints: &Constraints<&'a PolynomialReference, T>,
        left: &mut [AffineResult<&'a PolynomialReference, T>],
    ) {
        for affine_result in left.iter_mut() {
            match affine_result {
                Ok(ref mut affine_expression) => {
                    for (poly, constraint) in outer_constraints {
                        match constraint {
                            Constraint::Assignment(value) => affine_expression.assign(poly, *value),
                            Constraint::RangeConstraint(rc) => {
                                update_range_constraint(
                                    &mut self.outer_range_constraints,
                                    poly.poly_id(),
                                    rc.clone(),
                                );
                            }
                        }
                    }
                }
                Err(_) => {}
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

fn update_range_constraint<K: Eq + std::hash::Hash, T: FieldElement>(
    range_constraints: &mut HashMap<K, RangeConstraint<T>>,
    key: K,
    range_constraint: RangeConstraint<T>,
) {
    // Combine with existing range constraint if it exists.
    let range_constraint = range_constraints
        .get(&key)
        .map(|existing_rc| existing_rc.conjunction(&range_constraint))
        .unwrap_or(range_constraint);

    range_constraints.insert(key, range_constraint);
}

impl<T: FieldElement> RangeConstraintSet<&PolynomialReference, T> for BlockMachine<T> {
    fn range_constraint(&self, poly: &PolynomialReference) -> Option<RangeConstraint<T>> {
        assert!(poly.is_witness());
        self.global_range_constraints[&poly.poly_id()]
            .clone()
            .or_else(|| {
                let row = (self.row + poly.next as DegreeType) % self.degree;
                self.range_constraints
                    .get(&(poly.poly_id()))?
                    .get(&row)
                    .cloned()
            })
            .or_else(|| self.outer_range_constraints.get(&(poly.poly_id())).cloned())
    }
}

#[derive(Clone)]
struct WitnessData<'a, T> {
    pub fixed_data: &'a FixedData<'a, T>,
    pub data: &'a HashMap<PolyID, Vec<Option<T>>>,
    pub row: DegreeType,
}

impl<'a, T: FieldElement> WitnessColumnEvaluator<T> for WitnessData<'a, T> {
    fn value<'b>(&self, poly: &'b PolynomialReference) -> AffineResult<&'b PolynomialReference, T> {
        let id = poly.poly_id();
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

const MAX_ROUNDS_PER_ROW_DELTA: usize = 20;

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
                log::warn!("In witness generation for block machine, we have been stuck in the same row for {MAX_ROUNDS_PER_ROW_DELTA} rounds. \
                            This is likely a bug in the witness generation algorithm.");
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
