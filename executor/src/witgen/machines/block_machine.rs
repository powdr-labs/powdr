use std::collections::{BTreeMap, HashMap, HashSet};

use itertools::Itertools;

use super::{EvalResult, FixedData, FixedLookup};
use crate::witgen::util::try_to_simple_poly_ref;
use crate::witgen::EvalValue;
use crate::witgen::{
    affine_expression::{AffineExpression, AffineResult},
    bit_constraints::{BitConstraint, BitConstraintSet},
    expression_evaluator::ExpressionEvaluator,
    machines::Machine,
    symbolic_witness_evaluator::{SymoblicWitnessEvaluator, WitnessColumnEvaluator},
    util::WitnessColumnNamer,
    Constraint, EvalError,
};
use number::{DegreeType, FieldElement};
use pil_analyzer::{
    Expression, Identity, IdentityKind, PolyID, PolynomialReference, PolynomialType,
    SelectedExpressions,
};

/// A machine that produces multiple rows (one block) per query.
/// TODO we do not actually "detect" the machine yet, we just check if
/// the lookup has a binary selector that is 1 every k rows for some k > 1
pub struct BlockMachine {
    /// Block size, the period of the selector.
    block_size: usize,
    selector: PolyID,
    identities: Vec<Identity>,
    /// One column of values for each witness.
    data: HashMap<usize, Vec<Option<FieldElement>>>,
    /// Current row in the machine
    row: DegreeType,
    /// Bit constraints, are deleted outside the current block.
    bit_constraints: HashMap<usize, HashMap<DegreeType, BitConstraint>>,
    /// Global bit constraints on witness columns.
    global_bit_constraints: HashMap<usize, BitConstraint>,
    /// Number of witnesses (in general, not in this machine).
    witness_count: usize,
    /// Poly degree / absolute number of rows
    degree: DegreeType,
    /// Cache that states the order in which to evaluate identities
    /// to make progress most quickly.
    processing_sequence_cache: ProcessingSequenceCache,
}

impl BlockMachine {
    pub fn try_new(
        fixed_data: &FixedData,
        connecting_identities: &[&Identity],
        identities: &[&Identity],
        witness_names: &HashSet<&str>,
        global_bit_constraints: &BTreeMap<&str, BitConstraint>,
    ) -> Option<Box<Self>> {
        for id in connecting_identities {
            if let Some(sel) = &id.right.selector {
                // TODO we should check that the other constraints/fixed columns are also periodic.
                if let Some((selector, period)) = try_to_boolean_periodic_selector(sel, fixed_data)
                {
                    let mut machine = BlockMachine {
                        block_size: period,
                        selector,
                        identities: identities.iter().map(|&i| i.clone()).collect(),
                        data: witness_names
                            .iter()
                            .map(|n| (fixed_data.witness_ids[n], vec![]))
                            .collect(),
                        row: 0,
                        bit_constraints: Default::default(),
                        global_bit_constraints: global_bit_constraints
                            .iter()
                            .filter_map(|(n, c)| {
                                fixed_data.witness_ids.get(n).map(|n| (*n, c.clone()))
                            })
                            .collect(),
                        witness_count: fixed_data.witness_cols.len(),
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
fn try_to_boolean_periodic_selector(
    expr: &Expression,
    fixed_data: &FixedData,
) -> Option<(PolyID, usize)> {
    let poly = try_to_simple_poly_ref(expr)?;
    if poly.ptype != PolynomialType::Constant {
        return None;
    }

    let values = fixed_data.fixed_col_values[poly.id as usize];

    let period = 1 + values.iter().position(|v| *v == 1.into())?;
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

impl Machine for BlockMachine {
    fn process_plookup(
        &mut self,
        fixed_data: &FixedData,
        fixed_lookup: &mut FixedLookup,
        kind: IdentityKind,
        left: &[AffineResult],
        right: &SelectedExpressions,
    ) -> Option<EvalResult> {
        if try_to_simple_poly_ref(right.selector.as_ref()?)? != self.selector
            || kind != IdentityKind::Plookup
        {
            return None;
        }
        let previous_len = self.rows() as usize;
        Some({
            let result = self.process_plookup_internal(fixed_data, fixed_lookup, left, right);
            self.bit_constraints.clear();
            result.map_err(|e| {
                // rollback the changes.
                for col in self.data.values_mut() {
                    col.truncate(previous_len)
                }
                e
            })
        })
    }

    fn witness_col_values(&mut self, fixed_data: &FixedData) -> HashMap<String, Vec<FieldElement>> {
        std::mem::take(&mut self.data)
            .into_iter()
            .map(|(id, values)| {
                let mut values = values
                    .into_iter()
                    .map(|v| v.unwrap_or_default())
                    .collect::<Vec<_>>();

                values.resize(fixed_data.degree as usize, 0.into());
                (fixed_data.name(id), values)
            })
            .collect()
    }
}

impl BlockMachine {
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

    fn process_plookup_internal(
        &mut self,
        fixed_data: &FixedData,
        fixed_lookup: &mut FixedLookup,
        left: &[AffineResult],
        right: &SelectedExpressions,
    ) -> EvalResult {
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

        let old_len = self.rows();
        self.append_new_block(fixed_data.degree)?;
        let mut outer_assignments = EvalValue::complete(vec![]);

        // TODO this assumes we are always using the same lookup for this machine.
        let sequence = self.processing_sequence_cache.get_processing_sequence(left);

        let mut errors = vec![];
        // TODO The error handling currently does not handle contradictions properly.
        // If we can find an assignment of all LHS variables at the end, we do not return an error,
        // even if there is a conflict.

        // Record the steps where we made progress, so we can report this to the
        // cache later on.
        let mut progress_steps = vec![];
        for step in sequence {
            let SequenceStep {
                row_delta,
                identity,
            } = step;
            self.row = (old_len as i64 + row_delta + fixed_data.degree as i64) as DegreeType
                % fixed_data.degree;
            match self.process_identity(fixed_data, fixed_lookup, left, right, identity) {
                Ok(value) => {
                    if !value.is_empty() {
                        progress_steps.push(step);
                        errors.clear();
                        let value = self.handle_eval_result(value);
                        outer_assignments.combine(value);
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
            .collect::<HashSet<_>>();
        let value_assignments = outer_assignments
            .constraints
            .iter()
            .filter_map(|(var, con)| match con {
                Constraint::Assignment(_) => Some(*var),
                Constraint::BitConstraint(_) => None,
            })
            .collect::<HashSet<_>>();
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

    fn handle_eval_result(&mut self, value: EvalValue) -> EvalValue {
        EvalValue {
            constraints: value
                .constraints
                .into_iter()
                .filter_map(|(poly, constraint)| {
                    let (poly, next) = self.extract_next(poly);
                    let r = (self.row + next as DegreeType) % self.degree;
                    let is_outside_poly = !self.data.contains_key(&poly);
                    if is_outside_poly {
                        assert!(!next);

                        Some((poly, constraint))
                    } else {
                        match constraint {
                            Constraint::Assignment(a) => {
                                let values = self.data.get_mut(&poly).unwrap();
                                if (r as usize) < values.len() {
                                    // do not write to other rows for now
                                    values[r as usize] = Some(a);
                                }
                            }
                            Constraint::BitConstraint(bc) => {
                                self.bit_constraints.entry(poly).or_default().insert(r, bc);
                            }
                        }
                        None
                    }
                })
                .collect(),
            ..value
        }
    }

    /// Processes an identity which is either the query or
    /// an identity in the vector of identities.
    fn process_identity(
        &self,
        fixed_data: &FixedData,
        fixed_lookup: &mut FixedLookup,
        left: &[AffineResult],
        right: &SelectedExpressions,
        identity: IdentityInSequence,
    ) -> EvalResult {
        match identity {
            IdentityInSequence::Internal(index) => {
                let id = &self.identities[index];
                match id.kind {
                    IdentityKind::Polynomial => self.process_polynomial_identity(
                        fixed_data,
                        id.left.selector.as_ref().unwrap(),
                    ),
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
    fn process_outer_query(
        &self,
        fixed_data: &FixedData,
        left: &[AffineResult],
        right: &SelectedExpressions,
    ) -> EvalResult {
        assert!(self.row as usize % self.block_size == self.block_size - 1);
        let mut results = EvalValue::complete(vec![]);

        for (l, r) in left.iter().zip(right.expressions.iter()) {
            match (l, self.evaluate(fixed_data, r)) {
                (Ok(l), Ok(r)) => {
                    let result = (l.clone() - r).solve_with_bit_constraints(self)?;
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
    fn process_polynomial_identity(
        &self,
        fixed_data: &FixedData,
        identity: &Expression,
    ) -> EvalResult {
        let evaluated = match self.evaluate(fixed_data, identity) {
            Ok(evaluated) => evaluated,
            Err(cause) => return Ok(EvalValue::incomplete(cause)),
        };
        evaluated.solve_with_bit_constraints(self).map_err(|e| {
            let witness_data = WitnessData {
                fixed_data,
                data: &self.data,
                row: self.row,
            };
            let formatted = evaluated.format(&witness_data);
            if let EvalError::ConstraintUnsatisfiable(_) = e {
                EvalError::ConstraintUnsatisfiable(format!(
                    "Constraint is invalid ({formatted} != 0)."
                ))
            } else {
                format!("Could not solve expression {formatted} = 0: {e}").into()
            }
        })
    }

    /// Process a plookup internal to the machine against a set of fixed columns.
    fn process_plookup(
        &self,
        fixed_data: &FixedData,
        fixed_lookup: &mut FixedLookup,
        identity: &Identity,
    ) -> EvalResult {
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

    fn evaluate(&self, fixed_data: &FixedData, expression: &Expression) -> AffineResult {
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

    /// Converts a poly ID that might contain a next offset
    /// to a regular poly ID plus a boolean signifying "next".
    fn extract_next(&self, id: usize) -> (usize, bool) {
        extract_next(self.witness_count, id)
    }
}

impl BitConstraintSet for BlockMachine {
    fn bit_constraint(&self, id: usize) -> Option<BitConstraint> {
        let (poly, next) = self.extract_next(id);
        self.global_bit_constraints.get(&poly).cloned().or_else(|| {
            let row = (self.row + next as DegreeType) % self.degree;
            self.bit_constraints.get(&poly)?.get(&row).cloned()
        })
    }
}

#[derive(Clone)]
struct WitnessData<'a> {
    pub fixed_data: &'a FixedData<'a>,
    pub data: &'a HashMap<usize, Vec<Option<FieldElement>>>,
    pub row: DegreeType,
}

impl<'a> WitnessColumnEvaluator for WitnessData<'a> {
    fn value(&self, poly: &PolynomialReference) -> AffineResult {
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
            None => {
                let witness_count = self.fixed_data.witness_cols.len();
                let symbolic_id = if poly.next { id + witness_count } else { id };
                Ok(AffineExpression::from_variable_id(symbolic_id))
            }
        }
    }
}

impl<'a> WitnessColumnNamer for WitnessData<'a> {
    fn name(&self, i: usize) -> String {
        let (id, next) = extract_next(self.fixed_data.witness_cols.len(), i);
        self.fixed_data.name(id) + if next { "\'" } else { "" }
    }
}

/// Converts a poly ID that might contain a next offset
/// to a regular poly ID plus a boolean signifying "next".
fn extract_next(witness_count: usize, id: usize) -> (usize, bool) {
    if id < witness_count {
        (id, false)
    } else {
        (id - witness_count, true)
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

impl From<&[AffineResult]> for SequenceCacheKey {
    fn from(value: &[AffineResult]) -> Self {
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

impl ProcessingSequenceCache {
    pub fn new(block_size: usize, identities_count: usize) -> Self {
        ProcessingSequenceCache {
            block_size,
            identities_count,
            cache: Default::default(),
        }
    }

    pub fn get_processing_sequence(&self, left: &[AffineResult]) -> Vec<SequenceStep> {
        self.cache.get(&left.into()).cloned().unwrap_or_else(|| {
            let block_size = self.block_size as i64;
            (-1..=block_size)
                .chain((-1..block_size).rev())
                .chain(-1..=block_size)
                .flat_map(|row_delta| {
                    let mut identities = (0..self.identities_count)
                        .map(IdentityInSequence::Internal)
                        .collect::<Vec<_>>();
                    if row_delta + 1 == self.block_size as i64 {
                        // Process the query on the query row.
                        identities.push(IdentityInSequence::OuterQuery);
                    }
                    identities.into_iter().map(move |identity| SequenceStep {
                        row_delta,
                        identity,
                    })
                })
                .collect()
        })
    }

    pub fn report_processing_sequence(
        &mut self,
        left: &[AffineResult],
        sequence: Vec<SequenceStep>,
    ) {
        self.cache.entry(left.into()).or_insert(sequence);
    }
}
