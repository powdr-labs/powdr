use std::collections::{BTreeMap, HashMap, HashSet};

use itertools::Itertools;

use super::{EvalResult, FixedData, FixedLookup};
use crate::analyzer::{Expression, Identity, IdentityKind, SelectedExpressions};
use crate::number::{AbstractNumberType, DegreeType};
use crate::witness_generator::eval_error;
use crate::witness_generator::{
    affine_expression::AffineExpression,
    bit_constraints::{BitConstraint, BitConstraintSet},
    eval_error::EvalError,
    expression_evaluator::ExpressionEvaluator,
    machines::Machine,
    symbolic_witness_evaluator::{SymoblicWitnessEvaluator, WitnessColumnEvaluator},
    util::{is_simple_poly, WitnessColumnNamer},
    Constraint,
};

/// A machine that produces multiple rows (one block) per query.
/// TODO we do not actually "detect" the machine yet, we just check if
/// the lookup has a binary selector that is 1 every k rows for some k > 1
pub struct BlockMachine {
    /// Block size, the period of the selector.
    block_size: usize,
    selector: String,
    identities: Vec<Identity>,
    /// One column of values for each witness.
    data: HashMap<usize, Vec<Option<AbstractNumberType>>>,
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
                if let Some((selector, period)) = is_boolean_periodic_selector(sel, fixed_data) {
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
                    machine.append_new_block();

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
fn is_boolean_periodic_selector(
    expr: &Expression,
    fixed_data: &FixedData,
) -> Option<(String, usize)> {
    let poly = is_simple_poly(expr)?;

    let values = fixed_data.fixed_cols.get(poly)?;

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
        .then_some((poly.to_string(), period))
}

impl Machine for BlockMachine {
    fn process_plookup(
        &mut self,
        fixed_data: &FixedData,
        fixed_lookup: &mut FixedLookup,
        kind: IdentityKind,
        left: &[Result<AffineExpression, EvalError>],
        right: &SelectedExpressions,
    ) -> Option<EvalResult> {
        if is_simple_poly(right.selector.as_ref()?)? != self.selector
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

    fn witness_col_values(
        &mut self,
        fixed_data: &FixedData,
    ) -> HashMap<String, Vec<AbstractNumberType>> {
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
    fn append_new_block(&mut self) {
        for col in self.data.values_mut() {
            col.resize_with(col.len() + self.block_size, || None);
        }
    }

    fn rows(&self) -> DegreeType {
        self.data.values().next().unwrap().len() as DegreeType
    }

    fn process_plookup_internal(
        &mut self,
        fixed_data: &FixedData,
        fixed_lookup: &mut FixedLookup,
        left: &[Result<AffineExpression, EvalError>],
        right: &SelectedExpressions,
    ) -> EvalResult {
        // First check if we already store the value.
        if left
            .iter()
            .all(|v| v.as_ref().ok().map(|v| v.is_constant()) == Some(true))
        {
            return Ok(vec![]);
            // TOOD check that they really exist (maybe just check the last row)
        }

        let old_len = self.rows();
        if old_len + self.block_size as u64 >= fixed_data.degree {
            return Err("Rows in block machine exhausted.".to_string().into());
        }
        self.append_new_block();
        let mut outer_assignments = vec![];

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
                identity_index,
            } = step;
            self.row =
                (old_len as i64 + row_delta + fixed_data.degree as i64) as u64 % fixed_data.degree;
            match self.process_identity(fixed_data, fixed_lookup, left, right, identity_index) {
                Ok(result) => {
                    if !result.is_empty() {
                        progress_steps.push(step);
                        errors.clear();
                        outer_assignments.extend(self.handle_eval_result(result))
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
            Err(errors.into_iter().reduce(eval_error::combine).unwrap())
        } else {
            Err("Could not assign all variables in the query - maybe the machine does not have enough constraints?".to_string().into())
        }
    }

    fn handle_eval_result(&mut self, result: Vec<(usize, Constraint)>) -> Vec<(usize, Constraint)> {
        result
            .into_iter()
            .filter_map(|(poly, constraint)| {
                let (poly, row_offset) = self.convert_next_to_offset(poly);
                let r = (self.row + row_offset) % self.degree;
                let is_outside_poly = !self.data.contains_key(&poly);
                if is_outside_poly {
                    assert!(row_offset == 0);

                    Some((poly, constraint))
                } else {
                    match constraint {
                        Constraint::Assignment(a) => {
                            let values = self.data.get_mut(&poly).unwrap();
                            if r as usize <= values.len() {
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
            .collect()
    }

    /// Processes an identity which is either the query (identity_index is None) or
    /// an identity in the vector of identities.
    fn process_identity(
        &self,
        fixed_data: &FixedData,
        fixed_lookup: &mut FixedLookup,
        left: &[Result<AffineExpression, EvalError>],
        right: &SelectedExpressions,
        identity_index: Option<usize>,
    ) -> EvalResult {
        if let Some(index) = identity_index {
            let id = &self.identities[index];
            match id.kind {
                IdentityKind::Polynomial => {
                    self.process_polynomial_identity(fixed_data, id.left.selector.as_ref().unwrap())
                }
                IdentityKind::Plookup | IdentityKind::Permutation => {
                    self.process_plookup(fixed_data, fixed_lookup, id)
                }
                _ => Err("Unsupported lookup type".to_string().into()),
            }
        } else {
            self.process_outer_query(fixed_data, left, right)
        }
    }

    /// Processes the outer query / the plookup. This function should only be called
    /// on the acutal query row (the last one of the block).
    fn process_outer_query(
        &self,
        fixed_data: &FixedData,
        left: &[Result<AffineExpression, EvalError>],
        right: &SelectedExpressions,
    ) -> EvalResult {
        assert!(self.row as usize % self.block_size == self.block_size - 1);
        let mut errors = vec![];
        let mut results = vec![];

        // TODO how to properly hanlde the errors here?
        // We only return them if we do not make any progress.

        for (l, r) in left.iter().zip(right.expressions.iter()) {
            match (l, self.evaluate(fixed_data, r)) {
                (Ok(l), Ok(r)) => match (l.clone() - r).solve_with_bit_constraints(self) {
                    Ok(result) => results.extend(result),
                    Err(e) => errors.push(e),
                },
                (Err(e), Ok(_)) => errors.push(e.clone()),
                (Ok(_), Err(e)) => errors.push(e),
                (Err(e1), Err(e2)) => errors.extend([e1.clone(), e2]),
            }
        }
        if results.is_empty() && !errors.is_empty() {
            Err(errors.into_iter().reduce(eval_error::combine).unwrap())
        } else {
            Ok(results)
        }
    }

    fn process_polynomial_identity(
        &self,
        fixed_data: &FixedData,
        identity: &Expression,
    ) -> EvalResult {
        let evaluated = self.evaluate(fixed_data, identity)?;
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

    fn evaluate(
        &self,
        fixed_data: &FixedData,
        expression: &Expression,
    ) -> Result<AffineExpression, EvalError> {
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

    /// Converts a poly ID that migth contain a next offset
    /// to a regular poly ID plus a row offset.
    fn convert_next_to_offset(&self, id: usize) -> (usize, u64) {
        convert_next_to_offset(self.witness_count, id)
    }
}

impl BitConstraintSet for BlockMachine {
    fn bit_constraint(&self, id: usize) -> Option<BitConstraint> {
        let (poly, offset) = self.convert_next_to_offset(id);
        self.global_bit_constraints.get(&poly).cloned().or_else(|| {
            let row = (self.row + offset) % self.degree;
            self.bit_constraints.get(&poly)?.get(&row).cloned()
        })
    }
}

#[derive(Clone)]
struct WitnessData<'a> {
    pub fixed_data: &'a FixedData<'a>,
    pub data: &'a HashMap<usize, Vec<Option<AbstractNumberType>>>,
    pub row: DegreeType,
}

impl<'a> WitnessColumnEvaluator for WitnessData<'a> {
    fn value(&self, name: &str, next: bool) -> Result<AffineExpression, EvalError> {
        let id = self.fixed_data.witness_ids[name];
        let row = if next {
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
                let symbolic_id = if next { id + witness_count } else { id };
                Ok(AffineExpression::from_witness_poly_value(symbolic_id))
            }
        }
    }
}

impl<'a> WitnessColumnNamer for WitnessData<'a> {
    fn name(&self, i: usize) -> String {
        match convert_next_to_offset(self.fixed_data.witness_cols.len(), i) {
            (i, 0) => self.fixed_data.name(i),
            (i, 1) => self.fixed_data.name(i) + "\'",
            _ => panic!(),
        }
    }
}

/// Converts a poly ID that migth contain a next offset
/// to a regular poly ID plus a row offset.
fn convert_next_to_offset(witness_count: usize, id: usize) -> (usize, u64) {
    if id < witness_count {
        (id, 0)
    } else {
        (id - witness_count, 1)
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
    identity_index: Option<usize>,
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Debug)]
struct SequenceCacheKey {
    known_columns: Vec<bool>,
}

impl From<&[Result<AffineExpression, EvalError>]> for SequenceCacheKey {
    fn from(value: &[Result<AffineExpression, EvalError>]) -> Self {
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

    pub fn get_processing_sequence(
        &self,
        left: &[Result<AffineExpression, EvalError>],
    ) -> Vec<SequenceStep> {
        self.cache.get(&left.into()).cloned().unwrap_or_else(|| {
            let block_size = self.block_size as i64;
            (-1..=block_size)
                .chain((-1..block_size).rev())
                .chain(-1..=block_size)
                .flat_map(|row_delta| {
                    let mut indices = (0..self.identities_count).map(Some).collect::<Vec<_>>();
                    if row_delta + 1 == self.block_size as i64 {
                        // Process the query on the query row.
                        indices.push(None);
                    }
                    indices.into_iter().map(move |identity_index| SequenceStep {
                        row_delta,
                        identity_index,
                    })
                })
                .collect()
        })
    }

    pub fn report_processing_sequence(
        &mut self,
        left: &[Result<AffineExpression, EvalError>],
        sequence: Vec<SequenceStep>,
    ) {
        self.cache.entry(left.into()).or_insert(sequence);
    }
}
