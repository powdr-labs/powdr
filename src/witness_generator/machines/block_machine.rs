use std::collections::{BTreeMap, HashMap, HashSet};

use itertools::Itertools;

use super::{EvalResult, FixedData};
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
/// but do not enforce the offset.
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
    for (i, v) in values.iter().enumerate() {
        if *v
            != if (i + 1) % period == 0 {
                1.into()
            } else {
                0.into()
            }
        {
            return None;
        }
    }

    Some((poly.to_string(), period))
}

impl Machine for BlockMachine {
    fn process_plookup(
        &mut self,
        fixed_data: &FixedData,
        fixed_lookup: &mut Option<&mut dyn Machine>,
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
        fixed_lookup: &mut Option<&mut dyn Machine>,
        left: &[Result<AffineExpression, EvalError>],
        right: &SelectedExpressions,
    ) -> EvalResult {
        let old_len = self.rows();
        if old_len + self.block_size as u64 >= fixed_data.degree {
            return Err("Rows in block machine exhausted.".to_string().into());
        }
        self.append_new_block();
        let block_size = self.block_size as i64;
        let mut outer_assignments = vec![];

        let mut errors = vec![];
        // TODO The error handling currently does not handle contradictions properly.
        // If we can find an assignment of all LHS variables at the end, we do not return an error,
        // even if there is a conflict.

        for delta in (-1..=block_size)
            .chain((-1..block_size).rev())
            .chain(-1..=block_size)
        {
            let mut errors_in_row = vec![];
            self.row =
                (old_len as i64 + delta + fixed_data.degree as i64) as u64 % fixed_data.degree;
            if delta == 3 {
                // Query row
                for (l, r) in left.iter().zip(right.expressions.iter()) {
                    match (l, self.evaluate(fixed_data, r)) {
                        (Ok(l), Ok(r)) => match (l.clone() - r).solve_with_bit_constraints(self) {
                            Ok(result) => {
                                if !result.is_empty() {
                                    errors.clear();
                                    errors_in_row.clear();
                                    outer_assignments.extend(self.handle_eval_result(result));
                                }
                            }
                            Err(e) => errors_in_row.push(e),
                        },
                        (Err(e), Ok(_)) => errors_in_row.push(e.clone()),
                        (Ok(_), Err(e)) => errors_in_row.push(e),
                        (Err(e1), Err(e2)) => errors_in_row.extend([e1.clone(), e2]),
                    }
                }
            }

            for id in self.identities.clone() {
                let result = match id.kind {
                    IdentityKind::Polynomial => self.process_polynomial_identity(
                        fixed_data,
                        id.left.selector.as_ref().unwrap(),
                    ),
                    IdentityKind::Plookup | IdentityKind::Permutation => {
                        self.process_plookup(fixed_data, fixed_lookup, &id)
                    }
                    _ => Err("Unsupported lookup type".to_string().into()),
                };
                match result {
                    Ok(result) => {
                        if !result.is_empty() {
                            errors.clear();
                            errors_in_row.clear();
                            outer_assignments.extend(self.handle_eval_result(result))
                        }
                    }
                    Err(e) => errors_in_row.push(e),
                }
            }
            for e in errors_in_row {
                errors.push(format!("In row {}: {e}", self.row).into())
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
        &mut self,
        fixed_data: &FixedData,
        fixed_lookup: &mut Option<&mut dyn Machine>,
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
        if let Some(result) = fixed_lookup.as_mut().unwrap().process_plookup(
            fixed_data,
            &mut None,
            identity.kind,
            &left,
            &identity.right,
        ) {
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
