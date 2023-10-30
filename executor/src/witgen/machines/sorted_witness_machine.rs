use std::collections::{BTreeMap, HashMap, HashSet};

use ast::parsed::SelectedExpressions;
use itertools::Itertools;

use super::super::affine_expression::AffineExpression;
use super::Machine;
use super::{EvalResult, FixedData};
use crate::witgen::{
    expression_evaluator::ExpressionEvaluator, fixed_evaluator::FixedEvaluator,
    symbolic_evaluator::SymbolicEvaluator,
};
use crate::witgen::{EvalValue, IncompleteCause, MutableState, QueryCallback};
use ast::analyzed::{
    AlgebraicExpression as Expression, AlgebraicReference as Reference, Identity, IdentityKind,
    PolyID, PolynomialReference,
};
use number::FieldElement;

/// A machine that can support a lookup in a set of columns that are sorted
/// by one specific column and values in that column have to be unique.
/// This means there is a column A and a constraint of the form
/// NOTLAST { A' - A } in { POSITIVE }
/// Where
///  - NOTLAST is zero only on the last row
///  - POSITIVE has all values from 1 to half of the field size.
pub struct SortedWitnesses<'a, T> {
    key_col: PolyID,
    /// Position of the witness columns in the data.
    witness_positions: HashMap<PolyID, usize>,
    data: BTreeMap<T, Vec<Option<T>>>,
    fixed_data: &'a FixedData<'a, T>,
}

impl<'a, T: FieldElement> SortedWitnesses<'a, T> {
    pub fn try_new(
        fixed_data: &'a FixedData<T>,
        identities: &[&Identity<Expression<T>>],
        witnesses: &HashSet<PolyID>,
    ) -> Option<Self> {
        if identities.len() != 1 {
            return None;
        }
        check_identity(fixed_data, identities.first().unwrap()).map(|key_col| {
            let witness_positions = witnesses
                .iter()
                .filter(|&w| *w != key_col)
                .sorted()
                .enumerate()
                .map(|(i, &x)| (x, i))
                .collect();

            SortedWitnesses {
                key_col,
                witness_positions,
                data: Default::default(),
                fixed_data,
            }
        })
    }
}

fn check_identity<T: FieldElement>(
    fixed_data: &FixedData<T>,
    id: &Identity<Expression<T>>,
) -> Option<PolyID> {
    // Looking for NOTLAST { A' - A } in { POSITIVE }
    if id.kind != IdentityKind::Plookup
        || id.right.selector.is_some()
        || id.left.expressions.len() != 1
    {
        return None;
    }

    // Check for A' - A in the LHS
    let key_column = check_constraint(id.left.expressions.first().unwrap())?;

    let notlast = id.left.selector.as_ref()?;
    let positive = id.right.expressions.first().unwrap();

    // TODO this could be rather slow. We should check the code for identity instead
    // of evaluating it.
    let degree = fixed_data.degree as usize;
    for row in 0..(degree) {
        let ev = ExpressionEvaluator::new(FixedEvaluator::new(fixed_data, row));
        let nl = ev.evaluate(notlast).ok()?.constant_value()?;
        if (row == degree - 1 && !nl.is_zero()) || (row < degree - 1 && !nl.is_one()) {
            return None;
        }
        let pos = ev.evaluate(positive).ok()?.constant_value()?;
        if pos != (row as u64 + 1).into() {
            return None;
        }
    }
    Some(key_column)
}

/// Checks that the identity has a constraint of the form `a' - a` as the first expression
/// on the left hand side and returns the ID of the witness column.
fn check_constraint<T: FieldElement>(constraint: &Expression<T>) -> Option<PolyID> {
    let symbolic_ev = SymbolicEvaluator;
    let sort_constraint = match ExpressionEvaluator::new(symbolic_ev).evaluate(constraint) {
        Ok(c) => c,
        Err(_) => return None,
    };
    let key_column_id = match sort_constraint.nonzero_variables().as_slice() {
        [key, _] | [_, key] if !key.next => *key,
        _ => return None,
    };
    if key_column_id.next || key_column_id.is_fixed() {
        return None;
    }
    let poly_next = PolynomialReference {
        next: true,
        ..key_column_id.clone()
    };
    let pattern = AffineExpression::from_variable_id(&poly_next)
        - AffineExpression::from_variable_id(key_column_id);
    if sort_constraint != pattern {
        return None;
    }

    Some(key_column_id.poly_id())
}

impl<'a, T: FieldElement> Machine<'a, T> for SortedWitnesses<'a, T> {
    fn process_plookup<Q: QueryCallback<T>>(
        &mut self,
        _mutable_state: &mut MutableState<'a, '_, T, Q>,
        kind: IdentityKind,
        left: &[AffineExpression<&'a PolynomialReference, T>],
        right: &'a SelectedExpressions<Expression<T>>,
    ) -> Option<EvalResult<'a, T>> {
        if kind != IdentityKind::Plookup || right.selector.is_some() {
            return None;
        }
        let rhs = right
            .expressions
            .iter()
            .map(|e| match e {
                Expression::Reference(Reference::Poly(p)) => {
                    assert!(!p.next);
                    if p.poly_id() == self.key_col
                        || self.witness_positions.contains_key(&p.poly_id())
                    {
                        Some(p)
                    } else {
                        None
                    }
                }
                _ => None,
            })
            .collect::<Option<Vec<_>>>()?;

        Some(self.process_plookup_internal(left, right, rhs))
    }
    fn take_witness_col_values(&mut self) -> HashMap<String, Vec<T>> {
        let mut result = HashMap::new();

        let (mut keys, mut values): (Vec<_>, Vec<_>) =
            std::mem::take(&mut self.data).into_iter().unzip();

        let mut last_key = keys.last().cloned().unwrap_or_default();
        while keys.len() < self.fixed_data.degree as usize {
            last_key += 1u64.into();
            keys.push(last_key);
        }
        result.insert(self.fixed_data.column_name(&self.key_col).to_string(), keys);

        for (col, &i) in &self.witness_positions {
            let mut col_values = values
                .iter_mut()
                .map(|row| std::mem::take(&mut row[i]).unwrap_or_default())
                .collect::<Vec<_>>();
            col_values.resize(self.fixed_data.degree as usize, 0.into());
            result.insert(self.fixed_data.column_name(col).to_string(), col_values);
        }

        result
    }
}

impl<'a, T: FieldElement> SortedWitnesses<'a, T> {
    fn process_plookup_internal(
        &mut self,
        left: &[AffineExpression<&'a PolynomialReference, T>],
        right: &SelectedExpressions<Expression<T>>,
        rhs: Vec<&PolynomialReference>,
    ) -> EvalResult<'a, T> {
        let key_index = rhs
            .iter()
            .position(|&x| x.poly_id() == self.key_col)
            .unwrap();

        let key_value = left[key_index].constant_value().ok_or_else(|| {
            format!(
                "Value of unique key must be known: {} = {}",
                left[key_index], right.expressions[key_index]
            )
        })?;

        let mut assignments = EvalValue::complete(vec![]);
        let stored_values = self
            .data
            .entry(key_value)
            .or_insert_with(|| vec![None; self.witness_positions.len()]);
        for (l, &r) in left.iter().zip(rhs.iter()).skip(1) {
            let stored_value = &mut stored_values[self.witness_positions[&r.poly_id()]];
            match stored_value {
                // There is a stored value
                Some(v) => {
                    match (l.clone() - (*v).into()).solve() {
                        Err(_) => {
                            // The LHS value is known and it is differetn from the stored one.
                            return Err(format!(
                                "Lookup mismatch: There is already a unique row with {} = \
                            {key_value} and {r} = {v}, but wanted to store {r} = {l}",
                                self.fixed_data.column_name(&self.key_col),
                            )
                            .into());
                        }
                        Ok(ass) => {
                            if !ass.is_empty() {
                                log::trace!(
                                    "Read {} = {key_value} -> {r} = {v}",
                                    self.fixed_data.column_name(&self.key_col)
                                );
                            }
                            assignments.combine(ass);
                        }
                    }
                }
                // There is no value stored yet.
                None => match l.constant_value() {
                    Some(v) => {
                        log::trace!(
                            "Stored {} = {key_value} -> {r} = {v}",
                            self.fixed_data.column_name(&self.key_col)
                        );
                        *stored_value = Some(v);
                    }
                    None => {
                        return Ok(EvalValue::incomplete(IncompleteCause::DataNotYetAvailable));
                    }
                },
            }
        }
        Ok(assignments)
    }
}
