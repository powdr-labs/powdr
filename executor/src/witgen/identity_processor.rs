use std::{
    collections::{BTreeMap, HashMap},
    sync::Mutex,
};

use itertools::{Either, Itertools};
use lazy_static::lazy_static;
use powdr_ast::{
    analyzed::{AlgebraicExpression as Expression, AlgebraicReference, Identity, IdentityKind},
    parsed::SelectedExpressions,
};
use powdr_number::FieldElement;

use crate::witgen::{machines::Machine, EvalError};

use super::{
    affine_expression::AffineExpression,
    machines::{FixedLookup, KnownMachine},
    rows::RowPair,
    EvalResult, EvalValue, FixedData, IncompleteCause, MutableState, QueryCallback,
};

/// A list of mutable references to machines.
pub struct Machines<'a, 'b, T: FieldElement> {
    identity_to_machine_index: BTreeMap<u64, usize>,
    machines: Vec<&'b mut KnownMachine<'a, T>>,
}

impl<'a, 'b, T: FieldElement> Machines<'a, 'b, T> {
    /// Splits out the machine at `index` and returns it together with a list of all other machines.
    /// As a result, they can be mutated independently.
    /// With #515 implemented (making calling machines immutable), this could be removed as there
    /// would not be a need to split the list of machines.
    pub fn split<'c>(
        &'c mut self,
        index: usize,
    ) -> (&'c mut KnownMachine<'a, T>, Machines<'a, 'c, T>) {
        let (before, after) = self.machines.split_at_mut(index);
        let (current, after) = after.split_at_mut(1);
        let current: &'c mut KnownMachine<'a, T> = current.first_mut().unwrap();

        // Re-borrow machines to convert from `&'c mut &'b mut KnownMachine<'a, T>` to
        // `&'c mut KnownMachine<'a, T>`.
        let others: Vec<&'c mut KnownMachine<'a, T>> = before
            .iter_mut()
            .chain(after.iter_mut())
            .map(|m| &mut **m)
            .collect();

        (current, others.into_iter().into())
    }

    pub fn len(&self) -> usize {
        self.machines.len()
    }

    pub fn iter_mut(&'b mut self) -> impl Iterator<Item = &'b mut KnownMachine<'a, T>> {
        self.machines.iter_mut().map(|m| &mut **m)
    }

    pub fn call<Q: QueryCallback<T>>(
        &mut self,
        identity_id: u64,
        args: &[AffineExpression<&'a AlgebraicReference, T>],
        fixed_lookup: &mut FixedLookup<T>,
        query_callback: &mut Q,
    ) -> EvalResult<'a, T> {
        let machine_index = *self
            .identity_to_machine_index
            .get(&identity_id)
            .expect("No executor machine matched identity `{identity}`");

        let (current, others) = self.split(machine_index);
        let mut mutable_state = MutableState {
            fixed_lookup,
            machines: others,
            query_callback,
        };

        current.process_plookup_timed(&mut mutable_state, identity_id, args)
    }
}

impl<'a, 'b, T, I> From<I> for Machines<'a, 'b, T>
where
    T: FieldElement,
    I: Iterator<Item = &'b mut KnownMachine<'a, T>>,
{
    fn from(machines: I) -> Self {
        let machines = machines.collect::<Vec<_>>();
        let identity_to_machine_index = machines
            .iter()
            .enumerate()
            .flat_map(|(index, m)| m.identity_ids().into_iter().map(move |id| (id, index)))
            .collect();
        Self {
            machines,
            identity_to_machine_index,
        }
    }
}

/// Computes (value or range constraint) updates given a [RowPair] and [Identity].
/// The lifetimes mean the following:
/// - `'a`: The duration of the entire witness generation (e.g. references to identities)
/// - `'b`: The duration of this machine's call (e.g. the mutable references of the other machines)
/// - `'c`: The duration of this IdentityProcessor's lifetime (e.g. the reference to the mutable state)
pub struct IdentityProcessor<'a, 'b, 'c, T: FieldElement, Q: QueryCallback<T>> {
    fixed_data: &'a FixedData<'a, T>,
    mutable_state: &'c mut MutableState<'a, 'b, T, Q>,
}

impl<'a, 'b, 'c, T: FieldElement, Q: QueryCallback<T>> IdentityProcessor<'a, 'b, 'c, T, Q> {
    pub fn new(
        fixed_data: &'a FixedData<'a, T>,
        mutable_state: &'c mut MutableState<'a, 'b, T, Q>,
    ) -> Self {
        Self {
            fixed_data,
            mutable_state,
        }
    }

    /// Given an identity and a row pair, tries to figure out additional values / range constraints
    /// for the given cells.
    /// Fails if any constraint was not satisfiable.
    /// Returns the updates.
    pub fn process_identity(
        &mut self,
        identity: &'a Identity<Expression<T>>,
        rows: &RowPair<'_, 'a, T>,
    ) -> EvalResult<'a, T> {
        let result = match identity.kind {
            IdentityKind::Polynomial => self.process_polynomial_identity(identity, rows),
            IdentityKind::Plookup | IdentityKind::Permutation => {
                self.process_plookup(identity, rows)
            }
            kind => {
                unimplemented!(
                    "Identity of kind {kind:?} is not supported by the identity processor."
                )
            }
        };
        report_identity_solving(identity, &result);
        result
    }

    fn process_polynomial_identity(
        &self,
        identity: &'a Identity<Expression<T>>,
        rows: &RowPair<T>,
    ) -> EvalResult<'a, T> {
        match rows.evaluate(identity.expression_for_poly_id()) {
            Err(incomplete_cause) => Ok(EvalValue::incomplete(incomplete_cause)),
            Ok(evaluated) => evaluated.solve_with_range_constraints(rows),
        }
    }

    fn process_plookup(
        &mut self,
        identity: &'a Identity<Expression<T>>,
        rows: &RowPair<'_, 'a, T>,
    ) -> EvalResult<'a, T> {
        if let Some(left_selector) = &identity.left.selector {
            if let Some(status) = self.handle_left_selector(left_selector, rows) {
                return Ok(status);
            }
        }

        let left = identity
            .left
            .expressions
            .iter()
            .map(|e| rows.evaluate(e))
            .collect::<Vec<_>>();

        // Fail if the LHS has an error.
        let (left, errors): (Vec<_>, Vec<_>) = left.into_iter().partition_map(|x| match x {
            Ok(x) => Either::Left(x),
            Err(x) => Either::Right(x),
        });
        if !errors.is_empty() {
            return Ok(EvalValue::incomplete(
                errors.into_iter().reduce(|x, y| x.combine(y)).unwrap(),
            ));
        }

        // Now query the machines.
        // Note that we should always query all machines that match, because they might
        // update their internal data, even if all values are already known.
        // TODO could it be that multiple machines match?

        // query the fixed lookup "machine"
        if let Some(result) = self.mutable_state.fixed_lookup.process_plookup_timed(
            self.fixed_data,
            rows,
            identity.kind,
            &left,
            &identity.right,
        ) {
            return result;
        }

        self.mutable_state.machines.call(
            identity.id,
            &left,
            self.mutable_state.fixed_lookup,
            self.mutable_state.query_callback,
        )
    }

    /// Handles the lookup that connects the current machine to the calling machine.
    /// Arguments:
    /// - `left`: The evaluation of the left side of the lookup (symbolic for unknown values).
    /// - `right`: The expressions on the right side of the lookup.
    /// - `current_rows`: The [RowPair] needed to evaluate the right side of the lookup.
    /// Returns:
    /// - `Ok(updates)`: The updates for the lookup.
    /// - `Err(e)`: If the constraint system is not satisfiable.
    pub fn process_link(
        &mut self,
        left: &[AffineExpression<&'a AlgebraicReference, T>],
        right: &'a SelectedExpressions<Expression<T>>,
        current_rows: &RowPair<'_, 'a, T>,
    ) -> EvalResult<'a, T> {
        // sanity check that the right hand side selector is active
        let selector_value = right
            .selector
            .as_ref()
            .map(|s| {
                current_rows
                    .evaluate(s)
                    .ok()
                    .and_then(|affine_expression| affine_expression.constant_value())
                    .ok_or(EvalError::Generic("Selector is not 1!".to_string()))
            })
            .unwrap_or(Ok(T::one()))?;
        assert_eq!(selector_value, T::one());

        let mut updates = EvalValue::complete(vec![]);

        for (l, r) in left.iter().zip(right.expressions.iter()) {
            match current_rows.evaluate(r) {
                Ok(r) => {
                    let result = (l.clone() - r).solve_with_range_constraints(current_rows)?;
                    updates.combine(result);
                }
                Err(e) => {
                    updates.status = updates.status.combine(e);
                }
            }
        }
        Ok(updates)
    }

    /// Returns updates of the left selector cannot be evaluated to 1, otherwise None.
    fn handle_left_selector(
        &self,
        left_selector: &'a Expression<T>,
        rows: &RowPair<T>,
    ) -> Option<EvalValue<&'a AlgebraicReference, T>> {
        let value = match rows.evaluate(left_selector) {
            Err(incomplete_cause) => return Some(EvalValue::incomplete(incomplete_cause)),
            Ok(value) => value,
        };
        match value.constant_value() {
            Some(v) if v.is_zero() => Some(EvalValue::complete(vec![])),
            Some(v) if v.is_one() => None,
            _ => Some(EvalValue::incomplete(
                IncompleteCause::NonConstantLeftSelector,
            )),
        }
    }
}

pub struct IdentityData {
    pub invocations: u64,
    pub success: u64,
}

type IdentityID = u64;

lazy_static! {
    static ref STATISTICS: Mutex<HashMap<IdentityID, IdentityData>> =
        Mutex::new(Default::default());
}

fn report_identity_solving<T: FieldElement, K>(
    identity: &Identity<Expression<T>>,
    result: &EvalResult<T, K>,
) {
    let success = result.as_ref().map(|r| r.is_complete()).unwrap_or_default() as u64;
    let mut stat = STATISTICS.lock().unwrap();
    stat.entry(identity.id)
        .and_modify(|s| {
            s.invocations += 1;
            s.success += success;
        })
        .or_insert(IdentityData {
            invocations: 1,
            success,
        });
}

pub fn get_and_reset_solving_statistics() -> HashMap<IdentityID, IdentityData> {
    let mut stat = STATISTICS.lock().unwrap();
    std::mem::take(&mut (*stat))
}
