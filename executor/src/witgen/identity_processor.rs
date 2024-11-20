use std::{
    collections::{BTreeMap, HashMap},
    sync::Mutex,
};

use lazy_static::lazy_static;
use powdr_ast::analyzed::{
    AlgebraicExpression as Expression, LookupIdentity, PermutationIdentity, PhantomLookupIdentity,
    PhantomPermutationIdentity, PolynomialIdentity,
};
use powdr_number::FieldElement;

use crate::{
    witgen::{global_constraints::CombinedRangeConstraintSet, machines::Machine, EvalError},
    Identity,
};

use super::{
    affine_expression::AlgebraicVariable,
    machines::{KnownMachine, LookupCell},
    processor::OuterQuery,
    rows::RowPair,
    EvalResult, EvalValue, IncompleteCause, MutableState, QueryCallback,
};

/// A list of mutable references to machines.
pub struct Machines<'a, 'b, T: FieldElement> {
    identity_to_machine_index: BTreeMap<u64, usize>,
    machines: Vec<&'b mut KnownMachine<'a, T>>,
}

impl<'a, 'b, T: FieldElement> Machines<'a, 'b, T> {
    /// Splits out the machine at `index` and returns it together with a list of all other machines.
    /// As a result, they can be mutated independently.
    fn split<'c>(&'c mut self, index: usize) -> (&'c mut KnownMachine<'a, T>, Machines<'a, 'c, T>) {
        let (before, after) = self.machines.split_at_mut(index);
        let (current, after) = after.split_at_mut(1);
        let current: &'c mut KnownMachine<'a, T> = current.first_mut().unwrap();

        // Re-borrow machines to convert from `&'c mut &'b mut KnownMachine<'a, T>` to
        // `&'c mut KnownMachine<'a, T>`.
        let others: Machines<'a, 'c, T> = before
            .iter_mut()
            .chain(after.iter_mut())
            .map(|m| &mut **m)
            .into();

        (current, others)
    }

    /// Like `split`, but with the "other" machines only containing machines after the current one.
    fn split_skipping_previous_machines<'c>(
        &'c mut self,
        index: usize,
    ) -> (&'c mut KnownMachine<'a, T>, Machines<'a, 'c, T>) {
        let (before, after) = self.machines.split_at_mut(index + 1);
        let current: &'c mut KnownMachine<'a, T> = before.last_mut().unwrap();

        // Re-borrow machines to convert from `&'c mut &'b mut KnownMachine<'a, T>` to
        // `&'c mut KnownMachine<'a, T>`.
        let others: Machines<'a, 'c, T> = after.iter_mut().map(|m| &mut **m).into();

        (current, others)
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
        caller_rows: &RowPair<'_, 'a, T>,
        query_callback: &mut Q,
    ) -> EvalResult<'a, T> {
        let machine_index = *self
            .identity_to_machine_index
            .get(&identity_id)
            .unwrap_or_else(|| panic!("No executor machine matched identity ID: {identity_id}"));

        let (current, others) = self.split(machine_index);
        let mut mutable_state = MutableState {
            machines: others,
            query_callback,
        };

        current.process_plookup_timed(&mut mutable_state, identity_id, caller_rows)
    }

    pub fn call_direct<Q: QueryCallback<T>>(
        &mut self,
        identity_id: u64,
        values: Vec<LookupCell<'_, T>>,
        query_callback: &mut Q,
    ) -> Result<bool, EvalError<T>> {
        let machine_index = *self
            .identity_to_machine_index
            .get(&identity_id)
            .unwrap_or_else(|| panic!("No executor machine matched identity ID: {identity_id}"));

        // TOOD this has horrible performance, avoid this.
        // It's probably much better to use runtime borrow checks.
        // This will fail if we have circular calls.
        // At some point, we can probably turn this into an async message passing interface.
        let (current, others) = self.split(machine_index);

        let mut mutable_state = MutableState {
            machines: others,
            query_callback,
        };
        current.process_lookup_direct(&mut mutable_state, identity_id, values)
    }

    pub fn take_witness_col_values<Q: QueryCallback<T>>(
        &mut self,
        query_callback: &mut Q,
    ) -> HashMap<String, Vec<T>> {
        (0..self.len())
            .flat_map(|machine_index| {
                // Don't include the previous machines, as they are already finalized.
                let (current, others) = self.split_skipping_previous_machines(machine_index);
                let mut mutable_state = MutableState {
                    machines: others,
                    query_callback,
                };
                current
                    .take_witness_col_values(&mut mutable_state)
                    .into_iter()
            })
            .collect()
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
    mutable_state: &'c mut MutableState<'a, 'b, T, Q>,
}

impl<'a, 'b, 'c, T: FieldElement, Q: QueryCallback<T>> IdentityProcessor<'a, 'b, 'c, T, Q> {
    pub fn new(mutable_state: &'c mut MutableState<'a, 'b, T, Q>) -> Self {
        Self { mutable_state }
    }

    /// Given an identity and a row pair, tries to figure out additional values / range constraints
    /// for the given cells.
    /// Fails if any constraint was not satisfiable.
    /// Returns the updates.
    pub fn process_identity(
        &mut self,
        identity: &'a Identity<T>,
        rows: &RowPair<'_, 'a, T>,
    ) -> EvalResult<'a, T> {
        let result = match identity {
            Identity::Polynomial(identity) => self.process_polynomial_identity(identity, rows),
            Identity::Lookup(LookupIdentity { left, id, .. })
            | Identity::Permutation(PermutationIdentity { left, id, .. })
            | Identity::PhantomLookup(PhantomLookupIdentity { left, id, .. })
            | Identity::PhantomPermutation(PhantomPermutationIdentity { left, id, .. }) => {
                self.process_lookup_or_permutation(*id, left, rows)
            }
            Identity::Connect(..) => {
                // TODO this is not the right cause.
                Ok(EvalValue::incomplete(IncompleteCause::SolvingFailed))
                // unimplemented!(
                //     "Identity of kind {kind:?} is not supported by the identity processor."
                // )
            }
        };
        report_identity_solving(identity, &result);
        result
    }

    fn process_polynomial_identity(
        &self,
        identity: &'a PolynomialIdentity<T>,
        rows: &RowPair<T>,
    ) -> EvalResult<'a, T> {
        match rows.evaluate(&identity.expression) {
            Err(incomplete_cause) => Ok(EvalValue::incomplete(incomplete_cause)),
            Ok(evaluated) => evaluated.solve_with_range_constraints(rows),
        }
    }

    fn process_lookup_or_permutation(
        &mut self,
        id: u64,
        left: &'a powdr_ast::analyzed::SelectedExpressions<T>,
        rows: &RowPair<'_, 'a, T>,
    ) -> EvalResult<'a, T> {
        if let Some(status) = self.handle_left_selector(&left.selector, rows) {
            return Ok(status);
        }

        self.mutable_state
            .machines
            .call(id, rows, self.mutable_state.query_callback)
    }

    /// Handles the lookup that connects the current machine to the calling machine.
    /// Arguments:
    /// - `left`: The evaluation of the left side of the lookup (symbolic for unknown values).
    /// - `right`: The expressions on the right side of the lookup.
    /// - `current_rows`: The [RowPair] needed to evaluate the right side of the lookup.
    ///
    /// Returns:
    /// - `Ok(updates)`: The updates for the lookup.
    /// - `Err(e)`: If the constraint system is not satisfiable.
    pub fn process_link(
        &mut self,
        outer_query: &OuterQuery<'a, '_, T>,
        current_rows: &RowPair<'_, 'a, T>,
    ) -> EvalResult<'a, T> {
        let right = outer_query.connection.right;
        // sanity check that the right hand side selector is active
        current_rows
            .evaluate(&right.selector)
            .ok()
            .and_then(|affine_expression| affine_expression.constant_value())
            .and_then(|v| v.is_one().then_some(()))
            .ok_or(EvalError::Generic("Selector is not 1!".to_string()))?;

        let range_constraint =
            CombinedRangeConstraintSet::new(outer_query.caller_rows, current_rows);

        let mut updates = EvalValue::complete(vec![]);

        for (l, r) in outer_query.left.iter().zip(right.expressions.iter()) {
            match current_rows.evaluate(r) {
                Ok(r) => {
                    let result = (l.clone() - r).solve_with_range_constraints(&range_constraint)?;
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
    ) -> Option<EvalValue<AlgebraicVariable<'a>, T>> {
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

fn report_identity_solving<T: FieldElement, K>(identity: &Identity<T>, result: &EvalResult<T, K>) {
    let success = result.as_ref().map(|r| r.is_complete()).unwrap_or_default() as u64;
    let mut stat = STATISTICS.lock().unwrap();
    stat.entry(identity.id())
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
