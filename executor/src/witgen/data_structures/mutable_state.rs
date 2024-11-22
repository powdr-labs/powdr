use std::collections::{BTreeMap, HashMap};

use powdr_number::FieldElement;

use crate::witgen::{
    machines::{KnownMachine, Machine},
    rows::RowPair,
    EvalResult, QueryCallback,
};

/// Everything [Generator] needs to mutate in order to compute a new row.
pub struct MutableState<'a, 'b, T: FieldElement, Q: QueryCallback<T>> {
    pub machines: Machines<'a, 'b, T>,
    pub query_callback: &'b mut Q,
}

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
