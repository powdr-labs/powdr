use std::{
    cell::RefCell,
    collections::{BTreeMap, HashMap},
};

use powdr_number::FieldElement;

use crate::witgen::{
    machines::{KnownMachine, Machine},
    rows::RowPair,
    EvalError, EvalResult, QueryCallback,
};

/// Everything [Generator] needs to mutate in order to compute a new row.
pub struct MutableState<'a, 'b, T: FieldElement, Q: QueryCallback<T>> {
    machines: Vec<RefCell<KnownMachine<'a, T>>>,
    identity_to_machine_index: BTreeMap<u64, usize>,
    query_callback: &'b Q,
}

impl<'a, 'b, T: FieldElement, Q: QueryCallback<T>> MutableState<'a, 'b, T, Q> {
    pub fn new(machines: impl Iterator<Item = KnownMachine<'a, T>>, query_callback: &'b Q) -> Self {
        let machines: Vec<_> = machines.map(RefCell::new).collect();
        let identity_to_machine_index = machines
            .iter()
            .enumerate()
            .flat_map(|(index, m)| {
                m.borrow()
                    .identity_ids()
                    .into_iter()
                    .map(move |id| (id, index))
            })
            .collect();
        Self {
            machines,
            identity_to_machine_index,
            query_callback,
        }
    }

    pub fn call(&self, identity_id: u64, caller_rows: &RowPair<'_, 'a, T>) -> EvalResult<'a, T> {
        let machine_index = *self
            .identity_to_machine_index
            .get(&identity_id)
            .unwrap_or_else(|| panic!("No executor machine matched identity ID: {identity_id}"));

        self.machines[machine_index]
            .try_borrow_mut()
            .map_err(|_| {
                EvalError::RecursiveMachineCalls(format!(
                    "Detected when processing identity with ID {identity_id}"
                ))
            })?
            .process_plookup_timed(self, identity_id, caller_rows)
    }

    pub fn take_witness_col_values(self) -> HashMap<String, Vec<T>> {
        // We keep the already processed machines mutably borrowed so that
        // "later" machines do not try to create new rows in already processed
        // machines.
        let mut processed = vec![];
        self.machines
            .iter()
            .flat_map(|machine| {
                let mut machine = machine
                    .try_borrow_mut()
                    .map_err(|_| {
                        panic!("Recursive machine dependencies while finishing machines.");
                    })
                    .unwrap();
                let columns = machine.take_witness_col_values(&self).into_iter();
                processed.push(machine);
                columns
            })
            .collect()
    }

    pub fn query_callback(&self) -> &Q {
        self.query_callback
    }
}
