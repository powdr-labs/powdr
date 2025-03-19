use std::{
    cell::{RefCell, RefMut},
    collections::{BTreeMap, HashMap},
};

use bit_vec::BitVec;
use powdr_number::FieldElement;

use crate::witgen::{
    global_constraints::RangeConstraintSet,
    machines::{KnownMachine, LookupCell, Machine},
    range_constraints::RangeConstraint,
    AffineExpression, AlgebraicVariable, EvalError, EvalResult, QueryCallback,
};

/// The container and access method for machines and the query callback.
/// The machines contain the actual data tables.
/// This struct uses interior mutability for accessing the machines.
pub struct MutableState<'a, T: FieldElement, Q: QueryCallback<T>> {
    machines: Vec<RefCell<KnownMachine<'a, T>>>,
    bus_to_machine_index: BTreeMap<T, usize>,
    query_callback: &'a Q,
}

impl<'a, T: FieldElement, Q: QueryCallback<T>> MutableState<'a, T, Q> {
    pub fn new(machines: impl Iterator<Item = KnownMachine<'a, T>>, query_callback: &'a Q) -> Self {
        let machines: Vec<_> = machines.map(RefCell::new).collect();
        let bus_to_machine_index = machines
            .iter()
            .enumerate()
            .flat_map(|(index, m)| m.borrow().bus_ids().into_iter().map(move |id| (id, index)))
            .collect();
        Self {
            machines,
            bus_to_machine_index,
            query_callback,
        }
    }

    /// Runs the first machine (unless there are no machines) end returns the generated columns.
    /// The first machine might call other machines, which is handled automatically.
    pub fn run(self) -> (HashMap<String, Vec<T>>, BTreeMap<String, T>) {
        if let Some(first_machine) = self.machines.first() {
            first_machine.try_borrow_mut().unwrap().run_timed(&self);
        }
        self.take_witness_col_and_public_values()
    }

    pub fn can_process_call_fully(
        &self,
        bus_id: T,
        known_inputs: &BitVec,
        range_constraints: Vec<RangeConstraint<T>>,
    ) -> (bool, Vec<RangeConstraint<T>>) {
        let mut machine = self.responsible_machine(bus_id).ok().unwrap();
        machine.can_process_call_fully(self, bus_id, known_inputs, range_constraints)
    }

    /// Call the machine responsible for the right-hand-side of an identity given its ID,
    /// the evaluated arguments and the caller's range constraints.
    pub fn call(
        &self,
        bus_id: T,
        arguments: &[AffineExpression<AlgebraicVariable<'a>, T>],
        range_constraints: &dyn RangeConstraintSet<AlgebraicVariable<'a>, T>,
    ) -> EvalResult<'a, T> {
        self.responsible_machine(bus_id)?.process_plookup_timed(
            self,
            bus_id,
            arguments,
            range_constraints,
        )
    }

    /// Call the machine responsible for the right-hand-side of an identity given its ID,
    /// use the direct interface.
    pub fn call_direct(
        &self,
        bus_id: T,
        values: &mut [LookupCell<'_, T>],
    ) -> Result<bool, EvalError<T>> {
        self.responsible_machine(bus_id)?
            .process_lookup_direct_timed(self, bus_id, values)
    }

    fn responsible_machine(&self, bus_id: T) -> Result<RefMut<KnownMachine<'a, T>>, EvalError<T>> {
        let machine_index = *self
            .bus_to_machine_index
            .get(&bus_id)
            .unwrap_or_else(|| panic!("No executor machine matched identity ID: {bus_id}"));
        self.machines[machine_index].try_borrow_mut().map_err(|_| {
            EvalError::RecursiveMachineCalls(format!(
                "Detected when processing machine call with bus ID {bus_id}"
            ))
        })
    }

    /// Extracts the witness column values from the machines.
    fn take_witness_col_and_public_values(self) -> (HashMap<String, Vec<T>>, BTreeMap<String, T>) {
        // We keep the already processed machines mutably borrowed so that
        // "later" machines do not try to create new rows in already processed
        // machines.
        let (witness_columns, public_values, _processed) = self.machines.iter().fold(
            (HashMap::new(), BTreeMap::new(), Vec::new()),
            |(mut acc_witness, mut acc_public, mut processed), machine_cell| {
                let mut machine = machine_cell.try_borrow_mut().unwrap_or_else(|_| {
                    panic!("Recursive machine dependencies while finishing machines.")
                });

                // Merge witness columns: each machine provides a HashMap<String, Vec<T>>
                machine.take_witness_col_values(&self).into_iter().for_each(
                    |(key, mut vec_vals)| {
                        acc_witness
                            .entry(key)
                            .or_insert_with(Vec::new)
                            .append(&mut vec_vals);
                    },
                );

                // Merge public values: each machine provides a BTreeMap<String, T>
                machine
                    .take_public_values()
                    .into_iter()
                    .for_each(|(key, value)| {
                        acc_public.insert(key, value);
                    });

                processed.push(machine);
                (acc_witness, acc_public, processed)
            },
        );

        (witness_columns, public_values)
    }

    pub fn query_callback(&self) -> &Q {
        self.query_callback
    }
}
