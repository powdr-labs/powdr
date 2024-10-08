use std::collections::HashMap;

use super::{Machine, MachineParts};
use crate::witgen::rows::RowPair;
use crate::witgen::{EvalResult, FixedData, MutableState, QueryCallback};
use powdr_number::{FieldElement, KnownField};

use crate::witgen::machines::double_sorted_witness_machine_16;
use crate::witgen::machines::double_sorted_witness_machine_32;

pub enum DoubleSortedWitnesses<'a, T: FieldElement> {
    DoubleSortedWitnesses16(double_sorted_witness_machine_16::DoubleSortedWitnesses<'a, T>),
    DoubleSortedWitnesses32(double_sorted_witness_machine_32::DoubleSortedWitnesses<'a, T>),
}

impl<'a, T: FieldElement> DoubleSortedWitnesses<'a, T> {
    pub fn try_new(
        name: String,
        fixed_data: &'a FixedData<'a, T>,
        parts: &MachineParts<'a, T>,
    ) -> Option<Self> {
        match T::known_field().unwrap() {
            KnownField::BabyBearField | KnownField::Mersenne31Field => {
                let machine = double_sorted_witness_machine_16::DoubleSortedWitnesses::try_new(
                    name.clone(),
                    fixed_data,
                    parts,
                );
                machine.map(DoubleSortedWitnesses::DoubleSortedWitnesses16)
            }
            KnownField::GoldilocksField | KnownField::Bn254Field => {
                let machine = double_sorted_witness_machine_32::DoubleSortedWitnesses::try_new(
                    name.clone(),
                    fixed_data,
                    parts,
                );
                machine.map(DoubleSortedWitnesses::DoubleSortedWitnesses32)
            }
        }
    }
}

impl<'a, T: FieldElement> Machine<'a, T> for DoubleSortedWitnesses<'a, T> {
    fn identity_ids(&self) -> Vec<u64> {
        match self {
            DoubleSortedWitnesses::DoubleSortedWitnesses16(machine) => machine.identity_ids(),
            DoubleSortedWitnesses::DoubleSortedWitnesses32(machine) => machine.identity_ids(),
        }
    }

    fn name(&self) -> &str {
        match self {
            DoubleSortedWitnesses::DoubleSortedWitnesses16(machine) => machine.name(),
            DoubleSortedWitnesses::DoubleSortedWitnesses32(machine) => machine.name(),
        }
    }

    fn process_plookup<Q: QueryCallback<T>>(
        &mut self,
        _mutable_state: &mut MutableState<'a, '_, T, Q>,
        identity_id: u64,
        caller_rows: &RowPair<'_, 'a, T>,
    ) -> EvalResult<'a, T> {
        match self {
            DoubleSortedWitnesses::DoubleSortedWitnesses16(machine) => {
                machine.process_plookup_internal(identity_id, caller_rows)
            }
            DoubleSortedWitnesses::DoubleSortedWitnesses32(machine) => {
                machine.process_plookup_internal(identity_id, caller_rows)
            }
        }
    }

    fn take_witness_col_values<'b, Q: QueryCallback<T>>(
        &mut self,
        _mutable_state: &'b mut MutableState<'a, 'b, T, Q>,
    ) -> HashMap<String, Vec<T>> {
        match self {
            DoubleSortedWitnesses::DoubleSortedWitnesses16(machine) => {
                machine.take_witness_col_values(_mutable_state)
            }
            DoubleSortedWitnesses::DoubleSortedWitnesses32(machine) => {
                machine.take_witness_col_values(_mutable_state)
            }
        }
    }
}
