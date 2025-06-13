use crate::plonk::PlonkCircuit;
use crate::plonk::{Gate, Variable, NUMBER_OF_WITNESS_COLS};
use openvm_stark_backend::p3_field::{FieldAlgebra, PrimeField64};
use std::borrow::BorrowMut;
use std::collections::HashMap;

use crate::powdr_extension::plonk::air::PlonkColumns;
use openvm_stark_backend::{
    config::{StarkGenericConfig, Val},
    p3_field::PrimeField32,
};
use powdr_autoprecompiles::legacy_expression::AlgebraicReference;
pub fn generate_permutation_columns<SC, P>(
    values: &mut [Val<SC>],
    plonk_circuit: &PlonkCircuit<P, AlgebraicReference>,
    permutation_sets_by_variable: &HashMap<&Variable<AlgebraicReference>, Vec<u64>>,
    number_of_calls: usize,
    width: usize,
) -> ()
where
    SC: StarkGenericConfig,
    Val<SC>: FieldAlgebra + PrimeField32,
{
    for call_index in 0..number_of_calls {
        for (gate_index, gate) in plonk_circuit.gates().iter().enumerate() {
            let index = call_index * plonk_circuit.len() + gate_index;
            let columns: &mut PlonkColumns<_> =
                values[index * width..(index + 1) * width].borrow_mut();

            // Initializing fixed columns for copy constraints
            columns.a_id = <Val<SC>>::from_canonical_u64(NUMBER_OF_WITNESS_COLS * index as u64);
            columns.b_id = <Val<SC>>::from_canonical_u64(NUMBER_OF_WITNESS_COLS * index as u64 + 1);
            columns.c_id = <Val<SC>>::from_canonical_u64(NUMBER_OF_WITNESS_COLS * index as u64 + 2);
            columns.d_id = <Val<SC>>::from_canonical_u64(NUMBER_OF_WITNESS_COLS * index as u64 + 3);
            columns.e_id = <Val<SC>>::from_canonical_u64(NUMBER_OF_WITNESS_COLS * index as u64 + 4);
            // Initializing permutation columns the same way as id columns
            columns.a_perm = <Val<SC>>::from_canonical_u64(NUMBER_OF_WITNESS_COLS * index as u64);
            columns.b_perm =
                <Val<SC>>::from_canonical_u64(NUMBER_OF_WITNESS_COLS * index as u64 + 1);
            columns.c_perm =
                <Val<SC>>::from_canonical_u64(NUMBER_OF_WITNESS_COLS * index as u64 + 2);
            columns.d_perm =
                <Val<SC>>::from_canonical_u64(NUMBER_OF_WITNESS_COLS * index as u64 + 3);
            columns.e_perm =
                <Val<SC>>::from_canonical_u64(NUMBER_OF_WITNESS_COLS * index as u64 + 4);

            for (witness_in_gate, witness_col, witness_perm_col) in [
                (&gate.a, &mut columns.a, &mut columns.a_perm),
                (&gate.b, &mut columns.b, &mut columns.b_perm),
                (&gate.c, &mut columns.c, &mut columns.c_perm),
                (&gate.d, &mut columns.d, &mut columns.d_perm),
                (&gate.e, &mut columns.e, &mut columns.e_perm),
            ] {
                if let Some(vec) = permutation_sets_by_variable.get(witness_in_gate) {
                    // If the poly has a permutation set, set a_perm to the next value in the set.
                    if let Some(pos) = vec.iter().position(|x| {
                        *x == witness_perm_col.as_canonical_u64()
                            - (call_index * plonk_circuit.len()) as u64
                    }) {
                        let next_pos = (pos + 1) % vec.len(); // wraps to 0 if at the end
                        *witness_perm_col = <Val<SC>>::from_canonical_u64(
                            vec[next_pos] + (call_index * plonk_circuit.len()) as u64,
                        );
                    }
                }
            }
        }
    }
}
