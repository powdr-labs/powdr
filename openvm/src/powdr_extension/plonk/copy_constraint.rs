use crate::plonk::PlonkCircuit;
use crate::plonk::{Variable, NUMBER_OF_WITNESS_COLS};
use itertools::Itertools;
use openvm_stark_backend::p3_field::{FieldAlgebra, PrimeField32};
use std::borrow::BorrowMut;
use std::collections::HashMap;

use crate::powdr_extension::plonk::air::PlonkColumns;
use powdr_autoprecompiles::expression::AlgebraicReference;

pub fn generate_permutation_columns<F>(
    values: &mut [F],
    plonk_circuit: &PlonkCircuit<F, AlgebraicReference>,
    number_of_calls: usize,
    width: usize,
) where
    F: FieldAlgebra + PrimeField32,
{
    let mut permutation_sets_by_variable: HashMap<&Variable<AlgebraicReference>, Vec<u64>> =
        plonk_circuit
            .gates()
            .iter()
            .flat_map(|gate| [&gate.a, &gate.b, &gate.c, &gate.d, &gate.e])
            .filter(|var| !matches!(var, Variable::Unused))
            .unique()
            .map(|variable| (variable, vec![]))
            .collect::<HashMap<_, _>>();
    for (gate_index, gate) in plonk_circuit.gates().iter().enumerate() {
        for (i, variable) in [&gate.a, &gate.b, &gate.c, &gate.d, &gate.e]
            .into_iter()
            .enumerate()
        {
            if let Some(vec) = permutation_sets_by_variable.get_mut(variable) {
                vec.push(NUMBER_OF_WITNESS_COLS * gate_index as u64 + i as u64);
            }
        }
    }
    permutation_sets_by_variable.retain(|_, v| v.len() > 1);

    for call_index in 0..number_of_calls {
        for (gate_index, gate) in plonk_circuit.gates().iter().enumerate() {
            let index = call_index * plonk_circuit.len() + gate_index;
            let columns: &mut PlonkColumns<_> =
                values[index * width..(index + 1) * width].borrow_mut();

            // Initializing fixed columns for copy constraints
            columns.a_id = F::from_canonical_u64(NUMBER_OF_WITNESS_COLS * index as u64);
            columns.b_id = F::from_canonical_u64(NUMBER_OF_WITNESS_COLS * index as u64 + 1);
            columns.c_id = F::from_canonical_u64(NUMBER_OF_WITNESS_COLS * index as u64 + 2);
            columns.d_id = F::from_canonical_u64(NUMBER_OF_WITNESS_COLS * index as u64 + 3);
            columns.e_id = F::from_canonical_u64(NUMBER_OF_WITNESS_COLS * index as u64 + 4);
            // Initializing permutation columns the same way as id columns
            columns.a_perm = F::from_canonical_u64(NUMBER_OF_WITNESS_COLS * index as u64);
            columns.b_perm = F::from_canonical_u64(NUMBER_OF_WITNESS_COLS * index as u64 + 1);
            columns.c_perm = F::from_canonical_u64(NUMBER_OF_WITNESS_COLS * index as u64 + 2);
            columns.d_perm = F::from_canonical_u64(NUMBER_OF_WITNESS_COLS * index as u64 + 3);
            columns.e_perm = F::from_canonical_u64(NUMBER_OF_WITNESS_COLS * index as u64 + 4);

            for (witness_in_gate, witness_perm_col) in [
                (&gate.a, &mut columns.a_perm),
                (&gate.b, &mut columns.b_perm),
                (&gate.c, &mut columns.c_perm),
                (&gate.d, &mut columns.d_perm),
                (&gate.e, &mut columns.e_perm),
            ] {
                if let Some(vec) = permutation_sets_by_variable.get(witness_in_gate) {
                    // If the poly has a permutation set, set a_perm to the next value in the set.
                    if let Some(pos) = vec.iter().position(|x| {
                        *x == witness_perm_col.as_canonical_u64()
                            - (call_index * plonk_circuit.len() * NUMBER_OF_WITNESS_COLS as usize)
                                as u64
                    }) {
                        let next_pos = (pos + 1) % vec.len(); // wraps to 0 if at the end
                        *witness_perm_col = F::from_canonical_u64(
                            vec[next_pos]
                                + (call_index
                                    * plonk_circuit.len()
                                    * NUMBER_OF_WITNESS_COLS as usize)
                                    as u64,
                        );
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::bus_map::default_openvm_bus_map;
    use crate::plonk::air_to_plonkish::build_circuit;
    use crate::plonk::test_utils::var;
    use crate::powdr_extension::plonk::copy_constraint::generate_permutation_columns;
    use crate::powdr_extension::plonk::copy_constraint::FieldAlgebra;
    use crate::powdr_extension::plonk::copy_constraint::PrimeField32;
    use openvm_stark_sdk::p3_baby_bear::BabyBear;
    use powdr_autoprecompiles::{SymbolicConstraint, SymbolicMachine};
    use pretty_assertions::assert_eq;

    #[test]
    fn test_generate_perm_columns()
    where
        BabyBear: PrimeField32 + FieldAlgebra,
    {
        type F = BabyBear;
        let x = var("x", 0);
        let y = var("y", 1);
        let z = var("z", 2);
        let bus_map = default_openvm_bus_map();

        let expr = -(x.clone() * y.clone() - (x.clone() + y.clone())) * z.clone();
        let machine = SymbolicMachine {
            constraints: vec![SymbolicConstraint { expr }],
            bus_interactions: vec![],
        };

        let plonk_circuit = build_circuit(&machine, &bus_map);
        let width = 26;
        let number_of_calls = 2;
        let height = number_of_calls * plonk_circuit.len();
        let mut values = F::zero_vec(height * width);

        println!("circuit: {}", build_circuit(&machine, &bus_map));

        generate_permutation_columns::<F>(&mut values, &plonk_circuit, number_of_calls, width);

        let expected: Vec<Vec<i32>> = vec![
            // First call
            vec![0, 1, 2, 3, 4, 5, 6, 10, 3, 4],
            vec![5, 6, 7, 8, 9, 0, 1, 11, 8, 9],
            vec![10, 11, 12, 13, 14, 2, 7, 15, 13, 14],
            vec![15, 16, 17, 18, 19, 12, 16, 20, 18, 19],
            vec![20, 21, 22, 23, 24, 17, 21, 22, 23, 24],
            // Second call
            vec![25, 26, 27, 28, 29, 30, 31, 35, 28, 29],
            vec![30, 31, 32, 33, 34, 25, 26, 36, 33, 34],
            vec![35, 36, 37, 38, 39, 27, 32, 40, 38, 39],
            vec![40, 41, 42, 43, 44, 37, 41, 45, 43, 44],
            vec![45, 46, 47, 48, 49, 42, 46, 47, 48, 49],
        ];

        // Get the values corresponding to a_id, b_id, c_id, d_id, e_id, a_perm, b_perm, c_perm, d_perm, e_perm
        for (row, vec) in expected.iter().enumerate().take(height) {
            let start = row * width + 11;
            let end = row * width + 21;
            let actual: Vec<i32> = values[start..end]
                .iter()
                .map(|v| v.as_canonical_u32() as i32)
                .collect();

            assert_eq!(actual, *vec, "Mismatch at row {}: {:?}", row, actual);
        }

        assert_eq!(
            format!("{}", build_circuit(&machine, &bus_map)),
            "bus: none, x * y = tmp_2
bus: none, x + y = tmp_3
bus: none, tmp_2 + -tmp_3 = tmp_1
bus: none, -tmp_1 = tmp_0
bus: none, tmp_0 * z = 0
"
        );
    }
}
