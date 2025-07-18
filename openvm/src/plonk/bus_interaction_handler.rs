use crate::plonk::{Gate, NUMBER_OF_WITNESS_COLS};
use openvm_stark_backend::p3_field::PrimeField32;
use powdr_autoprecompiles::bus_map::{
    BusMap,
    BusType::{
        BitwiseLookup, ExecutionBridge, Memory, PcLookup, TupleRangeChecker, VariableRangeChecker,
    },
};
use powdr_autoprecompiles::expression::AlgebraicReference;
use powdr_autoprecompiles::SymbolicBusInteraction;

use super::air_to_plonkish::CircuitBuilder;

/// Allocates a bus interaction to the PLONK circuit.
/// The bus interaction is expected to be in the form:
/// bus_interaction(bus_type, [args0, args1, ...], mult)
/// witness allocation:
///        a         b         c
/// row 0: args0     args1     args2     
/// row 1: args3     ...       ...         
/// ...
pub fn add_bus_to_plonk_circuit<F: PrimeField32>(
    bus_interaction: SymbolicBusInteraction<F>,
    circuit_builder: &mut CircuitBuilder<F>,
    bus_map: &BusMap,
) {
    let number_of_gates =
        (bus_interaction.args.len() as u32).div_ceil(NUMBER_OF_WITNESS_COLS as u32) as usize;
    let mut gates: Vec<Gate<F, AlgebraicReference>> =
        (0..number_of_gates).map(|_| Gate::default()).collect();
    match bus_map.bus_type(bus_interaction.id) {
        Memory => {
            gates[0].q_memory = F::ONE;
        }
        BitwiseLookup => {
            gates[0].q_bitwise = F::ONE;
        }
        ExecutionBridge => {
            gates[0].q_execution = F::ONE;
        }
        PcLookup => {
            gates[0].q_pc = F::ONE;
        }
        VariableRangeChecker => {
            gates[0].q_range_check = F::ONE;
        }
        TupleRangeChecker => {
            gates[0].q_range_tuple = F::ONE;
        }
    }

    bus_interaction
        .args
        .iter()
        .chain([bus_interaction.mult].iter())
        .zip(gates.iter_mut().flat_map(|gate| {
            [
                &mut gate.a,
                &mut gate.b,
                &mut gate.c,
                &mut gate.d,
                &mut gate.e,
            ]
        }))
        .for_each(|(arg, payload)| {
            *payload = circuit_builder.evaluate_expression(arg, false);
        });

    // Add the gates to the circuit.
    for gate in gates {
        circuit_builder.add_gate(gate);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bus_map::{default_openvm_bus_map, DEFAULT_MEMORY};
    use crate::plonk::test_utils::{c, var};
    use openvm_stark_backend::p3_field::FieldAlgebra;
    use openvm_stark_sdk::p3_baby_bear::BabyBear;
    use powdr_autoprecompiles::expression::AlgebraicExpression;
    use powdr_autoprecompiles::SymbolicBusInteraction;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_add_memory_bus_to_plonk_circuit() {
        let bus_map = default_openvm_bus_map();

        let x = var("x", 0);
        let y = var("y", 1);

        let bus_interaction = SymbolicBusInteraction {
            id: DEFAULT_MEMORY,
            args: vec![
                AlgebraicExpression::Number(BabyBear::from_canonical_u32(42)),
                x.clone() + y.clone(),
                y.clone(),
                -(x.clone() * y.clone()),
                y.clone() * c(5),
                x.clone(),
                y.clone(),
            ],
            mult: AlgebraicExpression::Number(BabyBear::from_canonical_u32(1)),
        };

        let mut circuit_builder = CircuitBuilder::new();
        add_bus_to_plonk_circuit(bus_interaction, &mut circuit_builder, &bus_map);
        let plonk_circuit = circuit_builder.build();

        assert_eq!(
            format!("{plonk_circuit}"),
            "bus: none, 42 = tmp_0
bus: none, x + y = tmp_1
bus: none, x * y = tmp_3
bus: none, -tmp_3 = tmp_2
bus: none, 5 * y = tmp_4
bus: none, 1 = tmp_5
bus: memory, tmp_0, tmp_1, y, tmp_2, tmp_4
bus: none, x, y, tmp_5, Unused, Unused
"
        )
    }
}
