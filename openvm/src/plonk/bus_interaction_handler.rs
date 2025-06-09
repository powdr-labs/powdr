use crate::plonk::Gate;
use crate::{bus_interaction_handler, BusMap};
use bus_interaction_handler::BusType::{
    BitwiseLookup, ExecutionBridge, Memory, PcLookup, TupleRangeChecker, VariableRangeChecker,
};
use powdr_autoprecompiles::legacy_expression::AlgebraicReference;
use powdr_autoprecompiles::SymbolicBusInteraction;
use powdr_number::FieldElement;

use super::air_to_plonkish::CircuitBuilder;

/// Allocates a bus interaction to the PLONK circuit.
/// The bus interaction is expected to be in the form:
/// bus_interaction(bus_type, [args0, args1, ...], mult)
/// witness allocation:
///        a         b         c
/// row 0: args0     args1     args2     
/// row 1: args3     ...       ...         
/// ...
pub fn add_bus_to_plonk_circuit<T>(
    bus_interaction: SymbolicBusInteraction<T>,
    circuit_builder: &mut CircuitBuilder<T>,
    bus_map: &BusMap,
) where
    T: FieldElement,
{
    // There are 5 witness cell per gate to store the bus arguments, therefore divide the number of arguments by 5 to get the number of gates needed.
    let number_of_gates = (bus_interaction.args.len() as u32).div_ceil(5) as usize;
    let mut gates: Vec<Gate<T, AlgebraicReference>> =
        (0..number_of_gates).map(|_| Gate::default()).collect();
    match bus_map.bus_type(bus_interaction.id) {
        Memory => {
            gates[0].q_memory = T::ONE;
        }
        BitwiseLookup => {
            gates[0].q_bitwise = T::ONE;
        }
        ExecutionBridge => {
            gates[0].q_execution = T::ONE;
        }
        PcLookup => {
            gates[0].q_pc = T::ONE;
        }
        VariableRangeChecker => {
            gates[0].q_range_check = T::ONE;
        }
        TupleRangeChecker => {
            gates[0].q_range_tuple = T::ONE;
        }

        _ => {
            unimplemented!("bus interaction type is not implemented");
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
    use crate::bus_interaction_handler::DEFAULT_MEMORY;
    use crate::plonk::test_utils::{c, var};
    use powdr_autoprecompiles::legacy_expression::AlgebraicExpression;
    use powdr_autoprecompiles::SymbolicBusInteraction;
    use powdr_number::BabyBearField;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_add_memory_bus_to_plonk_circuit() {
        let bus_map = BusMap::openvm_base();

        let x = var("x", 0);
        let y = var("y", 1);

        let bus_interaction = SymbolicBusInteraction {
            id: DEFAULT_MEMORY,
            args: vec![
                AlgebraicExpression::Number(BabyBearField::from(42)),
                x.clone() + y.clone(),
                y.clone(),
                -(x.clone() * y.clone()),
                y.clone() * c(5),
                x.clone(),
                y.clone(),
            ],
            mult: AlgebraicExpression::Number(BabyBearField::from(1)),
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
