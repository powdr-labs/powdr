use crate::plonk::air_to_plonkish::air_to_plonkish;
use crate::plonk::Gate;
use crate::plonk::PlonkCircuit;
use crate::{bus_interaction_handler, BusMap};
use bus_interaction_handler::BusType::{
    BitwiseLookup, ExecutionBridge, Memory, PcLookup, TupleRangeChecker, VariableRangeChecker,
};
use powdr_ast::analyzed::AlgebraicReference;
use powdr_autoprecompiles::SymbolicBusInteraction;
use powdr_number::FieldElement;

/// Allocates a bus interaction to the PLONK circuit.
/// The bus interaction is expected to be in the form:
/// bus_interaction(bus_type, [args0, args1, ...], mult)
/// witness allocation:
///        a         b         c
/// row 0: args0     args1     args2     
/// row 1: args3     ...       ...         
/// ...
fn add_bus_to_plonk_circuit<T>(
    bus_interaction: SymbolicBusInteraction<T>,
    temp_id_offset: &mut usize,
    plonk_circuit: &mut PlonkCircuit<T, AlgebraicReference>,
    bus_map: &BusMap,
) where
    T: FieldElement,
{
    let number_of_gates = ((bus_interaction.args.len() + 1) as f64 / 3.0).ceil() as usize;
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
            gates[0].q_rang_tuple = T::ONE;
        }

        _ => {
            unimplemented!("bus interaction type is not implemented");
        }
    }

    bus_interaction
        .args
        .iter()
        .chain([bus_interaction.mult].iter())
        .zip(
            gates
                .iter_mut()
                .flat_map(|gate| [&mut gate.a, &mut gate.b, &mut gate.c]),
        )
        .for_each(|(arg, payload)| {
            *payload = air_to_plonkish(arg, plonk_circuit, temp_id_offset, false);
        });

    // Add the gates to the circuit.
    for gate in gates {
        plonk_circuit.add_gate(gate);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bus_interaction_handler::DEFAULT_MEMORY;
    use crate::plonk::test_utils::{c, var};
    use powdr_ast::analyzed::AlgebraicExpression;
    use powdr_ast::analyzed::AlgebraicReference;
    use powdr_autoprecompiles::SymbolicBusInteraction;
    use powdr_number::BabyBearField;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_add_memory_bus_to_plonk_circuit() {
        let mut temp_id_offset = 0;
        let mut plonk_circuit = PlonkCircuit::<BabyBearField, AlgebraicReference>::new();
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

        add_bus_to_plonk_circuit(
            bus_interaction,
            &mut temp_id_offset,
            &mut plonk_circuit,
            &bus_map,
        );

        assert_eq!(
            format!("{plonk_circuit}"),
            //           a          b       c
            // memory 0: x + y      y       -x * y
            //        1: y * 5      x       y
            //        2: temp_4(1)  tmp_5(42)
            //        3: tmp_0 = 42
            "bus: none, 42 = tmp_0
bus: none, x + y = tmp_1
bus: none, x * y = tmp_3
bus: none, -tmp_3 = tmp_2
bus: none, 5 * y = tmp_4
bus: none, 1 = tmp_5
bus: memory, tmp_0, tmp_1, y
bus: none, tmp_2, tmp_4, x
bus: none, y, tmp_5, Unused
"
        )
    }
}
