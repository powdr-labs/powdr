use crate::plonk::air_to_plonkish::air_to_plonkish;
use crate::plonk::Gate;
use crate::plonk::{PlonkCircuit, Variable};
use crate::{bus_interaction_handler, BusMap};
use bus_interaction_handler::BusType::{
    BitwiseLookup, ExecutionBridge, Memory, PcLookup, TupleRangeChecker, VariableRangeChecker,
};
use powdr_ast::analyzed::{AlgebraicExpression, AlgebraicReference};
use powdr_autoprecompiles::SymbolicBusInteraction;
use powdr_number::FieldElement;

fn add_bus_to_plonk_circuit<T>(
    bus_interaction: SymbolicBusInteraction<T>,
    temp_id_offset: &mut usize,
    plonk_circuit: &mut PlonkCircuit<T, AlgebraicReference>,
    bus_map: &BusMap,
) -> PlonkCircuit<T, AlgebraicReference>
where
    T: FieldElement,
{
    // build gates for multiplicity
    let mut multiplicity_number = T::ZERO;

    let mut bus_variables = Vec::new();

    match bus_interaction.mult {
        AlgebraicExpression::Number(n) => multiplicity_number = n,
        AlgebraicExpression::Reference(r) => {
            bus_variables.push(Variable::Witness(r));
        }
        _ => {
            // If the argument is not a polynomial reference, add plonk gates based on the arg expression,
            // put the temp of the last gate into the payloads_variables.
            let expr = air_to_plonkish(&bus_interaction.mult, plonk_circuit, temp_id_offset);
            bus_variables.push(plonk_circuit.gates.last_mut().unwrap().c.clone());
        }
    }

    // build gates for arguments
    for arg in bus_interaction.args {
        if let AlgebraicExpression::Reference(r) = arg {
            bus_variables.push(Variable::Witness(r));
        } else {
            let expr = air_to_plonkish(&arg, plonk_circuit, temp_id_offset);
            bus_variables.push(plonk_circuit.gates.last_mut().unwrap().c.clone());
        }
    }

    match bus_map.bus_type(bus_interaction.id) {
        Memory => {
            // Handle Memory bus interaction
            unimplemented!("Memory bus interaction is not implemented yet");
        }
        BitwiseLookup => {
            assert_eq!(
                bus_variables.len(),
                4,
                "Arguments number mismatch for BitwiseLookup"
            );
            let [x, y, z, op] = bus_variables.try_into().unwrap();
            plonk_circuit.add_gate(Gate {
                q_l: T::ZERO,
                q_r: T::ZERO,
                q_o: T::ZERO,
                q_mul: T::ZERO,
                q_const: T::ZERO,

                q_bitwise: T::ONE,
                q_memory: T::ZERO,
                q_execution: T::ZERO,
                q_pc: T::ZERO,
                q_rang_tuple: T::ZERO,
                q_range_check: T::ZERO,

                a: x,
                b: y,
                c: z,
            });
        }
        ExecutionBridge => {
            // Handle Execution bus interaction
            unimplemented!("Execution bus interaction is not implemented yet");
        }
        PcLookup => {
            // Handle other types of bus interactions
            unimplemented!("Other bus interaction types are not implemented yet");
        }
        VariableRangeChecker => {
            // Handle VariableRangeChecker bus interaction
            unimplemented!("VariableRangeChecker bus interaction is not implemented yet");
        }
        TupleRangeChecker => {
            // Handle TupleRangeChecker bus interaction
            unimplemented!("TupleRangeChecker bus interaction is not implemented yet");
        }

        _ => {
            unimplemented!("bus interaction type is not implemented");
        }
    }

    // build gates for multiplicity

    unimplemented!("Adding bus interaction to Plonk circuit is not fully implemented yet")
}
