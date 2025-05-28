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

// I might need to add a bus_interaction with its elements are Variables, check later.

fn add_bus_to_plonk_circuit<T>(
    bus_interaction: SymbolicBusInteraction<T>,
    temp_id_offset: &mut usize,
    plonk_circuit: &mut PlonkCircuit<T, AlgebraicReference>,
    bus_map: &BusMap,
) -> PlonkCircuit<T, AlgebraicReference>
where
    T: FieldElement,
{
    match bus_map.bus_type(bus_interaction.id) {
        Memory => {
            let mut bus_gate_1 = Gate::default();
            bus_gate_1.q_memory = T::ONE;

            let mut bus_gate_2 = Gate::default();

            let mut bus_gate_3 = Gate::default();

            let [as_bus, addr, val0, val1, val2, val3, timestamp]: [AlgebraicExpression<T>; 7] =
                bus_interaction
                    .args
                    .try_into()
                    .expect("Expected 7 arguments for Memory lookup");

            [addr, val0, val1, val2, val3, timestamp]
                .iter()
                .zip([
                    &mut bus_gate_1.a,
                    &mut bus_gate_1.b,
                    &mut bus_gate_1.c,
                    &mut bus_gate_2.a,
                    &mut bus_gate_2.b,
                    &mut bus_gate_2.c,
                ])
                .for_each(|(arg, payload)| {
                    if let AlgebraicExpression::Reference(r) = arg {
                        *payload = Variable::Witness(r.clone());
                    } else {
                        // If the argument is not a polynomial reference, add plonk gates based on the arg expression,
                        // put the temp of the last gate into the payload.
                        air_to_plonkish(&arg, plonk_circuit, temp_id_offset);
                        *payload = plonk_circuit.gates.last_mut().unwrap().c.clone();
                    }
                });

            match bus_interaction.mult {
                AlgebraicExpression::Number(n) => {
                    let mut gate = Gate::default();
                    gate.q_const = n;
                    gate.q_o = -T::ONE;
                    gate.c = Variable::Tmp(*temp_id_offset);
                    *temp_id_offset += 1;

                    bus_gate_3.a = gate.c.clone();
                    plonk_circuit.add_gate(gate);
                }
                AlgebraicExpression::Reference(r) => {
                    bus_gate_3.a = Variable::Witness(r.clone());
                }
                _ => {
                    air_to_plonkish(&bus_interaction.mult, plonk_circuit, temp_id_offset);
                    bus_gate_3.a= plonk_circuit.gates.last_mut().unwrap().c.clone();
                }
            }

            assert!(
                matches!(as_bus, AlgebraicExpression::Number(_)),
                "Memory lookup payload as should be a number"
            );

            plonk_circuit.add_gate(bus_gate_1);
            plonk_circuit.add_gate(bus_gate_2);
            plonk_circuit.add_gate(bus_gate_3);
        }
        BitwiseLookup => {
            let mut bus_gate = Gate::default();
            bus_gate.q_bitwise = T::ONE;

            let [x, y, z, op]: [AlgebraicExpression<T>; 4] = bus_interaction
                .args
                .try_into()
                .expect("Expected 4 arguments for bitwise lookup");

            [x, y, z]
                .iter()
                .zip([&mut bus_gate.a, &mut bus_gate.b, &mut bus_gate.c])
                .for_each(|(arg, payload)| {
                    if let AlgebraicExpression::Reference(r) = arg {
                        *payload = Variable::Witness(r.clone());
                    } else {
                        // If the argument is not a polynomial reference, add plonk gates based on the arg expression,
                        // put the temp of the last gate into the payload.
                        air_to_plonkish(&arg, plonk_circuit, temp_id_offset);
                        *payload = plonk_circuit.gates.last_mut().unwrap().c.clone();
                    }
                });
            // TODO: add gate for these constants
            assert!(
                matches!(bus_interaction.mult, AlgebraicExpression::Number(_)),
                "BitwiseLookup multiplicity should be a number"
            );

            assert!(
                matches!(op, AlgebraicExpression::Number(_)),
                "BitwiseLookup op payload should be a number"
            );
            plonk_circuit.add_gate(bus_gate);
        }
        ExecutionBridge => {
            todo!();
        }
        PcLookup => {
            todo!();
        }
        VariableRangeChecker => {
             todo!();
        }
        TupleRangeChecker => {
            todo!();
        }

        _ => {
            unimplemented!("bus interaction type is not implemented");
        }
    }

    // build gates for multiplicity

    unimplemented!("Adding bus interaction to Plonk circuit is not fully implemented yet")
}
