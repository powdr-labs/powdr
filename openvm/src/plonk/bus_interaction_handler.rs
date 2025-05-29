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
) where
    T: FieldElement,
{
    match bus_map.bus_type(bus_interaction.id) {
        Memory => {
            // Memory bus interaction:
            // bus_interaction(MEMORY, [address_space, addr, val0, val1, val2, val3, timestamp], mult));
            // witness allocation:
            //        a         b        c
            // row 0: addr      val0     val1
            // row 1: val2      val3     timestamp
            // row 2: mult      temp
            // row 3: temp = address_space (const)

            let mut bus_gate_0 = Gate::<T, AlgebraicReference> {
                q_memory: T::ONE,
                ..Default::default()
            };

            let mut bus_gate_1 = Gate::default();

            let mut bus_gate_2 = Gate::default();

            let [address_space, addr, val0, val1, val2, val3, timestamp]: [AlgebraicExpression<T>;
                7] = bus_interaction
                .args
                .try_into()
                .expect("Expected 7 arguments for Memory lookup");

            [addr, val0, val1, val2, val3, timestamp]
                .iter()
                .zip([
                    &mut bus_gate_0.a,
                    &mut bus_gate_0.b,
                    &mut bus_gate_0.c,
                    &mut bus_gate_1.a,
                    &mut bus_gate_1.b,
                    &mut bus_gate_1.c,
                ])
                .for_each(|(arg, payload)| {
                    match arg {
                        AlgebraicExpression::Reference(r) => {
                            *payload = Variable::Witness(r.clone());
                        }
                        AlgebraicExpression::Number(n) => {
                            panic!(
                                "Expected argument to be a polynomial reference, got number: {n}",
                            );
                        }
                        expr => {
                            // If the argument is not a polynomial reference, add plonk gates based on the arg expression,
                            // put the temp of the last gate into the payload.
                            air_to_plonkish(expr, plonk_circuit, temp_id_offset);
                            *payload = plonk_circuit.gates.last_mut().unwrap().c.clone();
                        }
                    }
                });

            match bus_interaction.mult {
                AlgebraicExpression::Number(n) => {
                    let gate = Gate::<T, AlgebraicReference> {
                        q_const: n,
                        q_o: -T::ONE,
                        c: Variable::Tmp(*temp_id_offset),
                        ..Default::default()
                    };

                    *temp_id_offset += 1;

                    bus_gate_2.a = gate.c.clone();
                    plonk_circuit.add_gate(gate);
                }
                AlgebraicExpression::Reference(r) => {
                    bus_gate_2.a = Variable::Witness(r.clone());
                }
                _ => {
                    air_to_plonkish(&bus_interaction.mult, plonk_circuit, temp_id_offset);
                    bus_gate_2.a = plonk_circuit.gates.last_mut().unwrap().c.clone();
                }
            }

            // add gate for address_space
            let mut bus_gate_3 = Gate::default();
            if let AlgebraicExpression::Number(n) = address_space {
                bus_gate_3.q_const = n;
                bus_gate_3.q_o = -T::ONE;
                bus_gate_3.c = Variable::Tmp(*temp_id_offset);
                *temp_id_offset += 1;
                bus_gate_2.b = bus_gate_3.c.clone();
            } else {
                panic!("Expected address_space to be a number");
            }

            // gates added in sequence to be consistant with next, next next reference
            plonk_circuit.add_gate(bus_gate_0);
            plonk_circuit.add_gate(bus_gate_1);
            plonk_circuit.add_gate(bus_gate_2);
            plonk_circuit.add_gate(bus_gate_3);
        }
        BitwiseLookup => {
            todo!();
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
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bus_interaction_handler::DEFAULT_MEMORY;
    use powdr_ast::analyzed::AlgebraicExpression;
    use powdr_ast::analyzed::AlgebraicReference;
    use powdr_ast::analyzed::PolyID;
    use powdr_ast::analyzed::PolynomialType;
    use powdr_autoprecompiles::SymbolicBusInteraction;
    use powdr_number::BabyBearField;

    fn var(name: &str, id: u64) -> AlgebraicExpression<BabyBearField> {
        AlgebraicExpression::Reference(AlgebraicReference {
            name: name.into(),
            poly_id: PolyID {
                id,
                ptype: PolynomialType::Committed,
            },
            next: false,
        })
    }

    fn c(value: u64) -> AlgebraicExpression<BabyBearField> {
        AlgebraicExpression::Number(BabyBearField::from(value))
    }

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
            "bus: none, 1 * x + 1 * y + -1 * tmp_0 + 0 * x * y + 0 = 0
bus: none, 0 * x + 0 * y + -1 * tmp_1 + 1 * x * y + 0 = 0
bus: none, -1 * tmp_1 + 0 * Unused + -1 * tmp_2 + 0 * tmp_1 * Unused + 0 = 0
bus: none, 5 * y + 0 * Unused + -1 * tmp_3 + 0 * y * Unused + 0 = 0
bus: none, 0 * Unused + 0 * Unused + -1 * tmp_4 + 0 * Unused * Unused + 1 = 0
bus: memory, 0 * tmp_0 + 0 * y + 0 * tmp_2 + 0 * tmp_0 * y + 0 = 0
bus: none, 0 * tmp_3 + 0 * x + 0 * y + 0 * tmp_3 * x + 0 = 0
bus: none, 0 * tmp_4 + 0 * tmp_5 + 0 * Unused + 0 * tmp_4 * tmp_5 + 0 = 0
bus: none, 0 * Unused + 0 * Unused + -1 * tmp_5 + 0 * Unused * Unused + 42 = 0
"
        )
    }
}
