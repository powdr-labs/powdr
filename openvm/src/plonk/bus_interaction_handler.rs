use core::slice;
use std::collections::{BTreeMap, HashSet};

use super::plonk_gates_builder::CircuitBuilderQuadratic;
use crate::plonk::{Gate, NUMBER_OF_WITNESS_COLS};
use powdr_autoprecompiles::bus_map::{
    BusMap,
    BusType::{
        BitwiseLookup, ExecutionBridge, Memory, PcLookup, TupleRangeChecker, VariableRangeChecker,
    },
};
use powdr_autoprecompiles::expression::AlgebraicReference;
use powdr_autoprecompiles::SymbolicBusInteraction;
use powdr_constraint_solver::quadratic_symbolic_expression::QuadraticSymbolicExpression;
use powdr_number::FieldElement;

use super::air_to_plonkish::CircuitBuilder;
use powdr_autoprecompiles::expression_conversion::algebraic_to_quadratic_symbolic_expression;

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
    let number_of_gates =
        (bus_interaction.args.len() as u32).div_ceil(NUMBER_OF_WITNESS_COLS as u32) as usize;
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

pub fn add_bus_to_plonk_circuit_from_quadratic_symbolic_expression<T>(
    bus_interaction: SymbolicBusInteraction<T>,
    circuit_builder: &mut CircuitBuilderQuadratic<T>,
    bus_map: &BusMap,
    constraint_format: &mut BTreeMap<(bool, usize, usize), (u64,u64)>,
    constraint_cache: &mut HashSet<QuadraticSymbolicExpression<T,AlgebraicReference>>
) where
    T: FieldElement,
{
    let mut length = circuit_builder.plonk_circuit.gates.len();
    let number_of_gates =
        (bus_interaction.args.len() as u32).div_ceil(NUMBER_OF_WITNESS_COLS as u32) as usize;
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
            *payload = circuit_builder
                .evaluate_expression(&algebraic_to_quadratic_symbolic_expression(arg), false);

            let quadratic_symbolic_expr = &algebraic_to_quadratic_symbolic_expression(arg);

            if constraint_format
                .get(&(
                    quadratic_symbolic_expr.is_quadratic(),
                    if quadratic_symbolic_expr.is_quadratic() {
                        quadratic_symbolic_expr.quadratic[0].0.linear.len()
                    } else {
                        quadratic_symbolic_expr.linear.len()
                    },
                    if quadratic_symbolic_expr.is_quadratic() {
                        quadratic_symbolic_expr.quadratic[0].1.linear.len()
                    } else {
                        0
                    },
                ))
                .is_none()
            {
                 constraint_format.insert(
                (
                    quadratic_symbolic_expr.is_quadratic(),
                    if quadratic_symbolic_expr.is_quadratic() {
                        quadratic_symbolic_expr.quadratic[0].0.linear.len()
                    } else {
                        quadratic_symbolic_expr.linear.len()
                    },
                    if quadratic_symbolic_expr.is_quadratic() {
                        quadratic_symbolic_expr.quadratic[0].1.linear.len()
                    } else {
                        0
                    },
                ),
                (1,1)
            );
            constraint_cache.insert(quadratic_symbolic_expr.clone());
        } else {
            let (format_count,unique_count) = constraint_format
                .get_mut(&(
                    quadratic_symbolic_expr.is_quadratic(),
                    if quadratic_symbolic_expr.is_quadratic() {
                        quadratic_symbolic_expr.quadratic[0].0.linear.len()
                    } else {
                        quadratic_symbolic_expr.linear.len()
                    },
                    if quadratic_symbolic_expr.is_quadratic() {
                        quadratic_symbolic_expr.quadratic[0].1.linear.len()
                    } else {
                        0
                    },
                ))
                .unwrap();
            *format_count += 1;

            if !constraint_cache.contains(&quadratic_symbolic_expr) {
                *unique_count += 1;
                constraint_cache.insert(quadratic_symbolic_expr.clone());
            }
            }
            if quadratic_symbolic_expr.is_quadratic() == false
                && quadratic_symbolic_expr.linear.len() == 1
            {


                if circuit_builder.plonk_circuit.gates.len() > length {
                    let slice = circuit_builder.plonk_circuit.gates[length..].to_vec();
                    for gate in slice {
                        println!("{gate}");
                    }
                }
            }
            length = circuit_builder.plonk_circuit.gates.len();
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
    use powdr_autoprecompiles::expression::AlgebraicExpression;
    use powdr_autoprecompiles::SymbolicBusInteraction;
    use powdr_number::BabyBearField;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_add_memory_bus_to_plonk_circuit() {
        let bus_map = default_openvm_bus_map();

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
    #[test]
    fn test_add_memory_bus_to_plonk_circuit_quadratic() {
        let bus_map = default_openvm_bus_map();

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

        let mut constraint_format = BTreeMap::new();
        let mut constraint_cache=HashSet::new();

        let mut circuit_builder = CircuitBuilderQuadratic::new();
        add_bus_to_plonk_circuit_from_quadratic_symbolic_expression(
            bus_interaction,
            &mut circuit_builder,
            &bus_map,
            &mut constraint_format,
            &mut constraint_cache
        );
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
