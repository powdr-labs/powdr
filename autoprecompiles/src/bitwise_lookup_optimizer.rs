use std::collections::BTreeSet;

use itertools::Itertools;
use powdr_ast::analyzed::AlgebraicExpression;
use powdr_number::FieldElement;

use crate::{SymbolicBusInteraction, SymbolicConstraint, SymbolicMachine, BITWISE_LOOKUP_BUS_ID};

/// Optimize interactions with the bitwise lookup bus. It mostly optimizes the use of
/// byte-range constraints.
pub fn optimize_bitwise_lookup<T: FieldElement>(
    mut machine: SymbolicMachine<T>,
) -> SymbolicMachine<T> {
    // Expressions that we need to byte-constrain at the edn.
    let mut to_byte_constrain = vec![];
    // Bus interaction indices we will remove.
    let mut to_remove = vec![];
    // New constraints (mainly substitutions) we will add.
    let mut new_constraints: Vec<SymbolicConstraint<T>> = vec![];
    for (index, bus_int) in machine.bus_interactions.iter().enumerate() {
        // See: https://github.com/openvm-org/openvm/blob/v1.0.0/crates/circuits/primitives/src/bitwise_op_lookup/bus.rs
        // Expects (x, y, z, op), where:
        // - if op == 0, then x and y are bytes and z = 0
        // - if op == 1, then x and y are bytes and z = x ^ y
        if bus_int.id != BITWISE_LOOKUP_BUS_ID {
            continue;
        }
        let [x, y, z, op] = &bus_int.args[..] else {
            panic!();
        };
        if bus_int.mult != T::from(1).into() {
            continue;
        }

        let AlgebraicExpression::Number(op) = op else {
            continue;
        };
        // Collect all expressions to be byte-constrained and
        // re-emit the constraints at the end in a (hopefully) more efficient way.
        // The idea behind is that we can have duplicates or concrete numbers
        // in those expressions.
        if op == &0.into() {
            // The bus interaction is equivalent to "x and y are bytes and z = 0".
            to_byte_constrain.extend([x.clone(), y.clone()]);
            to_remove.push(index);
            if z != &T::from(0).into() {
                new_constraints.push(z.clone().into());
            }
        } else if op == &1.into() {
            // The bus interaction is equivalent to "x, y and z are bytes and z = x ^ y".
            if x == &T::from(0).into() {
                // y = z and both are bytes.
                new_constraints.push((y.clone() - z.clone()).into());
                to_byte_constrain.push(y.clone());
                to_remove.push(index);
            } else if y == &T::from(0).into() {
                // x = z and both are bytes.
                new_constraints.push((x.clone() - z.clone()).into());
                to_byte_constrain.push(x.clone());
                to_remove.push(index);
            } else if z == &T::from(0).into() {
                // x = y are both are bytes.
                new_constraints.push((x.clone() - y.clone()).into());
                to_byte_constrain.push(x.clone());
                to_remove.push(index);
            }
        } else {
            panic!("Expected bitwise bus interaction operation to be either 0 or 1.");
        }
    }
    machine.bus_interactions = machine
        .bus_interactions
        .into_iter()
        .enumerate()
        .filter(|(i, _)| !to_remove.contains(i))
        .map(|(_, bus_int)| bus_int)
        .collect();
    // After we have removed the bus interactions, we check which of the
    // expressions we still need to byte-constrain are maybe already
    // byte-constrained by other bus interactions.
    let already_byte_constrained = all_byte_constrained_expressions(&machine)
        .cloned()
        .collect::<BTreeSet<_>>();
    let mut to_byte_constrain = to_byte_constrain
        .into_iter()
        .filter(|expr| {
            if let AlgebraicExpression::Number(n) = expr {
                assert!(*n >= T::from(0) && *n < T::from(256));
                // No need to byte-constrain numbers.
                false
            } else {
                !already_byte_constrained.contains(expr)
            }
        })
        .unique()
        .collect_vec();
    if to_byte_constrain.len() % 2 != 0 {
        to_byte_constrain.push(T::from(0).into());
    }
    for chunk in to_byte_constrain.chunks(2) {
        let [x, y] = chunk else {
            unreachable!();
        };
        machine.bus_interactions.push(SymbolicBusInteraction {
            id: BITWISE_LOOKUP_BUS_ID,
            args: vec![x.clone(), y.clone(), T::from(0).into(), T::from(0).into()],
            mult: T::from(1).into(),
        });
    }
    machine.constraints.extend(new_constraints);
    machine
}

/// Returns all expressions that are byte-constrained in the machine.
/// The list does not have to be exhaustive.
fn all_byte_constrained_expressions<T: FieldElement>(
    machine: &SymbolicMachine<T>,
) -> impl Iterator<Item = &AlgebraicExpression<T>> {
    machine
        .bus_interactions
        .iter()
        .filter(|bus_int| bus_int.id == BITWISE_LOOKUP_BUS_ID && bus_int.mult == T::from(1).into())
        .flat_map(|bus_int| {
            let [x, y, z, op] = &bus_int.args[..] else {
                panic!();
            };
            if let AlgebraicExpression::Number(op) = op {
                if *op == T::from(0) {
                    vec![x, y]
                } else if *op == T::from(1) {
                    vec![x, y, z]
                } else {
                    vec![]
                }
            } else {
                vec![]
            }
        })
}
