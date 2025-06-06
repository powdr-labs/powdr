use std::collections::HashSet;
use std::hash::Hash;
use std::{fmt::Debug, fmt::Display};

use itertools::Itertools;
use powdr_constraint_solver::constraint_system::{BusInteraction, ConstraintSystem};
use powdr_constraint_solver::quadratic_symbolic_expression::QuadraticSymbolicExpression;
use powdr_number::FieldElement;

use crate::BITWISE_LOOKUP_BUS_ID;

/// Optimize interactions with the bitwise lookup bus. It mostly optimizes the use of
/// byte-range constraints.
pub fn optimize_bitwise_lookup<T: FieldElement, V: Hash + Eq + Clone + Ord + Debug + Display>(
    mut system: ConstraintSystem<T, V>,
) -> ConstraintSystem<T, V> {
    // Expressions that we need to byte-constrain at the end.
    let mut to_byte_constrain = vec![];
    // New constraints (mainly substitutions) we will add.
    let mut new_constraints: Vec<QuadraticSymbolicExpression<T, V>> = vec![];
    system.bus_interactions.retain(|bus_int| {
        if !is_simple_multiplicity_bitwise_bus_interaction(bus_int) {
            return true;
        }
        // See: https://github.com/openvm-org/openvm/blob/v1.0.0/crates/circuits/primitives/src/bitwise_op_lookup/bus.rs
        // Expects (x, y, z, op), where:
        // - if op == 0, then x and y are bytes and z = 0
        // - if op == 1, then x and y are bytes and z = x ^ y
        let [x, y, z, op] = &bus_int.payload[..] else {
            panic!();
        };

        let Some(op) = op.try_to_number() else {
            return true;
        };
        // Collect all expressions to be byte-constrained and
        // re-emit the constraints at the end in a (hopefully) more efficient way.
        // The idea behind is that we can have duplicates or concrete numbers
        // in those expressions.
        if op == 0.into() {
            // The bus interaction is equivalent to "x and y are bytes and z = 0".
            to_byte_constrain.extend([x.clone(), y.clone()]);
            // If it is not zero, we could also add it as a new constraint.
            assert!(z == &T::from(0).into());
            false
        } else if op == 1.into() {
            // The bus interaction is equivalent to "x, y and z are bytes and z = x ^ y".

            // If any argument is zero, the other two have to be equal.
            let mut args = vec![x, y, z];
            if let Some(zero_pos) = args.iter().position(|e| *e == &T::from(0).into()) {
                args.remove(zero_pos);
                // The two remaning expressions in args are equal and bytes.
                let [a, b] = args.try_into().unwrap();
                new_constraints.push(a.clone() - b.clone());
                to_byte_constrain.push(a.clone());
                false
            } else {
                true
            }
        } else {
            panic!("Expected bitwise bus interaction operation to be either 0 or 1.");
        }
    });

    // After we have removed the bus interactions, we check which of the
    // expressions we still need to byte-constrain. Some are maybe already
    // byte-constrained by other bus interactions.
    let already_byte_constrained = all_byte_constrained_expressions(&system)
        .cloned()
        .collect::<HashSet<_>>();
    let mut to_byte_constrain = to_byte_constrain
        .into_iter()
        .filter(|expr| {
            if let Some(n) = expr.try_to_number() {
                assert!(n >= T::from(0) && n < T::from(256));
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
    for (x, y) in to_byte_constrain.into_iter().tuples() {
        system.bus_interactions.push(BusInteraction {
            bus_id: T::from(BITWISE_LOOKUP_BUS_ID).into(),
            payload: vec![x.clone(), y.clone(), T::from(0).into(), T::from(0).into()],
            multiplicity: T::from(1).into(),
        });
    }
    system.algebraic_constraints.extend(new_constraints);
    system
}

fn is_simple_multiplicity_bitwise_bus_interaction<T: FieldElement, V: Ord>(
    bus_int: &BusInteraction<QuadraticSymbolicExpression<T, V>>,
) -> bool {
    bus_int.bus_id == T::from(BITWISE_LOOKUP_BUS_ID).into()
        && bus_int.multiplicity == T::from(1).into()
}

/// Returns all expressions that are byte-constrained in the machine.
/// The list does not have to be exhaustive.
fn all_byte_constrained_expressions<T: FieldElement, V: Clone + Ord + Hash>(
    machine: &ConstraintSystem<T, V>,
) -> impl Iterator<Item = &QuadraticSymbolicExpression<T, V>> {
    machine
        .bus_interactions
        .iter()
        .filter(|bus_int| is_simple_multiplicity_bitwise_bus_interaction(bus_int))
        .flat_map(|bus_int| {
            let [x, y, z, op] = &bus_int.payload[..] else {
                panic!();
            };
            if let Some(op) = op.try_to_number() {
                if op == T::from(0) {
                    vec![x, y]
                } else if op == T::from(1) {
                    vec![x, y, z]
                } else {
                    vec![]
                }
            } else {
                vec![]
            }
        })
}
