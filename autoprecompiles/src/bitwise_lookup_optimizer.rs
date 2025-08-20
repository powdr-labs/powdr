use std::hash::Hash;
use std::{fmt::Debug, fmt::Display};

use itertools::Itertools;
use num_traits::{One, Zero};
use powdr_constraint_solver::constraint_system::{
    BusInteraction, BusInteractionHandler, ConstraintSystem,
};
use powdr_constraint_solver::grouped_expression::GroupedExpression;
use powdr_constraint_solver::range_constraint::RangeConstraint;
use powdr_constraint_solver::solver::{new_solver, Solver};
use powdr_number::FieldElement;

/// Optimize interactions with the bitwise lookup bus.
/// It optimizes bitwise lookups of boolean-constrained inputs and optimizes (re-groups)
/// the use of byte-range constraints.
pub fn optimize_bitwise_lookup<T: FieldElement, V: Hash + Eq + Clone + Ord + Debug + Display>(
    mut system: ConstraintSystem<T, V>,
    bitwise_lookup_bus_id: u64,
    solver: &mut impl Solver<T, V>,
    bus_interaction_handler: impl BusInteractionHandler<T> + Clone,
) -> ConstraintSystem<T, V> {
    // Expressions that we need to byte-constrain at the end.
    let mut to_byte_constrain = vec![];
    // New constraints (mainly substitutions) we will add.
    let mut new_constraints: Vec<GroupedExpression<T, V>> = vec![];
    system.bus_interactions.retain(|bus_int| {
        if !is_simple_multiplicity_bitwise_bus_interaction(bus_int, bitwise_lookup_bus_id) {
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
            assert!(z.is_zero());
            false
        } else if op == 1.into() {
            // The bus interaction is equivalent to "x, y and z are bytes and z = x ^ y".

            // If any argument is zero, the other two have to be equal.
            let mut args = vec![x, y, z];
            if let Some(zero_pos) = args.iter().position(|e| e.is_zero()) {
                args.remove(zero_pos);
                // The two remaining expressions in args are equal and bytes.
                let [a, b] = args.try_into().unwrap();
                new_constraints.push(a.clone() - b.clone());
                to_byte_constrain.push(a.clone());
                false
            } else if args.iter().all(|arg| {
                let rc = solver.range_constraint_for_expression(arg);
                rc.conjunction(&RangeConstraint::from_mask(1)) == rc
            }) {
                // All three expressions are either zero or one, we can replace the bus
                // interaction by an algebraic constraint.
                // TODO we could be a bit more clever about which variables to use in the
                // quadratic term
                let two = GroupedExpression::from_number(T::from(2));
                new_constraints
                    .push(x.clone() + y.clone() - z.clone() - two * x.clone() * y.clone());
                // Byte-constrain them to be sure we are not missing anything.
                to_byte_constrain.extend([x, y, z].into_iter().cloned());
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
    let byte_range_constraint = RangeConstraint::from_mask(0xffu64);
    let mut solver = new_solver(system.clone(), bus_interaction_handler);
    solver.solve().unwrap();

    let mut to_byte_constrain = to_byte_constrain
        .into_iter()
        .filter(|expr| {
            let rc = solver.range_constraint_for_expression(expr);
            rc != rc.conjunction(&byte_range_constraint)
        })
        .unique()
        .collect_vec();
    if to_byte_constrain.len() % 2 != 0 {
        to_byte_constrain.push(Zero::zero());
    }
    for (x, y) in to_byte_constrain.into_iter().tuples() {
        system.bus_interactions.push(BusInteraction {
            bus_id: GroupedExpression::from_number(T::from(bitwise_lookup_bus_id)),
            payload: vec![x.clone(), y.clone(), Zero::zero(), Zero::zero()],
            multiplicity: One::one(),
        });
    }
    system
        .algebraic_constraints
        .extend(new_constraints.into_iter().filter(|expr| !expr.is_zero()));
    system
}

fn is_simple_multiplicity_bitwise_bus_interaction<T: FieldElement, V: Clone + Hash + Eq + Ord>(
    bus_int: &BusInteraction<GroupedExpression<T, V>>,
    bitwise_lookup_bus_id: u64,
) -> bool {
    bus_int.bus_id == GroupedExpression::from_number(T::from(bitwise_lookup_bus_id))
        && bus_int.multiplicity.is_one()
}
