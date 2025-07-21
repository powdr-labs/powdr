use std::collections::HashMap;
use std::hash::Hash;
use std::{fmt::Debug, fmt::Display};

use itertools::Itertools;
use num_traits::{One, Zero};
use powdr_constraint_solver::constraint_system::{
    BusInteraction, BusInteractionHandler, ConstraintSystem,
};
use powdr_constraint_solver::effect::EffectImpl;
use powdr_constraint_solver::grouped_expression::{
    GroupedExpression, NoRangeConstraints, QuadraticSymbolicExpression, RangeConstraintProvider,
};
use powdr_constraint_solver::range_constraint::RangeConstraint;
use powdr_constraint_solver::runtime_constant::RuntimeConstant;
use powdr_constraint_solver::solver::{self, bus_interaction_variable_wrapper};
use powdr_number::FieldElement;

/// Optimize interactions with the bitwise lookup bus. It mostly optimizes the use of
/// byte-range constraints.
pub fn optimize_bitwise_lookup<T: FieldElement, V: Hash + Eq + Clone + Ord + Debug + Display>(
    mut system: ConstraintSystem<T, V>,
    bitwise_lookup_bus_id: u64,
    bus_interaction_handler: impl BusInteractionHandler<T>,
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
    let range_constraints =
        determine_range_constraints_using_solver(&system, bus_interaction_handler);

    let mut to_byte_constrain = to_byte_constrain
        .into_iter()
        .filter(|expr| {
            if let Some(n) = expr.try_to_number() {
                assert!(n >= T::from(0) && n < T::from(256));
                // No need to byte-constrain numbers.
                false
            } else if let Some(rc) = range_constraints.get(expr) {
                *rc != rc.conjunction(&byte_range_constraint)
            } else {
                true
            }
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
    system.algebraic_constraints.extend(new_constraints);
    system
}

fn is_simple_multiplicity_bitwise_bus_interaction<T: FieldElement, V: Clone + Hash + Eq + Ord>(
    bus_int: &BusInteraction<GroupedExpression<T, V>>,
    bitwise_lookup_bus_id: u64,
) -> bool {
    bus_int.bus_id == GroupedExpression::from_number(T::from(bitwise_lookup_bus_id))
        && bus_int.multiplicity.is_one()
}

struct RangeConstraints<T: FieldElement, V> {
    range_constraints: HashMap<V, RangeConstraint<T>>,
}

impl<T: FieldElement, V: Clone + Hash + Eq> RangeConstraintProvider<T, V>
    for RangeConstraints<T, V>
{
    fn get(&self, var: &V) -> RangeConstraint<T> {
        self.range_constraints.get(var).cloned().unwrap_or_default()
    }
}

impl<T: FieldElement, V: Clone + Hash + Ord + Display> RangeConstraints<T, V> {
    /// Computes range constraints for variables in the machine.
    /// This is a very basic way to do it, it does not do it iteratively
    /// and only considers bus interactions.
    /// We could get better results using the solver.
    fn new(
        machine: &ConstraintSystem<T, V>,
        bus_interaction_handler: &impl BusInteractionHandler<T>,
    ) -> Self {
        let range_constraints = machine
            .bus_interactions
            .iter()
            .flat_map(move |bus_interaction| {
                bus_interaction
                    .solve(bus_interaction_handler, &NoRangeConstraints)
                    .unwrap()
            })
            .flat_map(|effect| match effect {
                EffectImpl::Assignment(v, value) => Some((v, value.range_constraint())),
                EffectImpl::RangeConstraint(v, range_constraint) => Some((v, range_constraint)),
                EffectImpl::BitDecomposition(..)
                | EffectImpl::Assertion(..)
                | EffectImpl::ConditionalAssignment { .. } => {
                    None /* ignore */
                }
            })
            .into_grouping_map()
            .reduce(|acc, _key, new_rc| acc.conjunction(&new_rc));
        Self { range_constraints }
    }
}

fn determine_range_constraints_using_solver<
    T: FieldElement,
    V: Clone + Hash + Eq + Ord + Debug + Display,
>(
    system: &ConstraintSystem<T, V>,
    bus_interaction_handler: impl BusInteractionHandler<T>,
) -> HashMap<QuadraticSymbolicExpression<T, V>, RangeConstraint<T>> {
    let (wrapper, transformed_system) = solver::bus_interaction_variable_wrapper::BusInteractionVariableWrapper::replace_bus_interaction_expressions(system.clone());
    solver::determine_range_constraints(transformed_system, bus_interaction_handler)
        .unwrap()
        .into_iter()
        .map(|(var, range_constraint)| {
            let expr = match var {
                bus_interaction_variable_wrapper::Variable::BusInteractionField(..) => {
                    wrapper.bus_interaction_vars[&var].clone()
                }
                bus_interaction_variable_wrapper::Variable::Variable(v) => {
                    GroupedExpression::from_unknown_variable(v)
                }
            };
            (expr, range_constraint)
        })
        .collect()
}
