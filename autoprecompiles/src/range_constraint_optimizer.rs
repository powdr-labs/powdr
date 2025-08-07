use std::collections::BTreeMap;
use std::fmt::Display;
use std::hash::Hash;

use powdr_constraint_solver::constraint_system::{
    BusInteraction, BusInteractionHandler, ConstraintSystem,
};
use powdr_constraint_solver::grouped_expression::GroupedExpression;
use powdr_constraint_solver::range_constraint::RangeConstraint;
use powdr_constraint_solver::solver::{new_solver, Solver};
use powdr_number::FieldElement;

pub type RangeConstraintMap<T, V> = BTreeMap<GroupedExpression<T, V>, RangeConstraint<T>>;

pub trait PureRangeConstraintHandler<T: FieldElement> {
    fn pure_range_constraints<V: Ord + Clone + Eq>(
        &self,
        bus_interaction: &BusInteraction<GroupedExpression<T, V>>,
    ) -> Option<RangeConstraintMap<T, V>>;

    fn make_range_constraints<V>(
        &self,
        range_constraints: RangeConstraintMap<T, V>,
    ) -> Vec<BusInteraction<GroupedExpression<T, V>>>;
}

pub fn optimize_range_constraints<T: FieldElement, V: Ord + Clone + Hash + Eq + Display>(
    mut system: ConstraintSystem<T, V>,
    bus_interaction_handler: impl BusInteractionHandler<T> + PureRangeConstraintHandler<T> + Clone,
) -> ConstraintSystem<T, V> {
    // Remove all pure range constraints, but collect what was removed.
    let mut to_constrain = BTreeMap::new();
    system.bus_interactions.retain(|bus_int| {
        match bus_interaction_handler.pure_range_constraints(bus_int) {
            Some(range_constraints) => {
                for (expr, rc) in range_constraints {
                    let existing_rc = to_constrain
                        .entry(expr)
                        .or_insert_with(RangeConstraint::default);
                    *existing_rc = existing_rc.conjunction(&rc);
                }
                false
            }
            None => true,
        }
    });

    let range_constraints = bus_interaction_handler.make_range_constraints(to_constrain);
    let mut solver = new_solver(system.clone(), bus_interaction_handler);
    solver.solve().unwrap();

    // TODO: Filter redundant range constraints.

    system.bus_interactions.extend(range_constraints);

    system
}
