use std::fmt::Display;
use std::hash::Hash;

use itertools::Itertools;
use powdr_constraint_solver::constraint_system::BusInteractionHandler;
use powdr_constraint_solver::grouped_expression::RangeConstraintProvider;
use powdr_constraint_solver::indexed_constraint_system::IndexedConstraintSystem;
use powdr_constraint_solver::journaling_constraint_system::JournalingConstraintSystem;
use powdr_constraint_solver::range_constraint::RangeConstraint;
use powdr_number::FieldElement;

use crate::constraint_optimizer::IsBusStateful;

pub fn replace_equal_zero_checks<T: FieldElement, V: Clone + Ord + Hash + Display>(
    constraint_system: JournalingConstraintSystem<T, V>,
    bus_interaction_handler: impl BusInteractionHandler<T> + IsBusStateful<T> + Clone,
) -> JournalingConstraintSystem<T, V> {
    let solver = powdr_constraint_solver::solver::Solver::new(constraint_system.system().clone())
        .with_bus_interaction_handler(bus_interaction_handler.clone());
    let rc = solver.compute_range_constraints().unwrap();
    let binary_range_constraint = RangeConstraint::from_mask(1);
    for var in constraint_system.indexed_system().variables() {
        if rc.get(var) == binary_range_constraint {
            for value in [T::from(0), T::from(1)] {
                try_output(
                    constraint_system.indexed_system(),
                    bus_interaction_handler.clone(),
                    var.clone(),
                    value,
                );
            }
        }
    }
    constraint_system
}

fn try_output<T: FieldElement, V: Clone + Ord + Hash + Display>(
    constraint_system: &IndexedConstraintSystem<T, V>,
    bus_interaction_handler: impl BusInteractionHandler<T> + IsBusStateful<T> + Clone,
    var: V,
    value: T,
) {
    let mut first_system = constraint_system.clone();
    first_system.substitute_by_known(&var, &value);
    let Ok(solution) = powdr_constraint_solver::solver::solve_system(
        first_system.system().clone(),
        bus_interaction_handler.clone(),
    ) else {
        return;
    };
    let zero_assignments = solution
        .assignments
        .into_iter()
        .filter_map(|(v, val)| (val.try_to_number()? == 0.into()).then_some(v))
        .collect_vec();
    if zero_assignments.is_empty() {
        return;
    }
    // We know: if `var = value`, then `zero_assignments` are all zero.
    // Now check that if `var == 1 - value` is inconsisten with `zero_assignments` all being zero.

    let mut second_system = constraint_system.clone();
    second_system.substitute_by_known(&var, &(T::from(1) - value));
    for v in &zero_assignments {
        second_system.substitute_by_known(v, &T::from(0));
    }
    if powdr_constraint_solver::solver::solve_system(
        second_system.system().clone(),
        bus_interaction_handler,
    )
    .is_ok()
    {
        return;
    }

    println!("Found equal zero check for variable {var}:\n{var} = {value} if and only if all of {} are zero", 
        zero_assignments.iter().format(", ")
    );
    panic!();
}
