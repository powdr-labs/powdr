use std::fmt::Display;
use std::hash::Hash;

use itertools::Itertools;
use powdr_constraint_solver::constraint_system::BusInteractionHandler;
use powdr_constraint_solver::grouped_expression::RangeConstraintProvider;
use powdr_constraint_solver::indexed_constraint_system::IndexedConstraintSystem;
use powdr_constraint_solver::journaling_constraint_system::JournalingConstraintSystem;
use powdr_constraint_solver::range_constraint::RangeConstraint;
use powdr_constraint_solver::solver;
use powdr_number::FieldElement;

use crate::constraint_optimizer::IsBusStateful;

pub fn replace_equal_zero_checks<T: FieldElement, V: Clone + Ord + Hash + Display>(
    constraint_system: JournalingConstraintSystem<T, V>,
    bus_interaction_handler: impl BusInteractionHandler<T> + IsBusStateful<T> + Clone,
) -> JournalingConstraintSystem<T, V> {
    println!("Replacing equal zero checks in constraint system");
    let solver = solver::Solver::new(constraint_system.system().clone())
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
    let Ok(solution) = solve_with_assignments(
        constraint_system,
        bus_interaction_handler.clone(),
        [(var.clone(), value.clone())],
    ) else {
        return;
    };
    let inputs = zero_assignents(&solution).collect_vec();
    if inputs.is_empty() {
        return;
    }
    // We know: if `var = value`, then `inputs` are all zero.
    // Now check that `var == 1 - value` is inconsisten with `inputs` all being zero.

    if solve_with_assignments(
        constraint_system,
        bus_interaction_handler.clone(),
        inputs
            .iter()
            .map(|v| (v.clone(), T::from(0)))
            .chain([(var.clone(), T::from(1) - value)]),
    )
    .is_ok()
    {
        return;
    }

    // Some of the inputs are redundant, so try to reduce the size of the set.
    let redundant_inputs = determine_redundant_inputs(
        constraint_system,
        bus_interaction_handler.clone(),
        &inputs,
        var.clone(),
        value,
    );
    let inputs = inputs
        .into_iter()
        .filter(|v| !redundant_inputs.contains(v))
        .collect_vec();

    println!("Found equal zero check for variable {var}:\n{var} = {value} if and only if all of {} are zero. Plus, the variables {} are also set to zero but are redundant.", 
        inputs.iter().format(", "),
        redundant_inputs.iter().format(", ")
    );
    //    panic!();
}

fn determine_redundant_inputs<T: FieldElement, V: Clone + Ord + Hash + Display>(
    constraint_system: &IndexedConstraintSystem<T, V>,
    bus_interaction_handler: impl BusInteractionHandler<T> + IsBusStateful<T> + Clone,
    inputs: &[V],
    output: V,
    value: T,
) -> Vec<V> {
    // An input 'i' is redundant if setting `var = 1 - value` and `x = 0` for all `x` in `inputs \ {i}` is still inconsistent.
    let mut redundant_input_indices = vec![];
    for (i, _) in inputs.iter().enumerate() {
        let assignments = inputs
            .iter()
            .enumerate()
            .filter_map(|(j, val)| {
                (j != i && !redundant_input_indices.contains(&j)).then(|| (val.clone(), T::from(0)))
            })
            .chain([(output.clone(), T::from(1) - value)]);
        if solve_with_assignments(
            constraint_system,
            bus_interaction_handler.clone(),
            assignments,
        )
        .is_err()
        {
            redundant_input_indices.push(i);
        }
    }
    redundant_input_indices
        .into_iter()
        .map(|i| inputs[i].clone())
        .collect()
}

fn solve_with_assignments<T: FieldElement, V: Clone + Ord + Hash + Display>(
    constraint_system: &IndexedConstraintSystem<T, V>,
    bus_interaction_handler: impl BusInteractionHandler<T> + IsBusStateful<T> + Clone,
    assignments: impl IntoIterator<Item = (V, T)>,
) -> Result<solver::SolveResult<T, V>, solver::Error> {
    let mut system = constraint_system.clone();
    for (var, value) in assignments {
        system.substitute_by_known(&var, &value);
    }
    solver::solve_system(system.system().clone(), bus_interaction_handler)
}

fn zero_assignents<T: FieldElement, V: Clone + Ord + Hash + Display>(
    solution: &solver::SolveResult<T, V>,
) -> impl Iterator<Item = V> + '_ {
    solution
        .assignments
        .iter()
        .filter_map(|(v, val)| (val.try_to_number()? == 0.into()).then_some(v.clone()))
}
