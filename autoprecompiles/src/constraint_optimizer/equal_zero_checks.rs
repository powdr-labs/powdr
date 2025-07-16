use std::collections::HashSet;
use std::fmt::Display;
use std::hash::Hash;

use itertools::Itertools;
use powdr_constraint_solver::constraint_system::{BusInteractionHandler, ConstraintRef};
use powdr_constraint_solver::grouped_expression::RangeConstraintProvider;
use powdr_constraint_solver::indexed_constraint_system::IndexedConstraintSystem;
use powdr_constraint_solver::journaling_constraint_system::JournalingConstraintSystem;
use powdr_constraint_solver::range_constraint::RangeConstraint;
use powdr_constraint_solver::solver;
use powdr_number::FieldElement;

use crate::constraint_optimizer::reachability::reachable_variables;
use crate::constraint_optimizer::{variables_in_stateful_bus_interactions, IsBusStateful};

pub fn replace_equal_zero_checks<T: FieldElement, V: Clone + Ord + Hash + Display>(
    constraint_system: JournalingConstraintSystem<T, V>,
    bus_interaction_handler: impl BusInteractionHandler<T> + IsBusStateful<T> + Clone,
) -> JournalingConstraintSystem<T, V> {
    println!(
        "\n----------------------------------\nReplacing equal zero checks in constraint system"
    );
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

/// A candidate for an equal zero check, i.e.
/// in the given system, `output = value` if and only if
/// all `inputs` are zero.
struct Candidate<'a, T, V, B> {
    constraint_system: &'a IndexedConstraintSystem<T, V>,
    bus_interaction_handler: B,
    inputs: Vec<V>,
    output: V,
    value: T,
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

    let mut candidate = Candidate {
        constraint_system,
        bus_interaction_handler: bus_interaction_handler.clone(),
        inputs,
        output: var.clone(),
        value,
    };

    // Some of the inputs are redundant, so try to reduce the size of the set.
    determine_and_remove_redundant_inputs(&mut candidate);

    println!("\n\nFound equal zero check for variable {var}:\n{var} = {value} if and only if all of {} are zero.", 
        candidate.inputs.iter().format(", "),
   
    );

    // Now we need to find out which columns / constraints can be removed.
    // We do a reachability search starting from stateful bus interactions, but we stop
    // the search as soon as we reach an input or the output variable.
    // The variables that are not reachable in this sense can be removed, because they are only
    // connected to a stateful bus interaction via the input or output.

    let blocking_variables = candidate
        .inputs
        .iter()
        .chain([&candidate.output]).cloned();
    let outside_variables = reachable_variables(
        variables_in_stateful_bus_interactions(constraint_system.system(), bus_interaction_handler.clone()).cloned(),
        blocking_variables,
        constraint_system.system(),
    );

    // We could still have constraints that are only connected to the inputs but not the output, which constrain
    // the inputs.
    // Those we should keep, so we again do a reachability search from the output.

    let output_reachable_variables = reachable_variables(
        std::iter::once(candidate.output.clone()),
        candidate.inputs.iter().cloned(),
        candidate.constraint_system.system(),
    );

    let mut variables_to_remove = output_reachable_variables.difference(&outside_variables).collect::<HashSet<_>>();
    variables_to_remove.remove(&candidate.output);

    for constr in candidate.constraint_system.system().iter() {
        if constr
            .referenced_variables()
            .any(|var| variables_to_remove.contains(var))
        {
            assert!(constr.referenced_variables().all(|var| {
                candidate.inputs.contains(var) || var == &candidate.output 
                || variables_to_remove.contains(var)
            }));
            println!(
                "Can replace constraint: {constr}",
            );
        }
    }




    //    panic!();
}

fn determine_and_remove_redundant_inputs<T: FieldElement, V: Clone + Ord + Hash + Display>(
    candidate: &mut Candidate<T, V, impl BusInteractionHandler<T> + IsBusStateful<T> + Clone>,
) {
    // An input 'i' is redundant if setting `var = 1 - value` and `x = 0` for all `x` in `inputs \ {i}` is still inconsistent.
    let mut redundant_input_indices = vec![];
    for (i, _) in candidate.inputs.iter().enumerate() {
        let assignments = candidate
            .inputs
            .iter()
            .enumerate()
            .filter_map(|(j, val)| {
                (j != i && !redundant_input_indices.contains(&j)).then(|| (val.clone(), T::from(0)))
            })
            .chain([(candidate.output.clone(), T::from(1) - candidate.value)]);
        if solve_with_assignments(
            candidate.constraint_system,
            candidate.bus_interaction_handler.clone(),
            assignments,
        )
        .is_err()
        {
            redundant_input_indices.push(i);
        }
    }

    candidate.inputs = candidate.inputs.drain(..).enumerate().filter(|(i, _)| !redundant_input_indices.contains(i)).map(|(_, v)| v).collect();
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
