use std::collections::{BTreeSet, HashSet};
use std::fmt::Display;
use std::hash::Hash;

use itertools::Itertools;
use num_traits::One;
use powdr_constraint_solver::constraint_system::{BusInteractionHandler, ConstraintSystem};
use powdr_constraint_solver::grouped_expression::{GroupedExpression, RangeConstraintProvider};
use powdr_constraint_solver::indexed_constraint_system::IndexedConstraintSystem;
use powdr_constraint_solver::journaling_constraint_system::JournalingConstraintSystem;
use powdr_constraint_solver::range_constraint::RangeConstraint;
use powdr_constraint_solver::solver;
use powdr_number::FieldElement;

use crate::constraint_optimizer::reachability::reachable_variables;
use crate::constraint_optimizer::{variables_in_stateful_bus_interactions, IsBusStateful};

pub fn replace_equal_zero_checks<T: FieldElement, V: Clone + Ord + Hash + Display>(
    mut constraint_system: JournalingConstraintSystem<T, V>,
    bus_interaction_handler: impl BusInteractionHandler<T> + IsBusStateful<T> + Clone,
    new_var: &mut impl FnMut() -> V,
) -> JournalingConstraintSystem<T, V> {
    println!(
        "\n----------------------------------\nReplacing equal zero checks in constraint system"
    );
    let solver = solver::Solver::new(constraint_system.system().clone())
        .with_bus_interaction_handler(bus_interaction_handler.clone());
    let rc = solver.compute_range_constraints().unwrap();
    let binary_range_constraint = RangeConstraint::from_mask(1);
    let binary_variables = constraint_system
        .indexed_system()
        .variables()
        .filter(|v| rc.get(v) == binary_range_constraint)
        .cloned()
        .collect::<BTreeSet<_>>();
    for var in binary_variables {
        for value in [T::from(0), T::from(1)] {
            try_replace_equal_zero_check(
                &mut constraint_system,
                bus_interaction_handler.clone(),
                &rc,
                new_var,
                var.clone(),
                value,
            );
        }
    }
    constraint_system
}

fn try_replace_equal_zero_check<T: FieldElement, V: Clone + Ord + Hash + Display>(
    constraint_system: &mut JournalingConstraintSystem<T, V>,
    bus_interaction_handler: impl BusInteractionHandler<T> + IsBusStateful<T> + Clone,
    rc: &impl RangeConstraintProvider<T, V>,
    new_var: &mut impl FnMut() -> V,
    output: V,
    value: T,
) {
    let Ok(solution) = solve_with_assignments(
        constraint_system.indexed_system(),
        bus_interaction_handler.clone(),
        [(output.clone(), value)],
    ) else {
        return;
    };
    let inputs: BTreeSet<_> = zero_assignents(&solution).collect();
    if inputs.is_empty() {
        return;
    }
    // We know: if `var = value`, then `inputs` are all zero.
    // Now check that `var == 1 - value` is inconsisten with `inputs` all being zero.

    if solve_with_assignments(
        constraint_system.indexed_system(),
        bus_interaction_handler.clone(),
        inputs
            .iter()
            .map(|v| (v.clone(), T::from(0)))
            .chain([(output.clone(), T::from(1) - value)]),
    )
    .is_ok()
    {
        return;
    }
    println!("\n\nFound equal zero check for variable {output}:\n{output} = {value} if and only if all of {} are zero.", 
        inputs.iter().format(", ")   
    );

    // Some of the inputs are redundant, so try to reduce the size of the set.
    // TODO this is not how we find the best set of vars. Most are mutually redundant.
    // We should sort them by distance to a stateful bus interaction and start removing
    // those with a higher distance. or maybe distance to the output?
    let inputs = determine_and_remove_redundant_inputs(
        constraint_system.indexed_system(),
        bus_interaction_handler.clone(),
        inputs.iter().rev().cloned(), // TODO using rev as a bad heuristic.
        output.clone(),
        value,
    );

    println!("\n\nFound equal zero check for variable {output}:\n{output} = {value} if and only if all of {} are zero.", 
        inputs.iter().format(", ")   
    );

    // Check that each input is non-negative and that the sum of inputs cannot wrap.
    let min_input = inputs
        .iter()
        .map(|v| rc.get(v))
        .reduce(|a, b| a.disjunction(&b))
        .unwrap()
        .range()
        .0;
    if min_input != T::from(0) {
        return;
    }
    let sum_of_inputs = inputs
        .iter()
        .map(|v| rc.get(v))
        .reduce(|a, b| a.combine_sum(&b))
        .unwrap();
    if sum_of_inputs.is_unconstrained() {
        return;
    }

    // Ok, we can replace the constraints by a potentially more efficient version.
    // Let's find out which are the constraints that we need to replace.
    // We do a reachability search starting from stateful bus interactions, but we stop
    // the search as soon as we reach an input or the output variable.
    // The variables that are not reachable in this sense can be removed, because they are only
    // connected to a stateful bus interaction via the input or output.
    let blocking_variables = inputs.iter().chain([&output]).cloned();
    let outside_variables = reachable_variables(
        variables_in_stateful_bus_interactions(
            constraint_system.system(),
            bus_interaction_handler.clone(),
        )
        .cloned(),
        blocking_variables,
        constraint_system.system(),
    );

    // We could still have constraints that are only connected to the inputs but not the output, which constrain
    // the inputs.
    // Those we should keep, so we again do a reachability search from the output.

    let output_reachable_variables = reachable_variables(
        std::iter::once(output.clone()),
        inputs.iter().cloned(),
        constraint_system.system(),
    );

    let mut variables_to_remove = output_reachable_variables
        .difference(&outside_variables)
        .collect::<HashSet<_>>();
    variables_to_remove.remove(&output);

    if variables_to_remove.len() <= 2 {
        println!(
            "Not enough variables to remove ({}), keeping the system as it is.",
            variables_to_remove.len()
        );
        return;
    }

    for constr in constraint_system.system().iter() {
        if constr
            .referenced_variables()
            .any(|var| variables_to_remove.contains(var))
        {
            assert!(constr.referenced_variables().all(|var| {
                inputs.contains(var) || var == &output || variables_to_remove.contains(var)
            }));
            println!("Can replace constraint: {constr}",);
        }
    }
    constraint_system.retain_algebraic_constraints(|constr| {
        !constr
            .referenced_variables()
            .any(|var| variables_to_remove.contains(var))
    });
    constraint_system.retain_bus_interactions(|bus_interaction| {
        !bus_interaction
            .referenced_variables()
            .any(|var| variables_to_remove.contains(var))
    });

    let output = GroupedExpression::from_unknown_variable(output);
    let output = if value == T::from(1) {
        output
    } else {
        GroupedExpression::one() - output
    };

    let sum_of_inputs = inputs
        .iter()
        .map(|v| GroupedExpression::from_unknown_variable(v.clone()))
        .reduce(|a, b| a + b)
        .unwrap();
    let sum_inv = GroupedExpression::from_unknown_variable(new_var());
    let new_constraints = vec![
        output.clone() * (GroupedExpression::one() - output.clone()),
        output.clone() * sum_of_inputs.clone(),
        output - GroupedExpression::one() - sum_inv.clone() * sum_of_inputs,
    ];
    constraint_system.extend(ConstraintSystem {
        algebraic_constraints: new_constraints,
        bus_interactions: vec![],
    });

    // TODO we could check if the new system still has the property we started looking for.
}

fn determine_and_remove_redundant_inputs<T: FieldElement, V: Clone + Ord + Hash + Display>(
    constraint_system: &IndexedConstraintSystem<T, V>,
    bus_interaction_handler: impl BusInteractionHandler<T> + IsBusStateful<T> + Clone,
    inputs: impl IntoIterator<Item = V>,
    output: V,
    value: T,
) -> Vec<V> {
    let inputs = inputs.into_iter().collect_vec();
    // An input 'i' is redundant if setting `var = 1 - value` and `x = 0` for all `x` in `inputs \ {i}` is still inconsistent.
    let mut redundant_input_indices = vec![];
    for (i, var) in inputs.iter().enumerate() {
        let assignments = inputs
            .iter()
            .enumerate()
            .filter(|&(j, _)| j != i && !redundant_input_indices.contains(&j))
            .map(|(_, var)| (var.clone(), T::from(0)))
            .chain([(output.clone(), T::from(1) - value)]);
        if solve_with_assignments(
            constraint_system,
            bus_interaction_handler.clone(),
            assignments,
        )
        .is_err()
        {
            println!("Input {var} is redundant, removing it from the set of inputs.");
            redundant_input_indices.push(i);
        }
    }

    inputs
        .iter()
        .enumerate()
        .filter(|(i, _)| !redundant_input_indices.contains(i))
        .map(|(_, v)| v.clone())
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
