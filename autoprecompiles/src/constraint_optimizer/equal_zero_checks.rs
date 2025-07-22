use std::collections::{BTreeSet, HashSet};
use std::fmt::Display;
use std::hash::Hash;

use num_traits::One;
use powdr_constraint_solver::constraint_system::{BusInteractionHandler, ConstraintSystem};
use powdr_constraint_solver::grouped_expression::{GroupedExpression, RangeConstraintProvider};
use powdr_constraint_solver::indexed_constraint_system::IndexedConstraintSystem;
use powdr_constraint_solver::journaling_constraint_system::JournalingConstraintSystem;
use powdr_constraint_solver::range_constraint::RangeConstraint;
use powdr_constraint_solver::solver;
use powdr_number::FieldElement;

use crate::constraint_optimizer::reachability::reachable_variables_except_blocked;
use crate::constraint_optimizer::{variables_in_stateful_bus_interactions, IsBusStateful};

pub fn replace_equal_zero_checks<T: FieldElement, V: Clone + Ord + Hash + Display>(
    mut constraint_system: JournalingConstraintSystem<T, V>,
    bus_interaction_handler: impl BusInteractionHandler<T> + IsBusStateful<T> + Clone,
    new_var: &mut impl FnMut() -> V,
) -> JournalingConstraintSystem<T, V> {
    // TODO logging
    // println!(
    //     "\n----------------------------------\nReplacing equal zero checks in constraint system"
    // );
    let rc = solver::Solver::new(constraint_system.system().clone())
        .with_bus_interaction_handler(bus_interaction_handler.clone())
        .solve()
        .unwrap()
        .range_constraints;
    let binary_range_constraint = RangeConstraint::from_mask(1);
    let binary_variables = constraint_system
        .indexed_system()
        .unknown_variables()
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
    let inputs: BTreeSet<_> = zero_assigments(&solution).collect();
    if inputs.is_empty() {
        return;
    }
    // We know: if `var = value`, then `inputs` are all zero.
    // Now check that `var == 1 - value` is inconsistent with `inputs` all being zero.
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

    // Ok, we can replace the constraints by a potentially more efficient version.
    // Let's find out which are the constraints that we need to replace.
    // We do a reachability search starting from stateful bus interactions, but we stop
    // the search as soon as we reach an input or the output variable.
    // The variables that are not reachable in this sense can be removed, because they are only
    // connected to a stateful bus interaction via the input or output.
    let blocking_variables = inputs.iter().chain([&output]).cloned();
    let outside_variables = reachable_variables_except_blocked(
        variables_in_stateful_bus_interactions(
            constraint_system.system(),
            bus_interaction_handler.clone(),
        )
        .cloned(),
        blocking_variables,
        constraint_system.system(),
    );

    // If some of the inputs are not reachable via stateful bus interactions, they
    // are most likely redundant inputs that by chance share the property.
    let inputs = inputs
        .into_iter()
        .filter(|v| outside_variables.contains(v))
        .collect::<BTreeSet<_>>();

    // Let's verify the condition again.
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
        // Something is wrong here, but rather do nothing.
        return;
    }

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

    // Now that we have a reduce set of inputs, we can focus again on finding the
    // inner variables to remove.
    // We could still have constraints that are only connected to the inputs but not the output, which constrain
    // the inputs.
    // Those we should keep, so we again do a reachability search from the output.
    let output_reachable_variables = reachable_variables_except_blocked(
        std::iter::once(output.clone()),
        inputs.iter().cloned(),
        constraint_system.system(),
    );

    let mut variables_to_remove = output_reachable_variables
        .difference(&outside_variables)
        .collect::<HashSet<_>>();
    variables_to_remove.remove(&output);
    for input in &inputs {
        variables_to_remove.remove(input);
    }

    if variables_to_remove.len() <= 2 {
        return;
    }

    // We should be able to remove all constraints that contain
    // at least one variable in `variables_to_remove`.
    // All other variables in such a constraint should either be inputs,
    // the output or other variables we can remove.
    for constr in constraint_system.system().iter() {
        if constr
            .referenced_variables()
            .any(|var| variables_to_remove.contains(var))
        {
            assert!(constr.referenced_variables().all(|var| {
                inputs.contains(var) || var == &output || variables_to_remove.contains(var)
            }));
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

    // New we build the more efficient version of the function.
    let output_expr = GroupedExpression::from_unknown_variable(output.clone());
    let output_expr = if value == T::from(1) {
        output_expr
    } else {
        GroupedExpression::one() - output_expr
    };

    // We encode "output = 1 <=> all inputs are zero":

    let sum_of_inputs = inputs
        .iter()
        .map(|v| GroupedExpression::from_unknown_variable(v.clone()))
        .reduce(|a, b| a + b)
        .unwrap();
    let sum_inv = GroupedExpression::from_unknown_variable(new_var());
    let new_constraints = vec![
        output_expr.clone() * sum_of_inputs.clone(),
        output_expr - (GroupedExpression::one() - sum_inv.clone() * sum_of_inputs),
    ];
    constraint_system.extend(ConstraintSystem {
        algebraic_constraints: new_constraints,
        bus_interactions: vec![],
    });

    // Verify that the modified system still has the same property (optional)

    let Ok(solution) = solve_with_assignments(
        constraint_system.indexed_system(),
        bus_interaction_handler.clone(),
        [(output.clone(), value)],
    ) else {
        return;
    };
    let inputs_after_modification: BTreeSet<_> = zero_assigments(&solution).collect();
    assert!(inputs.iter().all(|v| inputs_after_modification.contains(v)));

    assert!(solve_with_assignments(
        constraint_system.indexed_system(),
        bus_interaction_handler.clone(),
        inputs
            .iter()
            .map(|v| (v.clone(), T::from(0)))
            .chain([(output.clone(), T::from(1) - value)]),
    )
    .is_err());
}

/// Runs the solver given a list of variable assignments.
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

/// Returns all variables that are assigned zero in the solution.
fn zero_assigments<T: FieldElement, V: Clone + Ord + Hash + Display>(
    solution: &solver::SolveResult<T, V>,
) -> impl Iterator<Item = V> + '_ {
    solution
        .assignments
        .iter()
        .filter_map(|(v, val)| (val.try_to_number()? == 0.into()).then_some(v.clone()))
}
