use std::collections::{BTreeSet, HashSet};
use std::fmt::Display;
use std::hash::Hash;

use itertools::Itertools;
use num_traits::One;
use powdr_constraint_solver::constraint_system::{
    AlgebraicConstraint, BusInteractionHandler, ConstraintSystem,
};
use powdr_constraint_solver::grouped_expression::GroupedExpression;
use powdr_constraint_solver::indexed_constraint_system::{
    apply_substitutions, IndexedConstraintSystem,
};
use powdr_constraint_solver::range_constraint::RangeConstraint;
use powdr_constraint_solver::solver::{self, Solver, VariableAssignment};
use powdr_constraint_solver::utils::get_all_possible_assignments;
use powdr_number::FieldElement;

use crate::constraint_optimizer::reachability::reachable_variables_except_blocked;
use crate::constraint_optimizer::{
    remove_trivial_constraints, variables_in_stateful_bus_interactions, IsBusStateful,
};

/// Tries to find variables that represent a zero check on a conjunction of other variables
/// and replaces the involved constraints by a more efficient version.
pub fn replace_equal_zero_checks<T: FieldElement, V: Clone + Ord + Hash + Display>(
    mut constraint_system: IndexedConstraintSystem<T, V>,
    solver: &mut impl Solver<T, V>,
    bus_interaction_handler: impl BusInteractionHandler<T> + IsBusStateful<T> + Clone,
    new_var: &mut impl FnMut() -> V,
) -> IndexedConstraintSystem<T, V> {
    let binary_range_constraint = RangeConstraint::from_mask(1);
    let binary_variables = constraint_system
        .unknown_variables()
        .filter(|v| solver.get(v) == binary_range_constraint)
        .cloned()
        .collect::<BTreeSet<_>>();
    for var in binary_variables {
        for value in [T::from(0), T::from(1)] {
            try_replace_equal_zero_check(
                &mut constraint_system,
                bus_interaction_handler.clone(),
                solver,
                new_var,
                var.clone(),
                value,
            );
        }
    }
    constraint_system
}

fn try_replace_equal_zero_check<T: FieldElement, V: Clone + Ord + Hash + Display>(
    constraint_system: &mut IndexedConstraintSystem<T, V>,
    bus_interaction_handler: impl BusInteractionHandler<T> + IsBusStateful<T> + Clone,
    solver: &mut impl Solver<T, V>,
    new_var: &mut impl FnMut() -> V,
    output: V,
    value: T,
) {
    let Ok(solution) = solve_with_assignments(
        constraint_system,
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
        constraint_system,
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
        constraint_system,
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
        .map(|v| solver.get(v))
        .reduce(|a, b| a.disjunction(&b))
        .unwrap()
        .range()
        .0;
    if min_input != T::from(0) {
        return;
    }
    let sum_of_inputs = inputs
        .iter()
        .map(|v| solver.get(v))
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

    let mut isolated_system = ConstraintSystem {
        algebraic_constraints: vec![],
        bus_interactions: vec![],
    };
    constraint_system.retain_algebraic_constraints(|constr| {
        // Remove the constraint if it references a variable to remove
        // or if it only references the output (which is fully determined
        // by the algebraic constraints we will add).
        let remove = constr
            .referenced_variables()
            .any(|var| variables_to_remove.contains(var))
            || constr.referenced_variables().all(|v| v == &output);
        if remove {
            isolated_system.algebraic_constraints.push(constr.clone());
        }
        !remove
    });
    constraint_system.retain_bus_interactions(|bus_interaction| {
        let remove = bus_interaction
            .referenced_variables()
            .any(|var| variables_to_remove.contains(var));
        if remove {
            isolated_system
                .bus_interactions
                .push(bus_interaction.clone());
        }
        !remove
    });
    check_redundancy(
        &isolated_system,
        &inputs,
        &output,
        value,
        solver,
        bus_interaction_handler.clone(),
    );

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
        AlgebraicConstraint::assert_zero(output_expr.clone() * sum_of_inputs.clone()),
        AlgebraicConstraint::assert_eq(
            output_expr,
            GroupedExpression::one() - sum_inv.clone() * sum_of_inputs,
        ),
    ];
    constraint_system.add_algebraic_constraints(new_constraints.clone());
    solver.add_algebraic_constraints(new_constraints);

    // Verify that the modified system still has the same property (optional)

    let Ok(solution) = solve_with_assignments(
        constraint_system,
        bus_interaction_handler.clone(),
        [(output.clone(), value)],
    ) else {
        return;
    };
    let inputs_after_modification: BTreeSet<_> = zero_assigments(&solution).collect();
    assert!(inputs.iter().all(|v| inputs_after_modification.contains(v)));

    assert!(solve_with_assignments(
        constraint_system,
        bus_interaction_handler.clone(),
        inputs
            .iter()
            .map(|v| (v.clone(), T::from(0)))
            .chain([(output.clone(), T::from(1) - value)]),
    )
    .is_err());
}

fn check_redundancy<T: FieldElement, V: Clone + Ord + Hash + Display>(
    isolated_system: &ConstraintSystem<T, V>,
    inputs: &BTreeSet<V>,
    output: &V,
    value: T,
    original_solver: &mut impl Solver<T, V>,
    bus_interaction_handler: impl BusInteractionHandler<T> + IsBusStateful<T> + Clone,
) {
    let mut booleans = isolated_system
        .unknown_variables()
        .filter(|v| {
            let range = original_solver.get(v);
            range == RangeConstraint::from_mask(1)
        })
        .cloned()
        .collect::<BTreeSet<_>>();
    booleans.remove(output);
    // TODO if we try the following:
    //
    println!("Checking\n-------------------------\n{isolated_system}--------------------");
    println!("Booleans: {}", booleans.iter().format(", "));
    // {
    //     println!("All zero:");
    //     let mut system = IndexedConstraintSystem::from(isolated_system.clone());
    //     for var in inputs {
    //         system.substitute_by_known(var, &T::from(0));
    //     }
    //     system.substitute_by_known(output, &value);

    //     let solution =
    //         solve_with_assignments(&system.clone().into(), bus_interaction_handler.clone(), [])
    //             .unwrap();
    //     let system = apply_substitutions(system.into(), solution);
    //     let system = remove_trivial_constraints(system.into());
    //     println!("{system}");
    // }
    for nonzero_index in 0..inputs.len() {
        println!("===================================================\nv_{nonzero_index} > 0\n==============================");
        let mut system = IndexedConstraintSystem::from(isolated_system.clone());
        let nonzero_variable = inputs.iter().skip(nonzero_index).next().unwrap().clone();
        // for var in inputs.iter().skip(1) {
        //     system.substitute_by_known(var, &T::from(0));
        // }
        system.substitute_by_known(output, &(T::from(1) - value));
        for assignment in get_all_possible_assignments(booleans.clone(), original_solver) {
            // TODO It is OK if we find at least one assignment that is non-conflicting
            // and leads to a trivial system and does not constrain the inputs
            // beyond the provided range constraints.
            let mut system = system.clone();
            for (var, val) in assignment {
                system.substitute_by_known(&var, &val);
            }
            let mut solver =
                solver::new_solver(system.system().clone(), bus_interaction_handler.clone());
            // TODO this range constraint does not work for all cases.
            solver.add_range_constraint(
                &nonzero_variable,
                RangeConstraint::from_range(T::from(2), T::from(0xff)),
            );
            let Ok(solution) = solver.solve() else {
                //                println!("  - lead to conflict");
                continue;
            };
            for (v, val) in &solution {
                if inputs.contains(v) {
                    println!("  - input {v} constrained to {val}");
                }
            }
            let system = apply_substitutions(system.into(), solution);
            let system = remove_trivial_constraints(system.into());
            println!("--------------------");
            println!("{system}");
            println!("--------------------");
        }
    }
    panic!();
}

/// Runs the solver given a list of variable assignments.
fn solve_with_assignments<T: FieldElement, V: Clone + Ord + Hash + Display>(
    constraint_system: &IndexedConstraintSystem<T, V>,
    bus_interaction_handler: impl BusInteractionHandler<T> + IsBusStateful<T> + Clone,
    assignments: impl IntoIterator<Item = (V, T)>,
) -> Result<Vec<VariableAssignment<T, V>>, solver::Error> {
    let mut system = constraint_system.clone();
    for (var, value) in assignments {
        system.substitute_by_known(&var, &value);
    }
    solver::solve_system(system.system().clone(), bus_interaction_handler)
}

/// Returns all variables that are assigned zero in the solution.
fn zero_assigments<T: FieldElement, V: Clone + Ord + Hash + Display>(
    solution: &[VariableAssignment<T, V>],
) -> impl Iterator<Item = V> + '_ {
    solution
        .iter()
        .filter_map(|(v, val)| (val.try_to_number()? == 0.into()).then_some(v.clone()))
}
