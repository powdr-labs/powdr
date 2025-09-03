use std::collections::{BTreeMap, BTreeSet, HashSet};
use std::fmt::Display;
use std::hash::Hash;

use itertools::Itertools;
use num_traits::One;
use powdr_constraint_solver::constraint_system::{
    AlgebraicConstraint, BusInteractionHandler, ConstraintSystem,
};
use powdr_constraint_solver::grouped_expression::{
    GroupedExpression, NoRangeConstraints, RangeConstraintProvider,
};
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
use crate::range_constraint_optimizer::RangeConstraintHandler;

/// Tries to find variables that represent a zero check on a conjunction of other variables
/// and replaces the involved constraints by a more efficient version.
pub fn replace_equal_zero_checks<T: FieldElement, V: Clone + Ord + Hash + Display>(
    mut constraint_system: IndexedConstraintSystem<T, V>,
    solver: &mut impl Solver<T, V>,
    bus_interaction_handler: impl BusInteractionHandler<T>
        + RangeConstraintHandler<T>
        + IsBusStateful<T>
        + Clone,
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
    bus_interaction_handler: impl BusInteractionHandler<T>
        + RangeConstraintHandler<T>
        + IsBusStateful<T> // TODO do we need this?
        + Clone,
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
        &inputs.iter().map(|v| (v.clone(), solver.get(v))).collect(),
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

/// Checks if the isolated system is indeed redundant given the inputs and output.
/// More formally, the system is satisfiable in the following two cases:
/// 1) if the inputs are all zero and the output is `value`
/// 2) if at least one input is not zero (but all satisfy the given range cosntraints)
///    and the output is `1 - value`
/// In particular, this holds for all input assignments, i.e. the system should
/// not impose any further restrictions on the inputs in the second case.
fn check_redundancy<T: FieldElement, V: Clone + Ord + Hash + Display>(
    isolated_system: &ConstraintSystem<T, V>,
    inputs: &BTreeSet<V>,
    output: &V,
    value: T,
    input_range_constraints: &BTreeMap<V, RangeConstraint<T>>,
    bus_interaction_handler: impl BusInteractionHandler<T>
        + RangeConstraintHandler<T>
        + IsBusStateful<T>
        + Clone,
) {
    let mut solver = solver::new_solver(isolated_system.clone(), bus_interaction_handler.clone());
    // TODO apply assignments.
    for (v, val) in solver.solve().unwrap() {
        println!("  - we already gott {v} = {val}");
    }
    let mut booleans = isolated_system
        .unknown_variables()
        .filter(|v| {
            let range = solver.get(v);
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
    for nonzero_var in inputs {
        println!("===================================================\n{nonzero_var} > 0\n==============================");
        let range_constraints = inputs
            .iter()
            .map(|v| {
                let rc = input_range_constraints[&v].clone();
                let rc = if v == nonzero_var {
                    rc.conjunction(&RangeConstraint::from_range(T::from(1), -T::from(1)))
                } else {
                    rc
                };
                (v.clone(), rc)
            })
            .collect();
        for result in
            get_all_possible_assignments(booleans.clone(), &solver).filter_map(|mut assignment| {
                assignment.insert(output.clone(), T::from(1) - value);
                is_satisfiable(
                    isolated_system,
                    assignment,
                    &range_constraints,
                    bus_interaction_handler.clone(),
                )
            })
        {
            if result.is_empty() {
                println!("SATISFIABLE!");
            } else {
                println!(
                    "  - satisfiable with restrictions: {}",
                    result
                        .iter()
                        .map(|(v, rc)| format!("{v}: {rc}"))
                        .format(", ")
                );
            }
        }
    }
    panic!();
}

/// Determines if the system is satisfiable.
/// If this returns `None`, the system is conflicting or we cannot determine satisfiability properly.
/// If it returns `Some(rc)`, then `rc` are the determined range constraints on variables
/// from `range_constraints` that are stricter than provided.
fn is_satisfiable<T: FieldElement, V: Clone + Ord + Hash + Display>(
    system: &ConstraintSystem<T, V>,
    assignment: BTreeMap<V, T>,
    range_constraints: &BTreeMap<V, RangeConstraint<T>>,
    bus_interaction_handler: impl BusInteractionHandler<T>
        + RangeConstraintHandler<T>
        + IsBusStateful<T>
        + Clone,
) -> Option<BTreeMap<V, RangeConstraint<T>>> {
    let mut system = IndexedConstraintSystem::from(system.clone());
    for (var, val) in assignment {
        system.substitute_by_known(&var, &val);
    }
    let mut solver = solver::new_solver(system.system().clone(), bus_interaction_handler.clone());
    for (v, rc) in range_constraints {
        solver.add_range_constraint(v, rc.clone());
    }
    let Ok(solution) = solver.solve() else {
        return None;
    };
    let mut output_rc: BTreeMap<_, _> = solution
        .iter()
        .filter_map(|(v, val)| {
            let rc = range_constraints.get(v)?;
            let conjunction = rc.conjunction(&val.range_constraint(&NoRangeConstraints));
            (conjunction != *rc).then_some((v.clone(), conjunction))
        })
        .collect();
    let system = apply_substitutions(system.into(), solution);
    let system = remove_trivial_constraints(system.into());
    let (system, rcs) =
        remove_range_constraint_bus_interactions(system.system().clone(), &bus_interaction_handler);
    // TODO this does return RCs on non-input variables. We can only ignore them if the system
    // is empty at the end!
    let rcs = rcs
        .into_iter()
        .filter_map(|(v, new_rc)| {
            let input_rc = range_constraints.get(&v)?;
            let conjunction = input_rc.conjunction(&new_rc);
            (conjunction != *input_rc).then_some((v.clone(), conjunction))
        })
        .collect_vec();
    for (v, rc) in rcs {
        let rc = output_rc
            .get(&v)
            .cloned()
            .unwrap_or_default()
            .conjunction(&rc);
        output_rc.insert(v, rc);
    }
    if system.algebraic_constraints.is_empty() && system.bus_interactions.is_empty() {
        println!("satisfiable");
        return Some(output_rc);
    } else {
        println!("Non-empty system:\n-----\n{system}\n-----");
    }
    None
}

fn remove_range_constraint_bus_interactions<
    T: FieldElement,
    V: Clone + Ord + Eq + Hash + Display,
>(
    mut system: ConstraintSystem<T, V>,
    bus_interaction_handler: &impl RangeConstraintHandler<T>,
) -> (ConstraintSystem<T, V>, BTreeMap<V, RangeConstraint<T>>) {
    // TODO this has code duplication with range_constraint_optimizer.
    let mut resulting_range_constraints = BTreeMap::<V, RangeConstraint<T>>::new();
    system.bus_interactions.retain(|bus_int| {
        if bus_int.multiplicity != GroupedExpression::from_number(T::one()) {
            return true;
        }
        match bus_interaction_handler.pure_range_constraints(bus_int) {
            Some(new_range_constraints) => {
                let mut to_constraint = vec![];
                for (expr, rc) in new_range_constraints {
                    if !expr.is_affine() {
                        // Keep the bus interaction
                        return true;
                    }
                    match expr.referenced_variables().count() {
                        0 => { /* TODO assert that it matches rc */ }
                        1 => {
                            let var = expr.referenced_variables().next().unwrap();
                            if expr.coefficient_of_variable(var).unwrap() != &T::one() {
                                // Keep the bus interaction
                                return true;
                            }
                            // This is lossles, at least for the range.
                            // TODO is it ok? If we model a mask?
                            // TODO we could check if undoing it results in the same.
                            let rc =
                                rc.combine_sum(&RangeConstraint::from_value(-*expr.components().2));
                            to_constraint.push((var.clone(), rc));
                        }
                        _ => {
                            //  Keep the bus interaction
                            return true;
                        }
                    }
                }
                for (v, rc) in to_constraint {
                    let rc = resulting_range_constraints
                        .get(&v)
                        .cloned()
                        .unwrap_or_default()
                        // TODO this conjunction might be lossy
                        .conjunction(&rc);
                    resulting_range_constraints.insert(v, rc);
                }
                false
            }
            None => true,
        }
    });
    (system, resulting_range_constraints)
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
