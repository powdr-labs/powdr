use std::collections::{BTreeMap, BTreeSet, HashSet};
use std::fmt::Display;
use std::hash::Hash;

use itertools::Itertools;
use num_traits::One;
use powdr_constraint_solver::constraint_system::{
    AlgebraicConstraint, BusInteractionHandler, ComputationMethod, ConstraintSystem,
};
use powdr_constraint_solver::grouped_expression::{
    GroupedExpression, NoRangeConstraints, RangeConstraintProvider,
};
use powdr_constraint_solver::indexed_constraint_system::{
    apply_substitutions, IndexedConstraintSystem,
};
use powdr_constraint_solver::range_constraint::RangeConstraint;
use powdr_constraint_solver::reachability::reachable_variables_except_blocked;
use powdr_constraint_solver::solver::{self, Solver, VariableAssignment};
use powdr_constraint_solver::system_splitter::split_system;
use powdr_constraint_solver::utils::get_all_possible_assignments;
use powdr_number::FieldElement;

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
    // To keep performance reasonable, we split the system at stateful bus interactions
    // into smaller sub-systems and optimize each of them separately.
    for subsystem in split_at_stateful_bus_interactions(
        constraint_system.clone(),
        bus_interaction_handler.clone(),
    ) {
        if subsystem.referenced_unknown_variables().count() > 200 {
            // Searching for equal zero checks in such a large
            // system would take too long.
            log::debug!(
                "Skipping equal zero check optimization for subsystem with {} variables",
                subsystem.referenced_unknown_variables().count()
            );
            continue;
        }
        let binary_variables = subsystem
            .referenced_unknown_variables()
            .filter(|v| solver.get(v) == binary_range_constraint)
            .cloned()
            .collect::<BTreeSet<_>>();
        for var in binary_variables {
            for value in [T::from(0), T::from(1)] {
                try_replace_equal_zero_check(
                    &mut constraint_system,
                    subsystem.clone().into(),
                    bus_interaction_handler.clone(),
                    solver,
                    new_var,
                    var.clone(),
                    value,
                );
            }
        }
    }
    constraint_system
}

/// Removes stateful bus interactions from the constraint system and then splits it into
/// independent sub-systems.
fn split_at_stateful_bus_interactions<T: FieldElement, V: Clone + Ord + Hash + Display>(
    mut constraint_system: IndexedConstraintSystem<T, V>,
    bus_interaction_handler: impl IsBusStateful<T> + Clone,
) -> Vec<ConstraintSystem<T, V>> {
    let mut stateful_bus_interactions = vec![];
    constraint_system.retain_bus_interactions(|bus_int| {
        if let Some(id) = bus_int.bus_id.try_to_number() {
            if bus_interaction_handler.is_stateful(id) {
                stateful_bus_interactions.push(bus_int.clone());
                return false;
            }
        }
        true
    });
    let stateful_bus_interactions_by_var = stateful_bus_interactions
        .iter()
        .enumerate()
        .flat_map(|(i, bus_int)| {
            bus_int
                .referenced_unknown_variables()
                .map(move |v| (v.clone(), i))
        })
        .into_group_map();
    split_system(constraint_system)
        .into_iter()
        .map(|mut subsystem| {
            let vars = subsystem
                .referenced_unknown_variables()
                .cloned()
                .collect::<BTreeSet<_>>();
            assert!(!vars.is_empty());
            // Re-add the stateful bus interactions that are connected to this subsystem.
            // This will lead to bus interactions potentially being added to multiple
            // subsystems.
            subsystem.bus_interactions.extend(
                vars.iter()
                    .flat_map(|v| stateful_bus_interactions_by_var.get(v))
                    .flatten()
                    .cloned()
                    .collect::<BTreeSet<_>>()
                    .into_iter()
                    .map(|i| stateful_bus_interactions[i].clone()),
            );
            subsystem
        })
        .collect_vec()
}

fn try_replace_equal_zero_check<T: FieldElement, V: Clone + Ord + Hash + Display>(
    constraint_system: &mut IndexedConstraintSystem<T, V>,
    subsystem: IndexedConstraintSystem<T, V>,
    bus_interaction_handler: impl BusInteractionHandler<T>
        + RangeConstraintHandler<T>
        + IsBusStateful<T> // TODO do we need this?
        + Clone,
    solver: &mut impl Solver<T, V>,
    new_var: &mut impl FnMut() -> V,
    output: V,
    value: T,
) {
    // First, we try to find input and output variables that satisfy the equal zero check property.
    let Ok(solution) = solve_with_assignments(
        &subsystem,
        bus_interaction_handler.clone(),
        [(output.clone(), value)],
    ) else {
        return;
    };
    let inputs: BTreeSet<_> = zero_assigments(&solution).collect();
    if inputs.is_empty() {
        return;
    }
    // Here we know: if `output = value`, then `inputs` are all zero.
    // Now check that `output = 1 - value` is inconsistent with `inputs` all being zero.
    if solve_with_assignments(
        &subsystem,
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
    // Here we know: `output = value` if and only if all variables in `inputs` are zero.
    log::debug!(
        "Candidate found: {output} == {value} <=> all of {{{}}} are zero",
        inputs.iter().format(", ")
    );

    // We might replace some constraints by a potentially more efficient version,
    // but we need to find out which are the constraints that we need to replace.
    // We do a reachability search starting from stateful bus interactions, but we stop
    // the search as soon as we reach an input or the output variable.
    // The variables that are not reachable in this sense lie "between" the inputs
    // and the output. They can be removed (and replaced by our more efficient
    // is-equal-zero constraint) because they are only connected to a stateful
    // bus interaction via the input or output.
    let blocking_variables = inputs.iter().chain([&output]).cloned();
    let outside_variables = reachable_variables_except_blocked(
        variables_in_stateful_bus_interactions(
            constraint_system.system(),
            bus_interaction_handler.clone(),
        )
        .cloned(),
        blocking_variables,
        constraint_system,
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
    // TODO we do not need this. If this property is false, we can still find
    // a better constraint that also models is-equal-zero.
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
        constraint_system,
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

    let mut isolated_system = ConstraintSystem {
        algebraic_constraints: vec![],
        bus_interactions: vec![],
        derived_variables: vec![],
    };
    constraint_system.retain_algebraic_constraints(|constr| {
        // Remove the constraint if it references a variable to remove
        // or if it only references the output (which is fully determined
        // by the algebraic constraints we will add).
        let remove = constr
            .referenced_unknown_variables()
            .any(|var| variables_to_remove.contains(var))
            || constr.referenced_unknown_variables().all(|v| v == &output);
        if remove {
            // Sanity check that we do not remove anything we should not.
            assert!(constr.referenced_unknown_variables().all(|var| {
                inputs.contains(var) || var == &output || variables_to_remove.contains(var)
            }));
            isolated_system.algebraic_constraints.push(constr.clone());
        }
        !remove
    });
    constraint_system.retain_bus_interactions(|bus_interaction| {
        let remove = bus_interaction
            .referenced_unknown_variables()
            .any(|var| variables_to_remove.contains(var));
        if remove {
            // Sanity check that we do not remove anything we should not.
            assert!(bus_interaction.referenced_unknown_variables().all(|var| {
                inputs.contains(var) || var == &output || variables_to_remove.contains(var)
            }));
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
    let sum_inv_var = new_var();
    let sum_inv = GroupedExpression::from_unknown_variable(sum_inv_var.clone());
    let new_constraints = vec![
        AlgebraicConstraint::assert_zero(output_expr.clone() * sum_of_inputs.clone()),
        AlgebraicConstraint::assert_eq(
            output_expr,
            GroupedExpression::one() - sum_inv.clone() * sum_of_inputs.clone(),
        ),
    ];
    constraint_system.add_algebraic_constraints(new_constraints.clone());
    constraint_system.add_derived_variable(
        sum_inv_var.clone(),
        ComputationMethod::InverseOrZero(sum_of_inputs),
    );
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
/// 2) if at least one input is not zero (but all satisfy the given range constraints)
///    and the output is `1 - value`j
///
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
) -> bool {
    let solver = solver::new_solver(isolated_system.clone(), bus_interaction_handler.clone());
    let mut booleans = isolated_system
        .referenced_unknown_variables()
        .filter(|v| {
            let range = solver.get(v);
            range == RangeConstraint::from_mask(1)
        })
        .cloned()
        .collect::<BTreeSet<_>>();
    booleans.remove(output);
    {
        if let Some(restrictions) = is_satisfiable(
            isolated_system,
            inputs
                .iter()
                .map(|v| (v.clone(), T::from(0)))
                .chain([(output.clone(), value)])
                .collect(),
            input_range_constraints,
            bus_interaction_handler.clone(),
        ) {
            if !restrictions.is_empty() {
                return false;
            }
        } else {
            return false;
        }
    }
    for nonzero_var in inputs {
        let range_constraints = inputs
            .iter()
            .map(|v| {
                let rc = input_range_constraints[v].clone();
                let rc = if v == nonzero_var {
                    rc.conjunction(&RangeConstraint::from_range(T::from(1), -T::from(1)))
                } else {
                    rc
                };
                (v.clone(), rc)
            })
            .collect();
        // TODO is there a way where we can avoid manually determining the booleans?
        let restrictions = get_all_possible_assignments(booleans.clone(), &solver)
            .filter_map(|mut assignment| {
                assignment.insert(output.clone(), T::from(1) - value);
                is_satisfiable(
                    isolated_system,
                    assignment,
                    &range_constraints,
                    bus_interaction_handler.clone(),
                )
            })
            .collect_vec();
        let restrictions = reduce_range_constraints(restrictions, &range_constraints);
        for result in restrictions {
            if !result.is_empty() {
                return false;
            }
        }
    }
    true
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
            improves_existing_range_constraint(
                v,
                &val.range_constraint(&NoRangeConstraints),
                range_constraints,
            )
        })
        .collect();
    let system = apply_substitutions(system.into(), solution);
    let system = inline_non_input_variables(system.into(), range_constraints);
    let system = remove_trivial_constraints(system);
    let (system, rcs) =
        remove_range_constraint_bus_interactions(system.system().clone(), &bus_interaction_handler);
    // TODO this does return RCs on non-input variables. We can only ignore them if the system
    // is empty at the end!
    let rcs = rcs
        .into_iter()
        .filter_map(|(v, new_rc)| {
            // TODO we should not filter out non-input RCs
            improves_existing_range_constraint(&v, &new_rc, range_constraints)
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
        Some(output_rc)
    } else {
        None
    }
}

fn inline_non_input_variables<T: FieldElement, V: Clone + Ord + Hash + Display>(
    mut system: IndexedConstraintSystem<T, V>,
    input_variables: &BTreeMap<V, RangeConstraint<T>>,
) -> IndexedConstraintSystem<T, V> {
    loop {
        let Some((v, substitution)) = system.algebraic_constraints().iter().find_map(|constr| {
            // If the constraint has exactly one non-input variable,
            // inline that one.
            let var = constr
                .referenced_unknown_variables()
                .filter(|v| !input_variables.contains_key(v))
                .exactly_one()
                .ok()?;
            let expr = constr.as_ref().try_solve_for(var)?;
            Some((var.clone(), expr))
        }) else {
            return system;
        };
        system.substitute_by_unknown(&v, &substitution);
    }
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
                let mut to_constrain = vec![];
                for (expr, rc) in new_range_constraints {
                    if !expr.is_affine() {
                        // Keep the bus interaction
                        return true;
                    }
                    match expr.referenced_unknown_variables().count() {
                        0 => { /* TODO assert that it matches rc */ }
                        1 => {
                            let var = expr.referenced_unknown_variables().next().unwrap();
                            if expr.coefficient_of_variable_in_affine_part(var).unwrap()
                                != &T::one()
                            {
                                // Keep the bus interaction
                                return true;
                            }
                            // This is lossles, at least for the range.
                            // TODO is it ok? If we model a mask?
                            // TODO we could check if undoing it results in the same.
                            let rc = rc.combine_sum(&RangeConstraint::from_value(
                                -*expr.constant_offset(),
                            ));
                            to_constrain.push((var.clone(), rc));
                        }
                        _ => {
                            //  Keep the bus interaction
                            return true;
                        }
                    }
                }
                for (v, rc) in to_constrain {
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

/// On input of a disjunction of range constraints, tries to reduce the number of items.
/// The `outer_rcs` are range constraints we know to hold "from the outside", so if
/// this function determines a range constraint on a variable to be a superset of
/// the one in `outer_rcs` it is considered unconstrained.
fn reduce_range_constraints<T: FieldElement, V: Clone + Ord + Hash + Display>(
    mut restrictions: Vec<BTreeMap<V, RangeConstraint<T>>>,
    outer_rcs: &BTreeMap<V, RangeConstraint<T>>,
) -> Vec<BTreeMap<V, RangeConstraint<T>>> {
    loop {
        let Some((i1, i2, replacement)) = restrictions
            .iter()
            .enumerate()
            .tuple_combinations()
            .find_map(|((i1, item1), (i2, item2))| {
                let combination = try_combine_range_constraint_pair(item1, item2, outer_rcs)?;
                Some((i1, i2, combination))
            })
        else {
            return restrictions;
        };
        restrictions = restrictions
            .into_iter()
            .enumerate()
            .filter(|(i, _)| *i != i1 && *i != i2)
            .map(|(_, r)| r)
            .collect();
        restrictions.push(replacement);
    }
}

fn try_combine_range_constraint_pair<T: FieldElement, V: Clone + Ord + Hash + Display>(
    item1: &BTreeMap<V, RangeConstraint<T>>,
    item2: &BTreeMap<V, RangeConstraint<T>>,
    outer_rcs: &BTreeMap<V, RangeConstraint<T>>,
) -> Option<BTreeMap<V, RangeConstraint<T>>> {
    // If the restrictions are on the same set of variables
    // but differ in exactly one variable and the difference
    // can be "concatenated" without loss, we can combine the two.
    if !item1.keys().eq(item2.keys()) {
        return None;
    }
    let (var, updated_rc) = item1
        .keys()
        .filter_map(|v| {
            let rc1 = &item1[v];
            let rc2 = &item2[v];
            if rc1 == rc2 {
                return None;
            }
            // TODO check that it is loss-less
            let rc = rc1.disjunction(rc2);
            if rc.conjunction(rc1) == *rc1 && rc2.conjunction(&rc) == *rc2 {
                Some((v, rc))
            } else {
                None
            }
        })
        .exactly_one()
        .ok()?;
    Some(
        item1
            .iter()
            .map(|(v, rc)| {
                if v == var {
                    (v.clone(), updated_rc.clone())
                } else {
                    (v.clone(), rc.clone())
                }
            })
            .filter_map(|(v, rc)| improves_existing_range_constraint(&v, &rc, outer_rcs))
            .collect(),
    )
}

/// Returns `Some(_)` if the given `rc` is tighter than the already known range
/// constraint on the variable `var`. In that case, returns the conjunction
/// with the already known range constraint.
/// If the variable is not present in `known_range_constraints`, or if
/// `rc` is not tighter, returns `None`.
fn improves_existing_range_constraint<T: FieldElement, V: Clone + Ord + Hash + Display>(
    var: &V,
    rc: &RangeConstraint<T>,
    known_range_constraints: &BTreeMap<V, RangeConstraint<T>>,
) -> Option<(V, RangeConstraint<T>)> {
    let known_rc = known_range_constraints.get(var)?;
    let conjunction = known_rc.conjunction(rc);
    (conjunction != *known_rc).then(|| (var.clone(), conjunction))
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
