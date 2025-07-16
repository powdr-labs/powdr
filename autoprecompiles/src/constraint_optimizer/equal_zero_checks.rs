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

use crate::constraint_optimizer::IsBusStateful;

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
    redundant_inputs: Vec<V>,
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
        bus_interaction_handler,
        inputs,
        redundant_inputs: vec![],
        output: var.clone(),
        value,
    };

    // Some of the inputs are redundant, so try to reduce the size of the set.
    determine_and_set_redundant_inputs(&mut candidate);

    // We find the system by reachability.
    // TODO but after that we still need to do path search:
    // we should only remove columns that lie on a simple path
    // from input to output.

    println!("\n\nFound equal zero check for variable {var}:\n{var} = {value} if and only if all of {} are zero. Plus, the variables {} are also set to zero but are redundant.", 
        candidate.inputs.iter().format(", "),
        candidate.redundant_inputs.iter().format(", ")
    );
    for constr in candidate
        .constraint_system
        .constraints_referencing_variables(candidate.redundant_inputs.iter().cloned())
    {
        println!("  - {constr}");
    }

    // Now we need to find out which columns / constraints can be removed.
    // We do a reachability search starting from stateful bus interactions, but we stop
    // the search as soon as we reach an input or the output variable.
    // The variables that are not reachable in this sense can be removed, because they are only
    // connected to a stateful bus interaction via the input or output.

    if !is_system_isolated(&candidate) {
        println!("The candidate system is not isolated, so we cannot replace the equal zero check for variable {var} with a more efficient representation.");
        return;
    }

    println!("The candidate system is isolated, so we can replace the equal zero check for variable {var} with a more efficient representation.\nWe can remove the following constraints:");

    for constr in candidate
        .constraint_system
        .constraints_referencing_variables(candidate.redundant_inputs.iter().cloned())
    {
        println!("  - {constr}");
    }

    //    panic!();
}

fn determine_and_set_redundant_inputs<T: FieldElement, V: Clone + Ord + Hash + Display>(
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

    let mut irredundant_inputs = vec![];
    for (i, input) in candidate.inputs.iter().enumerate() {
        if redundant_input_indices.contains(&i) {
            candidate.redundant_inputs.push(input.clone());
        } else {
            irredundant_inputs.push(input.clone());
        }
    }
    candidate.inputs = irredundant_inputs;
}

fn is_system_isolated<T: FieldElement, V: Clone + Ord + Hash + Display>(
    candidate: &Candidate<T, V, impl BusInteractionHandler<T> + IsBusStateful<T> + Clone>,
) -> bool {
    let system_vars = candidate
        .inputs
        .iter()
        .chain(&candidate.redundant_inputs)
        .chain(std::iter::once(&candidate.output))
        .collect::<HashSet<_>>();
    for constr in candidate
        .constraint_system
        .constraints_referencing_variables(candidate.redundant_inputs.iter().cloned())
    {
        if !constr
            .referenced_variables()
            .all(|v| system_vars.contains(&v))
        {
            return false; // The constraint references a variable not in the system.
        }
        if let ConstraintRef::BusInteraction(bus) = constr {
            let Some(bus_id) = bus.bus_id.try_to_number() else {
                return false; // The bus interaction is not a number, so we cannot check its statefulness.
            };
            if candidate.bus_interaction_handler.is_stateful(bus_id) {
                return false;
            }
        }
    }
    true
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
