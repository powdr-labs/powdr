use std::{collections::HashSet, fmt::Display, hash::Hash};

use inliner::DegreeBound;
use num_traits::Zero;
use powdr_constraint_solver::{
    constraint_system::BusInteractionHandler, grouped_expression::GroupedExpression, inliner,
    journaling_constraint_system::JournalingConstraintSystem, solver::solve_system,
};
use powdr_number::FieldElement;

use crate::stats_logger::StatsLogger;

#[derive(Debug)]
pub enum Error {
    ConstraintSolverError(powdr_constraint_solver::solver::Error),
}

impl From<powdr_constraint_solver::solver::Error> for Error {
    fn from(err: powdr_constraint_solver::solver::Error) -> Self {
        Error::ConstraintSolverError(err)
    }
}

/// Simplifies the constraints as much as possible.
/// This function is similar to powdr_pilopt::qse_opt::run_qse_optimization, except it:
/// - Runs on the entire constraint system, including bus interactions.
/// - Panics if the solver fails.
/// - Removes trivial constraints (e.g. `0 = 0` or bus interaction with multiplicity `0`)
///   from the constraint system.
pub fn optimize_constraints<P: FieldElement, V: Ord + Clone + Eq + Hash + Display>(
    constraint_system: JournalingConstraintSystem<P, V>,
    bus_interaction_handler: impl BusInteractionHandler<P> + IsBusStateful<P> + Clone,
    degree_bound: DegreeBound,
    stats_logger: &mut StatsLogger,
) -> Result<JournalingConstraintSystem<P, V>, Error> {
    let constraint_system =
        solver_based_optimization(constraint_system, bus_interaction_handler.clone())?;
    stats_logger.log("solver-based optimization", &constraint_system);

    let constraint_system =
        remove_disconnected_columns(constraint_system, bus_interaction_handler.clone());
    stats_logger.log("removing disconnected columns", &constraint_system);

    let constraint_system =
        inliner::replace_constrained_witness_columns(constraint_system, degree_bound);
    stats_logger.log("in-lining witness columns", &constraint_system);

    let constraint_system = remove_trivial_constraints(constraint_system);
    stats_logger.log("removing trivial constraints", &constraint_system);

    let constraint_system = remove_equal_constraints(constraint_system);
    stats_logger.log("removing equal constraints", &constraint_system);

    let constraint_system =
        remove_equal_bus_interactions(constraint_system, bus_interaction_handler);
    stats_logger.log("removing equal bus interactions", &constraint_system);

    Ok(constraint_system)
}

fn solver_based_optimization<T: FieldElement, V: Clone + Ord + Hash + Display>(
    mut constraint_system: JournalingConstraintSystem<T, V>,
    bus_interaction_handler: impl BusInteractionHandler<T>,
) -> Result<JournalingConstraintSystem<T, V>, Error> {
    let result = solve_system(constraint_system.system().clone(), bus_interaction_handler)?;
    log::trace!("Solver figured out the following assignments:");
    for (var, value) in result.assignments.iter() {
        log::trace!("  {var} = {value}");
    }
    constraint_system.apply_substitutions(result.assignments);
    // TODO could we somehow get rid of this special case by keeping
    // bus interaction field variables in the system for longer?
    constraint_system.apply_bus_field_assignments(result.bus_field_assignments);
    Ok(constraint_system)
}

/// Removes any columns that are not connected to *stateful* bus interactions (e.g. memory),
/// because those are the only way to interact with the rest of the zkVM (e.g. other
/// instructions).
/// We assume that the input constraint system is satisfiable. Because the removed constraints
/// are not connected to rest of the system, the prover can always satisfy them, so removing
/// them is safe.
/// Note that if there were unsatisfiable constraints, they might also be removed, which would
/// change the statement being proven.
fn remove_disconnected_columns<T: FieldElement, V: Clone + Ord + Hash + Display>(
    mut constraint_system: JournalingConstraintSystem<T, V>,
    bus_interaction_handler: impl IsBusStateful<T>,
) -> JournalingConstraintSystem<T, V> {
    // Initialize variables_to_keep with any variables that appear in stateful bus interactions.
    let mut variables_to_keep = constraint_system
        .system()
        .bus_interactions
        .iter()
        .filter(|bus_interaction| {
            let bus_id = bus_interaction.bus_id.try_to_number().unwrap();
            bus_interaction_handler.is_stateful(bus_id)
        })
        .flat_map(|bus_interaction| bus_interaction.referenced_variables())
        .cloned()
        .collect::<HashSet<_>>();

    // Any variable that is connected to a variable in variables_to_keep must also be kept.
    loop {
        let size_before = variables_to_keep.len();
        for expr in constraint_system.system().iter() {
            if expr
                .referenced_variables()
                .any(|var| variables_to_keep.contains(var))
            {
                variables_to_keep.extend(expr.referenced_variables().cloned());
            }
        }
        if variables_to_keep.len() == size_before {
            break;
        }
    }

    constraint_system.retain_algebraic_constraints(|constraint| {
        constraint
            .referenced_variables()
            .any(|var| variables_to_keep.contains(var))
    });

    constraint_system.retain_bus_interactions(|bus_interaction| {
        let bus_id = bus_interaction.bus_id.try_to_number().unwrap();
        let has_vars_to_keep = bus_interaction
            .referenced_variables()
            .any(|var| variables_to_keep.contains(var));
        // has_vars_to_keep would also be false for bus interactions containing only
        // constants, so we also check again whether it is stateful.
        bus_interaction_handler.is_stateful(bus_id) || has_vars_to_keep
    });

    constraint_system
}

fn remove_trivial_constraints<P: FieldElement, V: PartialEq + Clone + Hash + Ord>(
    mut constraint_system: JournalingConstraintSystem<P, V>,
) -> JournalingConstraintSystem<P, V> {
    let zero = GroupedExpression::zero();
    constraint_system.retain_algebraic_constraints(|constraint| constraint != &zero);
    constraint_system
        .retain_bus_interactions(|bus_interaction| bus_interaction.multiplicity != zero);
    constraint_system
}

fn remove_equal_constraints<P: FieldElement, V: Eq + Hash + Clone>(
    mut constraint_system: JournalingConstraintSystem<P, V>,
) -> JournalingConstraintSystem<P, V> {
    let mut seen = HashSet::new();
    constraint_system.retain_algebraic_constraints(|constraint| seen.insert(constraint.clone()));
    constraint_system
}

fn remove_equal_bus_interactions<P: FieldElement, V: Ord + Clone + Eq + Hash>(
    mut constraint_system: JournalingConstraintSystem<P, V>,
    bus_interaction_handler: impl IsBusStateful<P>,
) -> JournalingConstraintSystem<P, V> {
    let mut seen = HashSet::new();
    constraint_system.retain_bus_interactions(|interaction| {
        // We only touch interactions with non-stateful buses.
        if let Some(bus_id) = interaction.bus_id.try_to_number() {
            if !bus_interaction_handler.is_stateful(bus_id) && !seen.insert(interaction.clone()) {
                return false;
            }
        }
        true
    });
    constraint_system
}

pub trait IsBusStateful<T: FieldElement> {
    /// Returns true if the bus with the given ID is stateful, i.e., whether there is any
    /// interaction with the rest of the zkVM. Examples of stateful buses are memory and
    /// execution bridge. Examples of non-stateful buses are fixed lookups.
    fn is_stateful(&self, bus_id: T) -> bool;
}
