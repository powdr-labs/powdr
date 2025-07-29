use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    hash::Hash,
    iter::once,
};

use itertools::Itertools;
use num_traits::Zero;
use powdr_constraint_solver::{
    constraint_system::{BusInteractionHandler, ConstraintRef, ConstraintSystem},
    grouped_expression::GroupedExpression,
    indexed_constraint_system::IndexedConstraintSystem,
    inliner,
    journaling_constraint_system::JournalingConstraintSystem,
    solver::solve_system,
};
use powdr_number::FieldElement;

use crate::stats_logger::StatsLogger;

mod reachability;

use reachability::reachable_variables;

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
    should_inline: impl Fn(&V, &GroupedExpression<P, V>, &IndexedConstraintSystem<P, V>) -> bool,
    stats_logger: &mut StatsLogger,
) -> Result<JournalingConstraintSystem<P, V>, Error> {
    let constraint_system =
        solver_based_optimization(constraint_system, bus_interaction_handler.clone())?;
    stats_logger.log("solver-based optimization", &constraint_system);

    let constraint_system =
        remove_free_variables(constraint_system, bus_interaction_handler.clone());
    stats_logger.log("removing free variables", &constraint_system);

    let constraint_system =
        remove_disconnected_columns(constraint_system, bus_interaction_handler.clone());
    stats_logger.log("removing disconnected columns", &constraint_system);

    let constraint_system =
        inliner::replace_constrained_witness_columns(constraint_system, should_inline);
    stats_logger.log("in-lining witness columns", &constraint_system);

    let constraint_system = remove_trivial_constraints(constraint_system);
    stats_logger.log("removing trivial constraints", &constraint_system);

    let constraint_system =
        remove_equal_bus_interactions(constraint_system, bus_interaction_handler);
    stats_logger.log("removing equal bus interactions", &constraint_system);

    // TODO maybe we should keep learnt range constraints stored somewhere because
    // we might not be able to re-derive them if some constraints are missing.
    let constraint_system = remove_redundant_constraints(constraint_system);
    stats_logger.log("removing redundant constraints", &constraint_system);

    Ok(constraint_system)
}

fn solver_based_optimization<T: FieldElement, V: Clone + Ord + Hash + Display>(
    mut constraint_system: JournalingConstraintSystem<T, V>,
    bus_interaction_handler: impl BusInteractionHandler<T>,
) -> Result<JournalingConstraintSystem<T, V>, Error> {
    let assignments = solve_system(constraint_system.system().clone(), bus_interaction_handler)?;
    log::trace!("Solver figured out the following assignments:");
    for (var, value) in assignments.iter() {
        log::trace!("  {var} = {value}");
    }
    constraint_system.apply_substitutions(assignments);
    Ok(constraint_system)
}

fn remove_free_variables<T: FieldElement, V: Clone + Ord + Eq + Hash + Display>(
    mut constraint_system: JournalingConstraintSystem<T, V>,
    bus_interaction_handler: impl IsBusStateful<T> + Clone,
) -> JournalingConstraintSystem<T, V> {
    let all_variables = constraint_system
        .system()
        .expressions()
        .flat_map(|expr| expr.referenced_variables())
        .cloned()
        .collect::<HashSet<_>>();

    let variables_to_delete = all_variables
        .iter()
        .filter_map(|variable| {
            constraint_system
                .indexed_system()
                .constraints_referencing_variables(once(variable.clone()))
                .exactly_one()
                .ok()
                .map(|constraint| (variable.clone(), constraint))
        })
        .filter(|(_variable, constraint)| match constraint {
            // Even if the degree is 1, it could be connected to a stateful bus interaction!
            ConstraintRef::AlgebraicConstraint(..) => false,
            ConstraintRef::BusInteraction(bus_interaction) => {
                let bus_id = bus_interaction.bus_id.try_to_number().unwrap();
                !bus_interaction_handler.is_stateful(bus_id)
                    && bus_interaction
                        .fields()
                        .filter(|field| field.try_to_number().is_none())
                        .count()
                        == 1
            }
        })
        .map(|(variable, _constraint)| variable.clone())
        .collect::<HashSet<_>>();

    let variables_to_keep = all_variables
        .difference(&variables_to_delete)
        .cloned()
        .collect::<HashSet<_>>();

    // solver.retain_variables(&variables_to_keep);

    constraint_system.retain_algebraic_constraints(|constraint| {
        constraint
            .referenced_variables()
            .all(|var| variables_to_keep.contains(var))
    });

    constraint_system.retain_bus_interactions(|bus_interaction| {
        let bus_id = bus_interaction.bus_id.try_to_number().unwrap();
        bus_interaction_handler.is_stateful(bus_id)
            || bus_interaction
                .referenced_variables()
                .all(|var| variables_to_keep.contains(var))
    });

    constraint_system
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
    bus_interaction_handler: impl IsBusStateful<T> + Clone,
) -> JournalingConstraintSystem<T, V> {
    let initial_variables = variables_in_stateful_bus_interactions(
        constraint_system.system(),
        bus_interaction_handler.clone(),
    )
    .cloned();
    let variables_to_keep = reachable_variables(initial_variables, constraint_system.system());

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

/// Returns an iterator over all variables that are referenced in stateful bus interactions.
fn variables_in_stateful_bus_interactions<'a, P: FieldElement, V: Ord + Clone + Eq + Hash>(
    constraint_system: &'a ConstraintSystem<P, V>,
    bus_interaction_handler: impl IsBusStateful<P> + 'a,
) -> impl Iterator<Item = &'a V> {
    constraint_system
        .bus_interactions
        .iter()
        .filter(move |bus_interaction| {
            let bus_id = bus_interaction.bus_id.try_to_number().unwrap();
            bus_interaction_handler.is_stateful(bus_id)
        })
        .flat_map(|bus_interaction| bus_interaction.referenced_variables())
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

/// Removes constraints that are factors of other constraints.
fn remove_redundant_constraints<P: FieldElement, V: Clone + Ord + Hash + Display>(
    mut constraint_system: JournalingConstraintSystem<P, V>,
) -> JournalingConstraintSystem<P, V> {
    // Maps each factor to the set of constraints that contain it.
    let mut constraints_by_factor = HashMap::new();
    // Turns each constraint into a set of factors.
    let constraints_as_factors = constraint_system
        .algebraic_constraints()
        .enumerate()
        .map(|(i, c)| {
            let factors = c.to_factors();
            for f in &factors {
                constraints_by_factor
                    .entry(f.clone())
                    .or_insert_with(HashSet::new)
                    .insert(i);
            }
            factors
        })
        .collect_vec();

    let mut redundant_constraints = HashSet::<usize>::new();
    for (i, factors) in constraints_as_factors.iter().enumerate() {
        // Go through all factors `f` and compute the intersection of all
        // constraints in `constraints_by_factor[f]`. These constraints
        // are multiples of the current constraint, so they are redundant
        // if they are proper multiples, i.e. have at least one more factor.
        let mut redundant = factors
            .iter()
            .map(|f| constraints_by_factor[f].clone())
            .reduce(|a, b| a.intersection(&b).copied().collect())
            .unwrap();
        // Only remove constraints that have the same factors if their index
        // is larger than the current one.
        // Counting the factors is sufficient here.
        redundant.retain(|j| {
            let other_factors = &constraints_as_factors[*j];
            assert!(other_factors.len() >= factors.len());
            other_factors.len() > factors.len() || *j > i
        });
        redundant_constraints.extend(redundant);
    }
    let mut counter = 0;
    constraint_system.retain_algebraic_constraints(|_| {
        let retain = !redundant_constraints.contains(&counter);
        counter += 1;
        retain
    });
    constraint_system
}
