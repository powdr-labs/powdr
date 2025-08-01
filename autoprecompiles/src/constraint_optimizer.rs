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
    solver::Solver,
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
    solver: &mut impl Solver<P, V>,
    bus_interaction_handler: impl BusInteractionHandler<P> + IsBusStateful<P> + Clone,
    should_inline: impl Fn(&V, &GroupedExpression<P, V>, &IndexedConstraintSystem<P, V>) -> bool,
    stats_logger: &mut StatsLogger,
) -> Result<JournalingConstraintSystem<P, V>, Error> {
    let constraint_system = solver_based_optimization(constraint_system, solver)?;
    stats_logger.log("solver-based optimization", &constraint_system);

    let constraint_system =
        remove_free_variables(constraint_system, solver, bus_interaction_handler.clone());
    stats_logger.log("removing free variables", &constraint_system);

    let constraint_system =
        remove_disconnected_columns(constraint_system, solver, bus_interaction_handler.clone());
    stats_logger.log("removing disconnected columns", &constraint_system);

    // TODO should we remove inlined columns in the solver?
    // TODO should we inline here at all during solving (instead if only inside the solver)?
    let constraint_system =
        inliner::replace_constrained_witness_columns(constraint_system, should_inline);
    solver.add_algebraic_constraints(constraint_system.algebraic_constraints().cloned());
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
    solver: &mut impl Solver<T, V>,
) -> Result<JournalingConstraintSystem<T, V>, Error> {
    let assignments = solver.solve()?;
    log::trace!("Solver figured out the following assignments:");
    for (var, value) in assignments.iter() {
        log::trace!("  {var} = {value}");
    }
    constraint_system.apply_substitutions(assignments);
    Ok(constraint_system)
}

/// Removes free variables from the constraint system, under some conditions.
///
/// Motivation: Suppose there is a constraint `2 * foo = bar` and `foo` only appears in this constraint.
/// Then, if we assume that all constraints are satisfiable, the prover would be able to satisfy it for
/// any value of `bar` by solving for `foo`. Therefore, the constraint can be removed.
/// The same would be true for a *stateless* bus interaction, e.g. `[foo * bar] in [BYTES]`.
///
/// This function removes *some* constraints like this (see TODOs below).
fn remove_free_variables<T: FieldElement, V: Clone + Ord + Eq + Hash + Display>(
    mut constraint_system: JournalingConstraintSystem<T, V>,
    solver: &mut impl Solver<T, V>,
    bus_interaction_handler: impl IsBusStateful<T> + Clone,
) -> JournalingConstraintSystem<T, V> {
    let all_variables = constraint_system
        .system()
        .expressions()
        .flat_map(|expr| expr.referenced_unknown_variables())
        .cloned()
        .collect::<HashSet<_>>();

    let variables_to_delete = all_variables
        .iter()
        // Find variables that are referenced in exactly one constraint
        .filter_map(|variable| {
            constraint_system
                .indexed_system()
                .constraints_referencing_variables(once(variable.clone()))
                .exactly_one()
                .ok()
                .map(|constraint| (variable.clone(), constraint))
        })
        .filter(|(variable, constraint)| match constraint {
            // TODO: These constraints could be removed also if they are linear in the free variable.
            // The problem with this currently is that this removes constraints like
            // `writes_aux__prev_data__3_0 - BusInteractionField(15, 7)` (`writes_aux__prev_data__3_0` is a free variable)
            // which causes `remove_bus_interaction_variables` to fail, because it doesn't know the definition of the
            // bus interaction variable.
            ConstraintRef::AlgebraicConstraint(..) => false,
            ConstraintRef::BusInteraction(bus_interaction) => {
                let bus_id = bus_interaction.bus_id.try_to_number().unwrap();
                // Only stateless bus interactions can be removed.
                let is_stateless = !bus_interaction_handler.is_stateful(bus_id);
                // TODO: This is overly strict.
                // We assume that the bus interaction is satisfiable. Given that it is, there
                // will be at least one assignment of the payload fields that satisfies it.
                // If the prover has the freedom to choose each payload field, it can always find
                // a satisfying assignment.
                // This could be generalized to multiple unknown fields, but it would be more complicated,
                // because *each* field would need a *different* free variable.
                let has_one_unknown_field = bus_interaction
                    .payload
                    .iter()
                    .filter(|field| field.try_to_number().is_none())
                    .count()
                    == 1;
                // If the expression is linear in the free variable, the prover would be able to solve for it
                // to satisfy the constraint. Otherwise, this is not necessarily the case.
                // Note that if the above check is true, there will only be one field of degree > 0.
                let all_degrees_at_most_one = bus_interaction
                    .payload
                    .iter()
                    .all(|field| field.degree_of_variable(variable) <= 1);
                is_stateless && has_one_unknown_field && all_degrees_at_most_one
            }
        })
        .map(|(variable, _constraint)| variable.clone())
        .collect::<HashSet<_>>();

    let variables_to_keep = all_variables
        .difference(&variables_to_delete)
        .cloned()
        .collect::<HashSet<_>>();

    solver.retain_variables(&variables_to_keep);

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
fn remove_disconnected_columns<T: FieldElement, V: Clone + Ord + Eq + Hash + Display>(
    mut constraint_system: JournalingConstraintSystem<T, V>,
    solver: &mut impl Solver<T, V>,
    bus_interaction_handler: impl IsBusStateful<T> + Clone,
) -> JournalingConstraintSystem<T, V> {
    let initial_variables = variables_in_stateful_bus_interactions(
        constraint_system.system(),
        bus_interaction_handler.clone(),
    )
    .cloned();
    let variables_to_keep = reachable_variables(initial_variables, constraint_system.system());

    solver.retain_variables(&variables_to_keep);

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
