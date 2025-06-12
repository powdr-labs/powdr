use std::{
    collections::{BTreeMap, HashSet},
    fmt::Display,
    hash::Hash,
};

use inliner::DegreeBound;
use powdr_constraint_solver::{
    constraint_system::{BusInteraction, BusInteractionHandler, ConstraintSystem},
    inliner,
    journaling_constraint_system::JournalingConstraintSystem,
    quadratic_symbolic_expression::QuadraticSymbolicExpression,
    solver::Solver,
};
use powdr_number::FieldElement;
use std::fmt;

use crate::stats_logger::{IsWitnessColumn, StatsLogger};

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
/// - Calls `simplify_expression()` on the resulting expressions.
pub fn optimize_constraints<
    P: FieldElement,
    V: Ord + Clone + Eq + Hash + Display + IsWitnessColumn,
>(
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

#[derive(Clone, Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
enum Variable<V> {
    Variable(V),
    BusInteractionArg(usize, usize),
}

impl<V: Display> fmt::Display for Variable<V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Variable::Variable(v) => write!(f, "{v}"),
            Variable::BusInteractionArg(bus_index, field_index) => {
                write!(f, "BusInteractionArg({bus_index}, {field_index})")
            }
        }
    }
}

fn solver_based_optimization<T: FieldElement, V: Clone + Ord + Hash + Display>(
    mut constraint_system: JournalingConstraintSystem<T, V>,
    bus_interaction_handler: impl BusInteractionHandler<T>,
) -> Result<JournalingConstraintSystem<T, V>, Error> {
    let mut new_constraints = Vec::new();
    let mut bus_interaction_vars = BTreeMap::new();
    let bus_interactions = constraint_system
        .system()
        .bus_interactions
        .iter()
        .enumerate()
        .map(|(bus_interaction_index, bus_interaction)| {
            BusInteraction::from_iter(bus_interaction.fields().enumerate().map(
                |(field_index, expr)| {
                    let transformed_expr =
                        expr.transform_var_type(&mut |v| Variable::Variable(v.clone()));
                    let v = Variable::BusInteractionArg(bus_interaction_index, field_index);
                    new_constraints.push(
                        transformed_expr
                            - QuadraticSymbolicExpression::from_unknown_variable(v.clone()),
                    );
                    bus_interaction_vars.insert(v.clone(), expr.clone());
                    QuadraticSymbolicExpression::from_unknown_variable(v)
                },
            ))
        })
        .collect();
    let raw_constraint_system = ConstraintSystem {
        algebraic_constraints: constraint_system
            .system()
            .algebraic_constraints
            .iter()
            .map(|expr| expr.transform_var_type(&mut |v| Variable::Variable(v.clone())))
            .collect(),
        bus_interactions,
    };
    let result = Solver::new(raw_constraint_system)
        .with_bus_interaction_handler(bus_interaction_handler)
        .solve()?;
    log::trace!("Solver figured out the following assignments:");
    for (var, value) in result.assignments.iter() {
        log::trace!("  {var} = {value}");
    }
    // If a bus interaction field is equal to a constant, replace the definition by the constant.
    for (var, value) in result.assignments.iter() {
        if let Variable::BusInteractionArg(..) = var {
            if let Some(value) = value.try_to_number() {
                bus_interaction_vars.insert(var.clone(), value.into());
            }
        }
    }
    for (var, mut value) in result.assignments.into_iter() {
        if let Variable::Variable(v) = var {
            // Replace all bus interaction values with their definitions. Afterwards, there should not be any
            // `Variable::BusInteractionArg` left.
            // TODO: This loop seems inefficient.
            for (bus_interaction_var, value2) in bus_interaction_vars.iter() {
                value.substitute_by_unknown(
                    bus_interaction_var,
                    &value2.transform_var_type(&mut |v| Variable::Variable(v.clone())),
                );
            }
            // Unwrap the original variables.
            let value = value.transform_var_type(&mut |v| match v {
                Variable::Variable(v) => v.clone(),
                Variable::BusInteractionArg(..) => unreachable!(),
            });
            constraint_system.substitute_by_unknown(&v, &value);
        }
    }
    for (bus_interaction_var, value) in bus_interaction_vars {
        if let Variable::BusInteractionArg(bus_index, field_index) = bus_interaction_var {
            constraint_system.replace_bus_interaction_field(bus_index, field_index, value);
        } else {
            unreachable!();
        }
    }
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

fn remove_trivial_constraints<P: FieldElement, V: PartialEq>(
    mut constraint_system: JournalingConstraintSystem<P, V>,
) -> JournalingConstraintSystem<P, V> {
    let zero = QuadraticSymbolicExpression::from(P::zero());
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
