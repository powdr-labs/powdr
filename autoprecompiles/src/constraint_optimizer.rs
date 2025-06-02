use std::collections::HashSet;

use itertools::Itertools;
use powdr_constraint_solver::{
    constraint_system::{BusInteraction, BusInteractionHandler, ConstraintSystem},
    indexed_constraint_system::apply_substitutions,
    quadratic_symbolic_expression::QuadraticSymbolicExpression,
    solver::Solver,
    symbolic_expression::SymbolicExpression,
};
use powdr_number::FieldElement;
use powdr_pilopt::{
    inliner::replace_constrained_witness_columns,
    qse_opt::{
        algebraic_to_quadratic_symbolic_expression, quadratic_symbolic_expression_to_algebraic,
        Variable,
    },
    simplify_expression,
};

use crate::{
    stats_logger::StatsLogger, SymbolicBusInteraction, SymbolicConstraint, SymbolicMachine,
};

/// Simplifies the constraints as much as possible.
/// This function is similar to powdr_pilopt::qse_opt::run_qse_optimization, except it:
/// - Runs on the entire constraint system, including bus interactions.
/// - Panics if the solver fails.
/// - Removes trivial constraints (e.g. `0 = 0` or bus interaction with multiplicity `0`)
///   from the constraint system.
/// - Calls `simplify_expression()` on the resulting expressions.
pub fn optimize_constraints<P: FieldElement>(
    symbolic_machine: SymbolicMachine<P>,
    bus_interaction_handler: impl BusInteractionHandler<P> + IsBusStateful<P> + Clone,
    degree_bound: usize,
    stats_logger: &mut StatsLogger,
) -> SymbolicMachine<P> {
    let constraint_system = symbolic_machine_to_constraint_system(symbolic_machine);

    let constraint_system =
        solver_based_optimization(constraint_system, bus_interaction_handler.clone());
    stats_logger.log("solver-based optimization", &constraint_system);

    let constraint_system =
        remove_disconnected_columns(constraint_system, bus_interaction_handler.clone());
    stats_logger.log("removing disconnected columns", &constraint_system);

    let constraint_system = replace_constrained_witness_columns(constraint_system, degree_bound);
    stats_logger.log("in-lining witness columns", &constraint_system);

    let constraint_system = remove_trivial_constraints(constraint_system);
    stats_logger.log("removing trivial constraints", &constraint_system);

    let constraint_system = remove_equal_constraints(constraint_system);
    stats_logger.log("removing equal constraints", &constraint_system);

    let constraint_system =
        remove_equal_bus_interactions(constraint_system, bus_interaction_handler);
    stats_logger.log("removing equal bus interactions", &constraint_system);

    constraint_system_to_symbolic_machine(constraint_system)
}

fn symbolic_machine_to_constraint_system<P: FieldElement>(
    symbolic_machine: SymbolicMachine<P>,
) -> ConstraintSystem<P, Variable> {
    ConstraintSystem {
        algebraic_constraints: symbolic_machine
            .constraints
            .iter()
            .map(|constraint| algebraic_to_quadratic_symbolic_expression(&constraint.expr))
            .collect(),
        bus_interactions: symbolic_machine
            .bus_interactions
            .iter()
            .map(symbolic_bus_interaction_to_bus_interaction)
            .collect(),
    }
}

fn constraint_system_to_symbolic_machine<P: FieldElement>(
    constraint_system: ConstraintSystem<P, Variable>,
) -> SymbolicMachine<P> {
    SymbolicMachine {
        constraints: constraint_system
            .algebraic_constraints
            .iter()
            .map(|constraint| SymbolicConstraint {
                expr: simplify_expression(quadratic_symbolic_expression_to_algebraic(constraint)),
            })
            .collect(),
        bus_interactions: constraint_system
            .bus_interactions
            .into_iter()
            .map(bus_interaction_to_symbolic_bus_interaction)
            .collect(),
    }
}

fn solver_based_optimization<T: FieldElement>(
    constraint_system: ConstraintSystem<T, Variable>,
    bus_interaction_handler: impl BusInteractionHandler<T>,
) -> ConstraintSystem<T, Variable> {
    let result = Solver::new(constraint_system.clone())
        .with_bus_interaction_handler(bus_interaction_handler)
        .solve()
        .map_err(|e| {
            panic!("Solver failed: {e:?}");
        })
        .unwrap();
    log::trace!("Solver figured out the following assignments:");
    for (var, value) in result.assignments.iter() {
        log::trace!("  {var} = {value}");
    }
    apply_substitutions(constraint_system, result.assignments)
}

/// Removes any columns that are not connected to *stateful* bus interactions (e.g. memory),
/// because those are the only way to interact with the rest of the zkVM (e.g. other
/// instructions).
/// We assume that the input constraint system is satisfiable. Because the removed constraints
/// are not connected to rest of the system, the prover can always satisfy them, so removing
/// them is safe.
/// Note that if there were unsatisfiable constraints, they might also be removed, which would
/// change the statement being proven.
fn remove_disconnected_columns<T: FieldElement>(
    constraint_system: ConstraintSystem<T, Variable>,
    bus_interaction_handler: impl IsBusStateful<T>,
) -> ConstraintSystem<T, Variable> {
    // Initialize variables_to_keep with any variables that appear in stateful bus interactions.
    let mut variables_to_keep = constraint_system
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
        for constraint in constraint_system.iter() {
            if constraint
                .referenced_variables()
                .any(|var| variables_to_keep.contains(var))
            {
                variables_to_keep.extend(constraint.referenced_variables().cloned());
            }
        }
        if variables_to_keep.len() == size_before {
            break;
        }
    }

    ConstraintSystem {
        algebraic_constraints: constraint_system
            .algebraic_constraints
            .into_iter()
            .filter(|constraint| {
                constraint
                    .referenced_variables()
                    .any(|var| variables_to_keep.contains(var))
            })
            .collect(),
        bus_interactions: constraint_system
            .bus_interactions
            .into_iter()
            .filter(|bus_interaction| {
                let bus_id = bus_interaction.bus_id.try_to_number().unwrap();
                let has_vars_to_keep = bus_interaction
                    .referenced_variables()
                    .any(|var| variables_to_keep.contains(var));
                // has_vars_to_keep would also be false for bus interactions containing only
                // constants, so we also check again whether it is stateful.
                bus_interaction_handler.is_stateful(bus_id) || has_vars_to_keep
            })
            .collect(),
    }
}

fn remove_trivial_constraints<P: FieldElement>(
    mut symbolic_machine: ConstraintSystem<P, Variable>,
) -> ConstraintSystem<P, Variable> {
    let zero = QuadraticSymbolicExpression::from(P::zero());
    symbolic_machine
        .algebraic_constraints
        .retain(|constraint| constraint != &zero);
    symbolic_machine
        .bus_interactions
        .retain(|bus_interaction| bus_interaction.multiplicity != zero);
    symbolic_machine
}

fn remove_equal_constraints<P: FieldElement>(
    mut symbolic_machine: ConstraintSystem<P, Variable>,
) -> ConstraintSystem<P, Variable> {
    symbolic_machine.algebraic_constraints = symbolic_machine
        .algebraic_constraints
        .into_iter()
        .unique()
        .collect();
    symbolic_machine
}

fn remove_equal_bus_interactions<P: FieldElement>(
    mut symbolic_machine: ConstraintSystem<P, Variable>,
    bus_interaction_handler: impl IsBusStateful<P>,
) -> ConstraintSystem<P, Variable> {
    let mut seen = HashSet::new();
    symbolic_machine.bus_interactions = symbolic_machine
        .bus_interactions
        .into_iter()
        .filter_map(|interaction| {
            // We only touch interactions with non-stateful buses.
            if let Some(bus_id) = interaction.bus_id.try_to_number() {
                if !bus_interaction_handler.is_stateful(bus_id) && !seen.insert(interaction.clone())
                {
                    return None;
                }
            }
            Some(interaction)
        })
        .collect();
    symbolic_machine
}

fn symbolic_bus_interaction_to_bus_interaction<P: FieldElement>(
    bus_interaction: &SymbolicBusInteraction<P>,
) -> BusInteraction<QuadraticSymbolicExpression<P, Variable>> {
    BusInteraction {
        bus_id: SymbolicExpression::Concrete(P::from(bus_interaction.id)).into(),
        payload: bus_interaction
            .args
            .iter()
            .map(|arg| algebraic_to_quadratic_symbolic_expression(arg))
            .collect(),
        multiplicity: algebraic_to_quadratic_symbolic_expression(&bus_interaction.mult),
    }
}

fn bus_interaction_to_symbolic_bus_interaction<P: FieldElement>(
    bus_interaction: BusInteraction<QuadraticSymbolicExpression<P, Variable>>,
) -> SymbolicBusInteraction<P> {
    // We set the bus_id to a constant in `bus_interaction_to_symbolic_bus_interaction`,
    // so this should always succeed.
    let id = bus_interaction
        .bus_id
        .try_to_number()
        .unwrap()
        .to_arbitrary_integer()
        .try_into()
        .unwrap();
    SymbolicBusInteraction {
        id,
        args: bus_interaction
            .payload
            .into_iter()
            .map(|arg| simplify_expression(quadratic_symbolic_expression_to_algebraic(&arg)))
            .collect(),
        mult: simplify_expression(quadratic_symbolic_expression_to_algebraic(
            &bus_interaction.multiplicity,
        )),
    }
}

pub trait IsBusStateful<T: FieldElement> {
    /// Returns true if the bus with the given ID is stateful, i.e., whether there is any
    /// interaction with the rest of the zkVM. Examples of stateful buses are memory and
    /// execution bridge. Examples of non-stateful buses are fixed lookups.
    fn is_stateful(&self, bus_id: T) -> bool;
}
