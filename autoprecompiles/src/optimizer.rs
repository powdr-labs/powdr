use itertools::Itertools;
use powdr_ast::analyzed::{AlgebraicReference, PolyID, PolynomialType};
use powdr_constraint_solver::{
    constraint_system::{BusInteraction, BusInteractionHandler, ConstraintSystem},
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

use crate::{BusInteractionKind, SymbolicBusInteraction, SymbolicConstraint, SymbolicMachine};

/// Simplifies the constraints as much as possible.
/// This function is similar to powdr_pilopt::qse_opt::run_qse_optimization, except it:
/// - Runs on the entire constraint system, including bus interactions.
/// - Panics if the solver fails.
/// - Removes trivial constraints (e.g. `0 = 0` or bus interaction with multiplicity `0`)
///   from the constraint system.
/// - Calls `simplify_expression()` on the resulting expressions.
pub fn optimize<P: FieldElement>(
    symbolic_machine: SymbolicMachine<P>,
    bus_interaction_handler: impl BusInteractionHandler<P>
        + ConcreteBusInteractionHandler<P>
        + 'static
        + Clone,
) -> SymbolicMachine<P> {
    let constraint_system = symbolic_machine_to_constraint_system(symbolic_machine);

    log_constraint_system_stats("Starting optimize()", &constraint_system);
    let constraint_system =
        solver_based_optimization(constraint_system, bus_interaction_handler.clone());
    log_constraint_system_stats("After solver-based optimization", &constraint_system);
    let constraint_system =
        remove_trivial_bus_interactions(constraint_system, bus_interaction_handler);
    log_constraint_system_stats(
        "After removing trivial bus interactions",
        &constraint_system,
    );
    let mut constraint_system = remove_trivial_constraints(constraint_system);
    log_constraint_system_stats("After removing trivial constraints", &constraint_system);
    replace_constrained_witness_columns(&mut constraint_system, 3);
    log_constraint_system_stats("After in-lining witness columns", &constraint_system);

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
    bus_interaction_handler: impl BusInteractionHandler<T> + 'static,
) -> ConstraintSystem<T, Variable> {
    let result = Solver::new(constraint_system)
        .with_bus_interaction_handler(Box::new(bus_interaction_handler))
        .solve()
        .map_err(|e| {
            panic!("Solver failed: {e:?}");
        })
        .unwrap();
    log::trace!("Solver figured out the following assignments:");
    for (var, value) in result.assignments.iter() {
        log::trace!("  {var} = {value}");
    }
    result.simplified_constraint_system
}

fn remove_trivial_bus_interactions<T: FieldElement>(
    constraint_system: ConstraintSystem<T, Variable>,
    bus_interaction_handler: impl ConcreteBusInteractionHandler<T> + 'static,
) -> ConstraintSystem<T, Variable> {
    let ConstraintSystem {
        algebraic_constraints,
        bus_interactions,
    } = constraint_system;

    ConstraintSystem {
        algebraic_constraints,
        bus_interactions: bus_interactions
            .into_iter()
            .filter_map(|bus_interaction| {
                if let Some(concrete_bus_interaction) =
                    try_to_concrete_bus_interaction(&bus_interaction)
                {
                    // If all values are concrete, we might be able to remove the bus interaction
                    match bus_interaction_handler
                        .handle_concrete_bus_interaction(concrete_bus_interaction)
                    {
                        ConcreteBusInteractionResult::AlwaysSatisfied => None,
                        ConcreteBusInteractionResult::HasSideEffects => Some(bus_interaction), // Here we still keep the original bus interaction
                        ConcreteBusInteractionResult::ViolatesBusRules => {
                            panic!("Bus interaction {bus_interaction:?} violates bus rules");
                        }
                    }
                } else {
                    // If any value is symbolic, we keep the bus interaction
                    Some(bus_interaction)
                }
            })
            .collect(),
    }
}

fn try_to_concrete_bus_interaction<T: FieldElement>(
    bus_interaction: &BusInteraction<QuadraticSymbolicExpression<T, Variable>>,
) -> Option<BusInteraction<T>> {
    let BusInteraction {
        bus_id,
        multiplicity,
        payload,
    } = bus_interaction;
    Some(BusInteraction {
        bus_id: bus_id.try_to_number()?,
        multiplicity: multiplicity.try_to_number()?,
        payload: payload
            .iter()
            .map(|v| v.try_to_number())
            .collect::<Option<Vec<_>>>()?,
    })
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
        // TODO: The kind of SymbolicBusInteraction is ignored, this field should be removed
        kind: BusInteractionKind::Send,
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

fn log_constraint_system_stats<P: FieldElement>(
    step: &str,
    constraint_system: &ConstraintSystem<P, Variable>,
) {
    let num_constraints = constraint_system.algebraic_constraints.len();
    let num_bus_interactions = constraint_system.bus_interactions.len();
    let num_witness_columns = constraint_system
        .algebraic_constraints
        .iter()
        .flat_map(|constraint| constraint.referenced_variables())
        .chain(
            constraint_system
                .bus_interactions
                .iter()
                .flat_map(|bus_interaction| bus_interaction.referenced_variables()),
        )
        .filter_map(|expr| {
            if let Variable::Reference(AlgebraicReference {
                poly_id:
                    PolyID {
                        ptype: PolynomialType::Committed,
                        id,
                    },
                ..
            }) = expr
            {
                Some(id)
            } else {
                None
            }
        })
        .unique()
        .count();
    log::info!("{step} - Constraints: {num_constraints}, Bus Interactions: {num_bus_interactions}, Witness Columns: {num_witness_columns}");
}

pub enum ConcreteBusInteractionResult {
    /// This bus interaction can always be matched
    AlwaysSatisfied,
    /// This bus interaction can never be matched
    ViolatesBusRules,
    /// This bus interaction might be satisfied at run-time,
    /// but has side-effects and cannot be removed
    HasSideEffects,
}

pub trait ConcreteBusInteractionHandler<T: FieldElement> {
    fn handle_concrete_bus_interaction(
        &self,
        bus_interaction: BusInteraction<T>,
    ) -> ConcreteBusInteractionResult;
}
