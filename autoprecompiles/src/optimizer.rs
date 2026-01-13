use std::collections::HashSet;
use std::fmt::Debug;
use std::hash::Hash;
use std::{collections::BTreeMap, fmt::Display};

use itertools::Itertools;
use powdr_constraint_solver::constraint_system::{
    AlgebraicConstraint, ComputationMethod, DerivedVariable,
};
use powdr_constraint_solver::indexed_constraint_system::IndexedConstraintSystem;
use powdr_constraint_solver::inliner::{self, inline_everything_below_degree_bound};
use powdr_constraint_solver::rule_based_optimizer::rule_based_optimization;
use powdr_constraint_solver::solver::new_solver;
use powdr_constraint_solver::{
    constraint_system::{BusInteraction, ConstraintSystem},
    grouped_expression::GroupedExpression,
};
use powdr_number::FieldElement;

use crate::constraint_optimizer::trivial_simplifications;
use crate::range_constraint_optimizer::optimize_range_constraints;
use crate::ColumnAllocator;
use crate::{
    adapter::Adapter,
    constraint_optimizer::optimize_constraints,
    expression::{AlgebraicExpression, AlgebraicReference},
    expression_conversion::{algebraic_to_grouped_expression, grouped_expression_to_algebraic},
    powdr::{self},
    stats_logger::{self, StatsLogger},
    BusMap, BusType, DegreeBound, SymbolicBusInteraction, SymbolicMachine,
};

/// Optimizes a given symbolic machine and returns an equivalent, but "simpler" one.
/// All constraints in the returned machine will respect the given degree bound.
/// New variables may be introduced in the process.
pub fn optimize<A: Adapter>(
    mut machine: SymbolicMachine<A::PowdrField>,
    bus_interaction_handler: A::BusInteractionHandler,
    degree_bound: DegreeBound,
    bus_map: &BusMap<A::CustomBusTypes>,
    mut column_allocator: ColumnAllocator,
    block_boundaries: &HashSet<usize>,
) -> Result<(SymbolicMachine<A::PowdrField>, ColumnAllocator), crate::constraint_optimizer::Error> {
    let mut stats_logger = StatsLogger::start(&machine);

    if let Some(exec_bus_id) = bus_map.get_bus_id(&BusType::ExecutionBridge) {
        machine = optimize_exec_bus(machine, exec_bus_id, block_boundaries);
        stats_logger.log("exec bus optimization", &machine);
    }

    let mut new_var = |name: &str| {
        let id = column_allocator.issue_next_poly_id();
        AlgebraicReference {
            // TODO is it a problem that we do not check the name to be unique?
            name: format!("{name}_{id}").into(),
            id,
        }
    };

    let constraint_system = symbolic_machine_to_constraint_system(machine);
    stats_logger.log("system construction", &constraint_system);

    let mut constraint_system: IndexedConstraintSystem<_, _> = constraint_system.into();
    stats_logger.log("indexing", &constraint_system);

    // We could run the rule system before ever constructing the solver.
    // Currently, it does not yet save time.
    // let mut constraint_system = rule_based_optimization(
    //     constraint_system,
    //     NoRangeConstraints,
    //     bus_interaction_handler.clone(),
    //     &mut new_var,
    //     // No degree bound given, i.e. only perform replacements that
    //     // do not increase the degree.
    //     None,
    // )
    // .0;
    stats_logger.log("rule-based optimization", &constraint_system);

    let mut solver = new_solver(
        constraint_system.system().clone(),
        bus_interaction_handler.clone(),
    );
    stats_logger.log("constructing the solver", &constraint_system);
    loop {
        let stats = stats_logger::Stats::from(&constraint_system);
        constraint_system = optimize_constraints::<_, _, A::MemoryBusInteraction<_>>(
            constraint_system,
            &mut solver,
            bus_interaction_handler.clone(),
            &mut stats_logger,
            bus_map.get_bus_id(&BusType::Memory),
            degree_bound,
            &mut new_var,
        )?
        .into();
        if stats == stats_logger::Stats::from(&constraint_system) {
            break;
        }
    }

    let constraint_system = inliner::replace_constrained_witness_columns(
        constraint_system,
        inline_everything_below_degree_bound(degree_bound),
    );
    stats_logger.log("inlining", &constraint_system);

    let (constraint_system, _) = rule_based_optimization(
        constraint_system,
        &solver,
        bus_interaction_handler.clone(),
        &mut new_var,
        Some(degree_bound),
    );
    // Note that the rest of the optimization does not benefit from optimizing range constraints,
    // so we only do it once at the end.
    let constraint_system = optimize_range_constraints(
        constraint_system.into(),
        bus_interaction_handler.clone(),
        degree_bound,
    );
    stats_logger.log("optimizing range constraints", &constraint_system);

    let constraint_system = trivial_simplifications(
        constraint_system.into(),
        bus_interaction_handler,
        &mut stats_logger,
    )
    .system()
    .clone();

    // Sanity check: Degree bound should be respected:
    for algebraic_constraint in &constraint_system.algebraic_constraints {
        assert!(
            algebraic_constraint.degree() <= degree_bound.identities,
            "Degree bound violated ({} > {}): {algebraic_constraint}",
            algebraic_constraint.degree(),
            degree_bound.identities
        );
    }
    for bus_interaction in &constraint_system.bus_interactions {
        for (i, expr) in bus_interaction.fields().enumerate() {
            assert!(
                expr.degree() <= degree_bound.identities,
                "Degree bound violated in field {i} ({} > {}): {bus_interaction}",
                expr.degree(),
                degree_bound.identities
            );
        }
    }

    // Sanity check: All PC lookups should be removed, because we'd only have constants on the LHS.
    let pc_lookup_bus_id = bus_map.get_bus_id(&BusType::PcLookup).unwrap();
    assert!(
        !constraint_system
            .bus_interactions
            .iter()
            .any(|b| b.bus_id
                == GroupedExpression::from_number(A::PowdrField::from(pc_lookup_bus_id))),
        "Expected all PC lookups to be removed."
    );
    Ok((
        constraint_system_to_symbolic_machine(constraint_system),
        column_allocator,
    ))
}

pub fn optimize_exec_bus<T: FieldElement>(
    mut machine: SymbolicMachine<T>,
    exec_bus_id: u64,
    block_boundaries: &HashSet<usize>,
) -> SymbolicMachine<T> {
    let mut first_seen = false;
    let mut receive = true;
    let mut latest_send = None;
    let mut instruction_idx = 0;
    let mut subs: BTreeMap<AlgebraicExpression<T>, AlgebraicExpression<T>> = Default::default();

    machine.bus_interactions.retain(|bus_int| {
        if bus_int.id != exec_bus_id {
            return true;
        }

        if receive {
            // TODO assert that mult matches -expr
        }

        // Keep the first receive
        let keep = if !first_seen {
            first_seen = true;
            true
        } else if !receive {
            // Save the latest send and remove the bus interaction
            let mut send = bus_int.clone();
            send.args = bus_int
                .args
                .iter()
                .map(|arg| {
                    let mut arg = arg.clone();
                    powdr::substitute_subexpressions(&mut arg, &subs);
                    simplify_expression(arg)
                })
                .collect();

            latest_send = Some(send);
            instruction_idx += 1;
            false
        } else if block_boundaries.contains(&instruction_idx) {
            // At a block boundary: don't substitute, just remove both send and receive.
            // The PC transition is checked at runtime via optimistic constraints.
            latest_send = None;
            false
        } else {
            // Equate the latest send to the new receive and remove the bus interaction
            for (bus_arg, send_arg) in bus_int
                .args
                .iter()
                .zip_eq(latest_send.as_ref().unwrap().args.iter())
            {
                subs.insert(bus_arg.clone(), send_arg.clone());
            }
            false
        };

        receive = !receive;

        keep
    });

    // Re-add the last send
    machine.bus_interactions.push(latest_send.unwrap());

    for c in &mut machine.constraints {
        powdr::substitute_subexpressions(&mut c.expr, &subs);
        c.expr = simplify_expression(c.expr.clone());
    }
    for b in &mut machine.bus_interactions {
        powdr::substitute_subexpressions(&mut b.mult, &subs);
        b.mult = simplify_expression(b.mult.clone());
        for a in &mut b.args {
            powdr::substitute_subexpressions(a, &subs);
            *a = simplify_expression(a.clone());
        }
    }

    machine
}

fn symbolic_machine_to_constraint_system<P: FieldElement>(
    symbolic_machine: SymbolicMachine<P>,
) -> ConstraintSystem<P, AlgebraicReference> {
    ConstraintSystem {
        algebraic_constraints: symbolic_machine
            .constraints
            .iter()
            .map(|constraint| {
                AlgebraicConstraint::assert_zero(algebraic_to_grouped_expression(&constraint.expr))
            })
            .collect(),
        bus_interactions: symbolic_machine
            .bus_interactions
            .iter()
            .map(symbolic_bus_interaction_to_bus_interaction)
            .collect(),
        derived_variables: symbolic_machine
            .derived_columns
            .iter()
            .map(|(v, method)| {
                let method = match method {
                    ComputationMethod::Constant(c) => ComputationMethod::Constant(*c),
                    ComputationMethod::QuotientOrZero(e1, e2) => ComputationMethod::QuotientOrZero(
                        algebraic_to_grouped_expression(e1),
                        algebraic_to_grouped_expression(e2),
                    ),
                };
                DerivedVariable {
                    variable: v.clone(),
                    computation_method: method,
                }
            })
            .collect(),
    }
}

fn constraint_system_to_symbolic_machine<P: FieldElement>(
    constraint_system: ConstraintSystem<P, AlgebraicReference>,
) -> SymbolicMachine<P> {
    SymbolicMachine {
        constraints: constraint_system
            .algebraic_constraints
            .into_iter()
            .map(|constraint| grouped_expression_to_algebraic(constraint.expression).into())
            .collect(),
        bus_interactions: constraint_system
            .bus_interactions
            .into_iter()
            .map(bus_interaction_to_symbolic_bus_interaction)
            .collect(),
        derived_columns: constraint_system
            .derived_variables
            .into_iter()
            .map(|derived_var| {
                let method = match derived_var.computation_method {
                    ComputationMethod::Constant(c) => ComputationMethod::Constant(c),
                    ComputationMethod::QuotientOrZero(e1, e2) => ComputationMethod::QuotientOrZero(
                        grouped_expression_to_algebraic(e1),
                        grouped_expression_to_algebraic(e2),
                    ),
                };
                (derived_var.variable, method)
            })
            .collect(),
    }
}

fn symbolic_bus_interaction_to_bus_interaction<P: FieldElement>(
    bus_interaction: &SymbolicBusInteraction<P>,
) -> BusInteraction<GroupedExpression<P, AlgebraicReference>> {
    BusInteraction {
        bus_id: GroupedExpression::from_number(P::from(bus_interaction.id)),
        payload: bus_interaction
            .args
            .iter()
            .map(|arg| algebraic_to_grouped_expression(arg))
            .collect(),
        multiplicity: algebraic_to_grouped_expression(&bus_interaction.mult),
    }
}

fn bus_interaction_to_symbolic_bus_interaction<P: FieldElement>(
    bus_interaction: BusInteraction<GroupedExpression<P, AlgebraicReference>>,
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
            .map(|arg| grouped_expression_to_algebraic(arg))
            .collect(),
        mult: grouped_expression_to_algebraic(bus_interaction.multiplicity),
    }
}

pub fn simplify_expression<T: FieldElement>(e: AlgebraicExpression<T>) -> AlgebraicExpression<T> {
    grouped_expression_to_algebraic(algebraic_to_grouped_expression(&e))
}

/// A wrapped variable: Either a regular variable or a bus interaction field.
#[derive(Clone, Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub enum Variable<V> {
    Variable(V),
    BusInteractionField(usize, usize),
}

impl<V: Display> Display for Variable<V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Variable::Variable(v) => write!(f, "{v}"),
            Variable::BusInteractionField(bus_index, field_index) => {
                write!(f, "BusInteractionField({bus_index}, {field_index})")
            }
        }
    }
}
