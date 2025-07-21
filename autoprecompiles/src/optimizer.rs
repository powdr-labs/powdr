use std::collections::BTreeMap;

use itertools::Itertools;
use powdr_constraint_solver::{
    constraint_system::{BusInteraction, BusInteractionHandler, ConstraintSystem},
    grouped_expression::{GroupedExpression, NoRangeConstraints},
    journaling_constraint_system::JournalingConstraintSystem,
};
use powdr_number::FieldElement;

use crate::{
    bitwise_lookup_optimizer::optimize_bitwise_lookup,
    constraint_optimizer::{optimize_constraints, IsBusStateful},
    expression::{AlgebraicExpression, AlgebraicReference},
    expression_conversion::{algebraic_to_grouped_expression, grouped_expression_to_algebraic},
    memory_optimizer::{check_register_operation_consistency, optimize_memory},
    powdr::{self},
    stats_logger::{self, StatsLogger},
    BusMap, BusType, DegreeBound, SymbolicBusInteraction, SymbolicConstraint, SymbolicMachine,
};

pub fn optimize<T: FieldElement>(
    mut machine: SymbolicMachine<T>,
    bus_interaction_handler: impl BusInteractionHandler<T> + IsBusStateful<T> + Clone,
    degree_bound: DegreeBound,
    bus_map: &BusMap,
) -> Result<SymbolicMachine<T>, crate::constraint_optimizer::Error> {
    let mut stats_logger = StatsLogger::start(&machine);

    if let Some(exec_bus_id) = bus_map.get_bus_id(&BusType::ExecutionBridge) {
        machine = optimize_exec_bus(machine, exec_bus_id);
        stats_logger.log("exec bus optimization", &machine);
    }

    let mut constraint_system = symbolic_machine_to_constraint_system(machine);

    loop {
        let stats = stats_logger::Stats::from(&constraint_system);
        constraint_system = optimization_loop_iteration(
            constraint_system,
            bus_interaction_handler.clone(),
            degree_bound,
            &mut stats_logger,
            bus_map,
        )?;
        if stats == stats_logger::Stats::from(&constraint_system) {
            // Sanity check: All PC lookups should be removed, because we'd only have constants on the LHS.
            let pc_lookup_bus_id = bus_map.get_bus_id(&BusType::PcLookup).unwrap();
            assert!(
                !constraint_system
                    .bus_interactions
                    .iter()
                    .any(|b| b.bus_id == GroupedExpression::from_number(T::from(pc_lookup_bus_id))),
                "Expected all PC lookups to be removed."
            );

            return Ok(constraint_system_to_symbolic_machine(constraint_system));
        }
    }
}

fn optimization_loop_iteration<T: FieldElement>(
    constraint_system: ConstraintSystem<T, AlgebraicReference>,
    bus_interaction_handler: impl BusInteractionHandler<T> + IsBusStateful<T> + Clone,
    degree_bound: DegreeBound,
    stats_logger: &mut StatsLogger,
    bus_map: &BusMap,
) -> Result<ConstraintSystem<T, AlgebraicReference>, crate::constraint_optimizer::Error> {
    let constraint_system = JournalingConstraintSystem::from(constraint_system);
    let constraint_system = optimize_constraints(
        constraint_system,
        bus_interaction_handler.clone(),
        degree_bound,
        stats_logger,
    )?;
    let constraint_system = constraint_system.system().clone();
    let constraint_system = if let Some(memory_bus_id) = bus_map.get_bus_id(&BusType::Memory) {
        let constraint_system =
            optimize_memory(constraint_system, memory_bus_id, NoRangeConstraints);
        assert!(check_register_operation_consistency(
            &constraint_system,
            memory_bus_id
        ));
        stats_logger.log("memory optimization", &constraint_system);
        constraint_system
    } else {
        constraint_system
    };

    let system = if let Some(bitwise_bus_id) = bus_map.get_bus_id(&BusType::BitwiseLookup) {
        let system = optimize_bitwise_lookup(
            constraint_system,
            bitwise_bus_id,
            bus_interaction_handler.clone(),
        );
        stats_logger.log("optimizing bitwise lookup", &system);
        system
    } else {
        constraint_system
    };

    Ok(system)
}

pub fn optimize_exec_bus<T: FieldElement>(
    mut machine: SymbolicMachine<T>,
    exec_bus_id: u64,
) -> SymbolicMachine<T> {
    let mut first_seen = false;
    let mut receive = true;
    let mut latest_send = None;
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
                    powdr::substitute_algebraic_algebraic(&mut arg, &subs);
                    simplify_expression(arg)
                })
                .collect();

            latest_send = Some(send);
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
        powdr::substitute_algebraic_algebraic(&mut c.expr, &subs);
        c.expr = simplify_expression(c.expr.clone());
    }
    for b in &mut machine.bus_interactions {
        powdr::substitute_algebraic_algebraic(&mut b.mult, &subs);
        b.mult = simplify_expression(b.mult.clone());
        for a in &mut b.args {
            powdr::substitute_algebraic_algebraic(a, &subs);
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
            .map(|constraint| algebraic_to_grouped_expression(&constraint.expr))
            .collect(),
        bus_interactions: symbolic_machine
            .bus_interactions
            .iter()
            .map(symbolic_bus_interaction_to_bus_interaction)
            .collect(),
    }
}

fn constraint_system_to_symbolic_machine<P: FieldElement>(
    constraint_system: ConstraintSystem<P, AlgebraicReference>,
) -> SymbolicMachine<P> {
    SymbolicMachine {
        constraints: constraint_system
            .algebraic_constraints
            .iter()
            .map(|constraint| SymbolicConstraint {
                expr: grouped_expression_to_algebraic(constraint),
            })
            .collect(),
        bus_interactions: constraint_system
            .bus_interactions
            .into_iter()
            .map(bus_interaction_to_symbolic_bus_interaction)
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
            .map(|arg| grouped_expression_to_algebraic(&arg))
            .collect(),
        mult: grouped_expression_to_algebraic(&bus_interaction.multiplicity),
    }
}

pub fn simplify_expression<T: FieldElement>(e: AlgebraicExpression<T>) -> AlgebraicExpression<T> {
    grouped_expression_to_algebraic(&algebraic_to_grouped_expression(&e))
}
