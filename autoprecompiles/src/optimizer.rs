use std::collections::BTreeMap;

use super::simplify_expression;
use itertools::Itertools;
use powdr_constraint_solver::{
    constraint_system::{BusInteraction, BusInteractionHandler, ConstraintSystem},
    quadratic_symbolic_expression::QuadraticSymbolicExpression,
    symbolic_expression::SymbolicExpression,
};
use powdr_number::FieldElement;

use crate::{
    constraint_optimizer::{optimize_constraints, IsBusStateful},
    legacy_expression::{
        ast_compatibility::CompatibleWithAstExpression, AlgebraicExpression, AlgebraicReference,
    },
    memory_optimizer::{check_register_operation_consistency, optimize_memory},
    powdr::{self},
    stats_logger::StatsLogger,
    SymbolicBusInteraction, SymbolicConstraint, SymbolicMachine, EXECUTION_BUS_ID,
    PC_LOOKUP_BUS_ID,
};

pub fn optimize<T: FieldElement>(
    machine: SymbolicMachine<T>,
    bus_interaction_handler: impl BusInteractionHandler<T> + IsBusStateful<T> + Clone,
    opcode: Option<u32>,
    degree_bound: usize,
) -> Result<SymbolicMachine<T>, crate::constraint_optimizer::Error> {
    let mut stats_logger = StatsLogger::start(&machine);
    let machine = if let Some(opcode) = opcode {
        let machine = optimize_pc_lookup(machine, opcode);
        stats_logger.log("PC lookup optimization", &machine);
        machine
    } else {
        machine
    };
    let machine = optimize_exec_bus(machine);
    stats_logger.log("exec bus optimization", &machine);

    let mut constraint_system = symbolic_machine_to_constraint_system(machine);

    loop {
        let size = system_size(&constraint_system);
        constraint_system = optimization_loop_iteration(
            constraint_system,
            bus_interaction_handler.clone(),
            degree_bound,
            &mut stats_logger,
        )?;
        if system_size(&constraint_system) == size {
            return Ok(constraint_system_to_symbolic_machine(constraint_system));
        }
    }
}

fn optimization_loop_iteration<T: FieldElement>(
    constraint_system: ConstraintSystem<T, AlgebraicReference>,
    bus_interaction_handler: impl BusInteractionHandler<T> + IsBusStateful<T> + Clone,
    degree_bound: usize,
    stats_logger: &mut StatsLogger,
) -> Result<ConstraintSystem<T, AlgebraicReference>, crate::constraint_optimizer::Error> {
    let constraint_system = optimize_constraints(
        constraint_system,
        bus_interaction_handler.clone(),
        degree_bound,
        stats_logger,
    )?;
    // TODO avoid these this conversion..
    let machine = constraint_system_to_symbolic_machine(constraint_system);
    let machine = optimize_memory(machine);
    assert!(check_register_operation_consistency(&machine));
    stats_logger.log("memory optimization", &machine);

    Ok(symbolic_machine_to_constraint_system(machine))
}

fn system_size<T: FieldElement>(
    constraint_system: &ConstraintSystem<T, AlgebraicReference>,
) -> [usize; 3] {
    [
        constraint_system.algebraic_constraints.len(),
        constraint_system.bus_interactions.len(),
        constraint_system
            .expressions()
            .flat_map(|expr| expr.referenced_variables())
            .unique()
            .count(),
    ]
}

pub fn optimize_pc_lookup<T: FieldElement>(
    mut machine: SymbolicMachine<T>,
    opcode: u32,
) -> SymbolicMachine<T> {
    let mut first_pc = None;
    machine.bus_interactions.retain(|bus_int| {
        if bus_int.id == PC_LOOKUP_BUS_ID {
            if first_pc.is_none() {
                first_pc = Some(bus_int.clone());
            }
            return false;
        }
        true
    });
    let mut first_pc = first_pc.unwrap();
    assert_eq!(first_pc.args.len(), 9);
    first_pc.args[1] = AlgebraicExpression::Number(T::from(opcode));
    first_pc.args[2] = AlgebraicExpression::Number(T::from(0u32));
    first_pc.args[3] = AlgebraicExpression::Number(T::from(0u32));
    first_pc.args[4] = AlgebraicExpression::Number(T::from(0u32));
    first_pc.args[5] = AlgebraicExpression::Number(T::from(0u32));
    first_pc.args[6] = AlgebraicExpression::Number(T::from(0u32));
    first_pc.args[7] = AlgebraicExpression::Number(T::from(0u32));
    first_pc.args[8] = AlgebraicExpression::Number(T::from(0u32));

    machine.bus_interactions.push(first_pc);

    machine
}

pub fn optimize_exec_bus<T: FieldElement>(mut machine: SymbolicMachine<T>) -> SymbolicMachine<T> {
    let mut first_seen = false;
    let mut receive = true;
    let mut latest_send = None;
    let mut subs_pc: BTreeMap<AlgebraicExpression<T>, AlgebraicExpression<T>> = Default::default();
    let mut subs_ts: BTreeMap<AlgebraicExpression<T>, AlgebraicExpression<T>> = Default::default();
    machine.bus_interactions.retain(|bus_int| {
        if bus_int.id != EXECUTION_BUS_ID {
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
            let mut pc_expr = bus_int.args[0].clone();
            powdr::substitute_algebraic_algebraic(&mut pc_expr, &subs_pc);
            pc_expr = simplify_expression(pc_expr);

            let mut ts_expr = bus_int.args[1].clone();
            powdr::substitute_algebraic_algebraic(&mut ts_expr, &subs_ts);
            ts_expr = simplify_expression(ts_expr);

            let mut send = bus_int.clone();
            send.args[0] = pc_expr;
            send.args[1] = ts_expr;

            latest_send = Some(send);
            false
        } else {
            // Equate the latest send to the new receive and remove the bus interaction
            subs_pc.insert(
                bus_int.args[0].clone(),
                latest_send.clone().unwrap().args[0].clone(),
            );
            subs_ts.insert(
                bus_int.args[1].clone(),
                latest_send.clone().unwrap().args[1].clone(),
            );
            false
        };

        receive = !receive;

        keep
    });

    // Re-add the last send
    machine.bus_interactions.push(latest_send.unwrap());

    for c in &mut machine.constraints {
        powdr::substitute_algebraic_algebraic(&mut c.expr, &subs_pc);
        powdr::substitute_algebraic_algebraic(&mut c.expr, &subs_ts);
        c.expr = simplify_expression(c.expr.clone());
    }
    for b in &mut machine.bus_interactions {
        powdr::substitute_algebraic_algebraic(&mut b.mult, &subs_pc);
        powdr::substitute_algebraic_algebraic(&mut b.mult, &subs_ts);
        b.mult = simplify_expression(b.mult.clone());
        for a in &mut b.args {
            powdr::substitute_algebraic_algebraic(a, &subs_pc);
            powdr::substitute_algebraic_algebraic(a, &subs_ts);
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
    constraint_system: ConstraintSystem<P, AlgebraicReference>,
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

fn symbolic_bus_interaction_to_bus_interaction<P: FieldElement>(
    bus_interaction: &SymbolicBusInteraction<P>,
) -> BusInteraction<QuadraticSymbolicExpression<P, AlgebraicReference>> {
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
    bus_interaction: BusInteraction<QuadraticSymbolicExpression<P, AlgebraicReference>>,
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

/// Turns an algebraic expression into a quadratic symbolic expression,
/// assuming all [`AlgebraicReference`]s are unknown variables.
pub fn algebraic_to_quadratic_symbolic_expression<T: FieldElement>(
    expr: &AlgebraicExpression<T>,
) -> QuadraticSymbolicExpression<T, AlgebraicReference> {
    powdr_expression::conversion::convert(expr, &mut |reference| {
        QuadraticSymbolicExpression::from_unknown_variable(reference.clone())
    })
}

/// Turns a quadratic symbolic expression back into an algebraic expression.
/// Tries to simplify the expression wrt negation and constant factors
/// to aid human readability.
pub fn quadratic_symbolic_expression_to_algebraic<T: FieldElement>(
    expr: &QuadraticSymbolicExpression<T, AlgebraicReference>,
) -> AlgebraicExpression<T> {
    // Wrap `powdr_pilopt::qse_opt::quadratic_symbolic_expression_to_algebraic`, which
    // works on a `powdr_ast::analyzed::AlgebraicExpression`.
    let expr = expr.transform_var_type(&mut |algebraic_reference| {
        powdr_pilopt::qse_opt::Variable::Reference(algebraic_reference.clone().into())
    });
    // This is where the core conversion is implemented, including the simplification.
    let ast_algebraic_expression =
        powdr_pilopt::qse_opt::quadratic_symbolic_expression_to_algebraic(&expr);
    // Unwrap should be fine, because by construction we don't have challenges or public references,
    // and quadratic_symbolic_expression_to_algebraic should not introduce any exponentiations.
    AlgebraicExpression::try_from_ast_expression(ast_algebraic_expression).unwrap()
}
