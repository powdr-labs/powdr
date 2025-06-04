use std::{
    collections::BTreeMap,
    fmt::{self, Display},
};

use itertools::Itertools;
use powdr_ast::analyzed::{
    algebraic_expression_conversion, AlgebraicExpression, AlgebraicReference, Challenge,
};
use powdr_constraint_solver::{
    boolean_extractor,
    constraint_system::{BusInteraction, BusInteractionHandler, ConstraintSystem},
    quadratic_symbolic_expression::{
        NoRangeConstraints, QuadraticSymbolicExpression, RangeConstraintProvider,
    },
    range_constraint::RangeConstraint,
    symbolic_expression::SymbolicExpression,
};
use powdr_number::FieldElement;
use powdr_pilopt::{qse_opt::quadratic_symbolic_expression_to_algebraic, simplify_expression};

use crate::{
    bitwise_lookup_optimizer::optimize_bitwise_lookup,
    constraint_optimizer::{optimize_constraints, IsBusStateful},
    memory_optimizer::{check_register_operation_consistency, optimize_memory},
    powdr::{self},
    stats_logger::{IsWitnessColumn, StatsLogger},
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
    constraint_system: ConstraintSystem<T, Variable>,
    bus_interaction_handler: impl BusInteractionHandler<T> + IsBusStateful<T> + Clone,
    degree_bound: usize,
    stats_logger: &mut StatsLogger,
) -> Result<ConstraintSystem<T, Variable>, crate::constraint_optimizer::Error> {
    let constraint_system = optimize_constraints(
        constraint_system,
        bus_interaction_handler.clone(),
        degree_bound,
        stats_logger,
    )?;
    // TODO call boolean extractor.
    let machine = optimize_memory(constraint_system, NoRangeConstraints);
    assert!(check_register_operation_consistency(&machine));
    stats_logger.log("memory optimization", &machine);

    // TODO avoid these conversions.
    let machine = optimize_bitwise_lookup(constraint_system_to_symbolic_machine(machine));
    stats_logger.log("optimizing bitwise lookup", &machine);

    Ok(symbolic_machine_to_constraint_system(machine))
}

fn system_size<T: FieldElement>(constraint_system: &ConstraintSystem<T, Variable>) -> [usize; 3] {
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

/// Converts from a SymbolicMachine to a ConstraintSystem and
/// simplifies some quadratic constraints by introducing boolean variables.
fn symbolic_machine_to_simplified_constraint_systems<T: FieldElement>(
    machine: &SymbolicMachine<T>,
) -> ConstraintSystem<T, Variable> {
    let mut counter = 0..;
    let mut var_dispenser = || Variable::Boolean(counter.next().unwrap());

    let algebraic_constraints = machine
        .constraints
        .iter()
        .map(|constr| {
            let constr = algebraic_to_quadratic_symbolic_expression(&constr.expr);
            boolean_extractor::extract_boolean(&constr, &mut var_dispenser).unwrap_or(constr)
        })
        .collect_vec();
    let bus_interactions = machine
        .bus_interactions
        .iter()
        .map(symbolic_bus_interaction_to_bus_interaction)
        .collect_vec();
    ConstraintSystem {
        algebraic_constraints,
        bus_interactions,
    }
}

/// Turns an algebraic expression into a quadratic symbolic expression,
/// assuming all [`AlgebraicReference`]s, public references and challenges
/// are unknown variables.
fn algebraic_to_quadratic_symbolic_expression<T: FieldElement>(
    expr: &AlgebraicExpression<T>,
) -> QuadraticSymbolicExpression<T, Variable> {
    type Qse<T> = QuadraticSymbolicExpression<T, Variable>;

    struct TerminalConverter;

    impl<T: FieldElement> algebraic_expression_conversion::TerminalConverter<Qse<T>>
        for TerminalConverter
    {
        fn convert_reference(&mut self, reference: &AlgebraicReference) -> Qse<T> {
            Qse::from_unknown_variable(Variable::Reference(reference.clone()))
        }
        fn convert_public_reference(&mut self, reference: &str) -> Qse<T> {
            Qse::from_unknown_variable(Variable::PublicReference(reference.to_string()))
        }
        fn convert_challenge(&mut self, challenge: &Challenge) -> Qse<T> {
            Qse::from_unknown_variable(Variable::Challenge(*challenge))
        }
    }

    algebraic_expression_conversion::convert(expr, &mut TerminalConverter)
}

// TODO Maybe use an inner generic type?
#[derive(Clone, PartialOrd, Ord, PartialEq, Eq, Hash, Debug)]
enum Variable {
    Reference(AlgebraicReference),
    PublicReference(String),
    Challenge(Challenge),
    Boolean(usize),
}

impl Display for Variable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Variable::Reference(r) => write!(f, "{r}"),
            Variable::PublicReference(r) => write!(f, "{r}"),
            Variable::Challenge(c) => write!(f, "{c}"),
            Variable::Boolean(id) => write!(f, "boolean_{id}"),
        }
    }
}

impl IsWitnessColumn for Variable {
    fn is_witness_column(&self) -> bool {
        match self {
            Variable::Reference(poly) => poly.is_witness(),
            Variable::PublicReference(_) | Variable::Challenge(_) | Variable::Boolean(_) => false,
        }
    }
}

#[derive(Default)]
struct RangeConstraintsForBooleans;

impl<T: FieldElement> RangeConstraintProvider<T, Variable> for RangeConstraintsForBooleans {
    fn get(&self, variable: &Variable) -> RangeConstraint<T> {
        match variable {
            Variable::Boolean(_) => RangeConstraint::from_mask(1),
            _ => Default::default(),
        }
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

/// Converts from SymbolicConstraint to QuadraticSymbolicExpression and
/// simplifies constraints by introducing boolean variables.
fn symbolic_to_simplified_constraints<T: FieldElement>(
    constraints: &[SymbolicConstraint<T>],
) -> Vec<QuadraticSymbolicExpression<T, Variable>> {
    let mut counter = 0..;
    let mut var_dispenser = || Variable::Boolean(counter.next().unwrap());

    constraints
        .iter()
        .map(|constr| {
            let constr = algebraic_to_quadratic_symbolic_expression(&constr.expr);
            boolean_extractor::extract_boolean(&constr, &mut var_dispenser).unwrap_or(constr)
        })
        .collect_vec()
}
