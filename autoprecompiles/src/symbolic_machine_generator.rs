use itertools::Itertools;
use powdr_expression::AlgebraicBinaryOperation;
use powdr_number::FieldElement;

use crate::{
    adapter::Adapter, blocks::Instruction, expression::AlgebraicExpression, powdr, BusMap, BusType,
    InstructionMachineHandler, PcLookupBusInteraction, SymbolicBusInteraction, SymbolicConstraint,
    SymbolicInstructionStatement, SymbolicMachine,
};

pub fn convert_machine<T, U>(
    machine: SymbolicMachine<T>,
    convert: &impl Fn(T) -> U,
) -> SymbolicMachine<U> {
    SymbolicMachine {
        constraints: machine
            .constraints
            .into_iter()
            .map(|c| convert_symbolic_constraint(c, convert))
            .collect(),
        bus_interactions: machine
            .bus_interactions
            .into_iter()
            .map(|i| convert_bus_interaction(i, convert))
            .collect(),
    }
}

fn convert_symbolic_constraint<T, U>(
    constraint: SymbolicConstraint<T>,
    convert: &impl Fn(T) -> U,
) -> SymbolicConstraint<U> {
    SymbolicConstraint {
        expr: convert_expression(constraint.expr, convert),
    }
}

fn convert_bus_interaction<T, U>(
    constraint: SymbolicBusInteraction<T>,
    convert: &impl Fn(T) -> U,
) -> SymbolicBusInteraction<U> {
    SymbolicBusInteraction {
        id: constraint.id,
        mult: convert_expression(constraint.mult, convert),
        args: constraint
            .args
            .into_iter()
            .map(|e| convert_expression(e, convert))
            .collect(),
    }
}

fn convert_expression<T, U>(
    expr: AlgebraicExpression<T>,
    convert: &impl Fn(T) -> U,
) -> AlgebraicExpression<U> {
    match expr {
        AlgebraicExpression::Number(n) => AlgebraicExpression::Number(convert(n)),
        AlgebraicExpression::Reference(r) => AlgebraicExpression::Reference(r),
        AlgebraicExpression::BinaryOperation(algebraic_binary_operation) => {
            AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
                op: algebraic_binary_operation.op,
                left: Box::new(convert_expression(
                    *algebraic_binary_operation.left,
                    convert,
                )),
                right: Box::new(convert_expression(
                    *algebraic_binary_operation.right,
                    convert,
                )),
            })
        }
        AlgebraicExpression::UnaryOperation(algebraic_unary_operation) => {
            AlgebraicExpression::UnaryOperation(powdr_expression::AlgebraicUnaryOperation {
                op: algebraic_unary_operation.op,
                expr: Box::new(convert_expression(*algebraic_unary_operation.expr, convert)),
            })
        }
    }
}

pub fn statements_to_symbolic_machine<A: Adapter>(
    statements: &[A::Instruction],
    instruction_machine_handler: &A::InstructionMachineHandler,
    bus_map: &BusMap,
) -> (SymbolicMachine<A::PowdrField>, Vec<Vec<u64>>) {
    let mut constraints: Vec<SymbolicConstraint<_>> = Vec::new();
    let mut bus_interactions: Vec<SymbolicBusInteraction<_>> = Vec::new();
    let mut col_subs: Vec<Vec<u64>> = Vec::new();
    let mut global_idx: u64 = 3;

    for (i, instr) in statements.iter().enumerate() {
        let machine = instruction_machine_handler
            .get_instruction_air(instr)
            .unwrap()
            .clone();

        let machine: SymbolicMachine<<A as Adapter>::PowdrField> =
            convert_machine(machine, &|x| A::from_field(x));

        let instr = instr.clone().into_symbolic_instruction();

        let instr = SymbolicInstructionStatement {
            opcode: instr.opcode,
            args: instr
                .args
                .iter()
                .map(|a| A::from_field(a.clone()))
                .collect(),
        };

        let (next_global_idx, subs, machine) = powdr::globalize_references(machine, global_idx, i);
        global_idx = next_global_idx;

        let pc_lookup: PcLookupBusInteraction<_> = machine
            .bus_interactions
            .iter()
            .filter_map(|bus_int| {
                PcLookupBusInteraction::try_from_symbolic_bus_interaction(
                    bus_int,
                    bus_map.get_bus_id(&BusType::PcLookup).unwrap(),
                )
                .ok()
            })
            .exactly_one()
            .expect("Expected single pc lookup");

        let mut local_constraints: Vec<SymbolicConstraint<_>> = Vec::new();

        // To simplify constraint solving, we constrain `is_valid` to be 1, which effectively
        // removes the column. The optimized precompile will then have to be guarded by a new
        // `is_valid` column.
        let minus_is_valid: AlgebraicExpression<_> = exec_receive(
            &machine,
            bus_map.get_bus_id(&BusType::ExecutionBridge).unwrap(),
        )
        .mult
        .clone();
        let one = AlgebraicExpression::Number(1u64.into());
        local_constraints.push((minus_is_valid.clone() + one).into());

        // Constrain the opcode expression to equal the actual opcode.
        let opcode_constant = AlgebraicExpression::Number((instr.opcode as u64).into());
        local_constraints.push((pc_lookup.op.clone() - opcode_constant).into());

        assert_eq!(instr.args.len(), pc_lookup.args.len());
        instr
            .args
            .iter()
            .zip_eq(&pc_lookup.args)
            .for_each(|(instr_arg, pc_arg)| {
                let arg = AlgebraicExpression::Number(*instr_arg);
                local_constraints.push((arg - pc_arg.clone()).into());
            });

        constraints.extend(
            machine
                .constraints
                .iter()
                .chain(&local_constraints)
                .map(|expr| SymbolicConstraint {
                    expr: expr.expr.clone(),
                }),
        );
        bus_interactions.extend(machine.bus_interactions.iter().cloned());
        col_subs.push(subs);
    }

    (
        SymbolicMachine {
            constraints,
            bus_interactions,
        },
        col_subs,
    )
}

fn exec_receive<T: FieldElement>(
    machine: &SymbolicMachine<T>,
    exec_bus_id: u64,
) -> SymbolicBusInteraction<T> {
    let [r, _s] = machine
        .bus_interactions
        .iter()
        .filter(|bus_int| bus_int.id == exec_bus_id)
        .collect::<Vec<_>>()
        .try_into()
        .unwrap();
    // TODO assert that r.mult matches -expr
    r.clone()
}
