use itertools::Itertools;
use powdr_expression::AlgebraicBinaryOperation;
use powdr_number::FieldElement;

use crate::{
    adapter::Adapter, blocks::Instruction, expression::AlgebraicExpression, powdr, BasicBlock,
    BusMap, BusType, InstructionMachineHandler, SymbolicBusInteraction, SymbolicConstraint,
    SymbolicMachine,
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
    block: &BasicBlock<A::Instruction>,
    instruction_machine_handler: &A::InstructionMachineHandler,
    bus_map: &BusMap,
) -> (SymbolicMachine<A::PowdrField>, Vec<Vec<u64>>) {
    let mut constraints: Vec<SymbolicConstraint<_>> = Vec::new();
    let mut bus_interactions: Vec<SymbolicBusInteraction<_>> = Vec::new();
    let mut col_subs: Vec<Vec<u64>> = Vec::new();
    let mut global_idx: u64 = 3;

    for (i, instr) in block.statements.iter().enumerate() {
        let machine = instruction_machine_handler
            .get_instruction_air(instr)
            .unwrap()
            .clone();

        let machine: SymbolicMachine<<A as Adapter>::PowdrField> =
            convert_machine(machine, &|x| A::from_field(x));

        // It is sufficient to provide the initial PC, because the PC update should be
        // deterministic within a basic block. Therefore, all future PCs can be derived
        // by the solver.
        let pc = (i == 0).then_some(block.start_idx);
        let pc_lookup_row = instr
            .pc_lookup_row(pc)
            .into_iter()
            .map(|x| x.map(|f| A::from_field(f)))
            .collect::<Vec<_>>();

        let (next_global_idx, subs, machine) = powdr::globalize_references(machine, global_idx, i);
        global_idx = next_global_idx;

        let pc_lookup = machine
            .bus_interactions
            .iter()
            .filter(|bus_int| bus_int.id == bus_map.get_bus_id(&BusType::PcLookup).unwrap())
            .exactly_one()
            .expect("Expected single pc lookup");

        let mut local_constraints: Vec<SymbolicConstraint<_>> = Vec::new();

        // To simplify constraint solving, we constrain `is_valid` to be 1, which effectively
        // removes the column. The optimized precompile will then have to be guarded by a new
        // `is_valid` column.
        let first_exec_mult = first_exec_interaction(
            &machine,
            bus_map.get_bus_id(&BusType::ExecutionBridge).unwrap(),
        )
        .mult
        .clone();
        // We don't know whether the first execution bus interaction is a send or receive
        // (this might depend on the zkVM), but we know its multiplicity should be either 1 or -1.
        // Because we expect `is_valid` to also be bit-constrained, the solver will be able to
        // determine the correct value.
        let one = AlgebraicExpression::Number(1u64.into());
        local_constraints.push(
            ((first_exec_mult.clone() + one.clone()) * (first_exec_mult - one.clone())).into(),
        );

        // Constrain the pc lookup to the current instruction.
        local_constraints.extend(
            pc_lookup
                .args
                .iter()
                .zip_eq(pc_lookup_row)
                .filter_map(|(l, r)| r.map(|r| (l, r)))
                .map(|(l, r)| (l.clone() - r.into()).into()),
        );

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

fn first_exec_interaction<T: FieldElement>(
    machine: &SymbolicMachine<T>,
    exec_bus_id: u64,
) -> SymbolicBusInteraction<T> {
    let [first, _second] = machine
        .bus_interactions
        .iter()
        .filter(|bus_int| bus_int.id == exec_bus_id)
        .collect::<Vec<_>>()
        .try_into()
        .unwrap();
    first.clone()
}
