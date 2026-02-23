use itertools::Itertools;
use powdr_constraint_solver::constraint_system::ComputationMethod;
use powdr_expression::AlgebraicBinaryOperation;
use powdr_number::FieldElement;

use crate::{
    adapter::Adapter,
    blocks::{BasicBlock, Instruction},
    expression::AlgebraicExpression,
    powdr,
    symbolic_machine::{SymbolicBusInteraction, SymbolicConstraint, SymbolicMachine},
    Apc, BusMap, BusType, ColumnAllocator, InstructionHandler,
};

/// Converts the field type of a symbolic machine.
pub fn convert_apc_field_type<T, I, A, V, U>(
    apc: Apc<T, I, A, V>,
    convert_field_element: &impl Fn(T) -> U,
) -> Apc<U, I, A, V> {
    Apc {
        block: apc.block,
        machine: convert_machine_field_type(apc.machine, convert_field_element),
        subs: apc.subs,
        optimistic_constraints: apc.optimistic_constraints,
    }
}

/// Converts the field type of a symbolic machine.
pub fn convert_machine_field_type<T, U>(
    machine: SymbolicMachine<T>,
    convert_field_element: &impl Fn(T) -> U,
) -> SymbolicMachine<U> {
    SymbolicMachine {
        constraints: machine
            .constraints
            .into_iter()
            .map(|c| convert_symbolic_constraint(c, convert_field_element))
            .collect(),
        bus_interactions: machine
            .bus_interactions
            .into_iter()
            .map(|i| convert_bus_interaction(i, convert_field_element))
            .collect(),
        derived_columns: machine
            .derived_columns
            .into_iter()
            .map(|(v, method)| {
                let method = match method {
                    ComputationMethod::Constant(c) => {
                        ComputationMethod::Constant(convert_field_element(c))
                    }
                    ComputationMethod::QuotientOrZero(e1, e2) => ComputationMethod::QuotientOrZero(
                        convert_expression(e1, convert_field_element),
                        convert_expression(e2, convert_field_element),
                    ),
                };
                (v, method)
            })
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

/// Converts a basic block into a symbolic machines (all instruction circuits
/// concatenated) and a column allocator.
pub(crate) fn statements_to_symbolic_machine<A: Adapter>(
    block: &BasicBlock<A::Instruction>,
    instruction_handler: &A::InstructionHandler,
    bus_map: &BusMap<A::CustomBusTypes>,
) -> (SymbolicMachine<A::PowdrField>, ColumnAllocator) {
    let (machines, column_allocator) =
        statements_to_symbolic_machines::<A>(block, instruction_handler, bus_map);
    let machine = machines
        .into_iter()
        .reduce(SymbolicMachine::concatenate)
        .unwrap();
    (machine, column_allocator)
}

/// Converts a basic block into a list of symbolic machines (one per instruction)
/// and a column allocator. All columns are globally unique across all instructions.
pub(crate) fn statements_to_symbolic_machines<A: Adapter>(
    block: &BasicBlock<A::Instruction>,
    instruction_handler: &A::InstructionHandler,
    bus_map: &BusMap<A::CustomBusTypes>,
) -> (Vec<SymbolicMachine<A::PowdrField>>, ColumnAllocator) {
    let mut col_subs: Vec<Vec<u64>> = Vec::new();
    let mut global_idx = 0;
    let mut machines: Vec<SymbolicMachine<A::PowdrField>> = Vec::new();

    for (i, (instr, pc)) in block.instructions.iter().zip_eq(block.pcs()).enumerate() {
        let machine = instruction_handler
            .get_instruction_air_and_id(instr)
            .1
            .clone();

        let machine: SymbolicMachine<<A as Adapter>::PowdrField> =
            convert_machine_field_type(machine, &|x| A::from_field(x));

        let pc_lookup_row = instr
            .pc_lookup_row(pc)
            .into_iter()
            .map(|x| A::from_field(x))
            .collect::<Vec<_>>();

        let (next_global_idx, subs, machine) = powdr::globalize_references(machine, global_idx, i);
        global_idx = next_global_idx;

        // Make machine mutable, to add local constraints
        let mut machine = machine;

        let pc_lookup = machine
            .bus_interactions
            .iter()
            .filter(|bus_int| bus_int.id == bus_map.get_bus_id(&BusType::PcLookup).unwrap())
            .exactly_one()
            .expect("Expected single pc lookup");

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
        machine
            .constraints
            .push((minus_is_valid.clone() + one).into());

        // Constrain the pc lookup to the current instruction.
        machine.constraints.extend(
            pc_lookup
                .args
                .iter()
                .zip_eq(pc_lookup_row)
                .map(|(l, r)| (l.clone() - r.into()).into()),
        );

        col_subs.push(subs);
        machines.push(machine);
    }

    (
        machines,
        ColumnAllocator {
            subs: col_subs,
            next_poly_id: global_idx,
        },
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
