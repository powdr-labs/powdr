use itertools::Itertools;
use powdr_number::FieldElement;

use crate::{
    expression::AlgebraicExpression, powdr, BusMap, BusType, InstructionMachineHandler,
    PcLookupBusInteraction, SymbolicBusInteraction, SymbolicConstraint,
    SymbolicInstructionStatement, SymbolicMachine,
};

pub fn statements_to_symbolic_machine<T: FieldElement>(
    statements: &[SymbolicInstructionStatement<T>],
    instruction_machine_handler: &impl InstructionMachineHandler<T>,
    bus_map: &BusMap,
) -> (SymbolicMachine<T>, Vec<Vec<u64>>) {
    let mut constraints: Vec<SymbolicConstraint<T>> = Vec::new();
    let mut bus_interactions: Vec<SymbolicBusInteraction<T>> = Vec::new();
    let mut col_subs: Vec<Vec<u64>> = Vec::new();
    let mut global_idx: u64 = 3;

    for (i, instr) in statements.iter().enumerate() {
        let machine = instruction_machine_handler
            .get_instruction_air(instr.opcode)
            .unwrap()
            .clone();

        let (next_global_idx, subs, machine) = powdr::reassign_ids(machine, global_idx, i);
        global_idx = next_global_idx;

        let pc_lookup: PcLookupBusInteraction<T> = machine
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

        let mut local_constraints: Vec<SymbolicConstraint<T>> = Vec::new();

        // To simplify constraint solving, we constrain `is_valid` to be 1, which effectively
        // removes the column. The optimized precompile will then have to be guarded by a new
        // `is_valid` column.
        let minus_is_valid: AlgebraicExpression<T> = exec_receive(
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
