use std::collections::BTreeMap;

use super::simplify_expression;
use itertools::Itertools;
use powdr_number::FieldElement;

use crate::{
    expression::{AlgebraicExpression, AlgebraicReference},
    powdr, BusMap, BusType, InstructionMachineHandler, PcLookupBusInteraction,
    SymbolicBusInteraction, SymbolicConstraint, SymbolicInstructionStatement, SymbolicMachine,
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

        let mut sub_map: BTreeMap<AlgebraicReference, AlgebraicExpression<T>> = Default::default();
        let mut local_constraints: Vec<SymbolicConstraint<T>> = Vec::new();

        let is_valid: AlgebraicExpression<T> = exec_receive(
            &machine,
            bus_map.get_bus_id(&BusType::ExecutionBridge).unwrap(),
        )
        .mult
        .clone();
        let one = AlgebraicExpression::Number(1u64.into());
        local_constraints.push((is_valid.clone() + one).into());

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
                match pc_arg {
                    AlgebraicExpression::Reference(arg_ref) => {
                        sub_map.insert(arg_ref.clone(), arg);
                    }
                    AlgebraicExpression::BinaryOperation(_expr) => {
                        local_constraints.push((arg - pc_arg.clone()).into());
                    }
                    AlgebraicExpression::UnaryOperation(_expr) => {
                        local_constraints.push((arg - pc_arg.clone()).into());
                    }
                    _ => {}
                }
            });

        let local_identities = machine
            .constraints
            .iter()
            .chain(&local_constraints)
            .map(|expr| {
                let mut expr = expr.expr.clone();
                powdr::substitute_algebraic(&mut expr, &sub_map);
                expr = simplify_expression(expr);
                SymbolicConstraint { expr }
            })
            .collect::<Vec<_>>();

        constraints.extend(local_identities);

        for bus_int in &machine.bus_interactions {
            let mut link = bus_int.clone();
            link.args
                .iter_mut()
                .chain(std::iter::once(&mut link.mult))
                .for_each(|e| {
                    powdr::substitute_algebraic(e, &sub_map);
                    *e = simplify_expression(e.clone());
                });
            bus_interactions.push(link);
        }

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
