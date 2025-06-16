use std::collections::BTreeMap;

use super::simplify_expression;
use itertools::Itertools;
use powdr_expression::AlgebraicBinaryOperation;
use powdr_number::{FieldElement, LargeInt};

use crate::{
    expression::AlgebraicExpression,
    powdr::{self, Column},
    BusMap, BusType, PcLookupBusInteraction, SymbolicBusInteraction, SymbolicConstraint,
    SymbolicInstructionStatement, SymbolicMachine,
};

pub fn statements_to_symbolic_machine<T: FieldElement>(
    statements: &[SymbolicInstructionStatement<T>],
    instruction_machines: &BTreeMap<usize, SymbolicMachine<T>>,
    bus_map: &BusMap,
) -> (SymbolicMachine<T>, Vec<Vec<u64>>) {
    let mut constraints: Vec<SymbolicConstraint<T>> = Vec::new();
    let mut bus_interactions: Vec<SymbolicBusInteraction<T>> = Vec::new();
    let mut col_subs: Vec<Vec<u64>> = Vec::new();
    let mut global_idx: u64 = 3;

    for (i, instr) in statements.iter().enumerate() {
        let machine = instruction_machines.get(&instr.opcode).unwrap().clone();

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

        let mut sub_map: BTreeMap<Column, AlgebraicExpression<T>> = Default::default();
        let mut local_constraints: Vec<SymbolicConstraint<T>> = Vec::new();

        let is_valid: AlgebraicExpression<T> = exec_receive(
            &machine,
            bus_map.get_bus_id(&BusType::ExecutionBridge).unwrap(),
        )
        .mult
        .clone();
        let one = AlgebraicExpression::Number(1u64.into());
        local_constraints.push((is_valid.clone() + one).into());

        let mut sub_map_loadstore: BTreeMap<Column, AlgebraicExpression<T>> = Default::default();
        if is_loadstore(instr.opcode) {
            sub_map_loadstore.extend(loadstore_chip_info(&machine, instr.opcode));
        }

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
                    AlgebraicExpression::Reference(ref arg_ref) => {
                        sub_map.insert(Column::from(arg_ref), arg);
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
                powdr::substitute_algebraic(&mut expr, &sub_map_loadstore);
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
                    powdr::substitute_algebraic(e, &sub_map_loadstore);
                    *e = simplify_expression(e.clone());
                });
            bus_interactions.push(link);
        }

        col_subs.push(subs);

        // after the first round of simplifying,
        // we need to look for register memory bus interactions
        // and replace the addr by the first argument of the instruction
        for bus_int in &mut bus_interactions {
            if bus_int.id != bus_map.get_bus_id(&BusType::Memory).unwrap() {
                continue;
            }

            let addr_space = match bus_int.args[0] {
                AlgebraicExpression::Number(n) => n.to_integer().try_into_u32().unwrap(),
                _ => panic!(
                    "Address space must be a constant but got {}",
                    bus_int.args[0]
                ),
            };

            if addr_space != 1 {
                continue;
            }

            match bus_int.args[1] {
                AlgebraicExpression::Number(_) => {}
                _ => {
                    if let Some(arg) = bus_int.args.get_mut(1) {
                        *arg = instr.args[0].into();
                    } else {
                        panic!("Expected address argument");
                    }
                }
            };
        }
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

fn is_loadstore(opcode: usize) -> bool {
    (0x210..=0x215).contains(&opcode)
}

fn loadstore_chip_info<T: FieldElement>(
    machine: &SymbolicMachine<T>,
    opcode: usize,
) -> BTreeMap<Column, AlgebraicExpression<T>> {
    let is_load = if opcode == 0x210 || opcode == 0x211 || opcode == 0x212 {
        T::from(1u32)
    } else {
        T::from(0u32)
    };
    let is_load = AlgebraicExpression::Number(is_load);
    let is_load_expr = match &machine.constraints[7].expr {
        AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation { left, .. }) => left.clone(),
        _ => panic!("Expected subtraction."),
    };
    let is_load_col = if let AlgebraicExpression::Reference(r) = &*is_load_expr {
        r.into()
    } else {
        panic!("expected a single reference")
    };

    [(is_load_col, is_load)].into()
}
