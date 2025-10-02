use std::collections::{HashMap, HashSet};

use openvm_instructions::instruction::Instruction;
use openvm_stark_backend::p3_field::PrimeField32;
use powdr_autoprecompiles::blocks::BasicBlock;

use crate::{
    opcode::{BRANCH_OPCODES, BRANCH_OPCODES_BIGINT, OPCODE_JALR},
    Instr,
};

fn f_to_i64<F: PrimeField32>(f: &F) -> i64 {
    let u = f.as_canonical_u32();
    if u < F::ORDER_U32 / 2 {
        u as i64
    } else {
        (u as i64) - (F::ORDER_U32 as i64)
    }
}

fn possible_targets<F: PrimeField32>(
    instruction: &Instruction<F>,
    instruction_pc: u64,
    pc_step: u64,
) -> Vec<u64> {
    let opcode = instruction.opcode.as_usize();

    if opcode == OPCODE_JALR {
        // For JALR, we don't know the target statically.
        return vec![];
    }

    if BRANCH_OPCODES_BIGINT.contains(&opcode) || BRANCH_OPCODES.contains(&opcode) {
        // All other branch instructions conditionally add `c` to the current PC,
        // and otherwise increment the PC by the default step.
        let offset = f_to_i64(&instruction.c);
        return [
            instruction_pc + pc_step,
            (instruction_pc as i64 + offset) as u64,
        ]
        .into_iter()
        .collect();
    }
    [instruction_pc + pc_step].into_iter().collect()
}

pub fn control_flow_graph<F: PrimeField32>(
    basic_blocks: &[BasicBlock<Instr<F>>],
    pc_step: u64,
) -> HashMap<u64, Vec<u64>> {
    let known_targets = basic_blocks
        .iter()
        .map(|block| block.start_pc)
        .collect::<HashSet<_>>();
    basic_blocks
        .iter()
        .map(|block| {
            let id = block.start_pc;
            let last_pc = id + (block.statements.len() as u64 - 1) * pc_step;
            let last_instr = block.statements.last().unwrap();
            let targets = possible_targets(&last_instr.0, last_pc, pc_step);
            for target in &targets {
                assert!(
                    known_targets.contains(target),
                    "Unknown target {target} from block starting at {id}"
                );
            }
            log::info!("{id} -> {targets:?}");
            (id, targets)
        })
        .collect()
}
