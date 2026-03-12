use std::collections::{BTreeSet, HashMap};

use crate::{
    adapter::Adapter,
    blocks::{BasicBlock, Program},
};

fn merge_with_target_on_unconditional_jump<A: Adapter>(
    blocks: &[BasicBlock<A::Instruction>],
) -> Vec<BasicBlock<A::Instruction>> {
    let blocks_by_start_pc: HashMap<u64, &BasicBlock<A::Instruction>> =
        blocks.iter().map(|b| (b.start_pc(), b)).collect();
    let mut merged = Vec::with_capacity(blocks.len());

    for curr in blocks {
        let maybe_target_pc = curr
            .instructions
            .last()
            .and_then(|(pc, instr)| A::unconditional_jump_target(instr, *pc));

        let Some(target_pc) = maybe_target_pc else {
            merged.push(curr.clone());
            continue;
        };

        if target_pc == curr.start_pc() {
            // Avoid duplicating self-looping blocks.
            merged.push(curr.clone());
            continue;
        }

        if let Some(target) = blocks_by_start_pc.get(&target_pc) {
            let mut instructions = curr.instructions.clone();
            instructions.extend(target.instructions.clone());
            merged.push(BasicBlock { instructions });
        } else {
            merged.push(curr.clone());
        }
    }

    merged
}

/// Collects basic blocks from a program
pub fn collect_basic_blocks<A: Adapter>(
    program: &A::Program,
    jumpdest_set: &BTreeSet<u64>,
) -> Vec<BasicBlock<A::Instruction>> {
    let mut blocks = Vec::new();
    let mut curr_block = BasicBlock {
        instructions: Vec::new(),
    };

    for (i, instr) in program.instructions().enumerate() {
        let pc = program.instruction_index_to_pc(i);
        let is_target = jumpdest_set.contains(&pc);
        let is_branching = A::is_branching(&instr);
        let is_allowed = A::is_allowed(&instr);

        // If this opcode cannot be in an APC, make sure it's alone in a basic block.
        if !is_allowed {
            if !curr_block.instructions.is_empty() {
                blocks.push(curr_block);
                curr_block = BasicBlock {
                    instructions: Vec::new(),
                };
            }
            blocks.push(BasicBlock {
                instructions: vec![(pc, instr.clone())],
            });
            continue;
        }

        // If the instruction is a target, we need to close the previous block
        // as-is (if non-empty) and start a new block from this instruction.
        if is_target && !curr_block.instructions.is_empty() {
            blocks.push(curr_block);
            curr_block = BasicBlock {
                instructions: Vec::new(),
            };
        }

        curr_block.instructions.push((pc, instr.clone()));

        // If the instruction is a branch, close this block with this instruction.
        if is_branching {
            blocks.push(curr_block);
            curr_block = BasicBlock {
                instructions: Vec::new(),
            };
        }
    }

    if !curr_block.instructions.is_empty() {
        blocks.push(curr_block);
    }

    let blocks = merge_with_target_on_unconditional_jump::<A>(&blocks);
    tracing::info!("Got {} basic blocks from `collect_basic_blocks`", blocks.len());
    blocks
}
