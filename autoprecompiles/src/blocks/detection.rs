use std::collections::BTreeSet;

use crate::{
    adapter::Adapter,
    blocks::{BasicBlock, Program, StaticBlocks},
};

/// Collects static blocks from a program
pub fn collect_static_blocks<A: Adapter>(
    program: &A::Program,
    jumpdest_set: &BTreeSet<u64>,
) -> StaticBlocks<A::Instruction> {
    let mut blocks = Vec::new();
    let mut curr_block = BasicBlock {
        start_pc: program.instruction_index_to_pc(0),
        instructions: Vec::new(),
    };
    for (i, instr) in program.instructions().enumerate() {
        let is_target = jumpdest_set.contains(&program.instruction_index_to_pc(i));
        let is_branching = A::is_branching(&instr);
        let is_allowed = A::is_allowed(&instr);

        // If this opcode cannot be in an apc, we make sure it's alone in a BB.
        if !is_allowed {
            // If not empty, push the current block.
            if !curr_block.instructions.is_empty() {
                blocks.push(curr_block);
            }
            // Push the instruction itself
            blocks.push(BasicBlock {
                start_pc: program.instruction_index_to_pc(i),
                instructions: vec![instr.clone()],
            });
            // Skip the instruction and start a new block from the next instruction.
            curr_block = BasicBlock {
                start_pc: program.instruction_index_to_pc(i + 1),
                instructions: Vec::new(),
            };
        } else {
            // If the instruction is a target, we need to close the previous block
            // as is if not empty and start a new block from this instruction.
            if is_target {
                if !curr_block.instructions.is_empty() {
                    blocks.push(curr_block);
                }
                curr_block = BasicBlock {
                    start_pc: program.instruction_index_to_pc(i),
                    instructions: Vec::new(),
                };
            }
            curr_block.instructions.push(instr.clone());
            // If the instruction is a branch, we need to close this block
            // with this instruction and start a new block from the next one.
            if is_branching {
                blocks.push(curr_block); // guaranteed to be non-empty because an instruction was just pushed
                curr_block = BasicBlock {
                    start_pc: program.instruction_index_to_pc(i + 1),
                    instructions: Vec::new(),
                };
            }
        }
    }

    if !curr_block.instructions.is_empty() {
        blocks.push(curr_block);
    }

    tracing::info!(
        "Got {} static blocks from `collect_static_blocks`",
        blocks.len()
    );

    expand_blocks::<A>(blocks)
}

fn expand_blocks<A: Adapter>(
    blocks: Vec<BasicBlock<A::Instruction>>,
) -> StaticBlocks<A::Instruction> {
    StaticBlocks::new(blocks)
}
