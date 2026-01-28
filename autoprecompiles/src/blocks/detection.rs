use std::collections::BTreeSet;

use crate::{
    adapter::Adapter,
    blocks::{BasicBlock, Program},
    InstructionHandler,
};

/// Collects basic blocks from a program
pub fn collect_basic_blocks<A: Adapter>(
    program: &A::Program,
    jumpdest_set: &BTreeSet<u64>,
    instruction_handler: &A::InstructionHandler,
) -> Vec<BasicBlock<A::Instruction>> {
    let mut blocks = Vec::new();
    let mut curr_block =
        BasicBlock::from_start_pc_and_statements(program.instruction_index_to_pc(0), Vec::new());
    for (i, instr) in program.instructions().enumerate() {
        let is_target = jumpdest_set.contains(&program.instruction_index_to_pc(i));
        let is_branching = instruction_handler.is_branching(&instr);
        let is_allowed = instruction_handler.is_allowed(&instr);

        // If this opcode cannot be in an apc, we make sure it's alone in a BB.
        if !is_allowed {
            // If not empty, push the current block.
            if !curr_block.statements.is_empty() {
                blocks.push(curr_block);
            }
            // Push the instruction itself
            blocks.push(BasicBlock::from_start_pc_and_statements(
                program.instruction_index_to_pc(i),
                vec![instr.clone()],
            ));
            // Skip the instruction and start a new block from the next instruction.
            curr_block = BasicBlock::from_start_pc_and_statements(
                program.instruction_index_to_pc(i + 1),
                Vec::new(),
            );
        } else {
            // If the instruction is a target, we need to close the previous block
            // as is if not empty and start a new block from this instruction.
            if is_target {
                if !curr_block.statements.is_empty() {
                    blocks.push(curr_block);
                }
                curr_block = BasicBlock::from_start_pc_and_statements(
                    program.instruction_index_to_pc(i),
                    Vec::new(),
                );
            }
            curr_block.statements.push(instr.clone());
            // If the instruction is a branch, we need to close this block
            // with this instruction and start a new block from the next one.
            if is_branching {
                blocks.push(curr_block); // guaranteed to be non-empty because an instruction was just pushed
                curr_block = BasicBlock::from_start_pc_and_statements(
                    program.instruction_index_to_pc(i + 1),
                    Vec::new(),
                );
            }
        }
    }

    if !curr_block.statements.is_empty() {
        blocks.push(curr_block);
    }

    tracing::info!(
        "Got {} basic blocks from `collect_basic_blocks`",
        blocks.len()
    );

    blocks
}
