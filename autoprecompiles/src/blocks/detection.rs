use std::collections::BTreeSet;

use crate::{
    adapter::Adapter,
    blocks::{BasicBlock, Instruction, Program},
};

/// Collects basic blocks from a program
pub fn collect_basic_blocks<A: Adapter>(
    program: &A::Program,
    labels: &BTreeSet<u64>,
    opcode_allowlist: &BTreeSet<usize>,
    branch_opcodes: &BTreeSet<usize>,
) -> Vec<BasicBlock<A::Instruction>> {
    let mut blocks = Vec::new();
    let mut curr_block = BasicBlock {
        start_idx: 0,
        statements: Vec::new(),
    };
    for (i, instr) in program.instructions().enumerate() {
        let pc = program.base_pc() + i as u64 * program.pc_step() as u64;
        let is_target = labels.contains(&pc);
        let is_branch = branch_opcodes.contains(&instr.opcode());

        // If this opcode cannot be in an apc, we make sure it's alone in a BB.
        if !opcode_allowlist.contains(&instr.opcode()) {
            // If not empty, push the current block.
            if !curr_block.statements.is_empty() {
                blocks.push(curr_block);
            }
            // Push the instruction itself
            blocks.push(BasicBlock {
                start_idx: i,
                statements: vec![instr.clone()],
            });
            // Skip the instrucion and start a new block from the next instruction.
            curr_block = BasicBlock {
                start_idx: i + 1,
                statements: Vec::new(),
            };
        } else {
            // If the instruction is a target, we need to close the previous block
            // as is if not empty and start a new block from this instruction.
            if is_target {
                if !curr_block.statements.is_empty() {
                    blocks.push(curr_block);
                }
                curr_block = BasicBlock {
                    start_idx: i,
                    statements: Vec::new(),
                };
            }
            curr_block.statements.push(instr.clone());
            // If the instruction is a branch, we need to close this block
            // with this instruction and start a new block from the next one.
            if is_branch {
                blocks.push(curr_block); // guaranteed to be non-empty because an instruction was just pushed
                curr_block = BasicBlock {
                    start_idx: i + 1,
                    statements: Vec::new(),
                };
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
