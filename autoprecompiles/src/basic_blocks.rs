use std::collections::BTreeSet;

use itertools::Itertools;
use serde::{Deserialize, Serialize};

use crate::SymbolicInstructionStatement;

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct BasicBlock<T> {
    // The index of the first instruction in this block in the original program.
    pub start_idx: usize,
    pub statements: Vec<SymbolicInstructionStatement<T>>,
}

impl<T> BasicBlock<T> {
    pub fn pretty_print(
        &self,
        instr_formatter: impl Fn(&SymbolicInstructionStatement<T>) -> String,
    ) -> String {
        format!("BasicBlock(start_idx: {}, statements: [\n", self.start_idx)
            + &self
                .statements
                .iter()
                .enumerate()
                .map(|(i, instr)| format!("   instr {i:>3}:   {}", instr_formatter(instr)))
                .format("\n")
                .to_string()
            + "\n])"
    }
}

/// Represents a symbolic program, which is a sequence of symbolic instructions
pub struct Program<T> {
    // The address of the first instruction in the program.
    pub base_pc: u32,
    // The step size between addresses of consecutive instructions.
    pub pc_step: u32,
    // The instructions in the program.
    pub instructions: Vec<SymbolicInstructionStatement<T>>,
}

impl<T> Program<T> {
    pub fn new(
        instructions: Vec<SymbolicInstructionStatement<T>>,
        base_pc: u32,
        pc_step: u32,
    ) -> Self {
        Self {
            base_pc,
            pc_step,
            instructions,
        }
    }
}

pub fn collect_basic_blocks<T: Clone>(
    program: &Program<T>,
    labels: &BTreeSet<u32>,
    opcode_allowlist: &BTreeSet<usize>,
    branch_opcodes: &BTreeSet<usize>,
) -> Vec<BasicBlock<T>> {
    let mut blocks = Vec::new();
    let mut curr_block = BasicBlock {
        start_idx: 0,
        statements: Vec::new(),
    };
    for (i, instr) in program.instructions.iter().enumerate() {
        let pc = program.base_pc + i as u32 * program.pc_step;
        let is_target = labels.contains(&pc);
        let is_branch = branch_opcodes.contains(&instr.opcode);

        // If this opcode cannot be in an apc, we make sure it's alone in a BB.
        if !opcode_allowlist.contains(&instr.opcode) {
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

    blocks
}
