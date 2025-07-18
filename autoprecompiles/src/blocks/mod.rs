use itertools::Itertools;
use serde::{Deserialize, Serialize};

/// Tools to detect basic blocks in a program
mod detection;
/// Tools to generate autoprecompiles using different PGO strategies
mod pgo;
/// Tools to select autoprecompiles using a knapsack-like algorithm
mod selection;

pub use detection::collect_basic_blocks;
pub use pgo::PgoConfig;
pub use pgo::{generate_apcs_with_pgo, Candidate};
pub use selection::KnapsackItem;

use crate::SymbolicInstructionStatement;

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct BasicBlock<I> {
    /// The index of the first instruction in this block in the original program.
    pub start_idx: usize,
    pub statements: Vec<I>,
}

impl<I> BasicBlock<I> {
    pub fn pretty_print(&self, instr_formatter: impl Fn(&I) -> String) -> String {
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

    /// Returns the address of the first instruction of this block given
    /// the address of the first instruction of the whole program and the
    /// size of one instruction.
    pub fn start_address(&self, base_pc: u32, pc_step: u32) -> u32 {
        base_pc + self.start_idx as u32 * pc_step
    }
}

pub trait Program<I> {
    /// Returns the base program counter.
    fn base_pc(&self) -> u64;

    /// Returns the step size of the program counter.
    fn pc_step(&self) -> u32;

    /// Returns an iterator over the instructions in the program.
    fn instructions(&self) -> Box<dyn Iterator<Item = I> + '_>;
}

pub trait Instruction<T>: Clone {
    /// The opcode of the instruction.
    fn opcode(&self) -> usize;

    /// Turns the instruction into a symbolic representation.
    fn into_symbolic_instruction(self) -> SymbolicInstructionStatement<T>;
}
