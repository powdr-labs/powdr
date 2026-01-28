use std::fmt::Display;

use serde::{Deserialize, Serialize};

/// Tools to detect basic blocks in a program
mod detection;

pub use detection::collect_basic_blocks;

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct BasicBlock<I> {
    /// The program counter of the first instruction in this block.
    pub pcs: Vec<u64>,
    pub statements: Vec<I>,
}

impl<I: PcStep> BasicBlock<I> {
    pub fn from_start_pc_and_statements(start_pc: u64, statements: Vec<I>) -> Self {
        let pcs = (0..statements.len())
            .map(|i| start_pc + (i as u64 * I::pc_step() as u64))
            .collect();
        Self { pcs, statements }
    }
}

impl<I> BasicBlock<I> {
    pub fn start_pc(&self) -> u64 {
        self.pcs
            .first()
            .copied()
            .expect("basic block must have at least one pc")
    }

    /// Returns an iterator over the program counters of the instructions in this block.
    pub fn pcs(&self) -> impl Iterator<Item = u64> + '_ {
        self.pcs.iter().copied()
    }
}

impl<I: Display> Display for BasicBlock<I> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // TODO: Show all PCs.
        writeln!(f, "BasicBlock(start_pc: {}, statements: [", self.start_pc())?;
        for (i, instr) in self.statements.iter().enumerate() {
            writeln!(f, "   instr {i:>3}:   {instr}")?;
        }
        write!(f, "])")
    }
}

pub trait Program<I: PcStep> {
    /// Returns the base program counter.
    fn base_pc(&self) -> u64;

    /// Converts an instruction index to a program counter.
    fn instruction_index_to_pc(&self, idx: usize) -> u64 {
        self.base_pc() + (idx as u64 * I::pc_step() as u64)
    }

    /// Returns an iterator over the instructions in the program.
    fn instructions(&self) -> Box<dyn Iterator<Item = I> + '_>;

    /// Returns the number of instructions in the program.
    fn length(&self) -> u32;
}

pub trait PcStep {
    fn pc_step() -> u32;
}

pub trait Instruction<T>: Clone + Display + PcStep {
    /// Returns a list of concrete values that the LHS of the PC lookup should be assigned to.
    fn pc_lookup_row(&self, pc: u64) -> Vec<T>;
}
