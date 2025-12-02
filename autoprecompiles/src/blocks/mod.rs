use std::fmt::Display;

use serde::{Deserialize, Serialize};

/// Tools to detect basic blocks in a program
mod detection;

pub use detection::collect_basic_blocks;

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct BasicBlock<I> {
    /// The program counter of the first instruction in this block.
    pub start_pc: u64,
    /// Map instruction idx and other concrete pcs
    pub other_pcs: Vec<(usize, u64)>,
    pub statements: Vec<I>,
}

impl<I: Display> Display for BasicBlock<I> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "BasicBlock(start_pc: {}, statements: [", self.start_pc)?;
        for (i, instr) in self.statements.iter().enumerate() {
            writeln!(f, "   instr {i:>3}:   {instr}")?;
        }
        write!(f, "])")
    }
}

pub trait Program<I> {
    /// Returns the base program counter.
    fn base_pc(&self) -> u64;

    /// Returns the step size of the program counter.
    fn pc_step(&self) -> u32;

    /// Converts an instruction index to a program counter.
    fn instruction_index_to_pc(&self, idx: usize) -> u64 {
        self.base_pc() + (idx as u64 * self.pc_step() as u64)
    }

    /// Returns an iterator over the instructions in the program.
    fn instructions(&self) -> Box<dyn Iterator<Item = I> + '_>;

    /// Returns the number of instructions in the program.
    fn length(&self) -> u32;
}

pub trait Instruction<T>: Clone + Display {
    /// Returns a list of concrete values that the LHS of the PC lookup should be assigned to.
    /// An entry can be `None` to indicate that the value is not known at compile time.
    /// The provided PC will in practice be provided for the first instruction of the block.
    fn pc_lookup_row(&self, pc: Option<u64>) -> Vec<Option<T>>;
}
