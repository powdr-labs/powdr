use std::{collections::{HashMap, VecDeque}, fmt::Display};

use itertools::Itertools;
use serde::{Deserialize, Serialize};

/// Tools to detect basic blocks in a program
mod detection;

pub use detection::collect_basic_blocks;

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct BasicBlock<I> {
    /// The program counter of the first instruction in this block.
    pub start_pc: u64,
    /// When not empty, indicates a superblock.
    /// Each entry is a tuple of (relative instruction index inside the superblock, original BB start_pc).
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

/// Uses the PC sequence of a given execution to detect superblocks.
/// Returns all blocks and superblocks (of up a to maximum sequence length), together with their execution counts.
/// Doesn't return blocks with a single instruction or that were never executed.
pub fn generate_superblocks<I: Clone>(
    execution_pc_list: &[u64],
    blocks: &[BasicBlock<I>],
    max_len: usize
) -> (Vec<BasicBlock<I>>, HashMap<usize, u32>) {
    let mut bb_seq_count = HashMap::new();
    // we go through the PCs maintaining the last N basic blocks that executed
    let mut bb_window = VecDeque::new();

    println!("generating superblocks of size <= {max_len}, going over sequence of {} PCs", execution_pc_list.len());

    // make a hash map from start_pc to BB
    let pc_to_block_idx: HashMap<_,_> = blocks.iter().enumerate().map(|(idx, bb)| (bb.start_pc, idx)).collect();

    for pc in execution_pc_list {
        let Some(&bb_idx) = pc_to_block_idx.get(pc) else {
            // still in the same BB
            continue;
        };

        // if starting a single instruction BB, clear current sequence
        if blocks[bb_idx].statements.len() <= 1 {
            bb_window.clear();
            continue;
        }

        bb_window.push_back(bb_idx);
        // limit window size
        if bb_window.len() > max_len {
            bb_window.pop_front();
        }

        // update count for each superblock size in current window
        for len in 1..=bb_window.len() {
            let seq: Vec<usize> = bb_window.iter().skip(bb_window.len() - len).cloned().collect();
            bb_seq_count.entry(seq)
                .and_modify(|c| *c += 1)
                .or_insert(1);
        }
    }

    let mut super_blocks = vec![];
    let mut counts = HashMap::new();

    // go through the seen BB sequences and create the superblocks
    bb_seq_count.into_iter()
        .for_each(|(seq, count)| {
            let blocks = seq.iter().map(|&idx| &blocks[idx]).collect_vec();
            let start_pc = blocks[0].start_pc;
            let mut curr_statement = blocks[0].statements.len();
            let other_pcs = blocks.iter().skip(1).map(|block| {
                let relative_start = curr_statement;
                curr_statement += block.statements.len();
                (relative_start, block.start_pc)
            }).collect_vec();
            let statements = blocks.iter().flat_map(|block| &block.statements).cloned().collect_vec();

            let idx = super_blocks.len();
            super_blocks.push(BasicBlock { start_pc, other_pcs, statements });
            counts.insert(idx, count);
        });

    (super_blocks, counts)
}
