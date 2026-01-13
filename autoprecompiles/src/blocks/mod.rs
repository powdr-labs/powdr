use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
};

use itertools::{Either, Itertools};
use serde::{Deserialize, Serialize};

/// Tools to detect basic blocks in a program
mod detection;

pub use detection::collect_basic_blocks;

#[derive(Debug, Serialize, Deserialize, Clone)]
/// A sequence of instructions starting at a given PC.
/// If `other_pcs` is empty, this is a basic block.
/// If `other_pcs` is non-empty, this is a superblock, that is, a sequence of basic blocks.
pub struct BasicBlock<I> {
    /// The program counter of the first instruction in this block.
    pub start_pc: u64,
    pub statements: Vec<I>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct SuperBlock<I> {
    pub blocks: Vec<BasicBlock<I>>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub enum Block<I> {
    Basic(BasicBlock<I>),
    Super(SuperBlock<I>),
}

impl<I> SuperBlock<I> {
    pub fn original_pcs(&self) -> Vec<u64> {
        self.blocks.iter().map(|b| b.start_pc).collect()
    }
}

impl<I> Block<I> {
    /// Starting PCs of each original basic block
    pub fn original_pcs(&self) -> Vec<u64> {
        match self {
            Block::Basic(basic_block) => vec![basic_block.start_pc],
            Block::Super(super_block) => super_block.original_pcs(),
        }
    }

    /// Starting instruction index of each original PC
    pub fn insn_indexed_pcs(&self) -> Vec<(usize, u64)> {
        match self {
            Block::Basic(basic_block) => {
                vec![(0, basic_block.start_pc)]
            }
            Block::Super(super_block) => {
                let mut idx = 0;
                super_block
                    .blocks
                    .iter()
                    .map(|b| {
                        let elem = (idx, b.start_pc);
                        idx += b.statements.len();
                        elem
                    })
                    .collect()
            }
        }
    }

    pub fn original_blocks(&self) -> impl Iterator<Item = &BasicBlock<I>> {
        match self {
            Block::Basic(basic_block) => Either::Left(std::iter::once(basic_block)),
            Block::Super(super_block) => Either::Right(super_block.blocks.iter()),
        }
    }

    pub fn statements(&self) -> impl Iterator<Item = &I> + Clone {
        match self {
            Block::Basic(basic_block) => Either::Left(basic_block.statements.iter()),
            Block::Super(super_block) => {
                Either::Right(super_block.blocks.iter().flat_map(|b| &b.statements))
            }
        }
    }
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

impl<I: Display> Display for SuperBlock<I> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "SuperBlock(")?;
        let mut insn_idx = 0;
        for block in &self.blocks {
            writeln!(f, "   pc: {}, statements: [", block.start_pc)?;
            for instr in block.statements.iter() {
                writeln!(f, "      instr {insn_idx:>3}:   {instr}")?;
                insn_idx += 1;
            }
            write!(f, "   ],")?;
        }
        write!(f, ")")
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

/// Count how many times the `needle` sequence appears inside the `haystack` sequence.
/// It does not count overlapping sequences (e.g. `aba` is counted only twice in `abababa`).
pub fn count_non_overlapping<T: Eq>(haystack: &[T], needle: &[T]) -> u32 {
    let mut count = 0;
    let mut pos = 0;
    while pos + needle.len() <= haystack.len() {
        if haystack[pos..pos + needle.len()] == needle[..] {
            count += 1;
            pos += needle.len();
        } else {
            pos += 1;
        }
    }
    count
}

/// Uses the PC sequence of a given execution to detect superblocks.
/// Returns all blocks and superblocks (of up a to maximum sequence length), together with their execution counts.
/// Doesn't return blocks with a single instruction or that were never executed.
pub fn generate_superblocks<I: Clone>(
    execution_pc_list: &[u64],
    basic_blocks: &[BasicBlock<I>],
    max_len: usize,
) -> (Vec<Block<I>>, Vec<u32>) {
    tracing::info!(
        "Detecting superblocks of size <= {max_len}, over the sequence of {} PCs",
        execution_pc_list.len()
    );

    // make a hash map from start_pc to BB
    let bb_start_pc_to_idx: HashMap<_, _> = basic_blocks
        .iter()
        .enumerate()
        .map(|(idx, bb)| (bb.start_pc, idx))
        .collect();

    // set of all superblocks seen. Each superblock is identified by the starting PCs of its basic blocks.
    let mut seen_superblocks: HashSet<Vec<usize>> = HashSet::new();
    // list basic block runs in the execution (i.e., sequences of BBs without single-instruction BBs in between)
    let mut execution_bb_runs = vec![];
    let mut current_run = vec![];

    // here, we go over the execution PCs to:
    // (1) identify basic block runs
    // (2) collect the superblocks seen in the execution
    for pc in execution_pc_list {
        let Some(&bb_idx) = bb_start_pc_to_idx.get(pc) else {
            // still in the same BB
            continue;
        };

        // if starting a single instruction BB, clear current sequence
        if basic_blocks[bb_idx].statements.len() <= 1 {
            if !current_run.is_empty() {
                // add superblocks seen in this run
                for len in 1..=std::cmp::min(max_len, current_run.len()) {
                    seen_superblocks.extend(current_run.windows(len).map(|w| w.to_vec()));
                }
                execution_bb_runs.push(std::mem::take(&mut current_run));
            }
            continue;
        }

        current_run.push(bb_idx);
    }

    tracing::info!(
        "Found {} superblocks in {} basic block runs!",
        seen_superblocks.len(),
        execution_bb_runs.len()
    );

    // here, we go over the BB runs to count how many times each superblock can be executed
    let mut superblock_count = HashMap::new();
    for sblock in seen_superblocks {
        for run in &execution_bb_runs {
            let count = count_non_overlapping(run, &sblock);
            if count > 0 {
                *superblock_count.entry(sblock.clone()).or_default() += count;
            }
        }
    }

    // build the resulting BasicBlock's and counts
    let mut super_blocks = vec![];
    let mut counts = vec![];
    superblock_count.into_iter().for_each(|(sblock, count)| {
        let mut blocks = sblock
            .iter()
            .map(|&idx| basic_blocks[idx].clone())
            .collect_vec();

        if blocks.len() == 1 {
            super_blocks.push(Block::Basic(blocks.pop().unwrap()));
        } else {
            super_blocks.push(Block::Super(SuperBlock { blocks }));
        }
        counts.push(count);
    });

    (super_blocks, counts)
}
