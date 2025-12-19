use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
};

use itertools::Itertools;
use serde::{Deserialize, Serialize};

/// Tools to detect basic blocks in a program
mod detection;

pub use detection::collect_basic_blocks;

#[derive(Debug, Serialize, Deserialize, Clone)]
/// A sequence of instructions starting at a given PC.
/// If `other_pcs` is empty, this is a basic block.
/// If `other_pcs` is non-empty, this is a superblock, that is, a sequence of basic blocks.
pub struct Block<I> {
    /// The program counter of the first instruction in this block.
    pub start_pc: u64,
    /// When not empty, indicates a superblock.
    /// Each entry is a tuple of (relative instruction index inside the superblock, original basic block start_pc).
    pub other_pcs: Vec<(usize, u64)>,
    pub statements: Vec<I>,
}

impl<I> Block<I> {
    /// Starting PCs of each original basic block
    pub fn original_pcs(&self) -> Vec<u64> {
        [
            vec![self.start_pc],
            self.other_pcs.iter().map(|(_, pc)| *pc).collect(),
        ]
        .concat()
    }
}

impl<I: Display> Display for Block<I> {
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

/// Count how many times the `needle` sequence appears inside the `haystack` sequence.
/// It does not count overlapping sequences (e.g. `aba` is counted only twice in `abababa`).
fn count_non_overlapping(haystack: &[usize], needle: &[usize]) -> u32 {
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
    blocks: &[Block<I>],
    max_len: usize,
) -> (Vec<Block<I>>, HashMap<usize, u32>) {
    tracing::info!(
        "Detecting superblocks of size <= {max_len}, over the sequence of {} PCs",
        execution_pc_list.len()
    );

    // make a hash map from start_pc to BB
    let bb_start_pc_to_idx: HashMap<_, _> = blocks
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
        if blocks[bb_idx].statements.len() <= 1 {
            if !current_run.is_empty() {
                execution_bb_runs.push(std::mem::take(&mut current_run));
            }
            continue;
        }

        current_run.push(bb_idx);

        // add to seen set the superblocks (size 1..max_len) ending at the last BB of the current run
        for len in 1..=std::cmp::min(max_len, current_run.len()) {
            let sblock: Vec<usize> = current_run
                .iter()
                .skip(current_run.len() - len)
                .cloned()
                .collect();
            seen_superblocks.insert(sblock);
        }
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
    let mut counts = HashMap::new();
    superblock_count.into_iter().for_each(|(sblock, count)| {
        let blocks = sblock.iter().map(|&idx| &blocks[idx]).collect_vec();
        let start_pc = blocks[0].start_pc;
        let mut curr_statement = blocks[0].statements.len();
        let other_pcs = blocks
            .iter()
            .skip(1)
            .map(|block| {
                let relative_start = curr_statement;
                curr_statement += block.statements.len();
                (relative_start, block.start_pc)
            })
            .collect_vec();
        let statements = blocks
            .iter()
            .flat_map(|block| &block.statements)
            .cloned()
            .collect_vec();

        let idx = super_blocks.len();
        super_blocks.push(Block {
            start_pc,
            other_pcs,
            statements,
        });
        counts.insert(idx, count);
    });

    (super_blocks, counts)
}
