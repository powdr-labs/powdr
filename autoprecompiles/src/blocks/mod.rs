use std::{
    collections::HashMap,
    fmt::Display,
};

use itertools::{Either, Itertools};
use rayon::iter::{IndexedParallelIterator, IntoParallelRefIterator, ParallelIterator};
use serde::{Deserialize, Serialize};

/// Tools to detect basic blocks in a program
mod detection;

pub use detection::collect_basic_blocks;

#[derive(Debug, Serialize, Deserialize, Clone)]
/// A sequence of instructions starting at a given PC.
pub struct BasicBlock<I> {
    /// The program counter of the first instruction in this block.
    pub start_pc: u64,
    pub statements: Vec<I>,
}

impl<I: PcStep> BasicBlock<I> {
    /// Returns an iterator over the program counters of the instructions in this block.
    pub fn pcs(&self) -> impl Iterator<Item = u64> + '_ {
        (0..self.statements.len()).map(move |i| self.start_pc + (i as u64 * I::pc_step() as u64))
    }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct SuperBlock<I> {
    pub blocks: Vec<BasicBlock<I>>,
}

impl<I> SuperBlock<I> {
    pub fn original_bb_pcs(&self) -> Vec<u64> {
        self.blocks.iter().map(|b| b.start_pc).collect()
    }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub enum Block<I> {
    Basic(BasicBlock<I>),
    Super(SuperBlock<I>),
}

impl<I: PcStep> Block<I> {
    /// Returns an iterator over the program counters of the instructions in this block.
    pub fn pcs(&self) -> impl Iterator<Item = u64> + '_ {
        match self {
            Block::Basic(basic_block) => Either::Left(basic_block.pcs()),
            Block::Super(super_block) => Either::Right(super_block.blocks.iter().flat_map(BasicBlock::pcs)),
        }
    }
}

impl<I> Block<I> {
    pub fn is_superblock(&self) -> bool {
        matches!(self, Block::Super(_))
    }

    /// Starting PCs of each original basic block
    pub fn original_bb_pcs(&self) -> Vec<u64> {
        match self {
            Block::Basic(basic_block) => vec![basic_block.start_pc],
            Block::Super(super_block) => super_block.original_bb_pcs(),
        }
    }

    /// Instruction index of the start of each original basic
    pub fn insn_indexed_bb_pcs(&self) -> Vec<(usize, u64)> {
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

pub struct ProgramBlocks<I> {
    /// Superblocks of len (1..=max_len) seen in the execution.
    pub blocks: Vec<Block<I>>,
    /// Count of each of the seen superblocks. None if running without PGO.
    pub counts: Option<Vec<u32>>,
    /// Basic block runs in the given PGO execution.
    /// Each run is paired with the number of times it was seen in the execution.
    /// `None` if running without PGO.
    pub execution_bb_runs: Option<Vec<(Vec<u64>, u32)>>,
    /// Map from block to the runs it appears in (to avoid searching later)
    /// TODO(leandro): remove this if we dont use it
    pub block_to_runs: Option<Vec<Vec<usize>>>,
}

impl<I> ProgramBlocks<I> {
    pub fn new_without_pgo(blocks: Vec<Block<I>>) -> Self {
        Self {
            blocks,
            counts: None,
            execution_bb_runs: None,
            block_to_runs: None,
        }
    }
}

/// Find all superblocks up to max_len in run and count their ocurrences
fn superblocks_in_run(run: &[u64], max_len: usize) -> HashMap<Vec<u64>, u32> {
    let mut superblocks_in_run = HashMap::new();
    // first, we identify the superblocks in this run
    for len in 1..=std::cmp::min(max_len, run.len()) {
        superblocks_in_run.extend(run.windows(len).map(|w| (w.to_vec(), 0)));
    }
    // then we count their ocurrences
    for (sblock, tally) in superblocks_in_run.iter_mut() {
        let count = count_non_overlapping(&run, sblock);
        *tally = count
    }
    superblocks_in_run
}

/// Uses the PC sequence of a given execution to detect superblocks.
/// Returns all blocks and superblocks (of up a to maximum sequence length), together with their execution counts.
/// Ignores invalid APC blocks (i.e., single instruction) and blocks that were never executed.
pub fn generate_superblocks<I: Clone>(
    execution_pc_list: &[u64],
    basic_blocks: &[BasicBlock<I>],
    max_len: usize,
    exec_count_cutoff: u32,
) -> ProgramBlocks<I> {
    tracing::info!(
        "Detecting superblocks of size <= {max_len}, over the sequence of {} PCs",
        execution_pc_list.len()
    );

    let start = std::time::Instant::now();

    // make a hash map from start_pc to BB
    let bb_start_pc_to_idx: HashMap<_, _> = basic_blocks
        .iter()
        .enumerate()
        .map(|(idx, bb)| (bb.start_pc, idx))
        .collect();

    // List of basic block runs in the execution (i.e., sequences of BBs without single-instruction BBs in between).
    // The same run can appear multiple times in the execution, we keep a count using a hashmap.
    // Each BB is identified by its starting PC
    let mut execution_bb_runs = HashMap::new();
    let mut current_run = vec![];

    // split execution into basic block runs
    for pc in execution_pc_list {
        let Some(&bb_idx) = bb_start_pc_to_idx.get(pc) else {
            // still in the same BB
            continue;
        };
        // if starting a single instruction BB (i.e., invalid for APC), end current run
        if basic_blocks[bb_idx].statements.len() <= 1 {
            if !current_run.is_empty() {
                *execution_bb_runs.entry(std::mem::take(&mut current_run)).or_insert(0) += 1;
            }
            continue;
        }
        current_run.push(*pc);
    }
    if !current_run.is_empty() {
        *execution_bb_runs.entry(std::mem::take(&mut current_run)).or_insert(0) += 1;
    }

    let execution_bb_runs: Vec<(Vec<u64>, u32)> = execution_bb_runs.into_iter().collect();

    // Find and count the ocurrences of superblocks of up to max_len in each run.
    // Concurrently, build a map from superblock to the runs it appears in.
    let (mut block_to_runs_map, superblock_counts) = execution_bb_runs.par_iter().enumerate().map(|(run_idx, (run, run_count))| {
        let mut run_superblock_counts = superblocks_in_run(run, max_len);
        run_superblock_counts.values_mut().for_each(|v| *v *= run_count);
        let block_to_run: HashMap<_, _> = run_superblock_counts.keys()
            .map(|sblock| (sblock.clone(), vec![run_idx]))
            .collect();
        (block_to_run, run_superblock_counts)
    }).reduce(||(HashMap::new(), HashMap::new()), |(mut a_runs, mut a_counts), (b_runs, b_counts)| {
        for (sblock, count) in b_counts {
            *a_counts.entry(sblock).or_insert(0) += count;
        }
        for (sblock, runs) in b_runs {
            a_runs.entry(sblock).or_default().extend(runs);
        }
        (a_runs, a_counts)
    });

    tracing::info!(
        "Found {} blocks in {} basic block runs! Took {:?}",
        superblock_counts.len(),
        execution_bb_runs.len(),
        start.elapsed(),
    );

    // build the resulting BasicBlock's and counts
    let mut super_blocks = vec![];
    let mut counts = vec![];
    let mut block_to_runs = vec![];
    let mut skipped = 0;
    superblock_counts.into_iter().for_each(|(sblock, count)| {
        // convert PCs into BasicBlocks
        let mut blocks = sblock
            .iter()
            .map(|start_pc| basic_blocks[bb_start_pc_to_idx[start_pc]].clone())
            .collect_vec();

        if blocks.len() == 1 {
            super_blocks.push(Block::Basic(blocks.pop().unwrap()));
        } else {
            if count < exec_count_cutoff {
                // skip superblocks that were executed less than the cutoff
                tracing::trace!(
                    "Skipping superblock {:?} due to execution count below cutoff ({})",
                    sblock,
                    exec_count_cutoff,
                );
                skipped += 1;
                return;
            }
            super_blocks.push(Block::Super(SuperBlock { blocks }));
        }

        counts.push(count);

        let mut runs = block_to_runs_map.remove(&sblock).unwrap();
        runs.sort_unstable();
        block_to_runs.push(runs);
    });

    tracing::info!(
        "{} blocks were skipped due to execution cutoff of {}, {} blocks remain",
        skipped,
        exec_count_cutoff,
        super_blocks.len(),
    );

    tracing::info!(
        "Out of those, {} are basic blocks and {} are superblocks",
        super_blocks.iter().filter(|b| !b.is_superblock()).count(),
        super_blocks.iter().filter(|b| b.is_superblock()).count(),
    );


    ProgramBlocks {
        blocks: super_blocks,
        counts: Some(counts),
        execution_bb_runs: Some(execution_bb_runs),
        block_to_runs: Some(block_to_runs),
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_superblocks_in_run() {
        let run = vec![4, 1, 2, 3, 5, 1, 2, 3, 4];
        let max_len = 3;
        let counts = superblocks_in_run(&run, max_len);
        println!("{:?}", counts.keys().collect::<Vec<_>>());
        assert_eq!(counts.len(),
                   5 + // size 1
                   6 + // size 2
                   6   // size 3
        );
        assert_eq!(counts[&vec![1]], 2);
        assert_eq!(counts[&vec![1,2]], 2);
        assert_eq!(counts[&vec![4]], 2);
        assert_eq!(counts[&vec![5]], 1);
        assert_eq!(counts[&vec![4,1,2]], 1);
        assert_eq!(counts[&vec![1,2,3]], 2);
        assert_eq!(counts[&vec![2,3,4]], 1);
    }
}
