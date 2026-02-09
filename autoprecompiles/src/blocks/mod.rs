use std::collections::HashMap;
use std::fmt::Display;

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
/// A sequence of basic blocks observed during execution.
pub struct SuperBlock<I> {
    pub blocks: Vec<BasicBlock<I>>,
}

impl<I> SuperBlock<I> {
    /// Starting PCs of the original basic blocks.
    /// Uniquely identifies a superblock.
    pub fn original_bb_pcs(&self) -> Vec<u64> {
        self.blocks.iter().map(|b| b.start_pc).collect()
    }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
/// A sequence of instructions that can be made into an autoprecompile.
/// It can be either a basic block or a superblock.
pub enum Block<I> {
    Basic(BasicBlock<I>),
    Super(SuperBlock<I>),
}

impl<I: PcStep> Block<I> {
    /// Returns an iterator over the program counters of the instructions in this block.
    pub fn pcs(&self) -> impl Iterator<Item = u64> + '_ {
        match self {
            Block::Basic(basic_block) => Either::Left(basic_block.pcs()),
            Block::Super(super_block) => {
                Either::Right(super_block.blocks.iter().flat_map(BasicBlock::pcs))
            }
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

    /// Instruction index of the start of each original basic block
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

    /// Iterator over the original basic blocks
    pub fn original_bbs(&self) -> impl Iterator<Item = &BasicBlock<I>> {
        match self {
            Block::Basic(basic_block) => Either::Left(std::iter::once(basic_block)),
            Block::Super(super_block) => Either::Right(super_block.blocks.iter()),
        }
    }

    /// Iterator over all instructions in the block, in order
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

/// Blocks (basic blocks and superblocks) for a given program, together with execution data (if PGO is enabled).
pub struct ProgramBlocks<I> {
    /// Blocks seen in the execution (basic blocks and superblocks up to some length).
    pub blocks: Vec<Block<I>>,
    /// Count of each of the seen superblocks. None if running without PGO.
    pub counts: Option<Vec<u32>>,
    /// Basic block runs in the given PGO execution.
    /// Each run is paired with the number of times it was seen in the execution.
    /// `None` if running without PGO.
    pub execution_bb_runs: Option<Vec<(Vec<u64>, u32)>>,
    /// Map from block to the runs it appears in (to avoid searching later)
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

/// Find basic block runs in the execution (sequence of PCs).
/// A run is interrupted upon hitting an invalid APC block (i.e., a single-instruction block).
/// Returns a list of the runs, together with how many times it appears (a run may repeat in the execution).
fn detect_execution_bb_runs<I>(
    basic_blocks: &[BasicBlock<I>],
    // map from PC to the index of the basic block starting at that PC
    start_pc_to_bb: &HashMap<u64, usize>,
    execution: &[u64],
) -> Vec<(Vec<u64>, u32)> {
    // Basic block runs in the execution (i.e., sequences of BBs without single-instruction BBs in between).
    // The same run can appear multiple times in the execution, so we keep a count using a hashmap.
    // Each BB is identified by its starting PC
    let mut execution_bb_runs = HashMap::new();
    let mut current_run = vec![];

    // detect basic block runs
    for pc in execution {
        let Some(&bb_idx) = start_pc_to_bb.get(pc) else {
            // still in the same BB
            continue;
        };
        // if starting a single instruction BB (i.e., invalid for APC), end current run
        if basic_blocks[bb_idx].statements.len() <= 1 {
            if !current_run.is_empty() {
                *execution_bb_runs
                    .entry(std::mem::take(&mut current_run))
                    .or_insert(0) += 1;
            }
            continue;
        }
        current_run.push(*pc);
    }
    if !current_run.is_empty() {
        *execution_bb_runs
            .entry(std::mem::take(&mut current_run))
            .or_insert(0) += 1;
    }

    execution_bb_runs.into_iter().collect()
}

/// Find all superblocks up to max_len in run and count their ocurrences
fn count_superblocks_in_run(run: &[u64], max_len: usize) -> HashMap<Vec<u64>, u32> {
    let mut superblocks_in_run = HashMap::new();
    // first, we identify the superblocks in this run
    for len in 1..=std::cmp::min(max_len, run.len()) {
        superblocks_in_run.extend(run.windows(len).map(|w| (w.to_vec(), 0)));
    }
    // then we count their ocurrences
    #[allow(clippy::iter_over_hash_type)]
    for (sblock, count) in superblocks_in_run.iter_mut() {
        *count = count_non_overlapping(run, sblock);
    }
    superblocks_in_run
}

/// Find and count the ocurrences of blocks of up to max_len in each run.
/// Returns a map from block to its count and runs it is found in.
fn count_superblocks_in_execution(
    execution_bb_runs: &[(Vec<u64>, u32)],
    max_len: usize,
) -> HashMap<Vec<u64>, (u32, Vec<usize>)> {
    let sblocks = execution_bb_runs
        .par_iter()
        .enumerate()
        .map(|(run_idx, (run, run_count))| {
            count_superblocks_in_run(run, max_len)
                .into_iter()
                .map(|(sblock, count)| (sblock, (count * run_count, vec![run_idx])))
                .collect()
        })
        .reduce(HashMap::new, |mut sblocks_a, sblocks_b| {
            #[allow(clippy::iter_over_hash_type)]
            for (sblock, (count, runs)) in sblocks_b {
                match sblocks_a.entry(sblock) {
                    std::collections::hash_map::Entry::Vacant(entry) => {
                        entry.insert((count, runs));
                    }
                    std::collections::hash_map::Entry::Occupied(mut entry) => {
                        entry.get_mut().0 += count;
                        entry.get_mut().1.extend(runs);
                    }
                }
            }
            sblocks_a
        });
    sblocks
}

/// Detect blocks (basic blocks and superblocks up to the given length) present in the given execution.
/// Returns the detected blocks, together with their execution counts.
/// Does not return invalid APC blocks (i.e., single instruction) and blocks that are never executed.
pub fn detect_superblocks<I: Clone>(
    execution_pc_list: &[u64],
    basic_blocks: &[BasicBlock<I>],
    max_len: usize,
    exec_count_cutoff: u32,
) -> ProgramBlocks<I> {
    tracing::info!(
        "Detecting superblocks of size <= {max_len} over the execution (sequence of {} PCs)",
        execution_pc_list.len()
    );

    let start = std::time::Instant::now();

    // make a hash map from start_pc to BB
    let bb_start_pc_to_idx: HashMap<_, _> = basic_blocks
        .iter()
        .enumerate()
        .map(|(idx, bb)| (bb.start_pc, idx))
        .collect();

    let execution_bb_runs =
        detect_execution_bb_runs(basic_blocks, &bb_start_pc_to_idx, execution_pc_list);

    let blocks_found = count_superblocks_in_execution(&execution_bb_runs, max_len);

    tracing::info!(
        "Found {} blocks in {} basic block runs. Took {:?}",
        blocks_found.len(),
        execution_bb_runs.len(),
        start.elapsed(),
    );

    // build the result
    let mut super_blocks = vec![];
    let mut counts = vec![];
    let mut block_to_runs = vec![];
    let mut skipped = 0;
    blocks_found
        .into_iter()
        .for_each(|(sblock, (count, mut runs))| {
            // convert PCs into BasicBlock's
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
    fn test_count_superblocks_in_run() {
        let run = vec![4, 1, 2, 3, 5, 1, 2, 3, 4];
        let max_len = 3;
        let counts = count_superblocks_in_run(&run, max_len);
        assert_eq!(counts.len(), 17);
        assert_eq!(counts[&vec![1]], 2);
        assert_eq!(counts[&vec![2]], 2);
        assert_eq!(counts[&vec![3]], 2);
        assert_eq!(counts[&vec![4]], 2);
        assert_eq!(counts[&vec![5]], 1);
        assert_eq!(counts[&vec![4, 1]], 1);
        assert_eq!(counts[&vec![1, 2]], 2);
        assert_eq!(counts[&vec![2, 3]], 2);
        assert_eq!(counts[&vec![3, 5]], 1);
        assert_eq!(counts[&vec![5, 1]], 1);
        assert_eq!(counts[&vec![3, 4]], 1);
        assert_eq!(counts[&vec![4, 1, 2]], 1);
        assert_eq!(counts[&vec![1, 2, 3]], 2);
        assert_eq!(counts[&vec![2, 3, 5]], 1);
        assert_eq!(counts[&vec![3, 5, 1]], 1);
        assert_eq!(counts[&vec![5, 1, 2]], 1);
        assert_eq!(counts[&vec![2, 3, 4]], 1);
    }
}
