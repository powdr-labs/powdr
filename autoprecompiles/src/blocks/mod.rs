use std::{collections::HashMap, fmt::Display};

use itertools::Itertools;
use rayon::iter::{IndexedParallelIterator, IntoParallelIterator};
use serde::{Deserialize, Serialize};

/// Tools to detect basic blocks in a program
mod detection;

pub use detection::collect_basic_blocks;

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct BasicBlock<I> {
    /// The program counter of the first instruction in this block.
    pub start_pc: u64,
    pub instructions: Vec<I>,
}

impl<I: PcStep> BasicBlock<I> {
    /// Returns an iterator over the program counters of the instructions in this block.
    pub fn pcs(&self) -> impl Iterator<Item = u64> + '_ {
        (0..self.instructions.len()).map(move |i| self.start_pc + (i as u64 * I::pc_step() as u64))
    }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct SuperBlock<I> {
    blocks: Vec<BasicBlock<I>>,
}

impl<I> From<BasicBlock<I>> for SuperBlock<I> {
    fn from(basic_block: BasicBlock<I>) -> Self {
        SuperBlock {
            blocks: vec![basic_block],
        }
    }
}

impl<I> From<Vec<BasicBlock<I>>> for SuperBlock<I> {
    fn from(blocks: Vec<BasicBlock<I>>) -> Self {
        assert!(!blocks.is_empty());
        SuperBlock { blocks }
    }
}

impl<I> SuperBlock<I> {
    pub fn is_basic_block(&self) -> bool {
        self.blocks.len() == 1
    }

    pub fn try_as_basic_block(&self) -> Option<&BasicBlock<I>> {
        if self.is_basic_block() {
            Some(&self.blocks[0])
        } else {
            None
        }
    }

    /// Sequence of basic block start PCs, uniquely identifies this superblock
    pub fn start_pcs(&self) -> Vec<u64> {
        self.blocks.iter().map(|b| b.start_pc).collect()
    }

    /// Sequence of basic blocks composing this superblock
    pub fn blocks(&self) -> impl Iterator<Item = &BasicBlock<I>> {
        self.blocks.iter()
    }

    /// Sequence of instructions across all basic blocks in this superblock
    pub fn instructions(&self) -> impl Iterator<Item = &I> + Clone {
        self.blocks.iter().flat_map(|b| &b.instructions)
    }

    /// Parallel iterator over instructions across all basic blocks in this superblock
    pub fn par_instructions(&self) -> impl IndexedParallelIterator<Item = &I>
    where
        I: Sync,
    {
        // note: we need collect_vec() because parallel flat_map does not implement IndexedParallelIterator
        self.instructions().collect_vec().into_par_iter()
    }
}

impl<I: PcStep> SuperBlock<I> {
    /// Returns an iterator over the program counters of the instructions in this block.
    pub fn pcs(&self) -> impl Iterator<Item = u64> + '_ {
        self.blocks.iter().flat_map(BasicBlock::pcs)
    }
}

impl<I: Display> Display for SuperBlock<I> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(bb) = self.try_as_basic_block() {
            return bb.fmt(f);
        }
        writeln!(f, "SuperBlock(")?;
        let mut insn_idx = 0;
        for block in &self.blocks {
            writeln!(f, "   pc: {}, statements: [", block.start_pc)?;
            for instr in block.instructions.iter() {
                writeln!(f, "      instr {insn_idx:>3}:   {instr}")?;
                insn_idx += 1;
            }
            write!(f, "   ],")?;
        }
        write!(f, ")")
    }
}

impl<I: Display> Display for BasicBlock<I> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "BasicBlock(start_pc: {}, statements: [", self.start_pc)?;
        for (i, instr) in self.instructions.iter().enumerate() {
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


/// A sequence of basic blocks seen in the execution, identified by their start PCs.
/// A run is interrupted by an invalid APC block (i.e., single instruction).
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ExecutionBasicBlockRun(pub Vec<u64>);

/// A superblock present in the program, together with execution statistics (if PGO is enabled)
pub struct BlockAndStats<I> {
    pub block: SuperBlock<I>,
    /// amount of times this block appears in the execution
    pub count: Option<u32>,
    /// the indices of the basic block runs this block appears in (to avoid searching the whole execution later)
    pub runs: Option<Vec<usize>>,
}

/// The result of superblock generation: a set of blocks with optional statistics for PGO.
pub struct ExecutionBlocks<I> {
    /// Superblocks seen in the execution.
    pub blocks: Vec<BlockAndStats<I>>,
    /// Basic block runs in the execution (if PGO is enabled).
    /// Each run is paired with the number of times it was seen.
    /// The ordering here is arbitrary, but is fixed because the blocks refer to the indices.
    pub execution_bb_runs: Option<Vec<(ExecutionBasicBlockRun, u32)>>,
}

impl<I> ExecutionBlocks<I> {
    pub fn new_without_pgo(blocks: Vec<SuperBlock<I>>) -> Self {
        Self {
            blocks: blocks.into_iter().map(|block| BlockAndStats {
                block,
                count: None,
                runs: None,
            }).collect(),
            execution_bb_runs: None,
        }
    }
}

/// Count how many times the `needle` sequence appears inside the `haystack` sequence.
/// Does not count overlapping occurrences (e.g. `aba` is counted only twice in `abababa`).
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

/// Find all superblocks up to `max_len` in a single run and count their occurrences.
#[allow(clippy::iter_over_hash_type)]
fn superblocks_in_run(run: &ExecutionBasicBlockRun, max_len: usize) -> HashMap<Vec<u64>, u32> {
    let mut superblocks_in_run = HashMap::new();
    for len in 1..=std::cmp::min(max_len, run.0.len()) {
        superblocks_in_run.extend(run.0.windows(len).map(|w| (w.to_vec(), 0)));
    }
    for (sblock, tally) in superblocks_in_run.iter_mut() {
        *tally = count_non_overlapping(&run.0, sblock);
    }
    superblocks_in_run
}

/// Uses the PC sequence of a given execution to detect superblocks.
/// Returns all blocks and superblocks (up to a maximum sequence length),
/// together with their execution counts.
/// Ignores single-instruction blocks and blocks that were never executed.
pub fn generate_superblocks<I: Clone>(
    execution_pc_list: &[u64],
    basic_blocks: &[BasicBlock<I>],
    max_len: usize,
    exec_count_cutoff: u32,
) -> ExecutionBlocks<I> {
    tracing::info!(
        "Detecting superblocks of size <= {max_len}, over the sequence of {} PCs",
        execution_pc_list.len()
    );

    let start = std::time::Instant::now();

    let bb_start_pc_to_idx: HashMap<_, _> = basic_blocks
        .iter()
        .enumerate()
        .map(|(idx, bb)| (bb.start_pc, idx))
        .collect();

    // Split the execution into basic block runs (sequences of multi-instruction BBs).
    // The same run can appear multiple times; we keep a count using a hashmap.
    let mut execution_bb_runs: HashMap<ExecutionBasicBlockRun, u32> = HashMap::new();
    let mut current_run = vec![];

    for pc in execution_pc_list {
        let Some(&bb_idx) = bb_start_pc_to_idx.get(pc) else {
            // Still in the same BB
            continue;
        };
        // Single-instruction BBs are invalid for APCs; they end the current run.
        if basic_blocks[bb_idx].statements.len() <= 1 {
            if !current_run.is_empty() {
                *execution_bb_runs
                    .entry(ExecutionBasicBlockRun(std::mem::take(&mut current_run)))
                    .or_insert(0) += 1;
            }
            continue;
        }
        current_run.push(*pc);
    }
    if !current_run.is_empty() {
        *execution_bb_runs
            .entry(ExecutionBasicBlockRun(std::mem::take(&mut current_run)))
            .or_insert(0) += 1;
    }

    let execution_bb_runs: Vec<(ExecutionBasicBlockRun, u32)> = execution_bb_runs.into_iter().collect();

    // Find and count the occurrences of superblocks of up to max_len in each run.
    // Also build a map from superblock to the runs it appears in.
    #[allow(clippy::type_complexity, clippy::iter_over_hash_type)]
    let (mut block_to_runs_map, superblock_counts): (
        HashMap<Vec<u64>, Vec<usize>>,
        HashMap<Vec<u64>, u32>,
    ) = execution_bb_runs
        .par_iter()
        .enumerate()
        .map(|(run_idx, (run, run_count))| {
            let mut run_superblock_counts = superblocks_in_run(run, max_len);
            run_superblock_counts
                .values_mut()
                .for_each(|v| *v *= run_count);
            let block_to_run: HashMap<Vec<u64>, Vec<usize>> = run_superblock_counts
                .keys()
                .map(|sblock| (sblock.clone(), vec![run_idx]))
                .collect();
            (block_to_run, run_superblock_counts)
        })
        .reduce(
            || (HashMap::new(), HashMap::new()),
            |(mut a_runs, mut a_counts), (b_runs, b_counts)| {
                for (sblock, count) in b_counts {
                    *a_counts.entry(sblock).or_insert(0) += count;
                }
                for (sblock, runs) in b_runs {
                    a_runs.entry(sblock).or_default().extend(runs);
                }
                (a_runs, a_counts)
            },
        );

    tracing::info!(
        "Found {} blocks in {} basic block runs! Took {:?}",
        superblock_counts.len(),
        execution_bb_runs.len(),
        start.elapsed(),
    );

    let mut super_blocks = vec![];
    let mut skipped = 0;
    superblock_counts.into_iter().for_each(|(sblock, count)| {
        // convert PCs to actual basic blocks
        let basic_blocks: Vec<_> = sblock
            .iter()
            .map(|start_pc| basic_blocks[bb_start_pc_to_idx[start_pc]].clone())
            .collect();

        if basic_blocks.len() > 1 && count < exec_count_cutoff {
            tracing::trace!(
                "Skipping superblock {:?} due to execution count below cutoff ({})",
                sblock,
                exec_count_cutoff,
            );
            skipped += 1;
            return;
        }

        let mut runs = block_to_runs_map.remove(&sblock).unwrap();
        runs.sort_unstable();
        runs.dedup();

        super_blocks.push(BlockAndStats {
            block: SuperBlock::from(basic_blocks),
            count: Some(count),
            runs: Some(runs),
        });
    });

    tracing::info!(
        "{} blocks were skipped due to execution cutoff of {}, {} blocks remain",
        skipped,
        exec_count_cutoff,
        super_blocks.len(),
    );

    tracing::info!(
        "Out of those, {} are basic blocks and {} are superblocks",
        super_blocks.iter().filter(|b| b.block.is_basic_block()).count(),
        super_blocks.iter().filter(|b| !b.block.is_basic_block()).count(),
    );

    ExecutionBlocks {
        blocks: super_blocks,
        execution_bb_runs: Some(execution_bb_runs),
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_count_non_overlapping() {
        assert_eq!(count_non_overlapping(&[1, 2, 1, 2, 1], &[1, 2, 1]), 1);
        assert_eq!(count_non_overlapping(&[1, 2, 3], &[1, 2, 3]), 1);
        assert_eq!(count_non_overlapping(&[1, 2, 3], &[4]), 0);
        assert_eq!(count_non_overlapping(&[1, 1, 1], &[1]), 3);
    }

    #[test]
    fn test_superblocks_in_run() {
        let run = ExecutionBasicBlockRun(vec![4, 1, 2, 3, 5, 1, 2, 3, 4]);
        let max_len = 3;
        let counts = superblocks_in_run(&run, max_len);
        assert_eq!(
            counts.len(),
            5 + // size 1
            6 + // size 2
            6 // size 3
        );
        assert_eq!(counts[&vec![1]], 2);
        assert_eq!(counts[&vec![1, 2]], 2);
        assert_eq!(counts[&vec![4]], 2);
        assert_eq!(counts[&vec![5]], 1);
        assert_eq!(counts[&vec![4, 1, 2]], 1);
        assert_eq!(counts[&vec![1, 2, 3]], 2);
        assert_eq!(counts[&vec![2, 3, 4]], 1);
    }
}
