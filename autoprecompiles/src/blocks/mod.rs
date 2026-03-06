use std::{
    collections::{BTreeMap, HashMap},
    fmt::Display,
};

use itertools::Itertools;
use rayon::iter::{
    IndexedParallelIterator, IntoParallelIterator, IntoParallelRefIterator, ParallelIterator,
};
use serde::{Deserialize, Serialize};

/// Tools to detect basic blocks in a program
mod detection;

pub use detection::collect_basic_blocks;

use crate::PowdrConfig;

#[derive(Debug, Serialize, Deserialize, Clone)]
/// A sequence of instructions starting at a given PC.
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
/// A sequence of basic blocks that can be made into an autoprecompile.
/// A single basic block is represented as a SuperBlock with one element.
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

    /// Instruction index of the start pc of each original basic block
    pub fn instruction_indexed_start_pcs(&self) -> Vec<(usize, u64)> {
        let mut idx = 0;
        self.blocks
            .iter()
            .map(|b| {
                let elem = (idx, b.start_pc);
                idx += b.instructions.len();
                elem
            })
            .collect()
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

    /// Apply fn to every instruction in this superblock, returning a new superblock with the transformed instructions.
    pub fn map_instructions<F, I2>(self, f: F) -> SuperBlock<I2>
    where
        F: Fn(I) -> I2 + Clone,
    {
        SuperBlock {
            blocks: self
                .blocks
                .into_iter()
                .map(|b| BasicBlock {
                    start_pc: b.start_pc,
                    instructions: b.instructions.into_iter().map(f.clone()).collect(),
                })
                .collect(),
        }
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
    pub count: u32,
}

/// The result of superblock generation: a set of blocks with optional statistics for PGO.
pub struct ExecutionBlocks<I> {
    /// Superblocks seen in the execution.
    pub blocks: Vec<BlockAndStats<I>>,
    /// Basic block runs in the execution (if PGO is enabled).
    /// Each run is paired with the number of times it was seen.
    pub execution_bb_runs: Vec<(ExecutionBasicBlockRun, u32)>,
}

impl<I> ExecutionBlocks<I> {
    pub fn new_without_pgo(blocks: Vec<SuperBlock<I>>) -> Self {
        Self {
            blocks: blocks
                .into_iter()
                .map(|block| BlockAndStats { block, count: 0 })
                .collect(),
            execution_bb_runs: vec![],
        }
    }
}

/// Find the starting indices of non-overlapping occurrences of `needle` in `haystack`.
/// (e.g. `aba` is found at indices [0, 4] in `abababa`).
pub fn find_non_overlapping<T: Eq>(haystack: &[T], needle: &[T]) -> Vec<usize> {
    let mut indices = vec![];
    let mut pos = 0;
    while pos + needle.len() <= haystack.len() {
        if haystack[pos..pos + needle.len()] == needle[..] {
            indices.push(pos);
            pos += needle.len();
        } else {
            pos += 1;
        }
    }
    indices
}

/// Find basic block runs in the execution.
/// A run is interrupted upon hitting an invalid APC basic block (i.e., a single-instruction block).
/// Returns a list of the runs, coupled with how many times each appears (a run may repeat in the execution).
fn detect_execution_bb_runs<I>(
    // start PC to basic blocks. Should include every basic block in the program, including those with len=1 (invalid APC)
    start_pc_to_bb: &HashMap<u64, BasicBlock<I>>,
    execution: &[u64],
) -> Vec<(ExecutionBasicBlockRun, u32)> {
    // Basic block runs in the execution.
    // The same run can appear multiple times in the execution, so we keep a count using a hashmap.
    // Each BB is identified by its starting PC.
    let mut execution_bb_runs = BTreeMap::new();
    let mut current_run = vec![];

    let mut pos = 0;
    while pos < execution.len() {
        let pc = execution[pos];
        let bb = start_pc_to_bb
            .get(&pc)
            .expect("PC in execution not part of any basic blocks");
        assert!(!bb.instructions.is_empty());
        if bb.instructions.len() == 1 {
            // if starting a single instruction BB (i.e., invalid for APC), end current run
            if !current_run.is_empty() {
                *execution_bb_runs
                    .entry(std::mem::take(&mut current_run))
                    .or_insert(0) += 1;
            }
        } else {
            // extend the run with this basic block
            current_run.push(pc);
        }
        // move to next bb
        pos += bb.instructions.len();
    }
    if !current_run.is_empty() {
        *execution_bb_runs
            .entry(std::mem::take(&mut current_run))
            .or_insert(0) += 1;
    }

    execution_bb_runs
        .into_iter()
        .map(|(run, count)| (ExecutionBasicBlockRun(run), count))
        .collect()
}

/// Find all superblocks up to max_len in the basic block run and count their occurrences.
/// Returns a map from superblock to its count.
fn count_superblocks_in_run(
    bb_run: &ExecutionBasicBlockRun,
    max_len: usize,
) -> BTreeMap<Vec<u64>, u32> {
    let mut superblocks_in_run = BTreeMap::new();
    // first, we identify the superblocks in this run
    for len in 1..=std::cmp::min(max_len, bb_run.0.len()) {
        superblocks_in_run.extend(bb_run.0.windows(len).map(|w| (w.to_vec(), 0)));
    }
    // then we count their occurrences
    for (sblock, count) in superblocks_in_run.iter_mut() {
        *count = find_non_overlapping(&bb_run.0, sblock).len() as u32;
    }
    superblocks_in_run
}

/// Find all superblocks up to max_len in the execution and count their occurrences.
/// Returns a map from superblock to its count.
fn count_superblocks_in_execution(
    execution_bb_runs: &[(ExecutionBasicBlockRun, u32)],
    max_len: usize,
) -> BTreeMap<Vec<u64>, u32> {
    let sblocks = execution_bb_runs
        .par_iter()
        .map(|(run, run_count)| {
            count_superblocks_in_run(run, max_len)
                .into_iter()
                .map(|(sblock, sblock_occurrences_in_run)| {
                    (sblock, sblock_occurrences_in_run * run_count)
                })
                .collect()
        })
        .reduce(BTreeMap::new, |mut sblocks_a, sblocks_b| {
            // merge counts of b into a
            for (sblock, count) in sblocks_b {
                *sblocks_a.entry(sblock).or_insert(0) += count;
            }
            sblocks_a
        });
    sblocks
}

/// Detect basic blocks and superblocks present in the given execution.
/// Returns the detected blocks, together with their execution information.
/// Does not return invalid APC blocks (i.e., single instruction) and blocks that are never executed.
pub fn detect_superblocks<I: Clone>(
    cfg: &PowdrConfig,
    // program execution as a sequence of PCs
    execution_pc_list: &[u64],
    // all program basic blocks (including single instruction ones), in no particular order
    basic_blocks: Vec<BasicBlock<I>>,
) -> ExecutionBlocks<I> {
    tracing::info!(
        "Detecting superblocks with <= {} basic blocks, over the sequence of {} PCs",
        cfg.superblock_max_bb_count,
        execution_pc_list.len()
    );

    let start = std::time::Instant::now();

    // index basic blocks by start PC
    let start_pc_to_bb: HashMap<_, _> = basic_blocks
        .into_iter()
        .map(|bb| (bb.start_pc, bb))
        .collect();

    let execution_bb_runs = detect_execution_bb_runs(&start_pc_to_bb, execution_pc_list);

    let blocks_found =
        count_superblocks_in_execution(&execution_bb_runs, cfg.superblock_max_bb_count as usize);

    tracing::info!(
        "Found {} blocks in {} basic block runs. Took {:?}",
        blocks_found.len(),
        execution_bb_runs.len(),
        start.elapsed(),
    );

    // build the result
    let mut block_stats = vec![];
    let mut skipped_exec_count = 0;
    let mut skipped_max_insn = 0;
    blocks_found.into_iter().for_each(|(sblock_pcs, count)| {
        let block = SuperBlock::from(
            sblock_pcs
                .iter()
                .map(|start_pc| start_pc_to_bb[start_pc].clone())
                .collect_vec(),
        );

        // skip superblocks that were executed less than the cutoff
        if count < cfg.apc_exec_count_cutoff {
            skipped_exec_count += 1;
            return;
        }

        // skip superblocks with too many instructions
        if block.instructions().count() > cfg.apc_max_instructions as usize {
            skipped_max_insn += 1;
            return;
        }

        block_stats.push(BlockAndStats { block, count });
    });

    tracing::info!(
        "Skipped blocks: {} to execution cutoff, {} to instruction count",
        skipped_exec_count,
        skipped_max_insn,
    );

    tracing::info!(
        "Of the {} remaining blocks, {} are basic blocks and {} are superblocks",
        block_stats.len(),
        block_stats
            .iter()
            .filter(|b| b.block.is_basic_block())
            .count(),
        block_stats
            .iter()
            .filter(|b| !b.block.is_basic_block())
            .count(),
    );

    ExecutionBlocks {
        blocks: block_stats,
        execution_bb_runs,
    }
}

#[cfg(test)]
mod test {
    use std::collections::BTreeMap;

    use crate::{DegreeBound, PowdrConfig};

    use super::*;

    #[test]
    fn test_find_non_overlapping() {
        assert_eq!(find_non_overlapping(&[1, 2, 1, 2, 1], &[1, 2, 1]), vec![0]);
        assert_eq!(find_non_overlapping(&[1, 2, 3], &[1, 2, 3]), vec![0]);
        assert_eq!(find_non_overlapping(&[1, 2, 3], &[4]), vec![] as Vec<usize>);
        assert_eq!(find_non_overlapping(&[1, 1, 1], &[1]), vec![0, 1, 2]);
    }

    #[test]
    fn test_superblocks_in_run() {
        let run = ExecutionBasicBlockRun(vec![4, 1, 2, 3, 5, 1, 2, 3, 4]);
        let max_len = 3;
        let counts = count_superblocks_in_run(&run, max_len);
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

    #[test]
    fn test_detect_superblocks_counts_and_execution_runs() {
        let bb = |start_pc: u64, len: usize| BasicBlock {
            start_pc,
            instructions: vec![(); len],
        };

        let cfg = PowdrConfig::new(
            10,
            0,
            DegreeBound {
                identities: 2,
                bus_interactions: 2,
            },
        )
        .with_superblocks(2, None, None);

        let basic_blocks = vec![bb(100, 2), bb(200, 2), bb(300, 1), bb(400, 3), bb(500, 2)];

        let execution = vec![100, 101, 200, 201, 300, 400, 401, 402, 100, 101, 200, 201];

        let result = detect_superblocks(&cfg, &execution, basic_blocks);

        assert_eq!(
            result.execution_bb_runs,
            vec![
                (ExecutionBasicBlockRun(vec![100, 200]), 1),
                (ExecutionBasicBlockRun(vec![400, 100, 200]), 1),
            ]
        );

        let counts = result
            .blocks
            .into_iter()
            .map(|entry| (entry.block.start_pcs(), entry.count))
            .collect::<BTreeMap<_, _>>();

        assert_eq!(counts.get(&vec![100]), Some(&2));
        assert_eq!(counts.get(&vec![200]), Some(&2));
        assert_eq!(counts.get(&vec![400]), Some(&1));
        assert_eq!(counts.get(&vec![100, 200]), Some(&2));
        assert_eq!(counts.get(&vec![400, 100]), Some(&1));
        assert!(!counts.contains_key(&vec![300]));
        assert!(!counts.contains_key(&vec![500]));
    }
}
