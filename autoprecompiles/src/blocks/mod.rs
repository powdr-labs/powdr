use std::{
    collections::{BTreeMap, BTreeSet, HashMap},
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

    /// Returns an iterator over the program counters of the instructions in this block.
    pub fn instructions(&self) -> impl DoubleEndedIterator<Item = (u64, &I)> + '_ {
        self.instructions
            .iter()
            .enumerate()
            .map(|(index, i)| (self.start_pc + (index as u64 * I::pc_step() as u64), i))
    }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
/// A sequence of basic blocks that can be made into an autoprecompile.
/// A single basic block is represented as a SuperBlock with one element.
pub struct SuperBlock<I> {
    blocks: Vec<BasicBlock<I>>,
    /// True for optimistic superblocks (i.e., jumps between basic blocks are not statically known).
    /// False for basic blocks and static superblocks.
    #[serde(default)]
    optimistic: bool,
}

impl<I> From<BasicBlock<I>> for SuperBlock<I> {
    fn from(basic_block: BasicBlock<I>) -> Self {
        SuperBlock {
            blocks: vec![basic_block],
            optimistic: false,
        }
    }
}

impl<I> SuperBlock<I> {
    /// Build a static superblock from one or more basic blocks.
    pub fn new_static(blocks: Vec<BasicBlock<I>>) -> Self {
        assert!(!blocks.is_empty());
        SuperBlock {
            blocks,
            optimistic: false,
        }
    }

    /// Build an optimistic superblock from two or more basic blocks.
    pub fn new_optimistic(blocks: Vec<BasicBlock<I>>) -> Self {
        assert!(
            blocks.len() >= 2,
            "optimistic superblock needs 2 or more basic blocks"
        );
        SuperBlock {
            blocks,
            optimistic: true,
        }
    }

    pub fn is_optimistic(&self) -> bool {
        self.optimistic
    }

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

    pub fn len(&self) -> usize {
        self.blocks.iter().map(|b| b.instructions.len()).sum()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn extend(&mut self, other: Self) {
        self.blocks.extend(other.blocks);
    }

    /// Sequence of basic block start PCs, uniquely identifies this superblock
    pub fn start_pcs(&self) -> Vec<u64> {
        self.blocks.iter().map(|b| b.start_pc).collect()
    }

    /// For each basic block in the superblock, returns the index of its first instruction
    /// (within the superblock's flat instruction list) together with the block's start PC.
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
            optimistic: self.optimistic,
        }
    }
}

impl<I: PcStep> SuperBlock<I> {
    /// Returns an iterator over the program counters of the instructions in this block.
    pub fn pcs(&self) -> impl Iterator<Item = u64> + '_ {
        self.blocks.iter().flat_map(BasicBlock::pcs)
    }

    /// Sequence of instructions across all basic blocks in this superblock
    pub fn instructions(&self) -> impl DoubleEndedIterator<Item = (u64, &I)> {
        self.blocks.iter().flat_map(BasicBlock::instructions)
    }

    /// Parallel iterator over instructions across all basic blocks in this superblock
    pub fn par_instructions(&self) -> impl IndexedParallelIterator<Item = (u64, &I)>
    where
        I: Sync,
    {
        // note: we need collect_vec() because parallel flat_map does not implement IndexedParallelIterator
        self.instructions().collect_vec().into_par_iter()
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

/// A program's basic blocks together with their maximal static basic block
/// sequences, that is, sequences of basic blocks linked by statically
/// determined jumps.
/// `blocks` holds only basic blocks that are valid APC candidates.
pub struct BasicBlocks<I> {
    pub blocks: Vec<BasicBlock<I>>,
    pub static_sequences: Vec<Vec<u64>>,
}

impl<I> BasicBlocks<I> {
    /// Builds a set of basic blocks valid for APC generation.
    pub fn new(blocks: Vec<BasicBlock<I>>) -> Self {
        let blocks = blocks
            .into_iter()
            .filter(|bb| bb.instructions.len() > 1)
            .collect();
        Self {
            blocks,
            static_sequences: vec![],
        }
    }

    /// Builds a set of valid basic blocks, together with computed static sequences across them.
    pub fn new_with_static_sequences<A>(blocks: Vec<BasicBlock<I>>) -> Self
    where
        A: crate::adapter::Adapter<Instruction = I>,
    {
        let bbs = Self::new(blocks);
        let static_sequences = detection::compute_static_sequences::<A>(&bbs.blocks);
        Self {
            static_sequences,
            ..bbs
        }
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

/// A superblock together with execution statistics (if PGO is enabled)
pub struct BlockAndStats<I> {
    pub block: SuperBlock<I>,
    /// amount of times this block appears in the execution
    pub count: u32,
}

/// The result of superblock generation: a set of APC candidate blocks, with optional statistics for PGO.
pub struct ExecutionBlocks<I> {
    /// Superblocks seen in the execution.
    pub blocks: Vec<BlockAndStats<I>>,
    /// Basic block runs in the execution (if PGO is enabled).
    /// Each run is paired with the number of times it was seen.
    pub execution_bb_runs: Vec<(ExecutionBasicBlockRun, u32)>,
}

impl<I: Clone> ExecutionBlocks<I> {
    /// Construct a candidate set without PGO data.
    /// Includes static superblock candidates if provided.
    pub fn new_without_pgo(basic_blocks: BasicBlocks<I>) -> Self {
        let BasicBlocks {
            blocks: bb_list,
            static_sequences,
        } = basic_blocks;
        let start_pc_to_bb: HashMap<u64, BasicBlock<I>> =
            bb_list.into_iter().map(|bb| (bb.start_pc, bb)).collect();

        let mut blocks: Vec<BlockAndStats<I>> = Vec::new();

        // Materialize a superblock candidate for every basic block and for
        // every static sequence of length >= 2.
        let static_superblocks =
            enumerate_static_superblocks(start_pc_to_bb.keys().copied(), &static_sequences);
        for sb in static_superblocks {
            let bbs: Vec<BasicBlock<I>> = sb
                .iter()
                .map(|pc| {
                    start_pc_to_bb
                        .get(pc)
                        .expect("invalid start PC in static superblock")
                        .clone()
                })
                .collect();
            blocks.push(BlockAndStats {
                block: SuperBlock::new_static(bbs),
                count: 0,
            });
        }

        Self {
            blocks,
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
/// Returns a list of the runs, coupled with how many times each appears (a run may repeat in the execution).
///
/// `start_pc_to_bb` should only contain valid APC basic blocks.
fn detect_execution_bb_runs<I>(
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
        match start_pc_to_bb.get(&pc) {
            Some(bb) => {
                // valid BB, extend the current run.
                current_run.push(pc);
                pos += bb.instructions.len();
            }
            None => {
                // BB not found, end the run and advance to the next instruction
                if !current_run.is_empty() {
                    *execution_bb_runs
                        .entry(std::mem::take(&mut current_run))
                        .or_insert(0) += 1;
                }
                pos += 1;
            }
        }
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

/// Find all optimistic superblocks up to max_len in the basic block run, counting their occurrences
fn count_optimistic_superblocks_in_run(
    bb_run: &ExecutionBasicBlockRun,
    max_len: usize,
    static_superblocks: &BTreeSet<Vec<u64>>,
) -> BTreeMap<Vec<u64>, u32> {
    let mut superblocks_in_run = BTreeMap::new();
    // first, identify the optimistic sequences up to max_len
    for len in 2..=std::cmp::min(max_len, bb_run.0.len()) {
        superblocks_in_run.extend(
            bb_run
                .0
                .windows(len)
                .filter(|seq| !static_superblocks.contains(*seq))
                .map(|seq| (seq.to_vec(), 0)),
        );
    }

    // then, count their occurrences
    for (sblock, count) in superblocks_in_run.iter_mut() {
        *count = find_non_overlapping(&bb_run.0, sblock).len() as u32;
    }
    superblocks_in_run
}

/// Find all optimistic superblocks up to max_len in the execution, counting their occurrences.
fn count_optimistic_superblocks_in_execution(
    execution_bb_runs: &[(ExecutionBasicBlockRun, u32)],
    max_len: usize,
    // set of static candidates
    static_superblocks: &BTreeSet<Vec<u64>>,
) -> BTreeMap<Vec<u64>, u32> {
    execution_bb_runs
        .par_iter()
        .map(|(run, run_count)| {
            count_optimistic_superblocks_in_run(run, max_len, static_superblocks)
                .into_iter()
                .map(|(sblock, sblock_occurrences_in_run)| {
                    (sblock, sblock_occurrences_in_run * run_count)
                })
                .collect()
        })
        .reduce(BTreeMap::new, |mut a, b| {
            // merge counts of b into a
            for (sblock, count) in b {
                *a.entry(sblock).or_insert(0) += count;
            }
            a
        })
}

/// Enumerate every possible static superblock candidate (basic block or static sequence of length >= 2)
fn enumerate_static_superblocks(
    basic_block_pcs: impl IntoIterator<Item = u64>,
    static_sequences: &[Vec<u64>],
) -> BTreeSet<Vec<u64>> {
    basic_block_pcs
        .into_iter()
        .map(|pc| vec![pc])
        .chain(static_sequences.iter().flat_map(|seq| {
            (2..=seq.len()).flat_map(move |len| seq.windows(len).map(|w| w.to_vec()))
        }))
        .collect()
}

/// Count the occurrences of each static superblock candidate in the execution.
fn count_static_superblocks_in_execution(
    static_superblocks: &BTreeSet<Vec<u64>>,
    execution_bb_runs: &[(ExecutionBasicBlockRun, u32)],
) -> BTreeMap<Vec<u64>, u32> {
    execution_bb_runs
        .par_iter()
        .map(|(run, run_count)| {
            static_superblocks
                .iter()
                .map(|sb| {
                    let occurrences = find_non_overlapping(&run.0, sb).len() as u32;
                    (sb.clone(), occurrences * run_count)
                })
                .collect::<BTreeMap<_, _>>()
        })
        .reduce(BTreeMap::new, |mut a, b| {
            for (sb, count) in b {
                *a.entry(sb).or_insert(0) += count;
            }
            a
        })
}

/// Detect basic blocks and superblocks present in the given execution.
/// Returns the detected blocks, together with their execution information.
pub fn detect_superblocks<I: Clone + PcStep>(
    cfg: &PowdrConfig,
    // program execution as a sequence of PCs
    execution_pc_list: &[u64],
    basic_blocks: BasicBlocks<I>,
) -> ExecutionBlocks<I> {
    let BasicBlocks {
        blocks,
        static_sequences,
    } = basic_blocks;
    tracing::info!(
        "Detecting superblocks over the sequence of {} PCs",
        execution_pc_list.len()
    );

    let start = std::time::Instant::now();

    // Index basic blocks by start PC.
    let start_pc_to_bb: HashMap<u64, BasicBlock<I>> =
        blocks.into_iter().map(|bb| (bb.start_pc, bb)).collect();

    let execution_bb_runs = detect_execution_bb_runs(&start_pc_to_bb, execution_pc_list);

    // Static superblock candidates: basic blocks plus every static sequence of length >= 2.
    let static_superblocks =
        enumerate_static_superblocks(start_pc_to_bb.keys().copied(), &static_sequences);
    let static_counts =
        count_static_superblocks_in_execution(&static_superblocks, &execution_bb_runs);

    // Optimistic superblock candidates from execution
    let optimistic_counts = if cfg.optimistic_superblock_max_bb_count > 1 {
        count_optimistic_superblocks_in_execution(
            &execution_bb_runs,
            cfg.optimistic_superblock_max_bb_count as usize,
            &static_superblocks,
        )
    } else {
        Default::default()
    };

    tracing::info!(
        "Found {} static and {} optimistic superblock candidates in {} basic block runs. Took {:?}",
        static_counts.len(),
        optimistic_counts.len(),
        execution_bb_runs.len(),
        start.elapsed(),
    );

    let mut block_stats: Vec<BlockAndStats<I>> = vec![];
    let mut skipped_exec_count = 0usize;
    let mut skipped_max_insn = 0usize;

    // helper to materialize superblock for candidates that are not filtered out due to execution or instruction counts
    let mut emit = |sblock_pcs: Vec<u64>, count: u32, optimistic: bool| {
        let bbs: Vec<BasicBlock<I>> = sblock_pcs
            .iter()
            .map(|pc| start_pc_to_bb[pc].clone())
            .collect();
        let block = if optimistic {
            SuperBlock::new_optimistic(bbs)
        } else {
            SuperBlock::new_static(bbs)
        };

        if count < cfg.apc_exec_count_cutoff {
            skipped_exec_count += 1;
            return;
        }
        if block.instructions().count() > cfg.apc_max_instructions as usize {
            skipped_max_insn += 1;
            return;
        }
        block_stats.push(BlockAndStats { block, count });
    };

    for (superblock, count) in static_counts {
        emit(superblock, count, false);
    }
    for (sblock_pcs, count) in optimistic_counts {
        emit(sblock_pcs, count, true);
    }

    tracing::info!(
        "Skipped blocks: {} to execution cutoff, {} to instruction count",
        skipped_exec_count,
        skipped_max_insn,
    );

    let bb_count = block_stats
        .iter()
        .filter(|b| b.block.is_basic_block())
        .count();
    let static_sb_count = block_stats
        .iter()
        .filter(|b| !b.block.is_basic_block() && !b.block.is_optimistic())
        .count();
    let optimistic_sb_count = block_stats
        .iter()
        .filter(|b| b.block.is_optimistic())
        .count();
    tracing::info!(
        "Of the {} remaining blocks, {} are basic blocks, {} are static superblocks, {} are optimistic superblocks.",
        block_stats.len(),
        bb_count,
        static_sb_count,
        optimistic_sb_count,
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

    #[derive(Clone)]
    struct TestInstruction;

    impl PcStep for TestInstruction {
        fn pc_step() -> u32 {
            1
        }
    }

    #[test]
    fn test_find_non_overlapping() {
        assert_eq!(find_non_overlapping(&[1, 2, 1, 2, 1], &[1, 2, 1]), vec![0]);
        assert_eq!(find_non_overlapping(&[1, 2, 3], &[1, 2, 3]), vec![0]);
        assert_eq!(find_non_overlapping(&[1, 2, 3], &[4]), vec![] as Vec<usize>);
        assert_eq!(find_non_overlapping(&[1, 1, 1], &[1]), vec![0, 1, 2]);
    }

    #[test]
    fn test_optimistic_superblocks_in_run() {
        let run = ExecutionBasicBlockRun(vec![4, 1, 2, 3, 5, 1, 2, 3, 4]);
        let max_len = 3;
        // [1, 2] is a known static superblock, so it should be skipped here.
        let static_set: BTreeSet<Vec<u64>> = [vec![1, 2]].into_iter().collect();
        let counts = count_optimistic_superblocks_in_run(&run, max_len, &static_set);

        assert_eq!(
            counts.len(),
            5 + // 6 unique length-2 windows, minus [1, 2] which is static
            6 // length-3 windows
        );
        // length-1 windows are not counted
        assert!(!counts.contains_key(&vec![1]));
        assert!(!counts.contains_key(&vec![4]));
        // sequences in the static set are skipped
        assert!(!counts.contains_key(&vec![1, 2]));
        // longer windows containing [1, 2] as a substring are still counted
        assert_eq!(counts[&vec![4, 1, 2]], 1);
        assert_eq!(counts[&vec![1, 2, 3]], 2);
        assert_eq!(counts[&vec![2, 3, 4]], 1);
    }

    fn bb(start_pc: u64, len: usize) -> BasicBlock<TestInstruction> {
        BasicBlock {
            start_pc,
            instructions: vec![TestInstruction; len],
        }
    }

    #[test]
    fn test_detect_superblocks_counts_and_execution_runs() {
        let cfg = PowdrConfig::new(
            10,
            0,
            DegreeBound {
                identities: 2,
                bus_interactions: 2,
            },
        )
        .with_optimistic_superblocks(2);

        let basic_blocks = BasicBlocks::new(vec![
            bb(100, 2),
            bb(200, 2),
            bb(300, 1),
            bb(400, 3),
            bb(500, 2),
        ]);

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
            .iter()
            .map(|entry| (entry.block.start_pcs(), entry.count))
            .collect::<BTreeMap<_, _>>();

        assert_eq!(counts.get(&vec![100]), Some(&2));
        assert_eq!(counts.get(&vec![200]), Some(&2));
        assert_eq!(counts.get(&vec![400]), Some(&1));
        assert_eq!(counts.get(&vec![100, 200]), Some(&2));
        assert_eq!(counts.get(&vec![400, 100]), Some(&1));
        assert!(!counts.contains_key(&vec![300]));
        assert!(!counts.contains_key(&vec![500]));

        // all multi-BB candidates here are optimistic as no static sequences were provided with the basic blocks
        for entry in &result.blocks {
            if entry.block.start_pcs().len() > 1 {
                assert!(entry.block.is_optimistic());
            } else {
                assert!(!entry.block.is_optimistic());
            }
        }
    }

    #[test]
    fn test_detect_superblocks_static_superblock_takes_precedence_over_optimistic() {
        let cfg = PowdrConfig::new(
            10,
            0,
            DegreeBound {
                identities: 2,
                bus_interactions: 2,
            },
        )
        .with_optimistic_superblocks(2);

        let mut basic_blocks = BasicBlocks::new(vec![bb(100, 2), bb(200, 2), bb(300, 2)]);
        // [100, 200] is static; [200, 300] is not.
        basic_blocks.static_sequences = vec![vec![100, 200]];
        let execution = vec![100, 101, 200, 201, 300, 301];

        let result = detect_superblocks(&cfg, &execution, basic_blocks);

        let by_pcs: BTreeMap<Vec<u64>, &SuperBlock<TestInstruction>> = result
            .blocks
            .iter()
            .map(|e| (e.block.start_pcs(), &e.block))
            .collect();

        assert_eq!(by_pcs.len(), 5); // 3 BB candidates, 1 static SB, 1 optimistic SB

        // [100, 200] should be static (NOT in optimistic set).
        let s_100_200 = by_pcs
            .get(&vec![100, 200])
            .expect("static [100, 200] missing");
        assert!(!s_100_200.is_optimistic());

        // [200, 300] should be optimistic.
        let s_200_300 = by_pcs
            .get(&vec![200, 300])
            .expect("optimistic [200, 300] missing");
        assert!(s_200_300.is_optimistic());
    }
}
