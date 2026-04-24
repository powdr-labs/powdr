use std::{collections::BTreeMap, fmt::Display};

use itertools::Itertools;
use rayon::iter::{
    IndexedParallelIterator, IntoParallelIterator, IntoParallelRefIterator, ParallelIterator,
};
use serde::{Deserialize, Serialize};

/// Tools to detect static blocks in a program
mod detection;

pub use detection::collect_static_blocks;

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

/// A collection of basic blocks and superblocks derived statically from the program.  
/// As such, these superblocks only allow unconditional jumps.  
/// Each block must have its own unique start PC.  
/// As a set, the blocks should cover the whole program (including invalid APC instructions).
pub struct StaticBlocks<I> {
    blocks_by_start_pc: BTreeMap<u64, SuperBlock<I>>,
}

impl<I> StaticBlocks<I> {
    pub fn new(blocks: impl IntoIterator<Item = impl Into<SuperBlock<I>>>) -> Self {
        let mut blocks_by_start_pc = BTreeMap::new();
        for block in blocks.into_iter().map(Into::into) {
            let start_pc = block.blocks[0].start_pc;
            assert!(
                blocks_by_start_pc.insert(start_pc, block).is_none(),
                "multiple blocks share the same start pc: {start_pc}"
            );
        }
        StaticBlocks { blocks_by_start_pc }
    }

    pub fn len(&self) -> usize {
        self.blocks_by_start_pc.len()
    }

    pub fn is_empty(&self) -> bool {
        self.blocks_by_start_pc.is_empty()
    }

    pub fn iter(&self) -> impl Iterator<Item = (&u64, &SuperBlock<I>)> {
        self.blocks_by_start_pc.iter()
    }

    fn get(&self, pc: u64) -> &SuperBlock<I> {
        self.blocks_by_start_pc
            .get(&pc)
            .expect("PC was expected to be the start of a static block")
    }

    /// Find the static block containing basic block with the given start PC.
    /// Returns the containing static block.
    fn get_containing(&self, bb_pc: u64) -> &SuperBlock<I> {
        // First try direct lookup (bb_pc is the start of a static block)
        if let Some(sb) = self.blocks_by_start_pc.get(&bb_pc) {
            return sb;
        }
        // Otherwise search for the static block containing this basic block
        self.blocks_by_start_pc
            .values()
            .find(|sb| sb.blocks.iter().any(|b| b.start_pc == bb_pc))
            .unwrap_or_else(|| panic!("basic block {bb_pc} not found in any static block"))
    }
}

impl<I> IntoIterator for StaticBlocks<I> {
    type Item = (u64, SuperBlock<I>);

    type IntoIter = std::collections::btree_map::IntoIter<u64, SuperBlock<I>>;

    fn into_iter(self) -> Self::IntoIter {
        self.blocks_by_start_pc.into_iter()
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

/// A sequence of basic block start PCs seen in the execution.
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
/// A run is interrupted upon hitting an invalid APC block (i.e., a single-instruction block).
/// Each static block is expanded into its constituent basic block start PCs.
/// Returns a list of the runs, coupled with how many times each appears.
fn detect_execution_bb_runs<I>(
    static_blocks: &StaticBlocks<I>,
    execution: &[u64],
) -> Vec<(ExecutionBasicBlockRun, u32)> {
    let mut execution_bb_runs = BTreeMap::new();
    let mut current_run = vec![];

    let mut pos = 0;
    while pos < execution.len() {
        let pc = execution[pos];
        let bb = static_blocks.get(pc);
        assert!(!bb.is_empty());
        if bb.len() == 1 {
            // single instruction BB (invalid for APC), end current run
            if !current_run.is_empty() {
                *execution_bb_runs
                    .entry(std::mem::take(&mut current_run))
                    .or_insert(0) += 1;
            }
        } else {
            // expand static block into its basic block start PCs
            for block in bb.blocks() {
                current_run.push(block.start_pc);
            }
        }
        pos += bb.len();
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

/// Count occurrences of a set of candidates (identified by their basic block start PC sequences)
/// across all execution runs. Returns a map from candidate start PCs to count.
fn count_candidates_in_execution(
    execution_bb_runs: &[(ExecutionBasicBlockRun, u32)],
    candidates: &[Vec<u64>],
) -> BTreeMap<Vec<u64>, u32> {
    // Count each candidate independently across all runs
    candidates
        .par_iter()
        .map(|candidate| {
            let count: u32 = execution_bb_runs
                .iter()
                .map(|(run, run_count)| {
                    find_non_overlapping(&run.0, candidate).len() as u32 * run_count
                })
                .sum();
            (candidate.clone(), count)
        })
        .collect()
}

/// Detect basic blocks and static blocks present in the given execution.
/// Returns the detected blocks, together with their execution information.
/// Does not return invalid APC blocks (i.e., single instruction) and blocks that are never executed.
///
/// Produces candidates for both individual basic blocks and full static blocks.
/// Execution runs are over basic blocks, so candidates are counted independently.
pub fn detect_superblocks<I: Clone + PcStep>(
    cfg: &PowdrConfig,
    // program execution as a sequence of PCs
    execution_pc_list: &[u64],
    // all program static blocks
    static_blocks: StaticBlocks<I>,
) -> ExecutionBlocks<I> {
    assert!(
        cfg.superblock_max_bb_count == 1,
        "superblocks not being currently handled",
    );

    tracing::info!(
        "Detecting blocks over the sequence of {} PCs",
        execution_pc_list.len()
    );

    let start = std::time::Instant::now();

    let execution_bb_runs = detect_execution_bb_runs(&static_blocks, execution_pc_list);

    // Build candidate list: each static block + each individual basic block within multi-BB
    // static blocks. Candidates are identified by their basic block start PC sequences.
    let mut candidate_pcs: Vec<Vec<u64>> = vec![];
    for (_, sblock) in static_blocks.iter() {
        // The full static block
        let full_pcs: Vec<u64> = sblock.start_pcs();
        candidate_pcs.push(full_pcs.clone());
        // Individual basic blocks (only if the static block has more than one)
        if full_pcs.len() > 1 {
            for pc in &full_pcs {
                candidate_pcs.push(vec![*pc]);
            }
        }
    }
    // Deduplicate (a basic block PC may appear in multiple static blocks)
    candidate_pcs.sort();
    candidate_pcs.dedup();

    let counts = count_candidates_in_execution(&execution_bb_runs, &candidate_pcs);

    tracing::info!(
        "Found {} candidate patterns in {} basic block runs. Took {:?}",
        counts.len(),
        execution_bb_runs.len(),
        start.elapsed(),
    );

    // Build the result: look up each candidate's basic blocks from the static blocks
    let mut block_stats = vec![];
    let mut skipped_exec_count = 0;
    let mut skipped_max_insn = 0;
    for (bb_pcs, count) in counts {
        if count < cfg.apc_exec_count_cutoff {
            skipped_exec_count += 1;
            continue;
        }

        // Build the SuperBlock from individual basic blocks.
        // For a single BB [X], look it up in the static block that contains it.
        // For a full static block [A, B, C], look up the static block at A.
        let block = if bb_pcs.len() == 1 {
            // Single basic block — find it within its containing static block
            let pc = bb_pcs[0];
            let containing = static_blocks.get_containing(pc);
            let bb = containing
                .blocks()
                .find(|b| b.start_pc == pc)
                .unwrap_or_else(|| panic!("basic block {pc} not found in static block"));
            SuperBlock::from(bb.clone())
        } else {
            // Full static block — look up by first PC
            static_blocks.get(bb_pcs[0]).clone()
        };

        if block.instructions().count() > cfg.apc_max_instructions as usize {
            skipped_max_insn += 1;
            continue;
        }

        block_stats.push(BlockAndStats { block, count });
    }

    tracing::info!(
        "Skipped blocks: {} to execution cutoff, {} to instruction count",
        skipped_exec_count,
        skipped_max_insn,
    );

    tracing::info!(
        "{} remaining candidates: {} basic blocks and {} static blocks.",
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
    fn test_count_candidates_in_execution() {
        let runs = vec![
            (ExecutionBasicBlockRun(vec![1, 2, 3, 4]), 2),
            (ExecutionBasicBlockRun(vec![3, 4, 1]), 1),
        ];
        let candidates = vec![vec![1], vec![3, 4], vec![1, 2, 3]];
        let counts = count_candidates_in_execution(&runs, &candidates);
        // [1] appears: 2 times in first run * 2 + 1 time in second run = 5
        assert_eq!(counts[&vec![1]], 5);
        // [3, 4] appears: 1 time in first run * 2 + 1 time in second run = 3
        assert_eq!(counts[&vec![3, 4]], 3);
        // [1, 2, 3] appears: 1 time in first run * 2 + 0 in second = 2
        assert_eq!(counts[&vec![1, 2, 3]], 2);
    }

    #[test]
    fn test_detect_superblocks_basic_block_runs() {
        let bb = |start_pc: u64, len: usize| BasicBlock {
            start_pc,
            instructions: vec![TestInstruction; len],
        };

        let cfg = PowdrConfig::new(
            10,
            0,
            DegreeBound {
                identities: 2,
                bus_interactions: 2,
            },
        );

        // Static blocks: 100 (2 insn), 200 (2 insn), 300 (1 insn, invalid), 400 (3 insn)
        let basic_blocks = StaticBlocks::new(vec![bb(100, 2), bb(200, 2), bb(300, 1), bb(400, 3)]);

        // Execution: [100,101], [200,201], [300], [400,401,402], [100,101], [200,201]
        let execution = vec![100, 101, 200, 201, 300, 400, 401, 402, 100, 101, 200, 201];

        let result = detect_superblocks(&cfg, &execution, basic_blocks);

        // Runs should be basic-block level (300 is invalid, breaks the run)
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

        // Each single-BB static block counted independently
        assert_eq!(counts.get(&vec![100]), Some(&2));
        assert_eq!(counts.get(&vec![200]), Some(&2));
        assert_eq!(counts.get(&vec![400]), Some(&1));
        // 300 is single-instruction (invalid), not counted
        assert!(!counts.contains_key(&vec![300]));
    }
}
