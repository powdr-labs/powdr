use std::{
    collections::{BTreeMap, HashMap},
    io::BufWriter,
    path::Path,
    sync::{Arc, Mutex},
};

use rayon::iter::{IntoParallelIterator, ParallelIterator};
use serde::{Deserialize, Serialize};

use crate::{
    PowdrConfig, adapter::{Adapter, AdapterApc, AdapterApcWithStats, AdapterBasicBlock, AdapterVmConfig, PgoAdapter}, blocks::BasicBlock, evaluation::EvaluationResult, execution_profile::ExecutionProfile, pgo::cell::selection::parallel_fractional_knapsack
};

mod selection;

use itertools::Itertools;

pub use selection::KnapsackItem;

/// Trait for autoprecompile candidates.
/// Implementors of this trait wrap an APC with additional data used by the `KnapsackItem` trait to select the most cost-effective APCs.
pub trait Candidate<A: Adapter>: Sized + KnapsackItem {
    /// Try to create an autoprecompile candidate from a block.
    fn create(
        apc: Arc<AdapterApc<A>>,
        pgo_program_pc_count: &HashMap<u64, u32>,
        vm_config: AdapterVmConfig<A>,
        max_degree: usize,
    ) -> Self;

    /// Return a JSON export of the APC candidate.
    fn to_json_export(&self, apc_candidates_dir_path: &Path) -> ApcCandidateJsonExport;

    /// Convert the candidate into an autoprecompile and its statistics.
    fn into_apc_and_stats(self) -> AdapterApcWithStats<A>;
}

#[derive(Serialize, Deserialize)]
pub struct ApcCandidateJsonExport {
    // execution_frequency
    pub execution_frequency: usize,
    // original instructions (pretty printed)
    pub original_block: BasicBlock<String>,
    // before and after optimization stats
    pub stats: EvaluationResult,
    // width before optimisation, used for software version cells in effectiveness plot
    pub width_before: usize,
    // value used in ranking of candidates
    pub value: usize,
    // cost before optimisation, used for effectiveness calculation
    pub cost_before: f64,
    // cost after optimization, used for effectiveness calculation and ranking of candidates
    pub cost_after: f64,
    // path to the apc candidate file
    pub apc_candidate_file: String,
}

pub struct CellPgo<A, C> {
    _marker: std::marker::PhantomData<(A, C)>,
    data: ExecutionProfile,
    default_pc_step: u32,
    max_total_apc_columns: Option<usize>,
}

impl<A, C> CellPgo<A, C> {
    pub fn with_pgo_data_and_max_columns(
        data: ExecutionProfile,
        default_pc_step: u32,
        max_total_apc_columns: Option<usize>,
    ) -> Self {
        Self {
            _marker: std::marker::PhantomData,
            data,
            default_pc_step,
            max_total_apc_columns,
        }
    }
}

#[derive(Serialize, Deserialize)]
struct JsonExport {
    apcs: Vec<ApcCandidateJsonExport>,
    labels: BTreeMap<u64, Vec<String>>,
}

impl<A: Adapter + Send + Sync, C: Candidate<A> + Send + Sync> PgoAdapter for CellPgo<A, C> {
    type Adapter = A;

    fn create_apcs_with_pgo(
        &self,
        blocks: Vec<AdapterBasicBlock<Self::Adapter>>,
        config: &PowdrConfig,
        vm_config: AdapterVmConfig<Self::Adapter>,
        labels: BTreeMap<u64, Vec<String>>,
    ) -> Vec<AdapterApcWithStats<Self::Adapter>> {
        tracing::info!(
            "Generating autoprecompiles with cell PGO for {} blocks",
            blocks.len()
        );

        let prof = self.profiling_data().unwrap();

        let mut bb_count = vec![0; blocks.len()];
        // // count bb of first executed instruction
        // let initial_pc = original_program.exe.program.pc_base as u64;
        // if let Some(first_bb_idx) = blocks.iter().position(|bb| bb.start_pc == initial_pc) {
        //     bb_count[first_bb_idx] += 1;
        // }

        let mut bb_pair_count = HashMap::new();


        for (pc, next_pcs) in prof.next_pc.iter() {
            // check that PC ends a BB!
            let mut pc_ends_bb = false;
            let mut bb_idx_1 = 0;
            for (idx, bb) in blocks.iter().enumerate() {
                let last_pc = bb.start_pc + ((bb.statements.len() - 1) as u64 * self.default_pc_step as u64);

                if last_pc == *pc {
                    pc_ends_bb = true;
                    bb_idx_1 = idx;
                    break;
                }
            }
            if !pc_ends_bb {
                continue;
            }

            // println!("pc {} ends BB {}", pc, bb_idx_1);

            for (next_pc, next_pc_count) in next_pcs {
                // check this is the start of a BB!
                let mut is_start = false;
                let mut bb_idx_2 = 0;
                for (idx, bb) in blocks.iter().enumerate() {
                    if bb.start_pc == *next_pc {
                        is_start = true;
                        bb_idx_2 = idx;
                        break;
                    }
                }
                if is_start {
                    // only count BB if its a valid APC
                    if blocks[bb_idx_2].statements.len() > 1 {
                        bb_count[bb_idx_2] += next_pc_count;
                        // only count pair if both BBs are valid APC
                        if blocks[bb_idx_1].statements.len() > 1 {
                            bb_pair_count.entry((bb_idx_1, bb_idx_2))
                                .and_modify(|c| *c += next_pc_count)
                                .or_insert(*next_pc_count);
                        }
                    }
                    // println!("\tpc {} starts BB {}", next_pc, bb_idx_2);
                } else {
                    panic!("\tpc {} does not start a BB!", next_pc);
                }
            }
        }

        // BBs sorted by execution count
        let bb_sorted = bb_count
            .iter()
            .enumerate()
            // map to start_pc
            .map(|(idx, count)| (blocks[idx].start_pc, *count))
            .sorted_by_key(|(_, count)| *count)
            .filter(|&(_, count)| count > 0)
            .rev()
            .collect::<Vec<_>>();

        println!("BB execution counts: {bb_sorted:?}");

        // TODO: additional constraint to enforce the jump to the second block
        let super_blocks = bb_pair_count.iter()
            .filter_map(|(&(idx1, idx2), &count)| {
                if count > 1 {
                    let block_1 = blocks[idx1].clone();
                    let block_2 = blocks[idx2].clone();
                    Some(BasicBlock {
                        start_pc: block_1.start_pc,
                        other_pcs: vec![(block_1.statements.len(), block_2.start_pc)],
                        statements: [block_1.statements, block_2.statements].concat(),
                    })
                } else {
                    None
                }
            })
            .collect_vec();

        let bb_pair_sorted = bb_pair_count
            .iter()
            .sorted_by_key(|(_, count)| *count)
            // map to start_pc
            .map(|(&(bb_idx_1, bb_idx_2), count)| {
                (
                    (blocks[bb_idx_1].start_pc, blocks[bb_idx_2].start_pc),
                    *count,
                )
            })
            .rev()
            .collect::<Vec<_>>();

        println!("BB pairs execution counts: {bb_pair_sorted:?}");

        if config.autoprecompiles == 0 {
            return vec![];
        }

        let mut blocks = super_blocks;

        // drop any block whose start index cannot be found in pc_idx_count,
        // because a basic block might not be executed at all.
        // Also only keep basic blocks with more than one original instruction.
        blocks.retain(|b| self.data.pc_count.contains_key(&b.start_pc) && b.statements.len() > 1);


        tracing::debug!(
            "Retained {} basic blocks after filtering by pc_idx_count",
            blocks.len()
        );

        // generate apc for all basic blocks and only cache the ones we eventually use
        // calculate number of trace cells saved per row for each basic block to sort them by descending cost
        let max_cache = (config.autoprecompiles + config.skip_autoprecompiles) as usize;
        tracing::info!(
        "Generating autoprecompiles for all ({}) basic blocks in parallel and caching costliest {}",
        blocks.len(),
        max_cache,
    );

        let apc_candidates = Arc::new(Mutex::new(vec![]));

        // map–reduce over blocks into a single BinaryHeap<ApcCandidate<P>> capped at max_cache
        let res = parallel_fractional_knapsack(
            blocks.into_par_iter().filter_map(|block| {
                let apc = crate::build::<A>(
                    block.clone(),
                    vm_config.clone(),
                    config.degree_bound,
                    config.apc_candidates_dir_path.as_deref(),
                )
                .ok()?;
                let candidate = C::create(
                    Arc::new(apc),
                    &self.data.pc_count,
                    vm_config.clone(),
                    config.degree_bound.identities,
                );
                if let Some(apc_candidates_dir_path) = &config.apc_candidates_dir_path {
                    let json_export = candidate.to_json_export(apc_candidates_dir_path);
                    apc_candidates.lock().unwrap().push(json_export);
                }
                Some(candidate)
            }),
            max_cache,
            self.max_total_apc_columns,
        )
        .skip(config.skip_autoprecompiles as usize)
        .map(C::into_apc_and_stats)
        .collect();

        // Write the APC candidates JSON to disk if the directory is specified.
        if let Some(apc_candidates_dir_path) = &config.apc_candidates_dir_path {
            let apcs = apc_candidates.lock().unwrap().drain(..).collect();
            let json = JsonExport { apcs, labels };
            let json_path = apc_candidates_dir_path.join("apc_candidates.json");
            let file = std::fs::File::create(&json_path)
                .expect("Failed to create file for APC candidates JSON");
            serde_json::to_writer(BufWriter::new(file), &json)
                .expect("Failed to write APC candidates JSON to file");
        }

        res
    }

    fn pc_execution_count(&self, pc: u64) -> Option<u32> {
        self.data.pc_count.get(&pc).cloned()
    }

    fn profiling_data(&self) -> Option<&ExecutionProfile> {
        Some(&self.data)
    }
}
