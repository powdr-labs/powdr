use std::{
    cmp::Ordering, collections::{BTreeMap, HashMap}, io::BufWriter, path::Path, sync::{Arc, Mutex}
};

use rayon::iter::{ParallelIterator, ParallelBridge};
use serde::{Deserialize, Serialize};

use crate::{
    PowdrConfig, adapter::{Adapter, AdapterApc, AdapterApcWithStats, AdapterBasicBlock, AdapterVmConfig, ApcWithStats, PgoAdapter}, blocks::{BasicBlock, generate_superblocks}, evaluation::EvaluationResult, execution_profile::ExecutionProfile, pgo::cell::selection::parallel_fractional_knapsack
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
        exec_count: u32,
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
        block_exec_count: Option<HashMap<usize, u32>>,
        config: &PowdrConfig,
        vm_config: AdapterVmConfig<Self::Adapter>,
        labels: BTreeMap<u64, Vec<String>>,
    ) -> Vec<AdapterApcWithStats<Self::Adapter>> {
        tracing::info!(
            "Generating autoprecompiles with cell PGO for {} blocks",
            blocks.len()
        );

        if config.autoprecompiles == 0 {
            return vec![];
        }

        // ensure blocks are valid for APC
        let block_exec_count = block_exec_count.unwrap();
        blocks.iter().enumerate().for_each(|(idx, b)| assert!(block_exec_count[&idx] > 0 && b.statements.len() > 1));

        // generate apc for all basic blocks and only cache the ones we eventually use
        // calculate number of trace cells saved per row for each basic block to sort them by descending cost
        let max_cache = (config.autoprecompiles + config.skip_autoprecompiles) as usize;
        tracing::info!(
            "Generating autoprecompiles for all ({}) basic blocks in parallel and caching costliest {}",
            blocks.len(),
            max_cache,
        );

        let apc_candidates = Arc::new(Mutex::new(vec![]));

        // generate candidates in parallel
        let mut candidates: Vec<_> = blocks.iter().enumerate().par_bridge().filter_map(|(idx, block)| {
            let apc = crate::build::<A>(
                block.clone(),
                vm_config.clone(),
                config.degree_bound,
                config.apc_candidates_dir_path.as_deref(),
            )
                .ok()?;
            let candidate = C::create(
                Arc::new(apc),
                block_exec_count[&idx],
                vm_config.clone(),
                config.degree_bound.identities,
            );
            if let Some(apc_candidates_dir_path) = &config.apc_candidates_dir_path {
                let json_export = candidate.to_json_export(apc_candidates_dir_path);
                println!("APC pc {}, other_pcs {:?}, cost effectiveness: {:?}, freq: {:?}, value: {:?}",
                         json_export.original_block.start_pc,
                         json_export.original_block.other_pcs,
                         json_export.cost_before as f64 / json_export.cost_after as f64,
                         json_export.execution_frequency,
                         json_export.value,
                );
                apc_candidates.lock().unwrap().push(json_export);
            }
            Some(candidate)
        }).collect();

        // sort candidates by value / cost (larger first)
        candidates.sort_by(|a, b| {
            let a_density = a.value() as f32 / a.cost() as f32;
            let b_density = b.value() as f32 / b.cost() as f32;
            match b_density.partial_cmp(&a_density) {
                Some(Ordering::Equal) | None => {
                    b.tie_breaker().cmp(&a.tie_breaker())
                },
                Some(ordering) => ordering
            }
        });

        println!("=============================================");
        println!("====== APC SELECTION ORDER ==================");
        println!("=============================================");

        // knapsack algorithm for choosing APCs maximizing the total value
        let res: Vec<_> = candidates.into_iter().scan(0, move |cumulative_cost, e| {
            if let Some(max_cost) = self.max_total_apc_columns {
                // Try to add the item
                if *cumulative_cost + e.cost() <= max_cost {
                    // The item fits, increment the cumulative cost
                    *cumulative_cost += e.cost();
                    Some(Some(e))
                } else {
                    // The item does not fit, skip it
                    Some(None)
                }
            } else {
                // No max cost, just return the item
                Some(Some(e))
            }
        })
            .flatten()
            .inspect(|c| {
                let c = c.to_json_export(Path::new("bla"));
                println!("==============================================");
                println!("APC pc {}, other_pcs {:?}, cost effectiveness: {:?}, freq: {:?}, value: {:?}",
                         c.original_block.start_pc,
                         c.original_block.other_pcs,
                         c.cost_before as f64 / c.cost_after as f64,
                         c.execution_frequency,
                         c.value,
                );
                println!("");
                println!("instructions:");
                for instr in c.original_block.statements.iter() {
                    println!("  {}", instr);
                }
                // println!("constraints:");
                // for constraint in c.machine.constraints.iter() {
                //     println!("  {}", constraint);
                // }
                // println!("interactions:");
                // for constraint in c.machine.bus_interactions.iter() {
                //     println!("  {}", constraint);
                // }
            })
            .take(max_cache)
            .skip(config.skip_autoprecompiles as usize)
            .map(C::into_apc_and_stats)
            .collect();

        // // map–reduce over blocks into a single BinaryHeap<ApcCandidate<P>> capped at max_cache
        // let res: Vec<_> = parallel_fractional_knapsack(
        //     blocks.iter().enumerate().par_bridge().filter_map(|(idx, block)| {
        //         let apc = crate::build::<A>(
        //             block.clone(),
        //             vm_config.clone(),
        //             config.degree_bound,
        //             config.apc_candidates_dir_path.as_deref(),
        //         )
        //         .ok()?;
        //         let candidate = C::create(
        //             Arc::new(apc),
        //             counts[&idx],
        //             vm_config.clone(),
        //             config.degree_bound.identities,
        //         );
        //         if let Some(apc_candidates_dir_path) = &config.apc_candidates_dir_path {
        //             let json_export = candidate.to_json_export(apc_candidates_dir_path);
        //             apc_candidates.lock().unwrap().push(json_export);
        //         }
        //         Some(candidate)
        //     }),
        //     max_cache,
        //     self.max_total_apc_columns,
        // )
        // .skip(config.skip_autoprecompiles as usize)
        // .map(C::into_apc_and_stats)
        // .collect();

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

    fn profiling_data(&self) -> Option<&ExecutionProfile> {
        Some(&self.data)
    }
}
