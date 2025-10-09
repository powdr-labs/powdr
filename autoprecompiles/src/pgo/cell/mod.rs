use std::{collections::HashMap, sync::Arc};

use rayon::iter::{IntoParallelIterator, ParallelIterator};
use serde::{Deserialize, Serialize};

use crate::{
    adapter::{Adapter, AdapterApc, AdapterApcWithStats, AdapterVmConfig, PgoAdapter},
    blocks::BasicBlock,
    evaluation::EvaluationResult,
    pgo::cell::selection::parallel_fractional_knapsack,
    PowdrConfig,
};

mod selection;

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
    fn to_json_export(&self) -> ApcCandidateJsonExport;

    /// Convert the candidate into an autoprecompile and its statistics.
    fn into_apc_and_stats(self) -> AdapterApcWithStats<A>;
}

#[derive(Serialize, Deserialize, Clone)]
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
}

pub struct CellPgo<A, C> {
    _marker: std::marker::PhantomData<(A, C)>,
    data: HashMap<u64, u32>,
    max_total_apc_columns: Option<usize>,
}

impl<A, C> CellPgo<A, C> {
    pub fn with_pgo_data_and_max_columns(
        data: HashMap<u64, u32>,
        max_total_apc_columns: Option<usize>,
    ) -> Self {
        Self {
            _marker: std::marker::PhantomData,
            data,
            max_total_apc_columns,
        }
    }
}

impl<A: Adapter + Send + Sync, C: Candidate<A> + Send + Sync> PgoAdapter for CellPgo<A, C> {
    type Adapter = A;

    fn create_apcs_with_pgo(
        &self,
        mut blocks: Vec<BasicBlock<<Self::Adapter as Adapter>::Instruction>>,
        config: &PowdrConfig,
        vm_config: AdapterVmConfig<Self::Adapter>,
    ) -> Vec<AdapterApcWithStats<Self::Adapter>> {
        tracing::info!(
            "Generating autoprecompiles with cell PGO for {} blocks",
            blocks.len()
        );

        if config.autoprecompiles == 0 {
            return vec![];
        }

        // drop any block whose start index cannot be found in pc_idx_count,
        // because a basic block might not be executed at all.
        // Also only keep basic blocks with more than one original instruction.
        blocks.retain(|b| self.data.contains_key(&b.start_pc) && b.statements.len() > 1);

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
                    &self.data,
                    vm_config.clone(),
                    config.degree_bound.identities,
                );
                Some(candidate)
            }),
            max_cache,
            self.max_total_apc_columns,
        )
        .skip(config.skip_autoprecompiles as usize)
        .map(C::into_apc_and_stats)
        .collect();

        res
    }

    fn pc_execution_count(&self, pc: u64) -> Option<u32> {
        self.data.get(&pc).cloned()
    }
}
