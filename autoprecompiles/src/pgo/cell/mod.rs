use std::{
    collections::{BTreeMap, HashMap},
    io::BufWriter,
    marker::PhantomData,
    path::Path,
    sync::{Arc, Mutex},
};

use rayon::iter::{IntoParallelIterator, ParallelIterator};
use serde::{Deserialize, Serialize};

use crate::{
    adapter::{
        Adapter, AdapterApc, AdapterApcWithStats, AdapterVmConfig, ApcArithmetization,
        ApcWithReport, PgoAdapter,
    },
    blocks::BasicBlock,
    evaluation::{evaluate_apc, AirStats, ApcPerformanceReport, ApcStats},
    pgo::cell::selection::parallel_fractional_knapsack,
    PowdrConfig,
};

mod selection;

pub use selection::KnapsackItem;

/// While introducing apcs lead to savings, it also has costs.
/// This trait models the cost of introducing a new chip
pub trait Cost<S> {
    fn cost(air_metrics: S) -> usize;
}

/// A candidate APC
#[derive(Serialize, Deserialize)]
pub struct Candidate<A: Adapter, C: Cost<A::ApcStats>> {
    apc: Arc<AdapterApc<A>>,
    execution_frequency: usize,
    stats: ApcPerformanceReport<A::ApcStats>,
    _marker: PhantomData<C>,
}

impl<A: Adapter, C: Cost<A::ApcStats>> Candidate<A, C> {
    fn create<Air: ApcArithmetization<A>>(
        apc: Arc<AdapterApc<A>>,
        pgo_program_pc_count: &HashMap<u64, u32>,
        vm_config: AdapterVmConfig<A>,
        max_degree: usize,
    ) -> Self {
        let stats = evaluate_apc::<A, Air>(apc.clone(), vm_config.instruction_handler, max_degree);

        let execution_frequency =
            *pgo_program_pc_count.get(&apc.block.start_pc).unwrap_or(&0) as usize;

        Self {
            apc,
            execution_frequency,
            stats,
            _marker: PhantomData,
        }
    }

    /// Return a JSON export of the APC candidate.
    fn to_json_export(&self, apc_candidates_dir_path: &Path) -> ApcCandidateJsonExport {
        let stats: ApcPerformanceReport<AirStats> = self.stats.into();
        ApcCandidateJsonExport {
            execution_frequency: self.execution_frequency,
            original_block: BasicBlock {
                start_pc: self.apc.block.start_pc,
                statements: self
                    .apc
                    .block
                    .statements
                    .iter()
                    .map(ToString::to_string)
                    .collect(),
            },
            stats,
            width_before: self.stats.before.cells_per_call(),
            value: self.value(),
            cost_before: self.stats.before.cells_per_call() as f64,
            cost_after: self.stats.after.cells_per_call() as f64,
            apc_candidate_file: apc_candidates_dir_path
                .join(format!("apc_{}.cbor", self.apc.start_pc()))
                .display()
                .to_string(),
        }
    }

    fn into_apc_and_stats(self) -> AdapterApcWithStats<A> {
        ApcWithReport::new(self.apc, self.stats)
    }
}

impl<A: Adapter, C: Cost<A::ApcStats>> KnapsackItem for Candidate<A, C> {
    fn cost(&self) -> usize {
        C::cost(self.stats.after)
    }

    fn value(&self) -> usize {
        // For an APC which is called once and saves 1 cell, this would be 1.
        let value = self
            .execution_frequency
            .checked_mul(self.stats.cells_saved_per_call())
            .unwrap();
        // We need `value()` to be much larger than `cost()` to avoid ties when ranking by `value() / cost()`
        // Therefore, we scale it up by a constant factor.
        value.checked_mul(1000).unwrap()
    }

    fn tie_breaker(&self) -> usize {
        self.apc.start_pc() as usize
    }
}

#[derive(Serialize, Deserialize)]
pub struct ApcCandidateJsonExport {
    // execution_frequency
    pub execution_frequency: usize,
    // original instructions (pretty printed)
    pub original_block: BasicBlock<String>,
    // before and after optimization stats
    pub stats: ApcPerformanceReport<AirStats>,
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

pub struct CellPgo<A, C, Air> {
    _marker: std::marker::PhantomData<(A, C, Air)>,
    data: HashMap<u64, u32>,
    max_total_apc_columns: Option<usize>,
}

impl<A, C, Air> CellPgo<A, C, Air> {
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

#[derive(Serialize, Deserialize)]
struct JsonExport {
    apcs: Vec<ApcCandidateJsonExport>,
    labels: BTreeMap<u64, Vec<String>>,
}

impl<A: Adapter + Send + Sync, C: Cost<A::ApcStats> + Send + Sync, Air: ApcArithmetization<A>>
    PgoAdapter for CellPgo<A, C, Air>
{
    type Adapter = A;
    type Air = Air;

    fn create_apcs_with_pgo(
        &self,
        mut blocks: Vec<BasicBlock<<Self::Adapter as Adapter>::Instruction>>,
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
                let candidate: Candidate<A, C> = Candidate::create::<Air>(
                    Arc::new(apc),
                    &self.data,
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
        .map(Candidate::into_apc_and_stats)
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
        self.data.get(&pc).cloned()
    }
}
