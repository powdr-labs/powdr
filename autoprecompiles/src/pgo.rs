use std::{
    cmp::{Ordering, Reverse},
    collections::{BinaryHeap, HashMap},
    io::BufWriter,
    iter::once,
    path::{Path, PathBuf},
    sync::{Arc, Mutex},
};

use powdr_constraint_solver::{constraint_system::BusInteractionHandler, inliner::DegreeBound};
use powdr_number::FieldElement;
use rayon::iter::{IndexedParallelIterator, IntoParallelIterator, ParallelIterator};
use serde::{Deserialize, Serialize};

use crate::{
    constraint_optimizer::IsBusStateful, Apc, BasicBlock, InstructionMachineHandler, VmConfig,
};

const POWDR_OPCODE: usize = 0xDEADBEEF;

/// Three modes for profiler guided optimization with different cost functions to sort the basic blocks by descending cost and select the most costly ones to accelerate.
/// The inner HashMap contains number of time a pc is executed.
#[derive(Default)]
pub enum PgoConfig {
    /// cost = cells saved per apc * times executed
    /// max total columns
    Cell(HashMap<u32, u32>, Option<usize>),
    /// cost = instruction per apc * times executed
    Instruction(HashMap<u32, u32>),
    /// cost = instruction per apc
    /// cost = instruction per apc
    #[default]
    None,
}

impl PgoConfig {
    /// Returns the number of times a certain pc offset was executed in the profile.
    pub fn pc_offset_execution_count(&self, pc_offset: u32) -> Option<u32> {
        match self {
            PgoConfig::Cell(pc_index_count, _) | PgoConfig::Instruction(pc_index_count) => {
                pc_index_count.get(&pc_offset).copied()
            }
            PgoConfig::None => None,
        }
    }
}

#[derive(Clone)]
pub struct PowdrConfig {
    /// Number of autoprecompiles to generate.
    pub autoprecompiles: u64,
    /// Number of basic blocks to skip for autoprecompiles.
    /// This is either the largest N if no PGO, or the costliest N with PGO.
    pub skip_autoprecompiles: u64,
    /// Max degree of constraints.
    pub degree_bound: DegreeBound,
    /// The path to the APC candidates dir, if any.
    pub apc_candidates_dir_path: Option<PathBuf>,
}

impl PowdrConfig {
    pub fn new(autoprecompiles: u64, skip_autoprecompiles: u64, degree_bound: DegreeBound) -> Self {
        Self {
            autoprecompiles,
            skip_autoprecompiles,
            degree_bound,
            apc_candidates_dir_path: None,
        }
    }

    pub fn with_autoprecompiles(self, autoprecompiles: u64) -> Self {
        Self {
            autoprecompiles,
            ..self
        }
    }

    pub fn with_degree_bound(self, degree_bound: DegreeBound) -> Self {
        Self {
            degree_bound,
            ..self
        }
    }

    pub fn with_apc_candidates_dir<P: AsRef<Path>>(mut self, path: P) -> Self {
        self.apc_candidates_dir_path = Some(path.as_ref().to_path_buf());
        self
    }
}

pub trait Candidate<P, I, B>: Sized {
    type JsonExport: Serialize + for<'de> Deserialize<'de> + Send + Sync;

    /// Try to create an autoprecompile candidate from a block.
    fn create(
        apc: Apc<P>,
        pgo_program_idx_count: &HashMap<u32, u32>,
        vm_config: VmConfig<I, B>,
    ) -> Self;

    /// Return a JSON export of the APC candidate.
    fn to_json_export(&self, apc_candidates_dir_path: &Path) -> Self::JsonExport;

    fn into_apc(self) -> Apc<P>;
}

// Note: This function can lead to OOM since it generates the apc for many blocks.
pub fn create_apcs_with_cell_pgo<
    C: KnapsackItem + Candidate<P, I, B> + Send + Sync,
    P: FieldElement,
    I: InstructionMachineHandler<P> + Clone + Send + Sync,
    B: BusInteractionHandler<P> + Clone + IsBusStateful<P> + Send + Sync,
>(
    mut blocks: Vec<BasicBlock<P>>,
    pgo_program_idx_count: HashMap<u32, u32>,
    config: &PowdrConfig,
    max_total_apc_columns: Option<usize>,
    vm_config: VmConfig<I, B>,
) -> Vec<Apc<P>> {
    // drop any block whose start index cannot be found in pc_idx_count,
    // because a basic block might not be executed at all.
    // Also only keep basic blocks with more than one original instruction.
    blocks.retain(|b| {
        pgo_program_idx_count.contains_key(&(b.start_idx as u32)) && b.statements.len() > 1
    });

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
    let res = fractional_knapsack(
        blocks.into_par_iter().enumerate().filter_map(|(i, block)| {
            let apc = crate::build(
                block.clone(),
                vm_config.clone(),
                config.degree_bound,
                (POWDR_OPCODE + i) as u32,
                config.apc_candidates_dir_path.as_deref(),
            )
            .ok()?;
            let candidate = C::create(apc, &pgo_program_idx_count, vm_config.clone());
            if let Some(apc_candidates_dir_path) = &config.apc_candidates_dir_path {
                let json_export = candidate.to_json_export(apc_candidates_dir_path);
                apc_candidates.lock().unwrap().push(json_export);
            }
            Some(candidate)
        }),
        max_cache,
        max_total_apc_columns,
    )
    .skip(config.skip_autoprecompiles as usize)
    .map(C::into_apc)
    .collect();

    // Write the APC candidates JSON to disk if the directory is specified.
    if let Some(apc_candidates_dir_path) = &config.apc_candidates_dir_path {
        let apc_candidates_json_file = apc_candidates.lock().unwrap();
        let json_path = apc_candidates_dir_path.join("apc_candidates.json");
        let file = std::fs::File::create(&json_path)
            .expect("Failed to create file for APC candidates JSON");
        serde_json::to_writer(BufWriter::new(file), &*apc_candidates_json_file)
            .expect("Failed to write APC candidates JSON to file");
    }

    res
}

fn create_apcs_with_instruction_pgo<
    P: FieldElement,
    I: InstructionMachineHandler<P> + Clone + Send + Sync,
    B: BusInteractionHandler<P> + Clone + IsBusStateful<P> + Send + Sync,
>(
    mut blocks: Vec<BasicBlock<P>>,
    pgo_program_idx_count: HashMap<u32, u32>,
    config: &PowdrConfig,
    vm_config: VmConfig<I, B>,
) -> Vec<Apc<P>> {
    // drop any block whose start index cannot be found in pc_idx_count,
    // because a basic block might not be executed at all.
    // Also only keep basic blocks with more than one original instruction.
    blocks.retain(|b| {
        pgo_program_idx_count.contains_key(&(b.start_idx as u32)) && b.statements.len() > 1
    });

    tracing::debug!(
        "Retained {} basic blocks after filtering by pc_idx_count",
        blocks.len()
    );

    // cost = cells_saved_per_row
    blocks.sort_by(|a, b| {
        let a_cnt = pgo_program_idx_count[&(a.start_idx as u32)];
        let b_cnt = pgo_program_idx_count[&(b.start_idx as u32)];
        (b_cnt * (b.statements.len() as u32)).cmp(&(a_cnt * (a.statements.len() as u32)))
    });

    // Debug print blocks by descending cost
    for block in &blocks {
        let start_idx = block.start_idx;
        let frequency = pgo_program_idx_count[&(start_idx as u32)];
        let number_of_instructions = block.statements.len();
        let value = frequency * number_of_instructions as u32;

        tracing::debug!(
            "Basic block start_idx: {start_idx}, value: {value}, frequency: {frequency}, number_of_instructions: {number_of_instructions}",
        );
    }

    create_apcs_for_all_blocks(blocks, config, vm_config)
}

fn create_apcs_with_no_pgo<
    P: FieldElement,
    I: InstructionMachineHandler<P> + Clone + Send + Sync,
    B: BusInteractionHandler<P> + Clone + IsBusStateful<P> + Send + Sync,
>(
    mut blocks: Vec<BasicBlock<P>>,
    config: &PowdrConfig,
    vm_config: VmConfig<I, B>,
) -> Vec<Apc<P>> {
    // cost = number_of_original_instructions
    blocks.sort_by(|a, b| b.statements.len().cmp(&a.statements.len()));

    // Debug print blocks by descending cost
    for block in &blocks {
        let start_idx = block.start_idx;
        tracing::debug!(
            "Basic block start_idx: {}, number_of_instructions: {}",
            start_idx,
            block.statements.len(),
        );
    }

    create_apcs_for_all_blocks(blocks, config, vm_config)
}

pub fn generate_apcs_with_pgo<
    C: KnapsackItem + Candidate<P, I, B> + Send + Sync,
    P: FieldElement,
    I: InstructionMachineHandler<P> + Clone + Send + Sync,
    B: BusInteractionHandler<P> + Clone + IsBusStateful<P> + Send + Sync,
>(
    blocks: Vec<BasicBlock<P>>,
    config: &PowdrConfig,
    max_total_apc_columns: Option<usize>,
    pgo_config: PgoConfig,
    vm_config: VmConfig<I, B>,
) -> Vec<Apc<P>> {
    // sort basic blocks by:
    // 1. if PgoConfig::Cell, cost = frequency * cells_saved_per_row
    // 2. if PgoConfig::Instruction, cost = frequency * number_of_instructions
    // 3. if PgoConfig::None, cost = number_of_instructions
    let res = match pgo_config {
        PgoConfig::Cell(pgo_program_idx_count, _) => create_apcs_with_cell_pgo::<C, _, _, _>(
            blocks,
            pgo_program_idx_count,
            config,
            max_total_apc_columns,
            vm_config,
        ),
        PgoConfig::Instruction(pgo_program_idx_count) => {
            create_apcs_with_instruction_pgo(blocks, pgo_program_idx_count, config, vm_config)
        }
        PgoConfig::None => create_apcs_with_no_pgo(blocks, config, vm_config),
    };

    assert!(res.len() <= config.autoprecompiles as usize);

    res
}

// Only used for PgoConfig::Instruction and PgoConfig::None,
// because PgoConfig::Cell caches all APCs in sorting stage.
fn create_apcs_for_all_blocks<
    P: FieldElement,
    I: InstructionMachineHandler<P> + Clone + Send + Sync,
    B: BusInteractionHandler<P> + Clone + IsBusStateful<P> + Send + Sync,
>(
    blocks: Vec<BasicBlock<P>>,
    powdr_config: &PowdrConfig,
    vm_config: VmConfig<I, B>,
) -> Vec<Apc<P>> {
    let n_acc = powdr_config.autoprecompiles as usize;
    tracing::info!("Generating {n_acc} autoprecompiles in parallel");

    blocks
        .into_par_iter()
        .skip(powdr_config.skip_autoprecompiles as usize)
        .take(n_acc)
        .enumerate()
        .map(|(index, block)| {
            tracing::debug!(
                "Accelerating block of length {} and start idx {}",
                block.statements.len(),
                block.start_idx
            );

            // tracing::debug!(
            //     "Acc block: {}",
            //     block.pretty_print(openvm_instruction_formatter)
            // );

            let apc_opcode = POWDR_OPCODE + index;

            crate::build(
                block,
                vm_config.clone(),
                powdr_config.degree_bound,
                apc_opcode as u32,
                powdr_config.apc_candidates_dir_path.as_deref(),
            )
            .unwrap()
        })
        .collect()
}
pub trait KnapsackItem {
    /// Cost of the item, used for sorting and knapsack algorithm.
    fn cost(&self) -> usize;
    /// Value of the item, used for sorting and knapsack algorithm. Should be much larger than `cost` to avoid ties.
    fn value(&self) -> usize;
    /// Tie breaker for the case when two candidates have the same cost and value. When a tie occurs, the item with higher value of this function is chosen.
    fn tie_breaker(&self) -> usize;
}

/// Fractional knapsack algorithm that uses parallel iterators to find the best items.
/// It returns an iterator over the items that fit into the knapsack, sorted by their density (value/cost).
pub(crate) fn fractional_knapsack<E: KnapsackItem + Send>(
    elements: impl IntoParallelIterator<Item = E>,
    max_count: usize,
    max_cost: Option<usize>,
) -> impl Iterator<Item = E> {
    struct KnapsackItemWrapper<E> {
        item: E,
    }

    impl<E: KnapsackItem> KnapsackItemWrapper<E> {
        fn density(&self) -> usize {
            // Note: If the value and cost are of similar magnitude, this would lead to ties.
            self.item.value() / self.item.cost()
        }
    }

    impl<E: KnapsackItem> Ord for KnapsackItemWrapper<E> {
        fn cmp(&self, other: &Self) -> Ordering {
            self.density()
                .cmp(&other.density())
                .then_with(|| self.item.tie_breaker().cmp(&other.item.tie_breaker()))
        }
    }

    impl<E: KnapsackItem> PartialEq for KnapsackItemWrapper<E> {
        fn eq(&self, other: &Self) -> bool {
            self.cmp(other) == Ordering::Equal
        }
    }
    impl<E: KnapsackItem> Eq for KnapsackItemWrapper<E> {}
    impl<E: KnapsackItem> PartialOrd for KnapsackItemWrapper<E> {
        fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
            Some(self.cmp(other))
        }
    }

    elements
        .into_par_iter()
        .map(|e| once(Reverse(KnapsackItemWrapper { item: e })).collect())
        .reduce(BinaryHeap::new, |mut acc, mut heap| {
            for elem in heap.drain() {
                acc.push(elem);
                if acc.len() > max_count {
                    acc.pop();
                }
            }
            acc
        })
        // TODO: use `into_sorted_iter` when it is available without nightly feature
        .into_sorted_vec()
        .into_iter()
        .map(|Reverse(e)| e.item)
        .scan(0, move |cumulative_cost, e| {
            if let Some(max_cost) = max_cost {
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
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Debug, Clone, PartialEq, Eq)]
    struct TestItem {
        index: usize,
        cost: usize,
        value: usize,
    }

    impl TestItem {
        fn new(index: usize, cost: usize, density: usize) -> Self {
            Self {
                index,
                cost,
                value: cost * density,
            }
        }
    }

    impl KnapsackItem for TestItem {
        fn cost(&self) -> usize {
            self.cost
        }

        fn value(&self) -> usize {
            self.value
        }

        fn tie_breaker(&self) -> usize {
            self.index
        }
    }

    #[test]
    fn tie() {
        let items = vec![TestItem::new(0, 1, 10), TestItem::new(1, 1, 10)];

        let max_count = 10;
        let max_cost = 1;

        // In case of tie, the second item (with larger index) should be chosen
        for _ in 0..10 {
            let result: Vec<_> =
                fractional_knapsack(items.clone(), max_count, Some(max_cost)).collect();
            assert_eq!(result.len(), 1);
            assert_eq!(result[0].index, 1);
        }
    }

    #[test]
    fn all_items_fit() {
        let items = vec![TestItem::new(0, 1, 2), TestItem::new(1, 2, 1)];

        let max_count = 10;
        let max_cost = 3;

        // All items fit, so both should be returned in the order of their (density, index)
        for _ in 0..10 {
            let result: Vec<_> =
                fractional_knapsack(items.clone(), max_count, Some(max_cost)).collect();
            assert_eq!(result, items);
        }
    }

    #[test]
    fn some_items_fit() {
        let items = vec![
            TestItem::new(0, 1, 3),
            TestItem::new(1, 2, 2),
            TestItem::new(2, 3, 1),
        ];

        let max_count = 10;
        let max_cost = 3;

        // Only the first two items fit, since their costs add up to 3 and they have the highest density
        for _ in 0..10 {
            let result: Vec<_> =
                fractional_knapsack(items.clone(), max_count, Some(max_cost)).collect();
            assert_eq!(result.as_slice(), &items[0..2]);
        }
    }

    #[test]
    fn many_with_ties_and_skips() {
        let items = vec![
            TestItem::new(1, 1, 10),
            TestItem::new(0, 1, 10),
            TestItem::new(3, 2, 5),
            TestItem::new(2, 2, 5),
            TestItem::new(4, 3, 3), // should be skipped due to cost
            TestItem::new(5, 1, 2),
        ];

        let max_count = 10;
        let max_cost = 7;

        // Only the first four items fit, since they have the highest density and their costs add up to 6 with the final item blowing up the max_cost.
        // Due to the same density, tie is broken by items with higher index coming up first.
        for _ in 0..10 {
            let result: Vec<_> =
                fractional_knapsack(items.clone(), max_count, Some(max_cost)).collect();
            assert_eq!(result.len(), 5);
            assert_eq!(result[0].index, 1);
            assert_eq!(result[1].index, 0);
            assert_eq!(result[2].index, 3);
            assert_eq!(result[3].index, 2);
            assert_eq!(result[4].index, 5);
        }
    }
}
