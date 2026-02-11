use std::cmp::Reverse;
use std::collections::HashMap;

use itertools::Itertools;
use powdr_autoprecompiles::pgo::cell::selection;
use powdr_autoprecompiles::pgo::cell::selection::BlockCandidate;
use tracing_subscriber::EnvFilter;

fn is_block_in_run(bbs: &[u64], run: &[u64]) -> bool {
    run.windows(bbs.len()).any(|window| window == bbs)
}

// const MAX_COST: Option<usize> = Some(600_000);
const MAX_SELECTED: Option<usize> = None;
const SKIP: usize = 0;

fn main() {
    tracing_subscriber::fmt()
        .with_env_filter(EnvFilter::from_default_env())
        .init();

    // read file paths from arguments
    let blocks_json = std::env::args()
        .nth(1)
        .expect("No blocks file path provided");
    let execution_json = std::env::args()
        .nth(2)
        .expect("No execution file path provided");

    tracing::info!("Reading blocks...");
    let start = std::time::Instant::now();
    let blocks: Vec<BlockCandidate> =
        serde_json::from_str(&std::fs::read_to_string(blocks_json).unwrap()).unwrap();
    tracing::info!(
        "Done reading {} blocks! took {:?}",
        blocks.len(),
        start.elapsed()
    );
    tracing::info!("Reading execution...");
    let start = std::time::Instant::now();
    let execution_bb_runs: Vec<(Vec<u64>, u32)> =
        serde_json::from_str(&std::fs::read_to_string(execution_json).unwrap()).unwrap();
    tracing::info!(
        "Done reading {} runs! took {:?}",
        execution_bb_runs.len(),
        start.elapsed()
    );

    // go over the runs, and return a list of (run, count), where count is the number of times this run appears
    tracing::info!(
        "There are {} unique runs. Highest freq run appears {} times.",
        execution_bb_runs.len(),
        execution_bb_runs
            .iter()
            .map(|(_, count)| *count)
            .max()
            .unwrap()
    );

    // just checking that idx_runs values is correct
    tracing::info!("Checking block idx_runs...");
    for b in &blocks {
        for idx in &b.idx_runs {
            if !is_block_in_run(&b.bbs, &execution_bb_runs[*idx].0) {
                panic!(
                    "Block {:?} not in run {:?}",
                    b.bbs, execution_bb_runs[*idx].0
                );
            };
        }
    }
    tracing::info!("Done!");

    ////////////////////////////////////////////////

    // SELECT FROM BASIC BLOCKS ONLY
    let bbs_only: Vec<_> = blocks.iter().filter(|b| b.bbs.len() == 1).cloned().collect();

    tracing::info!("Selecting BBs only (greedy)...");
    let start = std::time::Instant::now();
    let selected = selection::select_blocks_greedy(bbs_only.clone(), Some(6_000), MAX_SELECTED, &execution_bb_runs, SKIP);
    let (savings, cost) = selection::savings_and_cost(&bbs_only, &selected);
    tracing::info!("Total savings BBs only: {}\tcost: {} took: {:?}", savings, cost, start.elapsed());


    ////////////////////////////////////////////////

    let indices_by_density: Vec<usize> = blocks
        .iter()
        .enumerate()
        .map(|(idx, block)| (idx, block.priority()))
        .sorted_by_key(|(_, prio)| Reverse(prio.clone()))
        .map(|(idx, _)| idx)
        .collect::<Vec<_>>();

    let seed_pools_greedy = [50, 40, 20, 20];
    let seed_pools_cluster: HashMap<_,_> = [
        (6_000, [50, 40, 20, 20]),
        (60_000, [40, 30, 20, 20]),
        (600_000, [20, 20, 15, 15]),
    ].into_iter().collect();

    for budget in [60_000, 600_000] {
        tracing::info!("== COST = {budget} =============================");

        tracing::info!("Selecting BBs only (greedy)...");
        let start = std::time::Instant::now();
        let selected = selection::select_blocks_greedy(bbs_only.clone(), Some(budget), MAX_SELECTED, &execution_bb_runs, SKIP);
        let (savings, cost) = selection::savings_and_cost(&bbs_only, &selected);
        tracing::info!("Total savings BBs only: {}\tcost: {} took: {:?}", savings, cost, start.elapsed());


        tracing::info!("Selecting greedy by density...");
        let start = std::time::Instant::now();
        let selected = selection::select_blocks_greedy(blocks.clone(), Some(budget), MAX_SELECTED, &execution_bb_runs, SKIP);
        let (savings, cost) = selection::savings_and_cost(&blocks, &selected);
        tracing::info!("Total savings greedy: {}\tcost: {} took: {:?}", savings, cost, start.elapsed());

        tracing::info!("Selecting greedy by value...");
        let start = std::time::Instant::now();
        let selected = selection::select_blocks_greedy_by_value(blocks.clone(), Some(budget), MAX_SELECTED, &execution_bb_runs, SKIP);
        let (savings, cost) = selection::savings_and_cost(&blocks, &selected);
        tracing::info!("Total savings greedy by value: {}\tcost: {} took: {:?}", savings, cost, start.elapsed());

        ///////////////////////////////////////////////////////////////

        let seeds = selection::combination_seeds(&indices_by_density, &seed_pools_greedy);
        selection::log_seeds(&seeds);
        tracing::info!("Selecting using seeded initial selection ({:?})", &seed_pools_greedy);

        let start = std::time::Instant::now();
        let selected = selection::select_blocks_seeded(
            blocks.clone(),
            Some(budget),
            MAX_SELECTED,
            &execution_bb_runs,
            SKIP,
            seeds,
        );
        let (savings, cost) = selection::savings_and_cost(&blocks, &selected);
        tracing::info!(
            "Total savings seeded: {}\tcost: {} took: {:?}",
            savings,
            cost,
            start.elapsed()
        );

        ////////////////////////////////////////////////////////////

        tracing::info!("Selecting from clusters seeded ({:?})", &seed_pools_cluster[&budget]);
        let start = std::time::Instant::now();
        let selected = selection::select_from_clusters_seeded(
            blocks.clone(),
            &seed_pools_cluster[&budget],
            Some(budget),
            MAX_SELECTED,
            &execution_bb_runs,
            SKIP,
        );
        let (savings, cost) = selection::savings_and_cost(&blocks, &selected);
        tracing::info!(
            "Total savings cluster seeded: {}\tcost: {} took: {:?}",
            savings,
            cost,
            start.elapsed()
        );
    }
}
