#![allow(clippy::print_stdout)]
//! Debug tool: run the APC selection algorithm from JSON files.
//!
//! Usage:
//!   cargo run -p powdr-autoprecompiles --bin run_selection -- \
//!     <apc_candidates.json> <execution_bb_runs.json> <max_selected> [budget]

use std::{fs::File, io::BufReader};

use powdr_autoprecompiles::{
    blocks::ExecutionBasicBlockRun,
    pgo::cell::{
        selection::{select_candidates_greedy, BlockCandidate},
        ApcCandidateJsonExport,
    },
};
use serde::Deserialize;

#[derive(Deserialize)]
struct JsonExport {
    #[allow(dead_code)]
    version: usize,
    apcs: Vec<ApcCandidateJsonExport>,
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 4 || args.len() > 5 {
        eprintln!(
            "Usage: {} <apc_candidates.json> <execution_bb_runs.json> <max_selected> [budget]",
            args[0]
        );
        std::process::exit(1);
    }

    let apc_path = &args[1];
    let runs_path = &args[2];
    let max_selected: usize = args[3].parse().expect("max_selected must be a number");
    let budget: usize = args
        .get(4)
        .map(|s| s.parse().expect("budget must be a number"))
        .unwrap_or(usize::MAX);

    // Load APC candidates
    let file = File::open(apc_path).expect("Failed to open apc_candidates.json");
    let export: JsonExport =
        serde_json::from_reader(BufReader::new(file)).expect("Failed to parse apc_candidates.json");

    let candidates: Vec<BlockCandidate> = export
        .apcs
        .iter()
        .map(|apc| {
            let start_pcs: Vec<u64> = apc.original_blocks.iter().map(|b| b.start_pc).collect();
            let cost_before = apc.cost_before as usize;
            let cost_after = apc.cost_after as usize;
            BlockCandidate {
                start_pcs,
                cost_before,
                cost_after,
                value_per_use: cost_before.saturating_sub(cost_after),
                execution_count: apc.execution_frequency as u32,
            }
        })
        .collect();

    // Load execution runs
    let file = File::open(runs_path).expect("Failed to open execution_bb_runs.json");
    let runs: Vec<(ExecutionBasicBlockRun, u32)> = serde_json::from_reader(BufReader::new(file))
        .expect("Failed to parse execution_bb_runs.json");

    eprintln!(
        "Loaded {} candidates and {} execution runs",
        candidates.len(),
        runs.len()
    );

    // Run selection (one_per_start_pc=true by default)
    let selection = select_candidates_greedy(candidates.clone(), budget, max_selected, true, &runs);

    println!(
        "Selected {} APCs (budget={}, max_selected={}):\n",
        selection.len(),
        budget,
        max_selected
    );

    let mut cumulative_cost = 0;
    for (rank, &idx) in selection.iter().enumerate() {
        let c = &candidates[idx];
        let start_pcs_hex: Vec<String> = c.start_pcs.iter().map(|pc| format!("{pc:#x}")).collect();
        let value = c.value();
        let density_val = if c.cost() > 0 {
            value as f64 / c.cost() as f64
        } else {
            f64::INFINITY
        };
        cumulative_cost += c.cost();
        println!(
            "  #{:<3} start_pcs={:<40} freq={:<6} cost={}->{} value={:<10} density={:<10.2} cumul_cost={}",
            rank + 1,
            format!("{:?}", start_pcs_hex),
            c.execution_count,
            c.cost_before,
            c.cost_after,
            value,
            density_val,
            cumulative_cost,
        );
    }
}
