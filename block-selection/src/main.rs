use std::fs::File;
use std::io::BufWriter;
use std::path::PathBuf;

use clap::{Parser, ValueEnum};
use powdr_autoprecompiles::blocks::ExecutionBasicBlockRun;
use powdr_autoprecompiles::pgo::cell::selection::{
    select_candidates_by_saved_cells, select_candidates_greedy, BlockCandidate,
};
use powdr_autoprecompiles::pgo::cell::JsonExport;

/// Prototype APC block-selection algorithms over already-generated candidates.
///
/// Reads an `apc_candidates.json` (from a `select-apcs --apc-candidates-dir` run) and the
/// matching `execution_bb_runs.cbor`, runs a selection algorithm with the given
/// parameters, and writes the selected set in the same format as `apc_selection.json`
/// (the per-APC rows are taken from `apc_candidates.json`, with execution counts/values
/// updated to their post-selection values).
///
/// The two inputs must come from the same run; this is not checked.
#[derive(Parser)]
#[command(name = "block_selection", about, long_about = None)]
struct Cli {
    /// Path to `apc_candidates.json`.
    #[arg(long)]
    apc_candidates: PathBuf,

    /// Path to `execution_bb_runs.cbor` (from the same run as `apc_candidates.json`).
    #[arg(long)]
    execution_bb_runs: PathBuf,

    /// Column budget for the selection.
    #[arg(long, default_value_t = usize::MAX)]
    budget: usize,

    /// Maximum number of APCs to select.
    #[arg(long)]
    max_selected: usize,

    /// Select at most one APC per starting PC.
    #[arg(long)]
    one_block_per_pc: bool,

    /// Selection mode.
    #[arg(long, default_value = "greedy-density")]
    algorithm: Algorithm,

    /// Output path for the selection JSON.
    #[arg(long)]
    out: PathBuf,
}

#[derive(Clone, Copy, ValueEnum)]
enum Algorithm {
    /// Greedy by density (value / cost) — the default, matches the production pipeline.
    GreedyDensity,
    /// Greedy by saved cells only (total cells saved), ignoring cost.
    GreedySaved,
}

fn main() {
    let cli = Cli::parse();

    // Read the candidate export.
    let input: JsonExport = {
        let file = File::open(&cli.apc_candidates).expect("Failed to open apc_candidates.json");
        serde_json::from_reader(file).expect("Failed to parse apc_candidates.json")
    };

    // Read the execution basic-block runs.
    let execution_bb_runs: Vec<(ExecutionBasicBlockRun, u32)> = {
        let file =
            File::open(&cli.execution_bb_runs).expect("Failed to open execution_bb_runs.cbor");
        serde_cbor::from_reader(file).expect("Failed to parse execution_bb_runs.cbor")
    };

    // Rebuild the candidates the selection algorithm operates on.
    let candidates: Vec<BlockCandidate> = input
        .apcs
        .iter()
        .map(BlockCandidate::from_candidate_json)
        .collect();

    // Run the selection: returns (candidate index, effective post-selection count).
    let selection = match cli.algorithm {
        Algorithm::GreedyDensity => select_candidates_greedy(
            candidates,
            cli.budget,
            cli.max_selected,
            &execution_bb_runs,
            cli.one_block_per_pc,
        ),
        Algorithm::GreedySaved => select_candidates_by_saved_cells(
            candidates,
            cli.budget,
            cli.max_selected,
            &execution_bb_runs,
            cli.one_block_per_pc,
        ),
    };

    // Re-emit the selected rows in the same format as `apc_selection.json`, taking the
    // per-APC data from the original candidate rows and updating the execution
    // count/value to their post-selection values.
    let apcs = selection
        .iter()
        .map(|&(idx, count)| {
            let mut row = input.apcs[idx].clone();
            let value_per_use = if row.execution_frequency == 0 {
                0
            } else {
                row.value / row.execution_frequency
            };
            row.execution_frequency = count as usize;
            row.value = value_per_use * count as usize;
            row
        })
        .collect();

    let output = JsonExport {
        version: input.version,
        apcs,
        labels: input.labels,
    };

    let out_file = File::create(&cli.out).expect("Failed to create output file");
    serde_json::to_writer(BufWriter::new(out_file), &output)
        .expect("Failed to write selection JSON");

    eprintln!(
        "Selected {} of {} candidates -> {}",
        output.apcs.len(),
        input.apcs.len(),
        cli.out.display()
    );
}
