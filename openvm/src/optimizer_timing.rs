//! Offline benchmark comparing the native Rust APC optimizer against the Lean apc-optimizer.
//!
//! When `POWDR_APC_DUMP_DIR` is set, [`powdr_autoprecompiles::build`] dumps every APC's
//! pre-optimization input circuit to a `.cbor` file instead of optimizing it. This module reads
//! those dumps back and re-runs *both* optimizers on each circuit, recording the input circuit's
//! size and each optimizer's runtime.
//!
//! All circuits across all benchmarks are timed in a single global rayon pool, so the few large
//! circuits that dominate any one benchmark don't leave cores idle while the rest of that
//! benchmark's circuits are done.
//!
//! Compiled only with the `lean-optimizer` feature (it calls
//! [`powdr_autoprecompiles::optimizer::compare_optimizers`]).
#![cfg(feature = "lean-optimizer")]

use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicUsize, Ordering};

use powdr_autoprecompiles::expression::AlgebraicReference;
use powdr_autoprecompiles::optimizer::{compare_optimizers, OptimizerComparison};
use powdr_autoprecompiles::ApcInput;
use powdr_number::BabyBearField;
use powdr_openvm_bus_interaction_handler::bus_map::OpenVmBusType;
use powdr_openvm_bus_interaction_handler::memory_bus_interaction::OpenVmMemoryBusInteraction;
use powdr_openvm_bus_interaction_handler::OpenVmBusInteractionHandler;
use rayon::prelude::*;
use serde::Serialize;

/// The concrete `ApcInput` produced by the OpenVM adapter (BabyBear field, OpenVM bus types).
type OpenVmApcInput = ApcInput<BabyBearField, OpenVmBusType>;

/// Per-benchmark results: the benchmark name and one comparison per APC input circuit.
#[derive(Serialize)]
struct BenchmarkResult {
    name: String,
    apcs: Vec<OptimizerComparison>,
}

#[derive(Serialize)]
struct TimingReport {
    benchmarks: Vec<BenchmarkResult>,
}

/// Time both optimizers over every dumped input circuit under `dump_dir` and write a JSON report.
///
/// `dump_dir` holds one subdirectory per benchmark (named after the benchmark); each contains the
/// `.cbor` input-circuit dumps written by [`powdr_autoprecompiles::build`]. The report has the
/// shape `{"benchmarks": [{"name", "apcs": [{"variables", "constraints", "bus_interactions",
/// "rust_runtime", "lean_runtime"}, ...]}, ...]}`.
pub fn time_optimizers(dump_dir: &Path, output_path: &Path) {
    // Collect the benchmark subdirectories (in a stable order) and, for each, its dump files.
    // Flatten into one global `(benchmark_index, file)` work list so a single parallel pass spans
    // every circuit of every benchmark.
    let mut subdirs: Vec<PathBuf> = std::fs::read_dir(dump_dir)
        .unwrap_or_else(|e| panic!("reading dump dir {}: {e}", dump_dir.display()))
        .map(|e| e.expect("reading dump dir entry").path())
        .filter(|p| p.is_dir())
        .collect();
    subdirs.sort();

    let mut benchmark_names: Vec<String> = Vec::new();
    let mut work: Vec<(usize, PathBuf)> = Vec::new();
    for subdir in &subdirs {
        let name = subdir
            .file_name()
            .expect("benchmark subdir name")
            .to_string_lossy()
            .into_owned();
        let bench_index = benchmark_names.len();
        let mut files: Vec<PathBuf> = std::fs::read_dir(subdir)
            .unwrap_or_else(|e| panic!("reading benchmark dir {}: {e}", subdir.display()))
            .map(|e| e.expect("reading benchmark dir entry").path())
            .filter(|p| p.extension().is_some_and(|ext| ext == "cbor"))
            .collect();
        files.sort();
        for f in files {
            work.push((bench_index, f));
        }
        benchmark_names.push(name);
    }

    let total = work.len();
    tracing::info!(
        "Timing both optimizers over {total} input circuits across {} benchmarks",
        benchmark_names.len()
    );
    let done = AtomicUsize::new(0);

    // One global parallel pass over every circuit from every benchmark.
    let timed: Vec<(usize, OptimizerComparison)> = work
        .into_par_iter()
        .map(|(bench_index, path)| {
            let comparison = time_one(&path);
            let n = done.fetch_add(1, Ordering::Relaxed) + 1;
            if n.is_multiple_of(50) || n == total {
                tracing::info!("  timed {n}/{total} circuits");
            }
            (bench_index, comparison)
        })
        .collect();

    // Regroup by benchmark, preserving the benchmark order.
    let mut apcs_by_bench: Vec<Vec<OptimizerComparison>> =
        benchmark_names.iter().map(|_| Vec::new()).collect();
    for (bench_index, comparison) in timed {
        apcs_by_bench[bench_index].push(comparison);
    }

    let report = TimingReport {
        benchmarks: benchmark_names
            .into_iter()
            .zip(apcs_by_bench)
            .map(|(name, apcs)| BenchmarkResult { name, apcs })
            .collect(),
    };

    let file = std::fs::File::create(output_path)
        .unwrap_or_else(|e| panic!("creating output file {}: {e}", output_path.display()));
    serde_json::to_writer_pretty(std::io::BufWriter::new(file), &report)
        .expect("writing timing report");
    tracing::info!("Wrote optimizer timing report to {}", output_path.display());
}

/// Deserialize a single dumped input circuit and time both optimizers on it.
fn time_one(path: &Path) -> OptimizerComparison {
    let file = std::fs::File::open(path)
        .unwrap_or_else(|e| panic!("opening dump {}: {e}", path.display()));
    let ApcInput {
        machine,
        bus_map,
        next_free_id,
        degree_bound,
    }: OpenVmApcInput = serde_cbor::from_reader(std::io::BufReader::new(file))
        .unwrap_or_else(|e| panic!("deserializing dump {}: {e}", path.display()));

    let handler = OpenVmBusInteractionHandler::<BabyBearField>::new(bus_map.clone());
    compare_optimizers::<
        BabyBearField,
        OpenVmBusInteractionHandler<BabyBearField>,
        OpenVmBusType,
        OpenVmMemoryBusInteraction<BabyBearField, AlgebraicReference>,
    >(machine, handler, degree_bound, &bus_map, next_free_id)
}
