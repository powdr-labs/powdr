//! Offline benchmark comparing the native Rust APC optimizer against the Lean apc-optimizer.
//!
//! When `POWDR_APC_DUMP_DIR` is set, [`powdr_autoprecompiles::build`] dumps every APC's
//! pre-optimization input circuit to a `.cbor` file instead of optimizing it. This module reads
//! those dumps back and re-runs *both* optimizers on each circuit, recording the input circuit's
//! size and each optimizer's runtime.
//!
//! ## Making the runtimes comparable across optimizer versions
//!
//! Timing 20k circuits one at a time would take many hours, so the bulk of the measurement runs in
//! a rayon pool and every runtime it reports is a *loaded* one. Two things keep those numbers
//! meaningful:
//!
//! * **One kind of work in flight.** The two optimizers are timed in separate passes rather than
//!   back-to-back per circuit. Interleaved, each optimizer's measured wall time depends on how much
//!   of the *other* is co-scheduled with it, and that mix shifts as their relative speed changes —
//!   so the same circuit could appear to get slower between two optimizer versions that both got
//!   faster. Within a pass the co-scheduled work is always the same kind.
//! * **Load-free anchors for the tail.** A few large circuits dominate the totals, and those are
//!   exactly the ones contention distorts most. After the parallel passes, the `isolate_top`
//!   costliest circuits are re-timed serially, alone on the machine, and reported with
//!   `isolated: true`. Those runtimes are directly comparable across versions.
//!
//! Compiled only with the `lean-optimizer` feature.
#![cfg(feature = "lean-optimizer")]

use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicUsize, Ordering};

use powdr_autoprecompiles::expression::AlgebraicReference;
use powdr_autoprecompiles::optimizer::{time_lean_optimizer, time_rust_optimizer, CircuitSize};
use powdr_autoprecompiles::ApcInput;
use powdr_number::BabyBearField;
use powdr_openvm_bus_interaction_handler::bus_map::OpenVmBusType;
use powdr_openvm_bus_interaction_handler::memory_bus_interaction::OpenVmMemoryBusInteraction;
use powdr_openvm_bus_interaction_handler::OpenVmBusInteractionHandler;
use rayon::prelude::*;
use serde::Serialize;

/// The concrete `ApcInput` produced by the OpenVM adapter (BabyBear field, OpenVM bus types).
type OpenVmApcInput = ApcInput<BabyBearField, OpenVmBusType>;

/// One input circuit's size and the runtime of each optimizer on it.
#[derive(Serialize, Clone)]
struct CircuitTiming {
    variables: usize,
    constraints: usize,
    bus_interactions: usize,
    rust_runtime: f64,
    lean_runtime: f64,
    /// Whether these runtimes were measured serially, alone on the machine. Loaded runtimes
    /// (`false`) are only comparable within one report; see the module docs.
    isolated: bool,
}

impl CircuitTiming {
    fn new(size: CircuitSize, lean_runtime: f64, rust_runtime: f64, isolated: bool) -> Self {
        Self {
            variables: size.variables,
            constraints: size.constraints,
            bus_interactions: size.bus_interactions,
            rust_runtime,
            lean_runtime,
            isolated,
        }
    }

    /// What the isolation phase ranks circuits by: the costlier optimizer decides.
    fn cost(&self) -> f64 {
        self.lean_runtime.max(self.rust_runtime)
    }
}

/// Per-benchmark results: the benchmark name and one timing per APC input circuit.
#[derive(Serialize)]
struct BenchmarkResult {
    name: String,
    apcs: Vec<CircuitTiming>,
}

#[derive(Serialize)]
struct TimingReport {
    /// Threads the parallel passes ran with — the load the non-`isolated` runtimes were measured
    /// under, and thus part of what makes them reproducible.
    parallelism: usize,
    benchmarks: Vec<BenchmarkResult>,
}

/// Time both optimizers over every dumped input circuit under `dump_dir` and write a JSON report.
///
/// `dump_dir` holds one subdirectory per benchmark (named after the benchmark); each contains the
/// `.cbor` input-circuit dumps written by [`powdr_autoprecompiles::build`]. `isolate_top` is how
/// many of the costliest circuits to re-time serially afterwards (0 disables the phase).
pub fn time_optimizers(dump_dir: &Path, output_path: &Path, isolate_top: usize) {
    // Collect the benchmark subdirectories (in a stable order) and, for each, its dump files.
    // Flatten into one global `(benchmark_index, file)` work list so a single parallel pass spans
    // every circuit of every benchmark: the few large circuits that dominate any one benchmark then
    // run alongside the many small circuits of the others, instead of leaving cores idle.
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
    let parallelism = rayon::current_num_threads();
    tracing::info!(
        "Timing both optimizers over {total} input circuits across {} benchmarks, \
         {parallelism} threads",
        benchmark_names.len()
    );

    // Pass 1 and 2: one parallel pass per optimizer, so only one kind of work is ever in flight.
    let lean: Vec<(CircuitSize, f64)> = timed_pass("lean", &work, |input| {
        let size = CircuitSize::of(&input.machine);
        let runtime = time_lean_optimizer(
            &input.machine,
            input.degree_bound,
            &input.bus_map,
            input.next_free_id,
        );
        (size, runtime)
    });
    let rust: Vec<f64> = timed_pass("rust", &work, run_rust);

    let mut timings: Vec<CircuitTiming> = lean
        .into_iter()
        .zip(rust)
        .map(|((size, lean_runtime), rust_runtime)| {
            CircuitTiming::new(size, lean_runtime, rust_runtime, false)
        })
        .collect();

    // Pass 3: re-time the costliest circuits serially, alone on the machine.
    let mut ranked: Vec<usize> = (0..timings.len()).collect();
    ranked.sort_by(|&a, &b| timings[b].cost().total_cmp(&timings[a].cost()));
    let isolated = &ranked[..isolate_top.min(ranked.len())];
    if !isolated.is_empty() {
        tracing::info!(
            "Re-timing the {} costliest circuits serially (isolated)",
            isolated.len()
        );
    }
    for (n, &index) in isolated.iter().enumerate() {
        let path = &work[index].1;
        let input = load(path);
        let size = CircuitSize::of(&input.machine);
        let lean_runtime = time_lean_optimizer(
            &input.machine,
            input.degree_bound,
            &input.bus_map,
            input.next_free_id,
        );
        let rust_runtime = run_rust(load(path));
        tracing::info!(
            "  isolated {}/{}: {} vars, lean {lean_runtime:.1}s (loaded {:.1}s), \
             rust {rust_runtime:.1}s (loaded {:.1}s)",
            n + 1,
            isolated.len(),
            size.variables,
            timings[index].lean_runtime,
            timings[index].rust_runtime,
        );
        timings[index] = CircuitTiming::new(size, lean_runtime, rust_runtime, true);
    }

    // Regroup by benchmark, preserving the benchmark order.
    let mut apcs_by_bench: Vec<Vec<CircuitTiming>> =
        benchmark_names.iter().map(|_| Vec::new()).collect();
    for ((bench_index, _), timing) in work.iter().zip(timings) {
        apcs_by_bench[*bench_index].push(timing);
    }

    let report = TimingReport {
        parallelism,
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

/// Run `time_one` over every circuit of `work` in one global parallel pass, logging progress.
/// Results come back in `work` order.
fn timed_pass<R: Send>(
    label: &str,
    work: &[(usize, PathBuf)],
    time_one: impl Fn(OpenVmApcInput) -> R + Send + Sync,
) -> Vec<R> {
    let total = work.len();
    let done = AtomicUsize::new(0);
    tracing::info!("Timing the {label} optimizer over {total} circuits");
    work.into_par_iter()
        .map(|(_, path)| {
            let result = time_one(load(path));
            let n = done.fetch_add(1, Ordering::Relaxed) + 1;
            if n.is_multiple_of(50) || n == total {
                tracing::info!("  {label}: timed {n}/{total} circuits");
            }
            result
        })
        .collect()
}

/// Time the Rust optimizer on one dumped circuit, instantiated for the OpenVM adapter.
fn run_rust(input: OpenVmApcInput) -> f64 {
    let handler = OpenVmBusInteractionHandler::<BabyBearField>::new(input.bus_map.clone());
    time_rust_optimizer::<
        BabyBearField,
        OpenVmBusInteractionHandler<BabyBearField>,
        OpenVmBusType,
        OpenVmMemoryBusInteraction<BabyBearField, AlgebraicReference>,
    >(
        input.machine,
        handler,
        input.degree_bound,
        &input.bus_map,
        input.next_free_id,
    )
}

/// Deserialize a single dumped input circuit.
fn load(path: &Path) -> OpenVmApcInput {
    let file = std::fs::File::open(path)
        .unwrap_or_else(|e| panic!("opening dump {}: {e}", path.display()));
    serde_cbor::from_reader(std::io::BufReader::new(file))
        .unwrap_or_else(|e| panic!("deserializing dump {}: {e}", path.display()))
}
