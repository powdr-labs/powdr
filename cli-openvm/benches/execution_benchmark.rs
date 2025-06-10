use std::{
    collections::HashMap,
    sync::{Arc, Mutex},
    time::{Duration, Instant},
};

use criterion::{criterion_group, criterion_main, Criterion};
use once_cell::sync::Lazy;
use tracing::{span, Subscriber};
use tracing_subscriber::{layer::Context, registry::LookupSpan, EnvFilter, Layer, Registry};
use tracing_subscriber::prelude::*;

use openvm_sdk::StdIn;
use powdr_openvm::{compile_guest, execute_and_generate, GuestOptions, PgoConfig, PowdrConfig};

const GUEST_KECCAK: &str = "guest-keccak";
const GUEST_KECCAK_ITER: u32 = 1000;
const GUEST_KECCAK_APC: u64 = 1;
const GUEST_KECCAK_SKIP: u64 = 0;

// GLOBAL: map from span name to all observed durations
static SPAN_TIMES: Lazy<Arc<Mutex<HashMap<String, Vec<Duration>>>>> =
    Lazy::new(|| Arc::new(Mutex::new(HashMap::new())));

/// A tracing layer that stamps each span with its enter-time, then on exit
/// records the elapsed time into SPAN_TIMES under that span's name.
struct DurationRecorder;

impl<S> Layer<S> for DurationRecorder
where
    S: Subscriber + for<'a> LookupSpan<'a>,
{
    fn on_enter(&self, id: &tracing::Id, ctx: Context<'_, S>) {
        if let Some(span_ref) = ctx.span(id) {
            let mut extensions = span_ref.extensions_mut();
            extensions.insert(Instant::now());
        }
    }

    fn on_exit(&self, id: &tracing::Id, ctx: Context<'_, S>) {
        if let Some(span_ref) = ctx.span(id) {
            // same pattern here
            let extensions = span_ref.extensions();
            if let Some(start) = extensions.get::<Instant>().copied() {
                let dur = start.elapsed();
                let name = span_ref.name().to_string();
                let mut map = SPAN_TIMES.lock().unwrap();
                map.entry(name).or_default().push(dur);
            }
        }
    }
}

fn keccak_benchmark(c: &mut Criterion) {
    // Install the global subscriber once
    static INIT: Lazy<()> = Lazy::new(|| {
        // This reads RUST_LOG (or defaults to "debug" if unset)
        let filter = EnvFilter::try_from_default_env()
            .unwrap_or_else(|_| EnvFilter::new("debug"));
    
        Registry::default()
            .with(filter)           // make sure DEBUG spans aren't dropped
            .with(DurationRecorder) // then our custom layer
            .init();
    });
    Lazy::force(&INIT);

    // To see the component run times (e.g. powdr chip vs non-powdr chip trace gen time, powdr chip dummy trace gen time for each dummy chip)
    // run with RUST_LOG=debug
    let mut group = c.benchmark_group("keccak-benchmark");

    // Compile the program to execute
    let guest_opts = GuestOptions::default();
    let powdr_config = PowdrConfig::new(GUEST_KECCAK_APC, GUEST_KECCAK_SKIP);
    let program = compile_guest(GUEST_KECCAK, guest_opts, powdr_config, PgoConfig::None).unwrap(); // we don't need PGO because it's guaranteed to run the biggest Keccak basic block without PGO
    let mut stdin = StdIn::default();
    stdin.write(&GUEST_KECCAK_ITER);

    // Default uses 100 samples, which is too many
    group.sample_size(10);

    // Run benchmark
    group.bench_function("Execute 1000 Keccaks with 1 APC", |b| {
        b.iter(|| {
            execute_and_generate(program.clone(), stdin.clone()).unwrap();
        })
    });

    group.finish();

    // POST‐RUN: print out average for *every* span
    let map = SPAN_TIMES.lock().unwrap();
    println!("\nSpan timing averages (95% of observed spans):");
    for (name, times) in map.iter() {
        let count = times.len();
        let total: Duration = times.iter().copied().sum();
        let avg = total / (count as u32);
        println!("{:20} ran {:>5} times, avg = {:?}", name, count, avg);
    }
}

criterion_group!(execution_benchmarks, keccak_benchmark);
criterion_main!(execution_benchmarks);


