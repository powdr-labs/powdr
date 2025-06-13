use criterion::{criterion_group, criterion_main, Criterion};
mod utils;
use utils::BenchmarkCollector;

use openvm_sdk::StdIn;
use powdr_openvm::{compile_guest, execute_and_generate, GuestOptions, PgoConfig, PowdrConfig};

const GUEST_KECCAK: &str = "guest-keccak";
const GUEST_KECCAK_ITER: u32 = 10;
const GUEST_KECCAK_APC: u64 = 1;
const GUEST_KECCAK_SKIP: u64 = 0;

fn keccak_benchmark(c: &mut Criterion) {
    // create a fresh collector for this benchmark
    let mut collector = BenchmarkCollector::new();
    collector.install();

    // To see the component run times (e.g. powdr chip vs non-powdr chip trace gen time, powdr chip dummy trace gen time for each dummy chip)
    // run with RUST_LOG=debug
    let mut group = c.benchmark_group("keccak-benchmark");

    // Compile the program to execute
    let guest_opts = GuestOptions::default();
    let powdr_config = PowdrConfig::new(GUEST_KECCAK_APC, GUEST_KECCAK_SKIP);
    let program = compile_guest(GUEST_KECCAK, guest_opts, powdr_config, PgoConfig::None).unwrap(); // we don't need PGO because it's guaranteed to run the biggest Keccak basic block without PGO
    let mut stdin = StdIn::default();
    stdin.write(&GUEST_KECCAK_ITER);

    // Default uses 100 samples, which is too many, but 10 samples is the required minimum
    group.sample_size(10);

    // Run benchmark
    group.bench_function("Execute 1000 Keccaks with 1 APC", |b| {
        b.iter(|| {
            execute_and_generate(program.clone(), stdin.clone()).unwrap();
        })
    });

    group.finish();

    // Print the tree of spans and their durations, clear the time collection layer for next benchmark
    collector.print_tree_and_clear();
}

criterion_group!(execution_benchmarks, keccak_benchmark);
criterion_main!(execution_benchmarks);
