use criterion::{criterion_group, criterion_main, Criterion};

const GUEST_KECCAK: &str = "guest_keccak";
const GUEST_KECCAK_ITER: u32 = 1000;
const GUEST_KECCAK_APC: u64 = 1;
const GUEST_KECCAK_SKIP: u64 = 0;

fn keccak_benchmark(c: &mut Criterion) {
    // To see the component run times (e.g. powdr chip vs non-powdr chip trace gen time, powdr chip dummy trace gen time for each dummy chip)
    // run with RUST_LOG=debug
    let mut group = c.benchmark_group("keccak-benchmark");

    // Compile the program to execute
    let guest_opts = powdr_openvm::GuestOptions::default();
    let powdr_config = powdr_openvm::PowdrConfig::new(GUEST_KECCAK_APC, GUEST_KECCAK_SKIP);
    let program =
        powdr_openvm::compile_guest(GUEST_KECCAK, guest_opts, powdr_config, None).unwrap(); // we don't need PGO because it's guaranteed to run the biggest Keccak basic block without PGO
    let mut stdin = openvm_sdk::StdIn::default();
    stdin.write(&GUEST_KECCAK_ITER);

	// Run benchmark
    group.bench_function("Execute 1000 Keccaks with 1 APC", |b| {
        b.iter(|| {
            powdr_openvm::execute(program.clone(), stdin.clone()).unwrap();
        })
    });

    group.finish();
}

criterion_group!(execution_benchmarks, keccak_benchmark);
criterion_main!(execution_benchmarks);
