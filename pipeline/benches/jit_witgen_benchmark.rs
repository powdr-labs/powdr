use ::powdr_pipeline::Pipeline;
use powdr_number::GoldilocksField;

use criterion::{criterion_group, criterion_main, Criterion};

type T = GoldilocksField;

fn jit_witgen_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("jit-witgen-benchmark");
    group.sample_size(10);

    // Poseidon benchmark
    let mut pipeline =
        Pipeline::<T>::default().from_file("../test_data/std/poseidon_benchmark.asm".into());
    pipeline.compute_optimized_pil().unwrap();
    pipeline.compute_fixed_cols().unwrap();

    group.bench_function("keccak", |b| {
        b.iter(|| pipeline.clone().compute_witness().unwrap())
    });
    group.finish();
}

criterion_group!(benches_riscv, jit_witgen_benchmark);
criterion_main!(benches_riscv);
