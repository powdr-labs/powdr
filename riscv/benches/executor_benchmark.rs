use ::powdr_pipeline::Pipeline;
use powdr_number::{GoldilocksField, KnownField};

use powdr_riscv::{
    compile_rust_crate_to_riscv, continuations::bootloader::default_input, elf, Runtime,
};

use criterion::{criterion_group, criterion_main, Criterion};
use mktemp::Temp;

type T = GoldilocksField;

fn executor_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("executor-benchmark");
    group.sample_size(10);

    // Keccak
    let tmp_dir = Temp::new_dir().unwrap();
    let executable =
        compile_rust_crate_to_riscv("./tests/riscv_data/keccak/Cargo.toml", &tmp_dir, None);
    let contents = elf::translate::<T>(&executable, &Runtime::base(), false);
    let mut pipeline = Pipeline::<T>::default().from_asm_string(contents, None);
    pipeline.compute_optimized_pil().unwrap();
    pipeline.compute_fixed_cols().unwrap();

    group.bench_function("keccak", |b| {
        b.iter(|| pipeline.clone().compute_witness().unwrap())
    });

    // The first chunk of `many_chunks`, with Poseidon co-processor & bootloader
    let executable =
        compile_rust_crate_to_riscv("./tests/riscv_data/many_chunks/Cargo.toml", &tmp_dir, None);
    let contents = elf::translate::<T>(
        &executable,
        &Runtime::base().with_poseidon_for_continuations(),
        true,
    );
    let contents = asm::compile(riscv_asm_files, options, true);
    let mut pipeline = Pipeline::<T>::default().from_asm_string(contents, None);
    pipeline.compute_optimized_pil().unwrap();
    pipeline.compute_fixed_cols().unwrap();

    let pipeline = pipeline.add_external_witness_values(vec![(
        "main_bootloader_inputs::value".to_string(),
        default_input(&[63, 64, 65]),
    )]);
    group.bench_function("many_chunks_chunk_0", |b| {
        b.iter(|| pipeline.clone().compute_witness().unwrap())
    });
    group.finish();
}

criterion_group!(benches_riscv, executor_benchmark);
criterion_main!(benches_riscv);
