use ::pipeline::inputs_to_query_callback;
use ::pipeline::Pipeline;
use ast::analyzed::Analyzed;
use criterion::{criterion_group, criterion_main, Criterion};

use mktemp::Temp;
use number::{FieldElement, GoldilocksField};
use riscv::continuations::bootloader::default_input;
use riscv::{compile_rust_crate_to_riscv_asm, compile_rust_to_riscv_asm, compiler};

use riscv::CoProcessors;

type T = GoldilocksField;

fn run_witgen<T: FieldElement>(
    analyzed: &Analyzed<T>,
    constants: &Vec<(String, Vec<T>)>,
    external_witness_values: Vec<(String, Vec<T>)>,
) {
    let query_callback = inputs_to_query_callback(vec![]);
    executor::witgen::WitnessGenerator::new(analyzed, constants, query_callback)
        .with_external_witness_values(external_witness_values)
        .generate();
}

fn criterion_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("executor-benchmark");
    group.sample_size(10);

    // Keccak
    let tmp_dir = Temp::new_dir().unwrap();
    let riscv_asm_files =
        compile_rust_crate_to_riscv_asm("../riscv/tests/riscv_data/keccak/Cargo.toml", &tmp_dir);
    let contents = compiler::compile(riscv_asm_files, &CoProcessors::base(), false);
    let pil_with_constants = Pipeline::<T>::default()
        .from_asm_string(contents, None)
        .pil_with_evaluated_fixed_cols()
        .unwrap();

    group.bench_function("keccak", |b| {
        b.iter(|| {
            run_witgen(
                &pil_with_constants.pil,
                &pil_with_constants.fixed_cols,
                vec![],
            )
        })
    });

    // The first chunk of `many_chunks`, with Poseidon co-processor & bootloader
    let riscv_asm_files =
        compile_rust_to_riscv_asm("../riscv/tests/riscv_data/many_chunks.rs", &tmp_dir);
    let contents = compiler::compile(riscv_asm_files, &CoProcessors::base().with_poseidon(), true);
    let pil_with_constants = Pipeline::<T>::default()
        .from_asm_string(contents, None)
        .pil_with_evaluated_fixed_cols()
        .unwrap();

    group.bench_function("many_chunks_chunk_0", |b| {
        b.iter(|| {
            run_witgen(
                &pil_with_constants.pil,
                &pil_with_constants.fixed_cols,
                vec![("main.bootloader_input_value".to_string(), default_input())],
            )
        })
    });
    group.finish();
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
