use ::powdr_pipeline::{inputs_to_query_callback, Pipeline};
use powdr_ast::analyzed::Analyzed;
use powdr_number::{FieldElement, GoldilocksField};

use powdr_pipeline::test_util::{evaluate_integer_function, std_analyzed};
use powdr_riscv::{
    compile_rust_crate_to_riscv_asm, compile_rust_to_riscv_asm, compiler,
    continuations::bootloader::default_input, CoProcessors,
};

use criterion::{criterion_group, criterion_main, Criterion};
use mktemp::Temp;
use num_traits::Num;

type T = GoldilocksField;

fn run_witgen<T: FieldElement>(
    analyzed: &Analyzed<T>,
    constants: &[(String, Vec<T>)],
    external_witness_values: Vec<(String, Vec<T>)>,
) {
    let query_callback = inputs_to_query_callback(vec![]);
    powdr_executor::witgen::WitnessGenerator::new(analyzed, constants, &query_callback)
        .with_external_witness_values(external_witness_values)
        .generate();
}

fn executor_benchmark(c: &mut Criterion) {
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
                vec![(
                    "main.bootloader_input_value".to_string(),
                    default_input(&[63, 64, 65]),
                )],
            )
        })
    });
    group.finish();
}

fn evaluator_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("evaluator-benchmark");

    let analyzed = std_analyzed::<GoldilocksField>();

    group.bench_function("std::math::ff::inverse", |b| {
        b.iter(|| {
            let modulus = num_bigint::BigInt::from_str_radix(
                "fffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2f",
                16,
            )
            .unwrap();
            let x = modulus.clone() - num_bigint::BigInt::from(17);

            evaluate_integer_function(
                &analyzed,
                "std::math::ff::inverse",
                vec![x.clone(), modulus.clone()],
            );
        })
    });

    group.bench_function("std::math::ff::reduce", |b| {
        b.iter(|| {
            let modulus = num_bigint::BigInt::from_str_radix(
                "fffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2f",
                16,
            )
            .unwrap();
            let x = modulus.clone() + num_bigint::BigInt::from(17);

            evaluate_integer_function(
                &analyzed,
                "std::math::ff::reduce",
                vec![x.clone(), modulus.clone()],
            );
        })
    });

    group.bench_function("std::math::ff::mul", |b| {
        b.iter(|| {
            let modulus = num_bigint::BigInt::from_str_radix(
                "fffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2f",
                16,
            )
            .unwrap();
            let x = modulus.clone() - num_bigint::BigInt::from(17);
            let y = modulus.clone() - num_bigint::BigInt::from(11);

            evaluate_integer_function(
                &analyzed,
                "std::math::ff::mul",
                vec![x.clone(), y.clone(), modulus.clone()],
            );
        })
    });

    group.finish();
}

criterion_group!(benches, evaluator_benchmark, executor_benchmark);
criterion_main!(benches);
