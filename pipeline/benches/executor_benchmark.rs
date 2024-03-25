use ::powdr_pipeline::{inputs_to_query_callback, Pipeline};
use powdr_ast::analyzed::Analyzed;
use powdr_number::{BigInt, FieldElement, GoldilocksField};

use powdr_pipeline::test_util::{evaluate_integer_function, std_analyzed};
use powdr_riscv::{
    compile_rust_crate_to_riscv_asm, compile_rust_to_riscv_asm, compiler,
    continuations::bootloader::default_input, CoProcessors,
};

use criterion::{criterion_group, criterion_main, Criterion};
use mktemp::Temp;

type T = GoldilocksField;

fn run_witgen<T: FieldElement>(
    analyzed: &Analyzed<T>,
    constants: &[(String, Vec<T>)],
    external_witness_values: &[(String, Vec<T>)],
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
    let contents = compiler::compile::<T>(riscv_asm_files, &CoProcessors::base::<T>(), false);
    let mut pipeline = Pipeline::<T>::default().from_asm_string(contents, None);
    let pil = pipeline.compute_optimized_pil().unwrap();
    let fixed_cols = pipeline.compute_fixed_cols().unwrap();

    group.bench_function("keccak", |b| b.iter(|| run_witgen(&pil, &fixed_cols, &[])));

    // The first chunk of `many_chunks`, with Poseidon co-processor & bootloader
    let riscv_asm_files =
        compile_rust_to_riscv_asm("../riscv/tests/riscv_data/many_chunks.rs", &tmp_dir);
    let contents = compiler::compile::<T>(
        riscv_asm_files,
        &CoProcessors::base::<T>().with_poseidon(),
        true,
    );
    let mut pipeline = Pipeline::<T>::default().from_asm_string(contents, None);
    let pil = pipeline.compute_optimized_pil().unwrap();
    let fixed_cols = pipeline.compute_fixed_cols().unwrap();

    group.bench_function("many_chunks_chunk_0", |b| {
        b.iter(|| {
            run_witgen(
                &pil,
                &fixed_cols,
                &[(
                    "main.bootloader_input_value".to_string(),
                    default_input(&[63, 64, 65])
                        .into_iter()
                        .map(|e| e.into_fe())
                        .collect(),
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
            let modulus = BigInt::from_str_radix(
                "fffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2f",
                16,
            )
            .unwrap();
            let x = modulus.clone() - BigInt::from(17);

            evaluate_integer_function(
                &analyzed,
                "std::math::ff::inverse",
                vec![x.clone(), modulus.clone()],
            );
        })
    });

    group.bench_function("std::math::ff::reduce", |b| {
        b.iter(|| {
            let modulus = BigInt::from_str_radix(
                "fffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2f",
                16,
            )
            .unwrap();
            let x = modulus.clone() + BigInt::from(17);

            evaluate_integer_function(
                &analyzed,
                "std::math::ff::reduce",
                vec![x.clone(), modulus.clone()],
            );
        })
    });

    group.bench_function("std::math::ff::mul", |b| {
        b.iter(|| {
            let modulus = BigInt::from_str_radix(
                "fffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2f",
                16,
            )
            .unwrap();
            let x = modulus.clone() - BigInt::from(17);
            let y = modulus.clone() - BigInt::from(11);

            evaluate_integer_function(
                &analyzed,
                "std::math::ff::mul",
                vec![x.clone(), y.clone(), modulus.clone()],
            );
        })
    });

    let sqrt_analyzed: Analyzed<GoldilocksField> = {
        // airgen needs a main machine.
        let code = "
            let sqrt: int -> int = |x| sqrt_rec(x, x);
            let sqrt_rec: int, int -> int = |y, x|
                if y * y <= x && (y + 1) * (y + 1) > x {
                    y
                } else {
                    sqrt_rec((y + x / y) / 2, x)
                };
            machine Main { }
        "
        .to_string();
        let mut pipeline = Pipeline::default().from_asm_string(code, None);
        pipeline.compute_analyzed_pil().unwrap().clone()
    };

    for x in [879882356, 1882356, 1187956, 56] {
        group.bench_with_input(format!("sqrt_{x}"), &x, |b, &x| {
            b.iter(|| {
                let y = BigInt::from(x) * BigInt::from(112655675);
                evaluate_integer_function(&sqrt_analyzed, "sqrt", vec![y.clone()]);
            });
        });
    }

    group.finish();
}

criterion_group!(benches, evaluator_benchmark, executor_benchmark);
criterion_main!(benches);
