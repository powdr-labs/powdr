use ::compiler::inputs_to_query_callback;
use ::compiler::pipeline::Pipeline;
use ast::analyzed::Analyzed;
use criterion::{criterion_group, criterion_main, Criterion};

use mktemp::Temp;
use number::{FieldElement, GoldilocksField};
use riscv::{compile_rust_crate_to_riscv_asm, compiler};

use riscv::CoProcessors;

type T = GoldilocksField;

fn run_witgen<T: FieldElement>(analyzed: &Analyzed<T>, constants: Vec<(String, Vec<T>)>) {
    let query_callback = inputs_to_query_callback(vec![]);
    executor::witgen::WitnessGenerator::new(analyzed, &constants, query_callback).generate();
}

fn criterion_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("keccak-executor-benchmark");
    group.sample_size(10);

    let tmp_dir = Temp::new_dir().unwrap();
    let riscv_asm_files =
        compile_rust_crate_to_riscv_asm("../riscv/tests/riscv_data/keccak/Cargo.toml", &tmp_dir);
    let contents = compiler::compile(riscv_asm_files, &CoProcessors::base(), false);
    let pil_with_constants = Pipeline::<T>::default()
        .from_asm_string(contents, None)
        .optimized_pil_with_constants()
        .unwrap();

    group.bench_function("keccak", |b| {
        b.iter(|| {
            run_witgen(
                &pil_with_constants.pil,
                pil_with_constants.constants.clone(),
            )
        })
    });
    group.finish();
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
