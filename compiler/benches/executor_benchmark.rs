use ::compiler::inputs_to_query_callback;
use analysis::analyze;
use ast::analyzed::Analyzed;
use criterion::{criterion_group, criterion_main, Criterion};

use executor::constant_evaluator;
use mktemp::Temp;
use number::{FieldElement, GoldilocksField};
use riscv::{compile_rust_crate_to_riscv_asm, compiler};

use riscv::CoProcessors;

type T = GoldilocksField;

fn get_pil() -> Analyzed<T> {
    let tmp_dir = Temp::new_dir().unwrap();
    let riscv_asm_files =
        compile_rust_crate_to_riscv_asm("../riscv/tests/riscv_data/keccak/Cargo.toml", &tmp_dir);
    let contents = compiler::compile(riscv_asm_files, &CoProcessors::base());
    let parsed = parser::parse_asm::<T>(None, &contents).unwrap();
    let resolved = importer::resolve(None, parsed).unwrap();
    let analyzed = analyze(resolved).unwrap();
    let graph = airgen::compile(analyzed);
    let pil = linker::link(graph).unwrap();
    let analyzed = pil_analyzer::analyze_string(&format!("{pil}"));
    pilopt::optimize(analyzed)
}

fn run_witgen<T: FieldElement>(analyzed: &Analyzed<T>, input: Vec<T>) {
    let query_callback = inputs_to_query_callback(input);
    let constants = constant_evaluator::generate(analyzed);
    executor::witgen::WitnessGenerator::new(analyzed, &constants, query_callback).generate();
}

fn criterion_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("keccak-executor-benchmark");
    group.sample_size(10);

    let keccak_pil = get_pil();
    group.bench_function("keccak", |b| b.iter(|| run_witgen(&keccak_pil, vec![])));
    group.finish();
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
