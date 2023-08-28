use analysis::analyze;
use ast::analyzed::Analyzed;
use criterion::{
    criterion_group, criterion_main, measurement::Measurement, BenchmarkGroup, Criterion,
};

use compiler::inputs_to_query_callback;
use executor::constant_evaluator;
use number::{FieldElement, GoldilocksField};
use std::{fs, path::Path};

type T = GoldilocksField;

fn to_field(arr: &[i32]) -> Vec<T> {
    arr.iter().cloned().map(|x| x.into()).collect()
}

fn asm_to_analyzed(input_file: &Path) -> Analyzed<T> {
    let contents = fs::read_to_string(input_file).unwrap();
    let parsed = parser::parse_asm::<T>(None, &contents).unwrap();
    let analyzed = analyze(parsed).unwrap();
    let graph = airgen::compile(analyzed);
    let pil = linker::link(graph).unwrap();

    pil_analyzer::analyze_string(&format!("{pil}"))
}

fn add_benchmark_asm<M: Measurement>(
    group: &mut BenchmarkGroup<'_, M>,
    name: &str,
    input: Vec<i32>,
) {
    let input_file = Path::new(&format!("../test_data/asm/{name}.asm"))
        .canonicalize()
        .unwrap();
    let name = input_file.file_name().unwrap().to_str().unwrap();
    let analyzed = asm_to_analyzed(&input_file);
    let analyzed = pilopt::optimize(analyzed);
    group.bench_function(name, |b| b.iter(|| run_witgen(&analyzed, to_field(&input))));
}

fn add_benchmark_pil<M: Measurement>(
    group: &mut BenchmarkGroup<'_, M>,
    name: &str,
    input: Vec<i32>,
) {
    let input_file = Path::new(&format!("../test_data/pil/{name}.pil"))
        .canonicalize()
        .unwrap();
    let name = input_file.file_name().unwrap().to_str().unwrap();
    let analyzed = pil_analyzer::analyze::<T>(&input_file);
    let analyzed = pilopt::optimize(analyzed);
    group.bench_function(name, |b| b.iter(|| run_witgen(&analyzed, to_field(&input))));
}

fn run_witgen<T: FieldElement>(analyzed: &Analyzed<T>, input: Vec<T>) {
    let query_callback = Some(inputs_to_query_callback(input));
    let (constants, degree) = constant_evaluator::generate(analyzed);
    executor::witgen::generate(analyzed, degree, &constants, query_callback);
}

fn criterion_benchmark(c: &mut Criterion) {
    // Keccak with smaller sample size
    let mut group = c.benchmark_group("keccak-executor-benchmark");
    group.sample_size(10);
    add_benchmark_pil(&mut group, "keccak", vec![]);
    group.finish();
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
