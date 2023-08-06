use analysis::analyze;
use ast::analyzed::Analyzed;
use criterion::{criterion_group, criterion_main, Criterion};

use compiler::inputs_to_query_callback;
use executor::constant_evaluator;
use number::{FieldElement, GoldilocksField};
use std::fs;

fn make_args<T: FieldElement>(f: &str, input: Vec<i64>) -> (Analyzed<T>, Vec<T>) {
    let input: Vec<T> = input.into_iter().map(|x| x.into()).collect();

    let contents = fs::read_to_string(format!("../test_data/asm/{f}")).unwrap();

    let parsed = parser::parse_asm::<T>(None, &contents).unwrap();
    let analysed = analyze(parsed).unwrap();
    let graph = asm_to_pil::compile(analysed);
    let pil = linker::link(graph);

    let analyzed = pil_analyzer::analyze_string(&format!("{pil}"));

    let analyzed = pilopt::optimize(analyzed);

    (analyzed, input)
}

fn run_witgen<'a, T: FieldElement>(analyzed: &'a Analyzed<T>, input: Vec<T>) {
    let query_callback = Some(inputs_to_query_callback(input));
    let (constants, degree) = constant_evaluator::generate(&analyzed);
    executor::witgen::generate(&analyzed, degree, &constants, query_callback);
}

fn criterion_benchmark(c: &mut Criterion) {
    let (analyzed, input) = make_args::<GoldilocksField>("keccak.asm", vec![]);

    let mut group = c.benchmark_group("sample-size-example");
    group.sample_size(10);
    group.bench_function("keccak", |b| {
        b.iter(|| run_witgen(&analyzed, input.clone()))
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
