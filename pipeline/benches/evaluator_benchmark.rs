use std::sync::Arc;

use ::powdr_pipeline::Pipeline;
use powdr_ast::analyzed::Analyzed;
use powdr_number::{BigInt, GoldilocksField};
use powdr_pil_analyzer::evaluator::Value;

use powdr_pipeline::test_util::{evaluate_function, evaluate_integer_function, std_analyzed};

use criterion::{criterion_group, criterion_main, Criterion};

const SQRT_CODE: &str = "
    let sqrt: int -> int = |x| sqrt_rec(x, x);
    let sqrt_rec: int, int -> int = |y, x|
        if y * y <= x && (y + 1) * (y + 1) > x {
            y
        } else {
            sqrt_rec((y + x / y) / 2, x)
        };
";

const SORT_SIZES: [i32; 5] = [33, 100, 300, 900, 2700];

/// Just some numbers to test the sqrt function on.
fn sqrt_inputs() -> Vec<(String, u64)> {
    [879882356, 1882356, 1187956, 56]
        .into_iter()
        .map(|x| (x.to_string(), (x as u64) * 112655675_u64))
        .collect()
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
        let mut pipeline = Pipeline::default().from_asm_string(SQRT_CODE.to_string(), None);
        pipeline.compute_analyzed_pil().unwrap().clone()
    };

    for (name, val) in sqrt_inputs() {
        group.bench_with_input(format!("sqrt_{name}"), &val, |b, val| {
            b.iter(|| {
                evaluate_integer_function(&sqrt_analyzed, "sqrt", vec![BigInt::from(*val)]);
            });
        });
    }

    let sort_analyzed: Analyzed<GoldilocksField> = {
        let code =
            "let sort_int: int[] -> int[] = |x| std::array::sort(x, |a, b| a < b);".to_string();
        let mut pipeline = Pipeline::default().from_asm_string(code, None);
        pipeline.compute_analyzed_pil().unwrap().clone()
    };

    for l in &SORT_SIZES {
        let input = Arc::new(Value::Array(
            (0..*l)
                .rev()
                .map(|x| Arc::new(Value::Integer(x.into())))
                .collect(),
        ));
        group.bench_with_input(format!("sort_{l}"), &input, |b, x| {
            b.iter(|| {
                evaluate_function(&sort_analyzed, "sort_int", vec![x.clone()]);
            });
        });
    }

    group.finish();
}

fn jit_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("jit-benchmark");

    let sqrt_analyzed: Analyzed<GoldilocksField> = {
        let mut pipeline = Pipeline::default().from_asm_string(SQRT_CODE.to_string(), None);
        pipeline.compute_analyzed_pil().unwrap().clone()
    };

    let sqrt_fun = &powdr_jit_compiler::compile(&sqrt_analyzed, &["sqrt"]).unwrap()["sqrt"];

    for (name, val) in sqrt_inputs() {
        group.bench_with_input(format!("sqrt_{name}"), &val, |b, val| {
            b.iter(|| {
                sqrt_fun.call(*val);
            });
        });
    }

    // TOOD this is not exactly the same code as in the interpreter case.
    // Here we are creating the array inside the code.
    let sort_code = "let sort_int: int -> int = |size| {
        let arr = std::array::new(size, |i| size - i - 1);
        std::array::sort(arr, |a, b| a < b)[20]
    };";
    let sort_analyzed: Analyzed<GoldilocksField> = {
        let mut pipeline = Pipeline::default().from_asm_string(sort_code.to_string(), None);
        pipeline.compute_analyzed_pil().unwrap().clone()
    };
    let sort_fun = &powdr_jit_compiler::compile(&sort_analyzed, &["sort_int"]).unwrap()["sort_int"];
    for l in &SORT_SIZES {
        group.bench_with_input(format!("sort_{l}"), &l, |b, l| {
            b.iter(|| {
                sort_fun.call(**l as u64);
            });
        });
    }
    group.finish();
}

criterion_group!(benches_pil, evaluator_benchmark, jit_benchmark);
criterion_main!(benches_pil);
