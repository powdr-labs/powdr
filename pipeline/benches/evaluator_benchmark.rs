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

    for x in [879882356, 1882356, 1187956, 56] {
        group.bench_with_input(format!("sqrt_{x}"), &x, |b, &x| {
            b.iter(|| {
                let y = BigInt::from(x) * BigInt::from(112655675);
                evaluate_integer_function(&sqrt_analyzed, "sqrt", vec![y.clone()]);
            });
        });
    }

    let sort_analyzed: Analyzed<GoldilocksField> = {
        let code =
            "let sort_int: int[] -> int[] = |x| std::array::sort(x, |a, b| a < b);".to_string();
        let mut pipeline = Pipeline::default().from_asm_string(code, None);
        pipeline.compute_analyzed_pil().unwrap().clone()
    };

    for l in [33, 100, 300, 900, 2700] {
        let input = Arc::new(Value::Array(
            (0..l)
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

    for x in [879882356, 1882356, 1187956, 56] {
        group.bench_with_input(format!("sqrt_{x}"), &x, |b, &x| {
            b.iter(|| {
                let y = (x as u64) * 112655675;
                sqrt_fun.call(y);
            });
        });
    }

    group.finish();
}

criterion_group!(benches_pil, evaluator_benchmark, jit_benchmark);
criterion_main!(benches_pil);
