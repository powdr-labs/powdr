use std::sync::Arc;

use powdr_number::{BigInt, Bn254Field, GoldilocksField};

use powdr_pil_analyzer::evaluator::Value;
use powdr_pipeline::{
    test_util::{
        evaluate_function, evaluate_integer_function, execute_test_file, gen_estark_proof,
        gen_halo2_proof, make_prepared_pipeline, resolve_test_file, run_pilcom_test_file,
        std_analyzed, test_halo2, BackendVariant,
    },
    Pipeline,
};
use test_log::test;

#[test]
fn poseidon_bn254_test() {
    let f = "std/poseidon_bn254_test.asm";
    test_halo2(f, Default::default());

    // `test_halo2` only does a mock proof in the PR tests.
    // This makes sure we test the whole proof generation for one example
    // file even in the PR tests.
    gen_halo2_proof(
        make_prepared_pipeline(f, vec![], vec![]),
        BackendVariant::Monolithic,
    );
    gen_halo2_proof(
        make_prepared_pipeline(f, vec![], vec![]),
        BackendVariant::Composite,
    );
}

#[test]
fn poseidon_gl_test() {
    let f = "std/poseidon_gl_test.asm";
    run_pilcom_test_file(f, Default::default(), vec![]).unwrap();
    gen_estark_proof(f, Default::default());
}

#[test]
fn poseidon_gl_memory_test() {
    let f = "std/poseidon_gl_memory_test.asm";
    run_pilcom_test_file(f, Default::default(), vec![]).unwrap();
    gen_estark_proof(f, Default::default());
}

#[test]
fn split_bn254_test() {
    let f = "std/split_bn254_test.asm";
    test_halo2(f, Default::default());
}

#[test]
fn split_gl_test() {
    let f = "std/split_gl_test.asm";
    run_pilcom_test_file(f, Default::default(), vec![]).unwrap();
    gen_estark_proof(f, Default::default());
}

#[test]
#[ignore = "Too slow"]
fn arith_test() {
    let f = "std/arith_test.asm";
    run_pilcom_test_file(f, Default::default(), vec![]).unwrap();

    // Running gen_estark_proof(f, Default::default())
    // is too slow for the PR tests. This will only create a single
    // eStark proof instead of 3.
    Pipeline::<GoldilocksField>::default()
        .from_file(resolve_test_file(f))
        .with_backend(powdr_backend::BackendType::EStarkStarky, None)
        .compute_proof()
        .unwrap();

    test_halo2(f, Default::default());
}

#[test]
fn memory_test() {
    let f = "std/memory_test.asm";
    run_pilcom_test_file(f, Default::default(), vec![]).unwrap();
    gen_estark_proof(f, Default::default());
    test_halo2(f, Default::default());
}

#[test]
fn memory_with_bootloader_write_test() {
    let f = "std/memory_with_bootloader_write_test.asm";
    run_pilcom_test_file(f, Default::default(), vec![]).unwrap();
    gen_estark_proof(f, Default::default());
    test_halo2(f, Default::default());
}

#[test]
fn memory_test_parallel_accesses() {
    let f = "std/memory_test_parallel_accesses.asm";
    run_pilcom_test_file(f, Default::default(), vec![]).unwrap();
    gen_estark_proof(f, Default::default());
    test_halo2(f, Default::default());
}

#[test]
fn permutation_via_challenges_bn() {
    let f = "std/permutation_via_challenges.asm";
    test_halo2(f, Default::default());
}

#[test]
#[should_panic = "Error reducing expression to constraint:\nExpression: std::protocols::permutation::permutation(main.is_first, [main.z], main.alpha, main.beta, main.permutation_constraint)\nError: FailedAssertion(\"The field is too small and needs to move to the extension field. Pass two elements instead!\")"]
fn permutation_via_challenges_gl() {
    let f = "std/permutation_via_challenges.asm";
    Pipeline::<GoldilocksField>::default()
        .from_file(resolve_test_file(f))
        .compute_witness()
        .unwrap();
}

#[test]
fn permutation_via_challenges_ext() {
    let f = "std/permutation_via_challenges_ext.asm";
    test_halo2(f, Default::default());
    // Note that this does not actually run the second-phase witness generation, because no
    // Goldilocks backend support challenges yet. But at least it tests that the panic from
    // the previous test is not happening.
    Pipeline::<GoldilocksField>::default()
        .from_file(resolve_test_file(f))
        .compute_witness()
        .unwrap();
}

#[test]
fn lookup_via_challenges_bn() {
    let f = "std/lookup_via_challenges.asm";
    test_halo2(f, Default::default());
}

#[test]
fn lookup_via_challenges_ext() {
    let f = "std/lookup_via_challenges_ext.asm";
    test_halo2(f, Default::default());
    // Note that this does not actually run the second-phase witness generation, because no
    // Goldilocks backend support challenges yet.
    Pipeline::<GoldilocksField>::default()
        .from_file(resolve_test_file(f))
        .compute_witness()
        .unwrap();
}

#[test]
fn lookup_via_challenges_ext_simple() {
    let f = "std/lookup_via_challenges_ext_simple.asm";
    test_halo2(f, Default::default());
    // Note that this does not actually run the second-phase witness generation, because no
    // Goldilocks backend support challenges yet.
    Pipeline::<GoldilocksField>::default()
        .from_file(resolve_test_file(f))
        .compute_witness()
        .unwrap();
}

#[test]
fn write_once_memory_test() {
    let f = "std/write_once_memory_test.asm";
    run_pilcom_test_file(f, Default::default(), vec![]).unwrap();
    gen_estark_proof(f, Default::default());
    test_halo2(f, Default::default());
}

#[test]
fn binary_test() {
    let f = "std/binary_test.asm";
    run_pilcom_test_file(f, Default::default(), vec![]).unwrap();
    test_halo2(f, Default::default());
}

#[test]
fn shift_test() {
    let f = "std/shift_test.asm";
    run_pilcom_test_file(f, Default::default(), vec![]).unwrap();
    test_halo2(f, Default::default());
}

#[test]
fn ff_reduce_mod_7() {
    let test_inputs = vec![
        -22, -21, -20, -8, -7, -6, -2, -1, -0, -1, -2, -3, 4, 5, 6, 7, 8, 9, 13, 14, 15, 20, 21, 22,
    ];
    let analyzed = std_analyzed::<GoldilocksField>();
    for x in test_inputs {
        let x = BigInt::from(x);
        let modulus = BigInt::from(7);
        let result = evaluate_integer_function(
            &analyzed,
            "std::math::ff::reduce",
            vec![x.clone(), modulus.clone()],
        );
        assert!(BigInt::from(0) <= result && result < modulus);
        if x < result {
            assert_eq!((result - x) % modulus, 0.into());
        } else {
            assert_eq!((x - result) % modulus, 0.into());
        }
    }
}

#[test]
fn ff_inverse() {
    let test_inputs = vec![
        (1, 11),
        (1, 7),
        (2, 7),
        (3, 7),
        (4, 7),
        (5, 7),
        (6, 7),
        (2, 17),
        (3, 17),
        (9, 17),
        (15, 17),
        (16, 17),
    ];
    let analyzed = std_analyzed::<GoldilocksField>();
    for (x, modulus) in test_inputs {
        let x = BigInt::from(x);
        let modulus = BigInt::from(modulus);
        let result = evaluate_integer_function(
            &analyzed,
            "std::math::ff::inverse",
            vec![x.clone(), modulus.clone()],
        );
        assert_eq!((result * x) % modulus, 1.into());
    }
}

#[test]
fn ff_add_sub_mul_div() {
    let inputs = vec![
        (1, 0, 11),
        (1, 6, 7),
        (6, 6, 7),
        (0, 0, 17),
        (0, 16, 17),
        (16, 16, 17),
        (3, 8, 17),
        (16, 1, 17),
        (5, 9, 17),
        (3, 14, 17),
    ];
    let analyzed = std_analyzed::<GoldilocksField>();
    for (x, y, modulus) in inputs {
        let x = BigInt::from(x);
        let y = BigInt::from(y);
        let modulus = BigInt::from(modulus);
        let result = evaluate_integer_function(
            &analyzed,
            "std::math::ff::add",
            vec![x.clone(), y.clone(), modulus.clone()],
        );
        assert_eq!((x.clone() + y.clone()) % modulus.clone(), result);

        let result = evaluate_integer_function(
            &analyzed,
            "std::math::ff::sub",
            vec![x.clone(), y.clone(), modulus.clone()],
        );
        assert_eq!(
            (x.clone() - y.clone() + modulus.clone()) % modulus.clone(),
            result
        );

        let result = evaluate_integer_function(
            &analyzed,
            "std::math::ff::mul",
            vec![x.clone(), y.clone(), modulus.clone()],
        );
        assert_eq!((x.clone() * y.clone()) % modulus.clone(), result);

        if y != 0.into() {
            let result = evaluate_integer_function(
                &analyzed,
                "std::math::ff::div",
                vec![x.clone(), y.clone(), modulus.clone()],
            );
            assert_eq!(x, (result * y) % modulus);
        }
    }
}

#[test]
fn ff_inv_big() {
    let analyzed = std_analyzed::<GoldilocksField>();
    // modulus of the secp256k1 base field
    let modulus = BigInt::from_str_radix(
        "fffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2f",
        16,
    )
    .unwrap();
    let x = modulus.clone() - BigInt::from(17);
    let result = evaluate_integer_function(
        &analyzed,
        "std::math::ff::inverse",
        vec![x.clone(), modulus.clone()],
    );
    assert_eq!((result * x) % modulus, 1.into());
}

#[test]
fn fp2() {
    let analyzed = std_analyzed::<GoldilocksField>();
    evaluate_function(&analyzed, "std::math::fp2::test::add", vec![]);
    evaluate_function(&analyzed, "std::math::fp2::test::sub", vec![]);
    evaluate_function(&analyzed, "std::math::fp2::test::mul", vec![]);
    evaluate_function(&analyzed, "std::math::fp2::test::inverse", vec![]);

    let analyzed = std_analyzed::<Bn254Field>();
    evaluate_function(&analyzed, "std::math::fp2::test::add", vec![]);
    evaluate_function(&analyzed, "std::math::fp2::test::sub", vec![]);
    evaluate_function(&analyzed, "std::math::fp2::test::mul", vec![]);
    evaluate_function(&analyzed, "std::math::fp2::test::inverse", vec![]);
}

#[test]
fn sort() {
    let test_inputs = vec![
        vec![],
        vec![1],
        vec![0, 0],
        vec![1, 2],
        vec![2, 1],
        vec![3, 2, 1],
        vec![0, 0, -1],
        vec![0, 0, -1, 0, 0, -1, -1, 2],
        vec![8, 0, 9, 20, 23, 88, 14, -9],
    ];
    let code =
        "let test_sort: int[] -> int[] = |x| std::array::sort(x, |a, b| a < b); machine Main { }"
            .to_string();
    let mut pipeline = Pipeline::<GoldilocksField>::default().from_asm_string(code, None);
    let analyzed = pipeline.compute_analyzed_pil().unwrap().clone();
    for input in test_inputs {
        let mut input_sorted = input.clone();
        input_sorted.sort();
        let result = evaluate_function(
            &analyzed,
            "test_sort",
            vec![Arc::new(Value::Array(
                input
                    .into_iter()
                    .map(|x| Arc::new(Value::Integer(x.into())))
                    .collect(),
            ))],
        );
        let Value::Array(result) = result else {
            panic!("Expected array")
        };
        let result: Vec<i32> = result
            .into_iter()
            .map(|x| match x.as_ref() {
                Value::Integer(x) => x.try_into().unwrap(),
                _ => panic!("Expected integer"),
            })
            .collect::<Vec<_>>();
        assert_eq!(input_sorted, result);
    }
}
#[test]
fn btree() {
    let f = "std/btree_test.asm";
    execute_test_file(f, Default::default(), vec![]).unwrap();
}
