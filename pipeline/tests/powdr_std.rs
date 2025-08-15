use std::sync::Arc;

use powdr_linker::LinkerMode;
use powdr_number::{BabyBearField, BigInt, Bn254Field, GoldilocksField};

use powdr_pil_analyzer::evaluator::Value;
use powdr_pipeline::{
    test_runner::run_tests,
    test_util::{
        evaluate_function, evaluate_integer_function, make_prepared_pipeline,
        make_simple_prepared_pipeline, regular_test_bb, regular_test_gl, regular_test_small_field,
        std_analyzed, test_mock_backend, test_plonky3_pipeline,
    },
    Pipeline,
};
use test_log::test;

#[test]
#[ignore = "Too slow"]
fn fingerprint_test() {
    let f = "std/fingerprint_test.asm";
    let pipeline = make_simple_prepared_pipeline::<GoldilocksField>(f, LinkerMode::Bus);
    test_plonky3_pipeline(pipeline);
}

#[test]
fn poseidon_gl_test() {
    let f = "std/poseidon_gl_test.asm";
    regular_test_gl(f, &[]);
}

#[test]
#[ignore = "Too slow"]
fn poseidon_gl_memory_test() {
    let f = "std/poseidon_gl_memory_test.asm";
    regular_test_gl(f, &[]);
}

#[test]
#[ignore = "Too slow"]
fn keccakf16_test() {
    let f = "std/keccakf16_test.asm";
    regular_test_small_field(f, &[]);
}

#[test]
#[ignore = "Too slow"]
fn keccakf16_memory_test() {
    let f = "std/keccakf16_memory_test.asm";
    regular_test_small_field(f, &[]);
}

#[test]
#[ignore = "Too slow"]
fn keccakf32_memory_test() {
    let f = "std/keccakf32_memory_test.asm";
    regular_test_gl(f, &[]);
}

#[test]
#[ignore = "Too slow"]
fn poseidon_bb_test() {
    let f = "std/poseidon_bb_test.asm";
    regular_test_bb(f, &[]);
}

#[test]
#[ignore = "Too slow"]
fn poseidon2_bb_test() {
    let f = "std/poseidon2_bb_test.asm";
    regular_test_bb(f, &[]);
}

#[test]
#[ignore = "Too slow"]
fn poseidon2_gl_test() {
    let f = "std/poseidon2_gl_test.asm";
    regular_test_gl(f, &[]);
}

#[test]
#[ignore = "Too slow"]
fn split_gl_vec_test() {
    let f = "std/split_gl_vec_test.asm";
    regular_test_gl(f, &[]);
}

#[test]
#[ignore = "Too slow"]
fn split_gl_test() {
    let f = "std/split_gl_test.asm";
    regular_test_gl(f, &[]);
}

#[test]
fn split_bb_test() {
    let f = "std/split_bb_test.asm";
    regular_test_bb(f, &[]);
}

#[test]
#[ignore = "Too slow"]
fn add_sub_small_test() {
    let f = "std/add_sub_small_test.asm";
    regular_test_small_field(f, &[]);
}

#[test]
#[ignore = "Too slow"]
fn arith_small_test() {
    let f = "std/arith_small_test.asm";
    regular_test_small_field(f, &[]);
}

#[test]
#[ignore = "Too slow"]
fn arith_large_test() {
    // We just run this test for the mock backend in bus linker mode,
    // because witgen and proving should be similar to the other backends.
    let f = "std/arith_large_test.asm";
    test_mock_backend(make_prepared_pipeline::<GoldilocksField>(
        f,
        vec![],
        vec![],
        LinkerMode::Bus,
    ));
}

#[test]
#[ignore = "Too slow"]
fn arith256_small_test() {
    let f = "std/arith256_small_test.asm";
    regular_test_small_field(f, &[]);
}

#[test]
#[ignore = "Too slow"]
fn arith256_memory_large_test() {
    // We just run this test for the mock backend in bus linker mode,
    // because witgen and proving should be similar to the other backends.
    let f = "std/arith256_memory_large_test.asm";
    test_mock_backend(make_prepared_pipeline::<GoldilocksField>(
        f,
        vec![],
        vec![],
        LinkerMode::Bus,
    ));
}

#[test]
#[ignore = "Too slow"]
fn memory_large_test() {
    let f = "std/memory_large_test.asm";
    regular_test_gl(f, &[]);
}

#[test]
fn memory_large_with_bootloader_write_test() {
    let f = "std/memory_large_with_bootloader_write_test.asm";
    regular_test_gl(f, &[]);
}

#[test]
fn memory_large_test_parallel_accesses() {
    let f = "std/memory_large_test_parallel_accesses.asm";
    regular_test_gl(f, &[]);
}

#[test]
#[ignore = "Too slow"]
fn memory_small_test() {
    let f = "std/memory_small_test.asm";
    regular_test_small_field(f, &[]);
}

#[test]
fn permutation_via_challenges() {
    let f = "std/permutation_via_challenges.asm";
    let pipeline = make_simple_prepared_pipeline::<GoldilocksField>(f, LinkerMode::Bus);
    test_mock_backend(pipeline.clone());
    test_plonky3_pipeline(pipeline);

    let pipeline = make_simple_prepared_pipeline::<BabyBearField>(f, LinkerMode::Bus);
    test_plonky3_pipeline(pipeline);
}

#[test]
fn lookup_via_challenges() {
    let f = "std/lookup_via_challenges.asm";
    let pipeline = make_simple_prepared_pipeline::<GoldilocksField>(f, LinkerMode::Bus);
    test_mock_backend(pipeline.clone());
    test_plonky3_pipeline(pipeline);

    let pipeline = make_simple_prepared_pipeline::<BabyBearField>(f, LinkerMode::Bus);
    test_plonky3_pipeline(pipeline);
}

#[test]
fn lookup_via_challenges_range_constraint() {
    let f = "std/lookup_via_challenges_range_constraint.asm";
    let pipeline = make_simple_prepared_pipeline::<GoldilocksField>(f, LinkerMode::Bus);
    test_mock_backend(pipeline.clone());
    test_plonky3_pipeline(pipeline);
}

#[test]
fn bus_lookup() {
    let f = "std/bus_lookup.asm";
    let pipeline = make_simple_prepared_pipeline::<GoldilocksField>(f, LinkerMode::Bus);
    test_mock_backend(pipeline.clone());
    test_plonky3_pipeline(pipeline);
}

#[test]
fn bus_multi_lookup() {
    let f = "std/bus_multi_lookup.asm";
    let pipeline = make_simple_prepared_pipeline::<GoldilocksField>(f, LinkerMode::Bus);
    test_mock_backend(pipeline.clone());
    test_plonky3_pipeline(pipeline);
}

#[test]
fn bus_permutation() {
    let f = "std/bus_permutation.asm";
    let pipeline = make_simple_prepared_pipeline::<GoldilocksField>(f, LinkerMode::Bus);
    test_mock_backend(pipeline.clone());
    test_plonky3_pipeline(pipeline);
}

#[test]
#[ignore = "Too slow"]
fn bus_multi_permutation() {
    let f = "std/bus_multi_permutation.asm";
    let pipeline = make_simple_prepared_pipeline::<GoldilocksField>(f, LinkerMode::Bus);
    test_mock_backend(pipeline.clone());
    test_plonky3_pipeline(pipeline);
}

#[test]
fn bus_multi_linker() {
    let f = "std/bus_multi_linker.asm";
    let pipeline = make_simple_prepared_pipeline::<GoldilocksField>(f, LinkerMode::Bus);
    test_mock_backend(pipeline.clone());
    test_plonky3_pipeline(pipeline);
}

#[test]
fn write_once_memory_test() {
    let f = "std/write_once_memory_test.asm";
    regular_test_gl(f, &[]);
}

#[test]
#[ignore = "Too slow"]
fn binary_large_test() {
    let f = "std/binary_large_test.asm";
    regular_test_gl(f, &[]);
}

#[test]
#[ignore = "Too slow"]
fn binary_small_8_test() {
    let f = "std/binary_small_8_test.asm";
    regular_test_small_field(f, &[]);
}

#[test]
#[ignore = "Too slow"]
fn binary_small_test() {
    let f = "std/binary_small_test.asm";
    regular_test_small_field(f, &[]);
}

#[test]
#[ignore = "Too slow"]
fn shift_large_test() {
    let f = "std/shift_large_test.asm";
    regular_test_gl(f, &[]);
}

#[test]
#[ignore = "Too slow"]
fn shift_small_test() {
    let f = "std/shift_small_test.asm";
    regular_test_small_field(f, &[]);
}

#[test]
#[ignore = "Too slow"]
fn rotate_large_test() {
    let f = "std/rotate_large_test.asm";
    regular_test_gl(f, &[]);
}

#[test]
#[ignore = "Too slow"]
fn rotate_small_test() {
    let f = "std/rotate_small_test.asm";
    regular_test_small_field(f, &[]);
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
fn std_tests() {
    let count1 = run_tests(&std_analyzed::<GoldilocksField>(), true).unwrap();
    let count2 = run_tests(&std_analyzed::<Bn254Field>(), true).unwrap();
    let count3 = run_tests(&std_analyzed::<BabyBearField>(), true).unwrap();
    assert_eq!(count1, count2);
    assert_eq!(count2, count3);
    assert!(count1 >= 9);
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
        "let test_sort: int[] -> int[] = |x| std::array::sort(x, |a, b| a < b); machine Main with degree: 1024 { }"
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

mod reparse {

    use powdr_pipeline::test_util::run_reparse_test_with_blacklist;
    use test_log::test;

    /// For convenience, all re-parsing tests run with the Goldilocks field,
    /// but these tests panic if the field is too small. This is *probably*
    /// fine, because all of these tests have a similar variant that does
    /// run on Goldilocks.
    const BLACKLIST: [&str; 2] = ["std/poseidon_bn254_test.asm", "std/split_bn254_test.asm"];

    fn run_reparse_test(file: &str) {
        run_reparse_test_with_blacklist(file, &BLACKLIST);
    }
    include!(concat!(env!("OUT_DIR"), "/std_reparse_tests.rs"));
}
