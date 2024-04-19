use std::{sync::Arc};


use powdr_number::{BigInt, GoldilocksField};


use powdr_pipeline::test_util::{
    evaluate_function, evaluate_integer_function, gen_estark_proof, gen_halo2_proof, std_analyzed,
    test_halo2, verify_test_file,
};
use test_log::test;

#[test]
fn poseidon_bn254_test() {
    let f = "std/poseidon_bn254_test.asm";
    test_halo2(f, Default::default());

    // `test_halo2` only does a mock proof in the PR tests.
    // This makes sure we test the whole proof generation for one example
    // file even in the PR tests.
    gen_halo2_proof(f, Default::default());
}

#[test]
fn poseidon_gl_test() {
    let f = "std/poseidon_gl_test.asm";
    verify_test_file(f, Default::default(), vec![]).unwrap();
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
    verify_test_file(f, Default::default(), vec![]).unwrap();
    gen_estark_proof(f, Default::default());
}

#[test]
#[ignore = "Too slow"]
fn arith_test() {
    let f = "std/arith_test.asm";
    verify_test_file(f, Default::default(), vec![]).unwrap();
    gen_estark_proof(f, Default::default());
    test_halo2(f, Default::default());
}

#[test]
fn memory_test() {
    let f = "std/memory_test.asm";
    verify_test_file(f, Default::default(), vec![]).unwrap();
    gen_estark_proof(f, Default::default());
    test_halo2(f, Default::default());
}

#[test]
fn write_once_memory_test() {
    let f = "std/write_once_memory_test.asm";
    verify_test_file(f, Default::default(), vec![]).unwrap();
    gen_estark_proof(f, Default::default());
    test_halo2(f, Default::default());
}

#[test]
fn binary_test() {
    let f = "std/binary_test.asm";
    verify_test_file(f, Default::default(), vec![]).unwrap();
    test_halo2(f, Default::default());
}

#[test]
fn shift_test() {
    let f = "std/shift_test.asm";
    verify_test_file(f, Default::default(), vec![]).unwrap();
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
fn keccakf() {
    use powdr_pil_analyzer::evaluator::Value;

    fn compare_integer_array_evaluations<T>(this: &Value<T>, other: &Value<T>) {
        if let (Value::Array(ref this), Value::Array(ref other)) = (this, other) {
            assert_eq!(this.len(), other.len());
            for (this_elem, other_elem) in this.iter().zip(other.iter()) {
                if let (Value::Integer(ref this_int), Value::Integer(ref other_int)) =
                    (this_elem.as_ref(), other_elem.as_ref())
                {
                    assert_eq!(this_int, other_int);
                } else {
                    panic!("Expected integer.");
                }
            }
        } else {
            panic!("Expected array.");
        }
    }

    fn test_main(input: Vec<i32>, expected: Vec<i32>) {
        let analyzed = std_analyzed::<GoldilocksField>();

        let result = evaluate_function(
            &analyzed,
            "std::hash::keccak::main",
            vec![
                Arc::new(Value::Integer(BigInt::from(32))), // W = 32 (output bytes)
                Arc::new(Value::Array(
                    input
                        .iter()
                        .map(|x| Arc::new(Value::Integer(BigInt::from(*x))))
                        .collect(),
                )),
                Arc::new(Value::Integer(BigInt::from(0x01))), // delim = 0x01
            ],
        );

        compare_integer_array_evaluations(
            &result,
            &Value::Array(
                expected
                    .iter()
                    .map(|x| Arc::new(Value::Integer(BigInt::from(*x))))
                    .collect(),
            ),
        );
    }

    let input = vec![
        // The following three test vectors are from Zokrates
        // https://github.com/Zokrates/ZoKrates/blob/develop/zokrates_stdlib/tests/tests/hashes/keccak/keccak.zok
        vec![0x7a, 0x6f, 0x6b, 0x72, 0x61, 0x74, 0x65, 0x73],
        [0x2a; 135].to_vec(),
        [0x2a; 136].to_vec(),
        // This test vector tests input size greater than compression function (keccakf) output size (200 bytes)
        {
            let mut v = vec![0x2a; 399];
            v.push(0x01);
            v
        },
        // All zero input test vector
        [0x00; 256].to_vec(),
    ];

    let expected = vec![
        // ca85d1976d40dcb6ca3becc8c6596e83c0774f4185cf016a05834f5856a37f39
        [
            0xca, 0x85, 0xd1, 0x97, 0x6d, 0x40, 0xdc, 0xb6, 0xca, 0x3b, 0xec, 0xc8, 0xc6, 0x59,
            0x6e, 0x83, 0xc0, 0x77, 0x4f, 0x41, 0x85, 0xcf, 0x01, 0x6a, 0x05, 0x83, 0x4f, 0x58,
            0x56, 0xa3, 0x7f, 0x39,
        ]
        .to_vec(),
        // 723e2ae02ca8d8fb45dca21e5f6369c4f124da72f217dca5e657a4bbc69b917d
        [
            0x72, 0x3e, 0x2a, 0xe0, 0x2c, 0xa8, 0xd8, 0xfb, 0x45, 0xdc, 0xa2, 0x1e, 0x5f, 0x63,
            0x69, 0xc4, 0xf1, 0x24, 0xda, 0x72, 0xf2, 0x17, 0xdc, 0xa5, 0xe6, 0x57, 0xa4, 0xbb,
            0xc6, 0x9b, 0x91, 0x7d,
        ]
        .to_vec(),
        // e60d5160227cb1b8dc8547deb9c6a2c5e6c3306a1c1a55611a73ed2c2324bfc0
        [
            0xe6, 0x0d, 0x51, 0x60, 0x22, 0x7c, 0xb1, 0xb8, 0xdc, 0x85, 0x47, 0xde, 0xb9, 0xc6,
            0xa2, 0xc5, 0xe6, 0xc3, 0x30, 0x6a, 0x1c, 0xa1, 0x55, 0x61, 0x1a, 0x73, 0xed, 0x2c,
            0x23, 0x24, 0xbf, 0xc0,
        ]
        .to_vec(),
        // cf54f48e5701fed7b85fa015ff3def02604863f68c585fcf6af54a86d42e1046
        [
            0xcf, 0x54, 0xf4, 0x8e, 0x57, 0x01, 0xfe, 0xd7, 0xb8, 0x5f, 0xa0, 0x15, 0xff, 0x3d,
            0xef, 0x02, 0x60, 0x48, 0x63, 0xf6, 0x8c, 0x58, 0x5f, 0xcf, 0x6a, 0xf5, 0x4a, 0x86,
            0xd4, 0x2e, 0x10, 0x46,
        ]
        .to_vec(),
        // d397b3b043d87fcd6fad1291ff0bfd16401c274896d8c63a923727f077b8e0b5
        [
            0xd3, 0x97, 0xb3, 0xb0, 0x43, 0xd8, 0x7f, 0xcd, 0x6f, 0xad, 0x12, 0x91, 0xff, 0x0b,
            0xfd, 0x16, 0x40, 0x1c, 0x27, 0x48, 0x96, 0xd8, 0xc6, 0x3a, 0x92, 0x37, 0x27, 0xf0,
            0x77, 0xb8, 0xe0, 0xb5,
        ]
        .to_vec(),
    ];

    input
        .into_iter()
        .zip(expected.into_iter())
        .for_each(|(input, expected)| {
            test_main(input, expected);
        });
}
