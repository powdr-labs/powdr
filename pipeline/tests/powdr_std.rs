use std::{result, sync::Arc};

use powdr_number::{BigInt, GoldilocksField};

use powdr_pil_analyzer::evaluator;
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
    
    // theta_bc
    let analyzed = std_analyzed::<GoldilocksField>();
    let st: Vec<u64> = vec![0x73657461726b6f7a, 0x1, 0x0, 0x0, 0x0, 
                            0x0, 0x0, 0x0, 0x0, 0x0, 
                            0x0, 0x0, 0x0, 0x0, 0x0,
                            0x0, 0x8000000000000000, 0x0, 0x0, 0x0, 
                            0x0, 0x0, 0x0, 0x0, 0x0];
    let st_input = Arc::new(evaluator::Value::Array(
        st.iter()
            .map(|x| Arc::new(evaluator::Value::Integer(BigInt::from(*x))))
            .collect(),
        ));
    let result: Vec<_> = (0..5).map(
        |i| {
            evaluate_function(
                &analyzed,
                "std::hash::keccak::theta_bc",
                vec![
                        st_input.clone(), 
                        Arc::new(evaluator::Value::Integer(BigInt::from(i)))
                    ],
            )
        }
    ).collect();
    result.iter().for_each(|x| println!("{}", x));

    // theta_st
    let analyzed = std_analyzed::<GoldilocksField>();
    let st: Vec<u64> = vec![0x73657461726b6f7a, 0x1, 0x0, 0x0, 0x0, 
                            0x0, 0x0, 0x0, 0x0, 0x0, 
                            0x0, 0x0, 0x0, 0x0, 0x0,
                            0x0, 0x8000000000000000, 0x0, 0x0, 0x0, 
                            0x0, 0x0, 0x0, 0x0, 0x0];
    let result_theta_st = evaluate_function(
        &analyzed,
        "std::hash::keccak::theta_st",
        vec![Arc::new(evaluator::Value::Array(
            st.iter()
                .map(|x| Arc::new(evaluator::Value::Integer(BigInt::from(*x))))
                .collect(),
        ))],
    );
    println!("theta st: ");
    println!("{}", result_theta_st);
    
    // rho_pi_loop and rho_pi_rearrange
    let analyzed = std_analyzed::<GoldilocksField>();

    let result_rho_pi_loop = evaluate_function(
        &analyzed,
        "std::hash::keccak::rho_pi_loop",
        vec![Arc::new(result_theta_st)],
    );

    let result_rho_pi_rearrange = evaluate_function(
        &analyzed,
        "std::hash::keccak::rho_pi_rearrange",
        vec![Arc::new(result_rho_pi_loop)],
    );
    println!("rho pi rearrange: ");
    println!("{}", result_rho_pi_rearrange);

    // chi
    let analyzed = std_analyzed::<GoldilocksField>();

    let result_chi = evaluate_function(
        &analyzed,
        "std::hash::keccak::chi",
        vec![Arc::new(result_rho_pi_rearrange)],
    );

    println!("chi: ");
    println!("{}", result_chi);

    // iota
    let analyzed = std_analyzed::<GoldilocksField>();

    let result_iota = evaluate_function(
        &analyzed,
        "std::hash::keccak::iota",
        vec![Arc::new(result_chi), 
            Arc::new(evaluator::Value::Integer(BigInt::from(0x0)))],
    );

    println!("iota: ");
    println!("{}", result_iota);

    // r_loop
    let analyzed = std_analyzed::<GoldilocksField>();

    let result_r_loop = evaluate_function(
        &analyzed,
        "std::hash::keccak::r_loop",
        vec![st_input.clone()]
    );

    println!("r loop: ");
    println!("{}", result_r_loop);

    // swap_u64_loop on st input (original endianness as raw input)
    let analyzed = std_analyzed::<GoldilocksField>();

    let result_swap_u64_loop = evaluate_function(
        &analyzed,
        "std::hash::keccak::swap_u64_loop",
        vec![Arc::new(result_r_loop.clone())]
    );

    println!("swap u64 loop: ");
    println!("{}", result_swap_u64_loop);


    // swap_u64_loop on r_loop
    let analyzed = std_analyzed::<GoldilocksField>();

    let result_r_loop_swap = evaluate_function(
        &analyzed,
        "std::hash::keccak::swap_u64_loop",
        vec![Arc::new(result_r_loop.clone())]
    );

    println!("r loop swap: ");
    println!("{}", result_r_loop_swap);

    // keccakf
    let analyzed = std_analyzed::<GoldilocksField>();

    let st_original_endianness: Vec<u64> = vec![0x7a6f6b7261746573, 0x0100000000000000, 0x0, 0x0, 0x0, 
                            0x0, 0x0, 0x0, 0x0, 0x0, 
                            0x0, 0x0, 0x0, 0x0, 0x0,
                            0x0, 0x80, 0x0, 0x0, 0x0, 
                            0x0, 0x0, 0x0, 0x0, 0x0];
    let st_input_original_endianness = Arc::new(evaluator::Value::Array(
        st_original_endianness.iter()
            .map(|x| Arc::new(evaluator::Value::Integer(BigInt::from(*x))))
            .collect(),
        ));

    let result_keccakf = evaluate_function(
        &analyzed,
        "std::hash::keccak::keccakf",
        vec![st_input_original_endianness.clone()],
    );

    println!("keccakf: ");
    println!("{}", result_keccakf);

    // update_finalize_b
    let W = 32;
    let rate = 200 - 2 * W;
    let input = vec![0x7a, 0x6f, 0x6b, 0x72, 0x61, 0x74, 0x65, 0x73];
    let delim = 0x01;
    let result_update_finalize_b = evaluate_function(
        &analyzed,
        "std::hash::keccak::update_finalize_b",
        vec![
            Arc::new(evaluator::Value::Array(
                input
                    .iter()
                    .map(|x| Arc::new(evaluator::Value::Integer(BigInt::from(*x))))
                    .collect(),
            )),
            Arc::new(evaluator::Value::Integer(BigInt::from(rate))),
            Arc::new(evaluator::Value::Integer(BigInt::from(delim))),
        ],
    );
    println!("update_finalize_b: ");
    println!("{}", result_update_finalize_b);

    // let st_original_endianness: Vec<u64> = 
    // vec![0x7a6f6b7261746573, 0x0100000000000000, 0x0, 0x0, 0x0, 
    // 0x0, 0x0, 0x0, 0x0, 0x0, 
    // 0x0, 0x0, 0x0, 0x0, 0x0,
    // 0x0, 0x80, 0x0, 0x0, 0x0, 
    // 0x0, 0x0, 0x0, 0x0, 0x0];

    // from_bytes
    let input = (0..25).map(|x| {
        let mut arr = [0usize; 8];
        arr[x % 8] = x;
        arr
    }).flatten().collect::<Vec<usize>>();

    println!("input len: {}", input.len());
    
    let result_from_bytes = evaluate_function(
        &analyzed,
        "std::hash::keccak::from_bytes",
        vec![
            Arc::new(evaluator::Value::Array(
                input
                    .iter()
                    .map(|x| Arc::new(evaluator::Value::Integer(BigInt::from(*x))))
                    .collect(),
            ))
        ],
    );

    println!("from_bytes: ");
    println!("{}", result_from_bytes);

    // to_bytes
    let result_to_bytes = evaluate_function(
        &analyzed,
        "std::hash::keccak::to_bytes",
        vec![
            Arc::new(result_from_bytes)
        ],
    );

    if let Value::Array(arr) = result_to_bytes.clone() {
        println!("to_bytes len: {}", arr.len());
    }
    
    println!("to_bytes: ");
    println!("{}", result_to_bytes);

    // main
    let W = 32;
    let input = vec![0x7a, 0x6f, 0x6b, 0x72, 0x61, 0x74, 0x65, 0x73];
    let delim = 0x01;
    let result_full = evaluate_function(
        &analyzed,
        "std::hash::keccak::main",
        vec![
            Arc::new(evaluator::Value::Integer(BigInt::from(W))),
            Arc::new(evaluator::Value::Array(
                input
                    .iter()
                    .map(|x| Arc::new(evaluator::Value::Integer(BigInt::from(*x))))
                    .collect(),
            )),
            Arc::new(evaluator::Value::Integer(BigInt::from(delim))),
        ],
    );
    println!("result full: ");
    println!("{}", result_full);

    // finalize b
    // |num_remaining, b_delim_idx, b_keccak, num_loop, rate, delim, input|
    let analyzed = std_analyzed::<GoldilocksField>();

    let W = 32;
    let num_remaining = 8;
    let b_delim_idx = 9;
    let b_keccak = [0; 200];
    let num_loop = 0;
    let rate = 200 - 2 * W;
    let delim = 0x01;
    let input = vec![0x7a, 0x6f, 0x6b, 0x72, 0x61, 0x74, 0x65, 0x73];

    let result_finalize_b = evaluate_function(
        &analyzed,
        "std::hash::keccak::finalize_b",
        vec![
            Arc::new(evaluator::Value::Integer(BigInt::from(num_remaining))),
            Arc::new(evaluator::Value::Integer(BigInt::from(b_delim_idx))),
            Arc::new(evaluator::Value::Array(
                b_keccak
                    .iter()
                    .map(|x| Arc::new(evaluator::Value::Integer(BigInt::from(*x))))
                    .collect(),
            )),
            Arc::new(evaluator::Value::Integer(BigInt::from(num_loop))),
            Arc::new(evaluator::Value::Integer(BigInt::from(rate))),
            Arc::new(evaluator::Value::Integer(BigInt::from(delim))),
            Arc::new(evaluator::Value::Array(
                input
                    .iter()
                    .map(|x| Arc::new(evaluator::Value::Integer(BigInt::from(*x))))
                    .collect(),
            )),
        ],
    );

    println!("result finalize b: ");
    println!("{}", result_finalize_b);


}
