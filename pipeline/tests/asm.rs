use powdr_number::{Bn254Field, FieldElement, GoldilocksField};
use powdr_pipeline::{
    test_util::{gen_estark_proof, resolve_test_file, test_halo2, verify_test_file},
    util::{try_read_poly_set, FixedPolySet, WitnessPolySet},
    Pipeline,
};
use test_log::test;

fn verify_asm(file_name: &str, inputs: Vec<GoldilocksField>) {
    verify_test_file(file_name, inputs, vec![]).unwrap();
}

fn slice_to_vec<T: FieldElement>(arr: &[i32]) -> Vec<T> {
    arr.iter().cloned().map(|x| x.into()).collect()
}

#[test]
fn simple_sum_asm() {
    let f = "asm/simple_sum.asm";
    let i = [16, 4, 1, 2, 8, 5];
    verify_asm(f, slice_to_vec(&i));
    test_halo2(f, slice_to_vec(&i));
    gen_estark_proof(f, slice_to_vec(&i));
}

#[test]
fn secondary_block_machine_add2() {
    let f = "asm/secondary_block_machine_add2.asm";
    verify_asm(f, Default::default());
    test_halo2(f, Default::default());
    gen_estark_proof(f, Default::default());
}

#[test]
fn mem_write_once() {
    let f = "asm/mem_write_once.asm";
    verify_asm(f, Default::default());
    test_halo2(f, Default::default());
    gen_estark_proof(f, Default::default());
}

#[test]
fn mem_write_once_external_write() {
    let f = "asm/mem_write_once_external_write.asm";
    let mut mem = vec![GoldilocksField::from(0); 256];
    mem[17] = GoldilocksField::from(42);
    mem[62] = GoldilocksField::from(123);
    mem[255] = GoldilocksField::from(-1);
    verify_test_file(
        f,
        Default::default(),
        vec![("main_memory.value".to_string(), mem)],
    )
    .unwrap();
}

#[test]
fn block_machine_cache_miss() {
    let f = "asm/block_machine_cache_miss.asm";
    verify_asm(f, Default::default());
    test_halo2(f, Default::default());
    gen_estark_proof(f, Default::default());
}

#[test]
fn palindrome() {
    let f = "asm/palindrome.asm";
    let i = [7, 1, 7, 3, 9, 3, 7, 1];
    verify_asm(f, slice_to_vec(&i));
    test_halo2(f, slice_to_vec(&i));
    // currently starky leads to
    // thread 'functional_instructions' has overflowed its stack
    // leave it out until that's fixed
    //gen_estark_proof(f, slice_to_vec(&i));
}

#[test]
fn single_function_vm() {
    let f = "asm/single_function_vm.asm";
    let i = [];
    verify_asm(f, slice_to_vec(&i));
    test_halo2(f, slice_to_vec(&i));
    gen_estark_proof(f, slice_to_vec(&i));
}

#[test]
fn empty() {
    let f = "asm/empty.asm";
    let i = [];
    verify_asm(f, slice_to_vec(&i));
    test_halo2(f, slice_to_vec(&i));
    gen_estark_proof(f, slice_to_vec(&i));
}

#[test]
fn single_operation() {
    let f = "asm/single_operation.asm";
    let i = [];
    verify_asm(f, slice_to_vec(&i));
    test_halo2(f, slice_to_vec(&i));
    gen_estark_proof(f, slice_to_vec(&i));
}

#[test]
fn empty_vm() {
    let f = "asm/empty_vm.asm";
    let i = [];
    verify_asm(f, slice_to_vec(&i));
    test_halo2(f, slice_to_vec(&i));
    gen_estark_proof(f, slice_to_vec(&i));
}

#[test]
fn vm_to_block_unique_interface() {
    let f = "asm/vm_to_block_unique_interface.asm";
    let i = [];
    verify_asm(f, slice_to_vec(&i));
    test_halo2(f, slice_to_vec(&i));
    // currently starky leads to
    // thread 'functional_instructions' has overflowed its stack
    // leave it out until that's fixed
    //gen_estark_proof(f, slice_to_vec(&i));
}

#[test]
fn vm_to_block_to_block() {
    let f = "asm/vm_to_block_to_block.asm";
    let i = [];
    verify_asm(f, slice_to_vec(&i));
    test_halo2(f, slice_to_vec(&i));
}

#[test]
fn block_to_block() {
    let f = "asm/block_to_block.asm";
    let i = [];
    verify_asm(f, slice_to_vec(&i));
    test_halo2(f, slice_to_vec(&i));
    gen_estark_proof(f, slice_to_vec(&i));
}

#[test]
fn vm_instr_param_mapping() {
    let f = "asm/vm_instr_param_mapping.asm";
    let i = [];
    verify_asm(f, slice_to_vec(&i));
    test_halo2(f, slice_to_vec(&i));
    gen_estark_proof(f, slice_to_vec(&i));
}

#[test]
fn vm_to_block_multiple_interfaces() {
    let f = "asm/vm_to_block_multiple_interfaces.asm";
    let i = [];
    verify_asm(f, slice_to_vec(&i));
    test_halo2(f, slice_to_vec(&i));
    gen_estark_proof(f, slice_to_vec(&i));
}

#[test]
fn vm_to_vm() {
    let f = "asm/vm_to_vm.asm";
    let i = [];
    verify_asm(f, slice_to_vec(&i));
    test_halo2(f, slice_to_vec(&i));
    gen_estark_proof(f, slice_to_vec(&i));
}

#[test]
fn vm_to_vm_dynamic_trace_length() {
    let f = "asm/vm_to_vm_dynamic_trace_length.asm";
    let i = [];
    verify_asm(f, slice_to_vec(&i));
    test_halo2(f, slice_to_vec(&i));
    gen_estark_proof(f, slice_to_vec(&i));
}

#[test]
fn vm_to_vm_to_block() {
    let f = "asm/vm_to_vm_to_block.asm";
    let i = [];
    verify_asm(f, slice_to_vec(&i));
    test_halo2(f, slice_to_vec(&i));
    gen_estark_proof(f, slice_to_vec(&i));
}

#[test]
fn vm_to_block_array() {
    let f = "asm/vm_to_block_array.asm";
    let i = [];
    verify_asm(f, slice_to_vec(&i));
    test_halo2(f, slice_to_vec(&i));
    gen_estark_proof(f, slice_to_vec(&i));
}

#[test]
fn vm_to_vm_to_vm() {
    let f = "asm/vm_to_vm_to_vm.asm";
    let i = [];
    verify_asm(f, slice_to_vec(&i));
    test_halo2(f, slice_to_vec(&i));
    gen_estark_proof(f, slice_to_vec(&i));
}

#[test]
fn test_mem_read_write() {
    let f = "asm/mem_read_write.asm";
    verify_asm(f, Default::default());
    test_halo2(f, Default::default());
    gen_estark_proof(f, Default::default());
}

#[test]
fn test_mem_read_write_no_memory_accesses() {
    let f = "asm/mem_read_write_no_memory_accesses.asm";
    verify_asm(f, Default::default());
    test_halo2(f, Default::default());
    gen_estark_proof(f, Default::default());
}

#[test]
fn test_mem_read_write_with_bootloader() {
    let f = "asm/mem_read_write_with_bootloader.asm";
    verify_asm(f, Default::default());
    test_halo2(f, Default::default());
    gen_estark_proof(f, Default::default());
}

#[test]
fn test_mem_read_write_large_diffs() {
    let f = "asm/mem_read_write_large_diffs.asm";
    verify_asm(f, Default::default());
    test_halo2(f, Default::default());
    gen_estark_proof(f, Default::default());
}

#[test]
fn test_multi_assign() {
    let f = "asm/multi_assign.asm";
    let i = [7];
    verify_asm(f, slice_to_vec(&i));
    test_halo2(f, slice_to_vec(&i));
    gen_estark_proof(f, slice_to_vec(&i));
}

#[test]
fn test_multi_return() {
    let f = "asm/multi_return.asm";
    let i = [];
    verify_asm(f, slice_to_vec(&i));
    test_halo2(f, slice_to_vec(&i));
    gen_estark_proof(f, Default::default());
}

#[test]
#[should_panic = "called `Result::unwrap()` on an `Err` value: [\"Assignment register `Z` is incompatible with `square_and_double(3)`. Try using `<==` with no explicit assignment registers.\", \"Assignment register `Y` is incompatible with `square_and_double(3)`. Try using `<==` with no explicit assignment registers.\"]"]
fn test_multi_return_wrong_assignment_registers() {
    let f = "asm/multi_return_wrong_assignment_registers.asm";
    let i = [];
    verify_asm(f, slice_to_vec(&i));
}

#[test]
#[should_panic = "Result::unwrap()` on an `Err` value: [\"Mismatched number of registers for assignment A, B <=Y= square_and_double(3);\"]"]
fn test_multi_return_wrong_assignment_register_length() {
    let f = "asm/multi_return_wrong_assignment_register_length.asm";
    let i = [];
    verify_asm(f, slice_to_vec(&i));
}

#[test]
fn test_bit_access() {
    let f = "asm/bit_access.asm";
    let i = [20];
    verify_asm(f, slice_to_vec(&i));
    test_halo2(f, slice_to_vec(&i));
    // currently starky leads to
    // thread 'functional_instructions' has overflowed its stack
    // leave it out until that's fixed
    //gen_estark_proof(f, slice_to_vec(&i));
}

#[test]
fn test_sqrt() {
    let f = "asm/sqrt.asm";
    verify_asm(f, Default::default());
    test_halo2(f, Default::default());
    gen_estark_proof(f, Default::default());
}

#[test]
fn functional_instructions() {
    let f = "asm/functional_instructions.asm";
    let i = [20];
    verify_asm(f, slice_to_vec(&i));
    test_halo2(f, slice_to_vec(&i));
    // currently starky leads to
    // thread 'functional_instructions' has overflowed its stack
    // leave it out until that's fixed
    //gen_estark_proof(f, slice_to_vec(&i));
}

#[test]
fn full_pil_constant() {
    let f = "asm/full_pil_constant.asm";
    verify_asm(f, Default::default());
    test_halo2(f, Default::default());
    gen_estark_proof(f, Default::default());
}

#[test]
fn intermediate() {
    let f = "asm/intermediate.asm";
    verify_asm(f, Default::default());
    test_halo2(f, Default::default());
    gen_estark_proof(f, Default::default());
}

#[test]
fn intermediate_nested() {
    let f = "asm/intermediate_nested.asm";
    verify_asm(f, Default::default());
    test_halo2(f, Default::default());
    gen_estark_proof(f, Default::default());
}

#[test]
fn pil_at_module_level() {
    let f = "asm/pil_at_module_level.asm";
    verify_asm(f, Default::default());
    test_halo2(f, Default::default());
    gen_estark_proof(f, Default::default());
}

#[test]
fn read_poly_files() {
    let asm_files = ["asm/vm_to_block_unique_interface.asm", "asm/empty.asm"];
    for f in asm_files {
        let tmp_dir = mktemp::Temp::new_dir().unwrap();

        // generate poly files
        let mut pipeline = Pipeline::<Bn254Field>::default()
            .from_file(resolve_test_file(f))
            .with_output(tmp_dir.to_path_buf(), true);
        pipeline.compute_witness().unwrap();
        let name = pipeline.name().to_string();
        let pil = pipeline.compute_optimized_pil().unwrap();

        // check fixed cols (may have no fixed cols)
        if let Some((fixed, degree)) =
            try_read_poly_set::<FixedPolySet, _>(&pil, tmp_dir.as_path(), &name)
        {
            assert_eq!(pil.degree(), degree);
            assert_eq!(pil.degree(), fixed[0].1.len() as u64);
        }

        // check witness cols (examples assumed to have at least one witness col)
        let (witness, degree) =
            try_read_poly_set::<WitnessPolySet, _>(&pil, tmp_dir.as_path(), &name).unwrap();
        assert_eq!(pil.degree(), degree);
        assert_eq!(pil.degree(), witness[0].1.len() as u64);
    }
}

#[test]
fn enum_in_asm() {
    let f = "asm/enum_in_asm.asm";
    verify_asm(f, Default::default());
    test_halo2(f, Default::default());
    gen_estark_proof(f, Default::default());
}

#[test]
fn permutation_simple() {
    let f = "asm/permutations/simple.asm";
    verify_asm(f, Default::default());
    test_halo2(f, Default::default());
    gen_estark_proof(f, Default::default());
}

#[test]
fn permutation_to_block() {
    let f = "asm/permutations/vm_to_block.asm";
    verify_asm(f, Default::default());
    test_halo2(f, Default::default());
    gen_estark_proof(f, Default::default());
}

#[test]
#[should_panic = "Witness generation failed"]
fn permutation_to_vm() {
    // TODO: witgen issue
    let f = "asm/permutations/vm_to_vm.asm";
    verify_asm(f, Default::default());
    test_halo2(f, Default::default());
    gen_estark_proof(f, Default::default());
}

#[test]
#[should_panic = "Witness generation failed"]
fn permutation_to_block_to_block() {
    // TODO: witgen issue
    let f = "asm/permutations/block_to_block.asm";
    verify_asm(f, Default::default());
    test_halo2(f, Default::default());
    gen_estark_proof(f, Default::default());
}

#[test]
#[should_panic = "has incoming permutations but doesn't declare call_selectors"]
fn permutation_incoming_needs_selector() {
    let f = "asm/permutations/incoming_needs_selector.asm";
    verify_asm(f, Default::default());
    test_halo2(f, Default::default());
    gen_estark_proof(f, Default::default());
}

#[test]
fn call_selectors_with_no_permutation() {
    let f = "asm/permutations/call_selectors_with_no_permutation.asm";
    verify_asm(f, Default::default());
    test_halo2(f, Default::default());
    gen_estark_proof(f, Default::default());
}

mod book {
    use super::*;
    use test_log::test;

    fn run_book_test(file: &str) {
        // passing 0 to all tests currently works as they either take no prover input or 0 works
        let i = [0];

        verify_asm(file, slice_to_vec(&i));
        test_halo2(file, slice_to_vec(&i));
        gen_estark_proof(file, slice_to_vec(&i));
    }

    include!(concat!(env!("OUT_DIR"), "/asm_book_tests.rs"));
}

#[test]
#[should_panic = "Witness generation failed."]
fn hello_world_asm_fail() {
    let f = "asm/book/hello_world.asm";
    let i = [1];
    verify_asm(f, slice_to_vec(&i));
}

#[test]
#[should_panic = "FailedAssertion(\"This should fail.\")"]
fn test_failing_assertion() {
    let f = "asm/failing_assertion.asm";
    let i = [];
    verify_asm(f, slice_to_vec(&i));
}

#[test]
fn keccak() {
    use powdr_ast::analyzed::Analyzed;
    use powdr_number::BigInt;
    use powdr_pil_analyzer::evaluator::Value;
    use powdr_pipeline::test_util::evaluate_function;
    use std::{fs, sync::Arc};

    // Set up the file to test
    let code_path = String::from(format!(
        "{}/../test_data/asm/keccak.asm",
        env!("CARGO_MANIFEST_DIR"),
    ));
    let code = fs::read_to_string(code_path).unwrap();
    let mut pipeline = Pipeline::<GoldilocksField>::default().from_asm_string(code, None);
    let analyzed = pipeline.compute_analyzed_pil().unwrap().clone();

    // Helper
    fn array_argument<T>(values: Vec<u64>) -> Value<'static, T> {
        Value::Array(
            values
                .iter()
                .map(|x| Arc::new(Value::Integer(BigInt::from(*x))))
                .collect(),
        )
    }

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

    // Test keccakf_inner
    let padded_endianness_swapped_input: Vec<u64> = vec![
        0x73657461726b6f7a,
        0x1,
        0x0,
        0x0,
        0x0,
        0x0,
        0x0,
        0x0,
        0x0,
        0x0,
        0x0,
        0x0,
        0x0,
        0x0,
        0x0,
        0x0,
        0x8000000000000000,
        0x0,
        0x0,
        0x0,
        0x0,
        0x0,
        0x0,
        0x0,
        0x0,
    ];

    let keccakf_inner_result = evaluate_function(
        &analyzed,
        "keccakf_inner",
        vec![Arc::new(array_argument(padded_endianness_swapped_input))],
    );

    let keccakf_inner_expected: Vec<u64> = vec![
        0xb6dc406d97d185ca,
        0x836e59c6c8ec3bca,
        0x6a01cf85414f77c0,
        0x397fa356584f8305,
        0xc8ad140a950d0cba,
        0x3dacc584b36c843f,
        0x428a466fad758146,
        0xa68af9b0cfafaf4c,
        0xffbba567083af2a9,
        0xb880d631598051a4,
        0x683f441da93e7295,
        0xbb5b42d2641b17c6,
        0xf4ec07dc2064443c,
        0x21959efb97953f8b,
        0x31c5e9b638335876,
        0xaa95e01d2bf963ed,
        0x82117b4b8decd828,
        0xe2a255871d47f57f,
        0x659b271c81bf6d3b,
        0x680b15e3d98b97ee,
        0x58118ac68850970d,
        0xada41f9e251307e6,
        0xf9a0529a1f229355,
        0x17cf3d9d8026e97f,
        0xdf84d5da988117d2,
    ];

    compare_integer_array_evaluations(&keccakf_inner_result, &array_argument(keccakf_inner_expected));

    // Test keccakf
    let padded_input: Vec<u64> = vec![
        0x7a6f6b7261746573,
        0x0100000000000000,
        0x0,
        0x0,
        0x0,
        0x0,
        0x0,
        0x0,
        0x0,
        0x0,
        0x0,
        0x0,
        0x0,
        0x0,
        0x0,
        0x0,
        0x80,
        0x0,
        0x0,
        0x0,
        0x0,
        0x0,
        0x0,
        0x0,
        0x0,
    ];

    let keccakf_result = evaluate_function(
        &analyzed,
        "keccakf",
        vec![Arc::new(array_argument(padded_input))],
    );

    let keccakf_expected: Vec<u64> = vec![
        0xca85d1976d40dcb6,
        0xca3becc8c6596e83,
        0xc0774f4185cf016a,
        0x5834f5856a37f39,
        0xba0c0d950a14adc8,
        0x3f846cb384c5ac3d,
        0x468175ad6f468a42,
        0x4cafafcfb0f98aa6,
        0xa9f23a0867a5bbff,
        0xa451805931d680b8,
        0x95723ea91d443f68,
        0xc6171b64d2425bbb,
        0x3c446420dc07ecf4,
        0x8b3f9597fb9e9521,
        0x76583338b6e9c531,
        0xed63f92b1de095aa,
        0x28d8ec8d4b7b1182,
        0x7ff5471d8755a2e2,
        0x3b6dbf811c279b65,
        0xee978bd9e3150b68,
        0xd975088c68a1158,
        0xe60713259e1fa4ad,
        0x5593221f9a52a0f9,
        0x7fe926809d3dcf17,
        0xd2178198dad584df,
    ];

    compare_integer_array_evaluations(&keccakf_result, &array_argument(keccakf_expected));

    // Helper for testing full keccak
    fn test_main(analyzed: &Analyzed<GoldilocksField>, input: Vec<i32>, expected: Vec<i32>) {
        let result = evaluate_function(
            &analyzed,
            "main",
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
            &array_argument(expected.iter().map(|x| *x as u64).collect()),
        );
    }

    // Test full keccak
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
            test_main(&analyzed, input, expected);
        });
}
