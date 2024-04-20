use powdr_backend::BackendType;
use powdr_number::{Bn254Field, FieldElement, GoldilocksField};
use powdr_pipeline::{
    test_util::{gen_estark_proof, resolve_test_file, test_halo2, test_plonky3, verify_test_file},
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
    test_plonky3(f, slice_to_vec(&i));
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
fn mem_read_write() {
    let f = "asm/mem_read_write.asm";
    verify_asm(f, Default::default());
    test_halo2(f, Default::default());
    gen_estark_proof(f, Default::default());
}

#[test]
fn mem_read_write_no_memory_accesses() {
    let f = "asm/mem_read_write_no_memory_accesses.asm";
    verify_asm(f, Default::default());
    test_halo2(f, Default::default());
    gen_estark_proof(f, Default::default());
}

#[test]
fn mem_read_write_with_bootloader() {
    let f = "asm/mem_read_write_with_bootloader.asm";
    verify_asm(f, Default::default());
    test_halo2(f, Default::default());
    gen_estark_proof(f, Default::default());
}

#[test]
fn mem_read_write_large_diffs() {
    let f = "asm/mem_read_write_large_diffs.asm";
    verify_asm(f, Default::default());
    test_halo2(f, Default::default());
    gen_estark_proof(f, Default::default());
}

#[test]
fn multi_assign() {
    let f = "asm/multi_assign.asm";
    let i = [7];
    verify_asm(f, slice_to_vec(&i));
    test_halo2(f, slice_to_vec(&i));
    gen_estark_proof(f, slice_to_vec(&i));
}

#[test]
fn multi_return() {
    let f = "asm/multi_return.asm";
    let i = [];
    verify_asm(f, slice_to_vec(&i));
    test_halo2(f, slice_to_vec(&i));
    gen_estark_proof(f, Default::default());
}

#[test]
#[should_panic = "called `Result::unwrap()` on an `Err` value: [\"Assignment register `Z` is incompatible with `square_and_double(3)`. Try using `<==` with no explicit assignment registers.\", \"Assignment register `Y` is incompatible with `square_and_double(3)`. Try using `<==` with no explicit assignment registers.\"]"]
fn multi_return_wrong_assignment_registers() {
    let f = "asm/multi_return_wrong_assignment_registers.asm";
    let i = [];
    verify_asm(f, slice_to_vec(&i));
}

#[test]
#[should_panic = "Result::unwrap()` on an `Err` value: [\"Mismatched number of registers for assignment A, B <=Y= square_and_double(3);\"]"]
fn multi_return_wrong_assignment_register_length() {
    let f = "asm/multi_return_wrong_assignment_register_length.asm";
    let i = [];
    verify_asm(f, slice_to_vec(&i));
}

#[test]
fn bit_access() {
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
fn sqrt() {
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
            .with_output(tmp_dir.to_path_buf(), true)
            .with_backend(BackendType::EStarkDump);
        pipeline.compute_witness().unwrap();
        let pil = pipeline.compute_optimized_pil().unwrap();
        pipeline.compute_proof().unwrap();

        // check fixed cols (may have no fixed cols)
        if let Some((fixed, degree)) = try_read_poly_set::<FixedPolySet, _>(&pil, tmp_dir.as_path())
        {
            assert_eq!(pil.degree(), degree);
            assert_eq!(pil.degree(), fixed[0].1.len() as u64);
        }

        // check witness cols (examples assumed to have at least one witness col)
        let (witness, degree) =
            try_read_poly_set::<WitnessPolySet, _>(&pil, tmp_dir.as_path()).unwrap();
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
fn failing_assertion() {
    let f = "asm/failing_assertion.asm";
    let i = [];
    verify_asm(f, slice_to_vec(&i));
}
