use compiler::verify_asm_string;
use number::{Bn254Field, FieldElement, GoldilocksField};
use std::fs;
use test_log::test;

fn verify_asm<T: FieldElement>(file_name: &str, inputs: Vec<T>) {
    let file_name = format!(
        "{}/../test_data/asm/{file_name}",
        env!("CARGO_MANIFEST_DIR")
    );

    let contents = fs::read_to_string(&file_name).unwrap();

    verify_asm_string(&file_name, &contents, inputs)
}

fn gen_estark_proof(file_name: &str, inputs: Vec<GoldilocksField>) {
    compiler::compile_pil_or_asm(
        format!(
            "{}/../test_data/asm/{file_name}",
            env!("CARGO_MANIFEST_DIR")
        )
        .as_str(),
        inputs,
        &mktemp::Temp::new_dir().unwrap(),
        true,
        Some(backend::BackendType::EStark),
        vec![],
        None,
    )
    .unwrap();
}

#[cfg(feature = "halo2")]
fn gen_halo2_proof(file_name: &str, inputs: Vec<Bn254Field>) {
    compiler::compile_pil_or_asm(
        format!(
            "{}/../test_data/asm/{file_name}",
            env!("CARGO_MANIFEST_DIR")
        )
        .as_str(),
        inputs,
        &mktemp::Temp::new_dir().unwrap(),
        true,
        Some(backend::BackendType::Halo2),
        vec![],
        None,
    )
    .unwrap();
}

#[cfg(not(feature = "halo2"))]
fn gen_halo2_proof(_file_name: &str, _inputs: Vec<Bn254Field>) {}

fn slice_to_vec<T: FieldElement>(arr: &[i32]) -> Vec<T> {
    arr.iter().cloned().map(|x| x.into()).collect()
}

#[test]
fn simple_sum_asm() {
    let f = "simple_sum.asm";
    let i = [16, 4, 1, 2, 8, 5];
    verify_asm::<GoldilocksField>(f, slice_to_vec(&i));
    gen_halo2_proof(f, slice_to_vec(&i));
    gen_estark_proof(f, slice_to_vec(&i));
}

#[test]
fn secondary_block_machine_add2() {
    let f = "secondary_block_machine_add2.asm";
    verify_asm::<GoldilocksField>(f, vec![]);
    gen_halo2_proof(f, vec![]);
    gen_estark_proof(f, vec![]);
}

#[test]
fn block_machine_cache_miss() {
    let f = "block_machine_cache_miss.asm";
    verify_asm::<GoldilocksField>(f, vec![]);
    gen_halo2_proof(f, vec![]);
    gen_estark_proof(f, vec![]);
}

#[test]
fn palindrome() {
    let f = "palindrome.asm";
    let i = [7, 1, 7, 3, 9, 3, 7, 1];
    verify_asm::<GoldilocksField>(f, slice_to_vec(&i));
    gen_halo2_proof(f, slice_to_vec(&i));
    // currently starky leads to
    // thread 'functional_instructions' has overflowed its stack
    // leave it out until that's fixed
    //gen_estark_proof(f, slice_to_vec(&i));
}

#[test]
fn single_function_vm() {
    let f = "single_function_vm.asm";
    let i = [];
    verify_asm::<GoldilocksField>(f, slice_to_vec(&i));
    gen_halo2_proof(f, slice_to_vec(&i));
    gen_estark_proof(f, slice_to_vec(&i));
}

#[test]
fn empty() {
    let f = "empty.asm";
    let i = [];
    verify_asm::<GoldilocksField>(f, slice_to_vec(&i));
    gen_halo2_proof(f, slice_to_vec(&i));
    gen_estark_proof(f, slice_to_vec(&i));
}

#[test]
fn single_operation() {
    let f = "single_operation.asm";
    let i = [];
    verify_asm::<GoldilocksField>(f, slice_to_vec(&i));
    gen_halo2_proof(f, slice_to_vec(&i));
    gen_estark_proof(f, slice_to_vec(&i));
}

#[test]
fn empty_vm() {
    let f = "empty_vm.asm";
    let i = [];
    verify_asm::<GoldilocksField>(f, slice_to_vec(&i));
    gen_halo2_proof(f, slice_to_vec(&i));
    gen_estark_proof(f, slice_to_vec(&i));
}

#[test]
fn vm_to_block_unique_interface() {
    let f = "vm_to_block_unique_interface.asm";
    let i = [];
    verify_asm::<GoldilocksField>(f, slice_to_vec(&i));
    gen_halo2_proof(f, slice_to_vec(&i));
    // currently starky leads to
    // thread 'functional_instructions' has overflowed its stack
    // leave it out until that's fixed
    //gen_estark_proof(f, slice_to_vec(&i));
}

#[test]
fn vm_to_block_to_block() {
    let f = "vm_to_block_to_block.asm";
    let i = [];
    verify_asm::<GoldilocksField>(f, slice_to_vec(&i));
    gen_halo2_proof(f, slice_to_vec(&i));
}

#[test]
fn block_to_block() {
    let f = "block_to_block.asm";
    let i = [];
    verify_asm::<GoldilocksField>(f, slice_to_vec(&i));
    gen_halo2_proof(f, slice_to_vec(&i));
    gen_estark_proof(f, slice_to_vec(&i));
}

#[test]
#[should_panic = "not implemented: No executor machine matched identity `main.instr_sub { 1, main.X, main.Y, main.Z } in 1 { main_arith.operation_id, main_arith.z, main_arith.x, main_arith.y };`"]
fn vm_to_block_multiple_interfaces() {
    let f = "vm_to_block_multiple_interfaces.asm";
    let i = [];
    verify_asm::<GoldilocksField>(f, slice_to_vec(&i));
    gen_halo2_proof(f, slice_to_vec(&i));
    gen_estark_proof(f, slice_to_vec(&i));
}

#[test]
fn vm_to_vm() {
    let f = "vm_to_vm.asm";
    let i = [];
    verify_asm::<GoldilocksField>(f, slice_to_vec(&i));
    gen_halo2_proof(f, slice_to_vec(&i));
    gen_estark_proof(f, slice_to_vec(&i));
}

#[test]
fn vm_to_vm_dynamic_trace_length() {
    let f = "vm_to_vm_dynamic_trace_length.asm";
    let i = [];
    verify_asm::<GoldilocksField>(f, slice_to_vec(&i));
    gen_halo2_proof(f, slice_to_vec(&i));
    gen_estark_proof(f, slice_to_vec(&i));
}

#[test]
fn vm_to_vm_to_block() {
    let f = "vm_to_vm_to_block.asm";
    let i = [];
    verify_asm::<GoldilocksField>(f, slice_to_vec(&i));
    gen_halo2_proof(f, slice_to_vec(&i));
    gen_estark_proof(f, slice_to_vec(&i));
}

#[test]
fn vm_to_vm_to_vm() {
    let f = "vm_to_vm_to_vm.asm";
    let i = [];
    verify_asm::<GoldilocksField>(f, slice_to_vec(&i));
    gen_halo2_proof(f, slice_to_vec(&i));
    gen_estark_proof(f, slice_to_vec(&i));
}

#[test]
fn test_mem_read_write() {
    let f = "mem_read_write.asm";
    verify_asm::<GoldilocksField>(f, Default::default());
    gen_halo2_proof(f, Default::default());
    gen_estark_proof(f, Default::default());
}

#[test]
fn test_multi_assign() {
    let f = "multi_assign.asm";
    let i = [7];
    verify_asm::<GoldilocksField>(f, slice_to_vec(&i));
    gen_halo2_proof(f, slice_to_vec(&i));
    gen_estark_proof(f, slice_to_vec(&i));
}

#[test]
fn test_multi_return() {
    let f = "multi_return.asm";
    let i = [];
    verify_asm::<GoldilocksField>(f, slice_to_vec(&i));
    gen_halo2_proof(f, slice_to_vec(&i));
    gen_estark_proof(f, Default::default());
}

#[test]
#[should_panic = "called `Result::unwrap()` on an `Err` value: [\"Assignment register `Z` is incompatible with `square_and_double(3)`. Try using `<==` with no explicit assignment registers.\", \"Assignment register `Y` is incompatible with `square_and_double(3)`. Try using `<==` with no explicit assignment registers.\"]"]
fn test_multi_return_wrong_assignment_registers() {
    let f = "multi_return_wrong_assignment_registers.asm";
    let i = [];
    verify_asm::<GoldilocksField>(f, slice_to_vec(&i));
}

#[test]
#[should_panic = "Result::unwrap()` on an `Err` value: [\"Mismatched number of registers for assignment A, B <=Y= square_and_double(3);\"]"]
fn test_multi_return_wrong_assignment_register_length() {
    let f = "multi_return_wrong_assignment_register_length.asm";
    let i = [];
    verify_asm::<GoldilocksField>(f, slice_to_vec(&i));
}

#[test]
fn test_bit_access() {
    let f = "bit_access.asm";
    let i = [20];
    verify_asm::<GoldilocksField>(f, slice_to_vec(&i));
    gen_halo2_proof(f, slice_to_vec(&i));
    // currently starky leads to
    // thread 'functional_instructions' has overflowed its stack
    // leave it out until that's fixed
    //gen_estark_proof(f, slice_to_vec(&i));
}

#[test]
fn test_sqrt() {
    let f = "sqrt.asm";
    verify_asm::<GoldilocksField>(f, Default::default());
    gen_halo2_proof(f, Default::default());
    gen_estark_proof(f, Default::default());
}

#[test]
fn functional_instructions() {
    let f = "functional_instructions.asm";
    let i = [20];
    verify_asm::<GoldilocksField>(f, slice_to_vec(&i));
    gen_halo2_proof(f, slice_to_vec(&i));
    // currently starky leads to
    // thread 'functional_instructions' has overflowed its stack
    // leave it out until that's fixed
    //gen_estark_proof(f, slice_to_vec(&i));
}

#[test]
fn full_pil_constant() {
    let f = "full_pil_constant.asm";
    verify_asm::<GoldilocksField>(f, Default::default());
    gen_halo2_proof(f, Default::default());
    gen_estark_proof(f, Default::default());
}

#[test]
fn intermediate() {
    let f = "intermediate.asm";
    verify_asm::<GoldilocksField>(f, Default::default());
    gen_halo2_proof(f, Default::default());
    gen_estark_proof(f, Default::default());
}

#[test]
fn intermediate_nested() {
    let f = "intermediate_nested.asm";
    verify_asm::<GoldilocksField>(f, Default::default());
    gen_halo2_proof(f, Default::default());
    gen_estark_proof(f, Default::default());
}

mod book {
    use super::*;
    use number::GoldilocksField;
    use test_log::test;

    fn run_book_test(file: &str) {
        // passing 0 to all tests currently works as they either take no prover input or 0 works
        let i = [0];

        verify_asm::<GoldilocksField>(file, slice_to_vec(&i));
        gen_halo2_proof(file, slice_to_vec(&i));
        gen_estark_proof(file, slice_to_vec(&i));
    }

    include!(concat!(env!("OUT_DIR"), "/asm_book_tests.rs"));
}

#[test]
#[should_panic = "Witness generation failed."]
fn hello_world_asm_fail() {
    let f = "book/hello_world.asm";
    let i = [1];
    verify_asm::<GoldilocksField>(f, slice_to_vec(&i));
}

#[test]
fn test_macros_in_instructions() {
    let f = "macros_in_instructions.asm";
    verify_asm::<GoldilocksField>(f, Default::default());
    gen_halo2_proof(f, Default::default());
    gen_estark_proof(f, Default::default());
}
