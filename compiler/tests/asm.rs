use compiler::verify_asm_string;
use number::{Bn254Field, FieldElement, GoldilocksField};
use std::fs;
use test_log::test;

fn verify_asm<T: FieldElement>(file_name: &str, inputs: Vec<T>) {
    let contents = fs::read_to_string(format!("../test_data/asm/{file_name}")).unwrap();
    verify_asm_string(file_name, &contents, inputs)
}

#[cfg(feature = "halo2")]
fn gen_halo2_proof(file_name: &str, inputs: Vec<Bn254Field>) {
    compiler::compile_pil_or_asm(
        format!("../test_data/asm/{file_name}").as_str(),
        inputs,
        &mktemp::Temp::new_dir().unwrap(),
        true,
        Some(backend::BackendType::Halo2),
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
}

#[test]
fn secondary_block_machine_add2() {
    let f = "secondary_block_machine_add2.asm";
    verify_asm::<GoldilocksField>(f, vec![]);
    gen_halo2_proof(f, vec![]);
}

#[test]
fn palindrome() {
    let f = "palindrome.asm";
    let i = [7, 1, 7, 3, 9, 3, 7, 1];
    verify_asm::<GoldilocksField>(f, slice_to_vec(&i));
    gen_halo2_proof(f, slice_to_vec(&i));
}

#[test]
fn single_function_vm() {
    let f = "single_function_vm.asm";
    let i = [];
    verify_asm::<GoldilocksField>(f, slice_to_vec(&i));
    gen_halo2_proof(f, slice_to_vec(&i));
}

#[test]
#[should_panic = "Witness generation failed."]
fn empty_vm() {
    // TODO: an empty vm does not work because witgen does not find the infinite loop
    // this can be fixed by removing the assumption that we run exactly one block before
    // hitting the loop
    let f = "empty_vm.asm";
    let i = [];
    verify_asm::<GoldilocksField>(f, slice_to_vec(&i));
    gen_halo2_proof(f, slice_to_vec(&i));
}

#[test]
fn vm_to_block_unique_interface() {
    let f = "vm_to_block_unique_interface.asm";
    let i = [];
    verify_asm::<GoldilocksField>(f, slice_to_vec(&i));
    gen_halo2_proof(f, slice_to_vec(&i));
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
}

#[test]
#[ignore = "Too slow"]
fn keccak() {
    let f = "keccak.asm";
    let i = [];
    verify_asm::<GoldilocksField>(f, slice_to_vec(&i));
    gen_halo2_proof(f, slice_to_vec(&i));
}

#[test]
#[should_panic = "not implemented: No executor machine matched identity `main.instr_sub { 1, main.X, main.Y, main.Z } in 1 { main_arith.operation_id, main_arith.z, main_arith.x, main_arith.y };`"]
fn vm_to_block_multiple_interfaces() {
    let f = "vm_to_block_multiple_interfaces.asm";
    let i = [];
    verify_asm::<GoldilocksField>(f, slice_to_vec(&i));
    gen_halo2_proof(f, slice_to_vec(&i));
}

#[test]
#[should_panic = "not implemented: No executor machine matched identity `main.instr_add { 2, main.X, main.Y, main.Z } in main_vm.instr_return { main_vm._operation_id, main_vm._input_0, main_vm._input_1, main_vm._output_0 };`"]
fn vm_to_vm() {
    let f = "vm_to_vm.asm";
    let i = [];
    verify_asm::<GoldilocksField>(f, slice_to_vec(&i));
    gen_halo2_proof(f, slice_to_vec(&i));
}

#[test]
fn test_mem_read_write() {
    let f = "mem_read_write.asm";
    verify_asm::<GoldilocksField>(f, Default::default());
    gen_halo2_proof(f, Default::default());
}

#[test]
fn test_multi_assign() {
    let f = "multi_assign.asm";
    let i = [7];
    verify_asm::<GoldilocksField>(f, slice_to_vec(&i));
    gen_halo2_proof(f, slice_to_vec(&i));
}

#[test]
fn test_bit_access() {
    let f = "bit_access.asm";
    let i = [20];
    verify_asm::<GoldilocksField>(f, slice_to_vec(&i));
    gen_halo2_proof(f, slice_to_vec(&i));
}

#[test]
fn functional_instructions() {
    let f = "functional_instructions.asm";
    let i = [20];
    verify_asm::<GoldilocksField>(f, slice_to_vec(&i));
    gen_halo2_proof(f, slice_to_vec(&i));
}

#[test]
fn full_pil_constant() {
    let f = "full_pil_constant.asm";
    verify_asm::<GoldilocksField>(f, Default::default());
    gen_halo2_proof(f, Default::default());
}

#[test]
fn book() {
    for f in fs::read_dir("../test_data/asm/book/").unwrap() {
        let f = f.unwrap().path();
        let f = f.strip_prefix("../test_data/asm/").unwrap();
        // passing 0 to all tests currently works as they either take no prover input or 0 works
        let i = [0];

        verify_asm::<GoldilocksField>(f.to_str().unwrap(), slice_to_vec(&i));
        gen_halo2_proof(f.to_str().unwrap(), slice_to_vec(&i));
    }
}

#[test]
#[should_panic = "Witness generation failed."]
fn hello_world_asm_fail() {
    let f = "book/hello_world.asm";
    let i = [1];
    verify_asm::<GoldilocksField>(f, slice_to_vec(&i));
}
