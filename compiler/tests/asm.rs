use compiler::test_util::{gen_estark_proof, gen_halo2_proof, verify_test_file};
use number::{FieldElement, GoldilocksField};
use test_log::test;

fn verify_asm<T: FieldElement>(file_name: &str, inputs: Vec<T>) {
    verify_test_file(file_name, inputs, vec![]);
}

fn slice_to_vec<T: FieldElement>(arr: &[i32]) -> Vec<T> {
    arr.iter().cloned().map(|x| x.into()).collect()
}

#[test]
fn simple_sum_asm() {
    let f = "asm/simple_sum.asm";
    let i = [16, 4, 1, 2, 8, 5];
    verify_asm::<GoldilocksField>(f, slice_to_vec(&i));
    gen_halo2_proof(f, slice_to_vec(&i));
    gen_estark_proof(f, slice_to_vec(&i));
}

#[test]
fn secondary_block_machine_add2() {
    let f = "asm/secondary_block_machine_add2.asm";
    verify_asm::<GoldilocksField>(f, vec![]);
    gen_halo2_proof(f, vec![]);
    gen_estark_proof(f, vec![]);
}

#[test]
fn mem_write_once() {
    let f = "asm/mem_write_once.asm";
    verify_asm::<GoldilocksField>(f, vec![]);
    gen_halo2_proof(f, vec![]);
    gen_estark_proof(f, vec![]);
}

#[test]
fn mem_write_once_external_write() {
    let f = "asm/mem_write_once_external_write.asm";
    let mut mem = vec![GoldilocksField::from(0); 256];
    mem[17] = GoldilocksField::from(42);
    mem[62] = GoldilocksField::from(123);
    mem[255] = GoldilocksField::from(-1);
    verify_test_file::<GoldilocksField>(f, vec![], vec![("main.v", mem)]);
}

#[test]
fn block_machine_cache_miss() {
    let f = "asm/block_machine_cache_miss.asm";
    verify_asm::<GoldilocksField>(f, vec![]);
    gen_halo2_proof(f, vec![]);
    gen_estark_proof(f, vec![]);
}

#[test]
fn palindrome() {
    let f = "asm/palindrome.asm";
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
    let f = "asm/single_function_vm.asm";
    let i = [];
    verify_asm::<GoldilocksField>(f, slice_to_vec(&i));
    gen_halo2_proof(f, slice_to_vec(&i));
    gen_estark_proof(f, slice_to_vec(&i));
}

#[test]
fn empty() {
    let f = "asm/empty.asm";
    let i = [];
    verify_asm::<GoldilocksField>(f, slice_to_vec(&i));
    gen_halo2_proof(f, slice_to_vec(&i));
    gen_estark_proof(f, slice_to_vec(&i));
}

#[test]
fn single_operation() {
    let f = "asm/single_operation.asm";
    let i = [];
    verify_asm::<GoldilocksField>(f, slice_to_vec(&i));
    gen_halo2_proof(f, slice_to_vec(&i));
    gen_estark_proof(f, slice_to_vec(&i));
}

#[test]
fn empty_vm() {
    let f = "asm/empty_vm.asm";
    let i = [];
    verify_asm::<GoldilocksField>(f, slice_to_vec(&i));
    gen_halo2_proof(f, slice_to_vec(&i));
    gen_estark_proof(f, slice_to_vec(&i));
}

#[test]
fn vm_to_block_unique_interface() {
    let f = "asm/vm_to_block_unique_interface.asm";
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
    let f = "asm/vm_to_block_to_block.asm";
    let i = [];
    verify_asm::<GoldilocksField>(f, slice_to_vec(&i));
    gen_halo2_proof(f, slice_to_vec(&i));
}

#[test]
fn block_to_block() {
    let f = "asm/block_to_block.asm";
    let i = [];
    verify_asm::<GoldilocksField>(f, slice_to_vec(&i));
    gen_halo2_proof(f, slice_to_vec(&i));
    gen_estark_proof(f, slice_to_vec(&i));
}

#[test]
fn vm_to_block_multiple_interfaces() {
    let f = "asm/vm_to_block_multiple_interfaces.asm";
    let i = [];
    verify_asm::<GoldilocksField>(f, slice_to_vec(&i));
    gen_halo2_proof(f, slice_to_vec(&i));
    gen_estark_proof(f, slice_to_vec(&i));
}

#[test]
fn vm_to_vm() {
    let f = "asm/vm_to_vm.asm";
    let i = [];
    verify_asm::<GoldilocksField>(f, slice_to_vec(&i));
    gen_halo2_proof(f, slice_to_vec(&i));
    gen_estark_proof(f, slice_to_vec(&i));
}

#[test]
fn vm_to_vm_dynamic_trace_length() {
    let f = "asm/vm_to_vm_dynamic_trace_length.asm";
    let i = [];
    verify_asm::<GoldilocksField>(f, slice_to_vec(&i));
    gen_halo2_proof(f, slice_to_vec(&i));
    gen_estark_proof(f, slice_to_vec(&i));
}

#[test]
fn vm_to_vm_to_block() {
    let f = "asm/vm_to_vm_to_block.asm";
    let i = [];
    verify_asm::<GoldilocksField>(f, slice_to_vec(&i));
    gen_halo2_proof(f, slice_to_vec(&i));
    gen_estark_proof(f, slice_to_vec(&i));
}

#[test]
fn vm_to_block_array() {
    let f = "asm/vm_to_block_array.asm";
    let i = [];
    verify_asm::<GoldilocksField>(f, slice_to_vec(&i));
    gen_halo2_proof(f, slice_to_vec(&i));
    gen_estark_proof(f, slice_to_vec(&i));
}

#[test]
fn vm_to_vm_to_vm() {
    let f = "asm/vm_to_vm_to_vm.asm";
    let i = [];
    verify_asm::<GoldilocksField>(f, slice_to_vec(&i));
    gen_halo2_proof(f, slice_to_vec(&i));
    gen_estark_proof(f, slice_to_vec(&i));
}

#[test]
fn test_mem_read_write() {
    let f = "asm/mem_read_write.asm";
    verify_asm::<GoldilocksField>(f, Default::default());
    gen_halo2_proof(f, Default::default());
    gen_estark_proof(f, Default::default());
}

#[test]
fn test_multi_assign() {
    let f = "asm/multi_assign.asm";
    let i = [7];
    verify_asm::<GoldilocksField>(f, slice_to_vec(&i));
    gen_halo2_proof(f, slice_to_vec(&i));
    gen_estark_proof(f, slice_to_vec(&i));
}

#[test]
fn test_multi_return() {
    let f = "asm/multi_return.asm";
    let i = [];
    verify_asm::<GoldilocksField>(f, slice_to_vec(&i));
    gen_halo2_proof(f, slice_to_vec(&i));
    gen_estark_proof(f, Default::default());
}

#[test]
#[should_panic = "called `Result::unwrap()` on an `Err` value: [\"Assignment register `Z` is incompatible with `square_and_double(3)`. Try using `<==` with no explicit assignment registers.\", \"Assignment register `Y` is incompatible with `square_and_double(3)`. Try using `<==` with no explicit assignment registers.\"]"]
fn test_multi_return_wrong_assignment_registers() {
    let f = "asm/multi_return_wrong_assignment_registers.asm";
    let i = [];
    verify_asm::<GoldilocksField>(f, slice_to_vec(&i));
}

#[test]
#[should_panic = "Result::unwrap()` on an `Err` value: [\"Mismatched number of registers for assignment A, B <=Y= square_and_double(3);\"]"]
fn test_multi_return_wrong_assignment_register_length() {
    let f = "asm/multi_return_wrong_assignment_register_length.asm";
    let i = [];
    verify_asm::<GoldilocksField>(f, slice_to_vec(&i));
}

#[test]
fn test_bit_access() {
    let f = "asm/bit_access.asm";
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
    let f = "asm/sqrt.asm";
    verify_asm::<GoldilocksField>(f, Default::default());
    gen_halo2_proof(f, Default::default());
    gen_estark_proof(f, Default::default());
}

#[test]
fn functional_instructions() {
    let f = "asm/functional_instructions.asm";
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
    let f = "asm/full_pil_constant.asm";
    verify_asm::<GoldilocksField>(f, Default::default());
    gen_halo2_proof(f, Default::default());
    gen_estark_proof(f, Default::default());
}

#[test]
fn intermediate() {
    let f = "asm/intermediate.asm";
    verify_asm::<GoldilocksField>(f, Default::default());
    gen_halo2_proof(f, Default::default());
    gen_estark_proof(f, Default::default());
}

#[test]
fn intermediate_nested() {
    let f = "asm/intermediate_nested.asm";
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

        verify_asm::<GoldilocksField>(&format!("{file}"), slice_to_vec(&i));
        gen_halo2_proof(file, slice_to_vec(&i));
        gen_estark_proof(file, slice_to_vec(&i));
    }

    include!(concat!(env!("OUT_DIR"), "/asm_book_tests.rs"));
}

#[test]
#[should_panic = "Witness generation failed."]
fn hello_world_asm_fail() {
    let f = "asm/book/hello_world.asm";
    let i = [1];
    verify_asm::<GoldilocksField>(f, slice_to_vec(&i));
}
