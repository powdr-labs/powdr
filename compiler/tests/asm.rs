use compiler::verify_asm_string;
use number::{FieldElement, GoldilocksField};
use std::fs;

fn verify_asm<T: FieldElement>(file_name: &str, inputs: Vec<T>) {
    let contents = fs::read_to_string(format!("../test_data/asm/{file_name}")).unwrap();
    verify_asm_string(file_name, &contents, inputs)
}

#[test]
fn simple_sum_asm() {
    verify_asm::<GoldilocksField>(
        "simple_sum.asm",
        [16, 4, 1, 2, 8, 5].iter().map(|&x| x.into()).collect(),
    );
}

#[test]
fn palindrome() {
    verify_asm::<GoldilocksField>(
        "palindrome.asm",
        [7, 1, 7, 3, 9, 3, 7, 1].iter().map(|&x| x.into()).collect(),
    );
}

#[test]
fn test_mem_read_write() {
    verify_asm::<GoldilocksField>("mem_read_write.asm", Default::default());
}

#[test]
fn test_multi_assign() {
    verify_asm::<GoldilocksField>("multi_assign.asm", [7].iter().map(|&x| x.into()).collect());
}

#[test]
fn test_bit_access() {
    verify_asm::<GoldilocksField>("bit_access.asm", [20].iter().map(|&x| x.into()).collect());
}

#[test]
fn functional_instructions() {
    verify_asm::<GoldilocksField>(
        "functional_instructions.asm",
        [20].iter().map(|&x| x.into()).collect(),
    );
}

#[test]
fn full_pil_constant() {
    verify_asm::<GoldilocksField>("full_pil_constant.asm", Default::default());
}
