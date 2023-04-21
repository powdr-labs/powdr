use compiler::verify_asm_string;
use number::FieldElement;
use std::fs;

fn verify_asm(file_name: &str, inputs: Vec<FieldElement>) {
    let contents = fs::read_to_string(format!("../test_data/asm/{file_name}")).unwrap();
    verify_asm_string(file_name, &contents, inputs)
}

#[test]
fn simple_sum_asm() {
    verify_asm(
        "simple_sum.asm",
        [16, 4, 1, 2, 8, 5].iter().map(|&x| x.into()).collect(),
    );
}

#[test]
fn palindrome() {
    verify_asm(
        "palindrome.asm",
        [7, 1, 7, 3, 9, 3, 7, 1].iter().map(|&x| x.into()).collect(),
    );
}

#[test]
fn test_mem_read_write() {
    verify_asm("mem_read_write.asm", Default::default());
}

#[test]
fn test_multi_assign() {
    verify_asm("multi_assign.asm", [7].iter().map(|&x| x.into()).collect());
}

#[test]
fn test_bit_access() {
    verify_asm("bit_access.asm", [20].iter().map(|&x| x.into()).collect());
}

#[test]
fn functional_instructions() {
    verify_asm(
        "functional_instructions.asm",
        [20].iter().map(|&x| x.into()).collect(),
    );
}

#[test]
fn full_pil_column() {
    verify_asm("full_pil_constant.asm", Default::default());
}
