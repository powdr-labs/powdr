use common::verify_asm_string;
use powdr::number::{AbstractNumberType, DegreeType};
use std::fs;

mod common;

fn verify_asm(file_name: &str, inputs: Vec<AbstractNumberType>, row_count: Option<DegreeType>) {
    let contents = fs::read_to_string(format!("./tests/asm_data/{file_name}")).unwrap();
    verify_asm_string(file_name, &contents, inputs, row_count)
}

#[test]
fn simple_sum_asm() {
    verify_asm(
        "simple_sum.asm",
        [16, 4, 1, 2, 8, 5].iter().map(|&x| x.into()).collect(),
        None,
    );
}

#[test]
fn palindrome() {
    verify_asm(
        "palindrome.asm",
        [7, 1, 7, 3, 9, 3, 7, 1].iter().map(|&x| x.into()).collect(),
        None,
    );
}

#[test]
fn test_mem_read_write() {
    verify_asm("mem_read_write.asm", Default::default(), None);
}

#[test]
fn test_multi_assign() {
    verify_asm(
        "multi_assign.asm",
        [7].iter().map(|&x| x.into()).collect(),
        None,
    );
}

#[test]
fn test_bit_access() {
    verify_asm(
        "bit_access.asm",
        [20].iter().map(|&x| x.into()).collect(),
        None,
    );
}

#[test]
fn functional_instructions() {
    verify_asm(
        "functional_instructions.asm",
        [20].iter().map(|&x| x.into()).collect(),
        None,
    );
}
