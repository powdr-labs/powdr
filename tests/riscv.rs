use common::verify_asm_string;
use powdr::number::{AbstractNumberType, DegreeType};

mod common;

#[test]
fn test_sum() {
    let case = "sum.rs";
    verify(
        case,
        [16, 4, 1, 2, 8, 5].iter().map(|&x| x.into()).collect(),
        None,
    );
}

fn verify(case: &str, inputs: Vec<AbstractNumberType>, row_count: Option<DegreeType>) {
    let riscv_asm = powdr::riscv::compile_rust_to_riscv_asm(&format!("tests/riscv_data/{case}"));
    let powdr_asm = powdr::riscv::compiler::compile_riscv_asm(&riscv_asm);

    verify_asm_string(&format!("{case}.asm"), &powdr_asm, inputs, row_count);
}
