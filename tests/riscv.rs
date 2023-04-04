use common::verify_asm_string;
use powdr::number::AbstractNumberType;

mod common;

#[test]
fn test_sum() {
    let case = "sum.rs";
    verify_file(
        case,
        [16, 4, 1, 2, 8, 5].iter().map(|&x| x.into()).collect(),
    );
}

#[test]
fn test_byte_access() {
    let case = "byte_access.rs";
    verify_file(case, [0, 104, 707].iter().map(|&x| x.into()).collect());
}

#[test]
fn test_keccak() {
    let case = "keccak";
    verify_crate(case, vec![]);
}

fn verify_file(case: &str, inputs: Vec<AbstractNumberType>) {
    let riscv_asm = powdr::riscv::compile_rust_to_riscv_asm(&format!("tests/riscv_data/{case}"));
    let powdr_asm = powdr::riscv::compiler::compile_riscv_asm(&riscv_asm);

    verify_asm_string(&format!("{case}.asm"), &powdr_asm, inputs);
}

fn verify_crate(case: &str, inputs: Vec<AbstractNumberType>) {
    let riscv_asm = powdr::riscv::compile_rust_crate_to_riscv_asm(&format!(
        "tests/riscv_data/{case}/Cargo.toml"
    ));
    let powdr_asm = powdr::riscv::compiler::compile_riscv_asm(&riscv_asm);

    verify_asm_string(&format!("{case}.asm"), &powdr_asm, inputs);
}
