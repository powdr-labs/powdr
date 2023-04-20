use common::verify_asm_string;
use powdr::number::FieldElement;

mod common;

#[test]
#[ignore = "Too slow"]
fn test_trivial() {
    let case = "trivial.rs";
    verify_file(case, vec![]);
}

#[test]
#[ignore = "Too slow"]
fn test_sum() {
    let case = "sum.rs";
    verify_file(
        case,
        [16, 4, 1, 2, 8, 5].iter().map(|&x| x.into()).collect(),
    );
}

#[test]
#[ignore = "Too slow"]
fn test_byte_access() {
    let case = "byte_access.rs";
    verify_file(case, [0, 104, 707].iter().map(|&x| x.into()).collect());
}

#[test]
#[ignore = "Too slow"]
fn test_double_word() {
    let case = "double_word.rs";
    let a0 = 0x01000000u32;
    let a1 = 0x010000ffu32;
    let b0 = 0xf100b00fu32;
    let b1 = 0x0100f0f0u32;
    let c = ((a0 as u64) | ((a1 as u64) << 32)).wrapping_mul((b0 as u64) | ((b1 as u64) << 32));
    verify_file(
        case,
        [
            a0,
            a1,
            b0,
            b1,
            (c & 0xffffffff) as u32,
            ((c >> 32) & 0xffffffff) as u32,
        ]
        .iter()
        .map(|&x| x.into())
        .collect(),
    );
}

#[test]
#[ignore = "Too slow"]
fn test_keccak() {
    let case = "keccak";
    verify_crate(case, vec![]);
}

fn verify_file(case: &str, inputs: Vec<FieldElement>) {
    let riscv_asm = powdr::riscv::compile_rust_to_riscv_asm(&format!("tests/riscv_data/{case}"));
    let powdr_asm = powdr::riscv::compiler::compile_riscv_asm(riscv_asm);

    verify_asm_string(&format!("{case}.asm"), &powdr_asm, inputs);
}

fn verify_crate(case: &str, inputs: Vec<FieldElement>) {
    let riscv_asm = powdr::riscv::compile_rust_crate_to_riscv_asm(&format!(
        "tests/riscv_data/{case}/Cargo.toml"
    ));
    let powdr_asm = powdr::riscv::compiler::compile_riscv_asm(riscv_asm);

    verify_asm_string(&format!("{case}.asm"), &powdr_asm, inputs);
}
