mod common;

use powdr_autoprecompiles::blocks::BasicBlock;
use powdr_leanvm::*;
use powdr_number::BabyBearField;
use test_log::test;

fn assert_machine_output(
    program: Vec<instruction::LeanVmInstruction<BabyBearField>>,
    test_name: &str,
) {
    let bb = BasicBlock {
        start_pc: 0,
        instructions: program,
    };
    common::assert_machine_output(bb.into(), "single_instructions", test_name);
}

// ADD instruction tests

#[test]
fn single_add_mem() {
    // m[fp+5] = m[fp+3] + m[fp+4] (all memory reads)
    let program = [add(5, 3, 4, false, false, false)];
    assert_machine_output(program.to_vec(), "single_add_mem");
}

#[test]
fn single_add_imm_c() {
    // m[fp+5] = m[fp+3] + 7 (operand C is immediate)
    let program = [add(5, 3, 7, false, false, true)];
    assert_machine_output(program.to_vec(), "single_add_imm_c");
}

// MUL instruction tests

#[test]
fn single_mul_mem() {
    // m[fp+5] = m[fp+3] * m[fp+4]
    let program = [mul(5, 3, 4, false, false, false)];
    assert_machine_output(program.to_vec(), "single_mul_mem");
}

// DEREF instruction tests

#[test]
fn single_deref() {
    // m[m[fp+2] + 3] = m[fp+4]
    let program = [deref(2, 3, 4, false, false)];
    assert_machine_output(program.to_vec(), "single_deref");
}

// JUMP instruction tests

#[test]
fn single_jump() {
    // Jump: if m[fp+1] then pc=m[fp+2], fp=m[fp+3]
    let program = [jump(1, 2, 3, false, false, false, false)];
    assert_machine_output(program.to_vec(), "single_jump");
}
