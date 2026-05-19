mod common;

use common::assert_machine_output;
use sp1_core_executor::{Instruction, Opcode};

#[test]
fn mv() {
    let basic_block = vec![Instruction::new(Opcode::ADDI, 29, 30, 0, false, true)];
    assert_machine_output(basic_block, "pseudo_instructions", "mv");
}

#[test]
fn not() {
    let basic_block = vec![Instruction::new(
        Opcode::XOR,
        8,
        5,
        0xFFFF_FFFF_FFFF_FFFF,
        false,
        true,
    )];
    assert_machine_output(basic_block, "pseudo_instructions", "not");
}

#[test]
fn neg() {
    let basic_block = vec![Instruction::new(Opcode::SUB, 8, 0, 5, false, false)];
    assert_machine_output(basic_block, "pseudo_instructions", "neg");
}

#[test]
fn seqz() {
    let basic_block = vec![Instruction::new(Opcode::SLTU, 8, 5, 1, false, true)];
    assert_machine_output(basic_block, "pseudo_instructions", "seqz");
}

#[test]
fn snez() {
    let basic_block = vec![Instruction::new(Opcode::SLTU, 8, 0, 5, false, false)];
    assert_machine_output(basic_block, "pseudo_instructions", "snez");
}

#[test]
fn sltz() {
    let basic_block = vec![Instruction::new(Opcode::SLT, 8, 5, 0, false, false)];
    assert_machine_output(basic_block, "pseudo_instructions", "sltz");
}

#[test]
fn sgtz() {
    let basic_block = vec![Instruction::new(Opcode::SLT, 8, 0, 5, false, false)];
    assert_machine_output(basic_block, "pseudo_instructions", "sgtz");
}

#[test]
fn beqz() {
    let basic_block = vec![Instruction::new(Opcode::BEQ, 5, 0, 8, false, true)];
    assert_machine_output(basic_block, "pseudo_instructions", "beqz");
}

#[test]
fn bnez() {
    let basic_block = vec![Instruction::new(Opcode::BNE, 5, 0, 8, false, true)];
    assert_machine_output(basic_block, "pseudo_instructions", "bnez");
}

#[test]
fn blez() {
    let basic_block = vec![Instruction::new(Opcode::BGE, 0, 5, 8, false, true)];
    assert_machine_output(basic_block, "pseudo_instructions", "blez");
}

#[test]
fn bgez() {
    let basic_block = vec![Instruction::new(Opcode::BGE, 5, 0, 8, false, true)];
    assert_machine_output(basic_block, "pseudo_instructions", "bgez");
}

#[test]
fn bltz() {
    let basic_block = vec![Instruction::new(Opcode::BLT, 5, 0, 8, false, true)];
    assert_machine_output(basic_block, "pseudo_instructions", "bltz");
}

#[test]
fn bgtz() {
    let basic_block = vec![Instruction::new(Opcode::BLT, 0, 5, 8, false, true)];
    assert_machine_output(basic_block, "pseudo_instructions", "bgtz");
}

#[test]
fn j() {
    let basic_block = vec![Instruction::new(Opcode::JAL, 0, 0, 8, true, true)];
    assert_machine_output(basic_block, "pseudo_instructions", "j");
}

#[test]
fn jr() {
    let basic_block = vec![Instruction::new(Opcode::JALR, 0, 5, 0, false, true)];
    assert_machine_output(basic_block, "pseudo_instructions", "jr");
}

#[test]
fn ret() {
    let basic_block = vec![Instruction::new(Opcode::JALR, 0, 1, 0, false, true)];
    assert_machine_output(basic_block, "pseudo_instructions", "ret");
}
