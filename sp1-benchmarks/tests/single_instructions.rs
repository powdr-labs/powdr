mod common;

use common::assert_machine_output;
use sp1_core_executor::{Instruction, Opcode};

#[test]
fn addi() {
    let basic_block = vec![Instruction::new(Opcode::ADDI, 29, 0, 5, false, true)];
    assert_machine_output(basic_block, "single_instructions", "addi");
}

#[test]
fn add() {
    let basic_block = vec![Instruction::new(Opcode::ADD, 1, 2, 3, false, false)];
    assert_machine_output(basic_block, "single_instructions", "add");
}

#[test]
fn sub() {
    let basic_block = vec![Instruction::new(Opcode::SUB, 1, 2, 3, false, false)];
    assert_machine_output(basic_block, "single_instructions", "sub");
}

#[test]
fn xor() {
    let basic_block = vec![Instruction::new(Opcode::XOR, 1, 2, 3, false, false)];
    assert_machine_output(basic_block, "single_instructions", "xor");
}

#[test]
fn or() {
    let basic_block = vec![Instruction::new(Opcode::OR, 1, 2, 3, false, false)];
    assert_machine_output(basic_block, "single_instructions", "or");
}

#[test]
fn and() {
    let basic_block = vec![Instruction::new(Opcode::AND, 1, 2, 3, false, false)];
    assert_machine_output(basic_block, "single_instructions", "and");
}

#[test]
fn sll() {
    let basic_block = vec![Instruction::new(Opcode::SLL, 1, 2, 3, false, false)];
    assert_machine_output(basic_block, "single_instructions", "sll");
}

#[test]
fn srl() {
    let basic_block = vec![Instruction::new(Opcode::SRL, 1, 2, 3, false, false)];
    assert_machine_output(basic_block, "single_instructions", "srl");
}

#[test]
fn sra() {
    let basic_block = vec![Instruction::new(Opcode::SRA, 1, 2, 3, false, false)];
    assert_machine_output(basic_block, "single_instructions", "sra");
}

#[test]
fn slt() {
    let basic_block = vec![Instruction::new(Opcode::SLT, 1, 2, 3, false, false)];
    assert_machine_output(basic_block, "single_instructions", "slt");
}

#[test]
fn sltu() {
    let basic_block = vec![Instruction::new(Opcode::SLTU, 1, 2, 3, false, false)];
    assert_machine_output(basic_block, "single_instructions", "sltu");
}

#[test]
fn sltui() {
    let basic_block = vec![Instruction::new(Opcode::SLTU, 1, 2, 3, false, true)];
    assert_machine_output(basic_block, "single_instructions", "sltui");
}

#[test]
fn mul() {
    let basic_block = vec![Instruction::new(Opcode::MUL, 1, 2, 3, false, false)];
    assert_machine_output(basic_block, "single_instructions", "mul");
}

#[test]
fn mulh() {
    let basic_block = vec![Instruction::new(Opcode::MULH, 1, 2, 3, false, false)];
    assert_machine_output(basic_block, "single_instructions", "mulh");
}

#[test]
fn mulhu() {
    let basic_block = vec![Instruction::new(Opcode::MULHU, 1, 2, 3, false, false)];
    assert_machine_output(basic_block, "single_instructions", "mulhu");
}

#[test]
fn mulhsu() {
    let basic_block = vec![Instruction::new(Opcode::MULHSU, 1, 2, 3, false, false)];
    assert_machine_output(basic_block, "single_instructions", "mulhsu");
}

#[test]
fn div() {
    let basic_block = vec![Instruction::new(Opcode::DIV, 1, 2, 3, false, false)];
    assert_machine_output(basic_block, "single_instructions", "div");
}

#[test]
fn divu() {
    let basic_block = vec![Instruction::new(Opcode::DIVU, 1, 2, 3, false, false)];
    assert_machine_output(basic_block, "single_instructions", "divu");
}

#[test]
fn rem() {
    let basic_block = vec![Instruction::new(Opcode::REM, 1, 2, 3, false, false)];
    assert_machine_output(basic_block, "single_instructions", "rem");
}

#[test]
fn remu() {
    let basic_block = vec![Instruction::new(Opcode::REMU, 1, 2, 3, false, false)];
    assert_machine_output(basic_block, "single_instructions", "remu");
}

#[test]
fn lb() {
    let basic_block = vec![Instruction::new(Opcode::LB, 1, 2, 100, false, true)];
    assert_machine_output(basic_block, "single_instructions", "lb");
}

#[test]
fn lh() {
    let basic_block = vec![Instruction::new(Opcode::LH, 1, 2, 100, false, true)];
    assert_machine_output(basic_block, "single_instructions", "lh");
}

#[test]
fn lw() {
    let basic_block = vec![Instruction::new(Opcode::LW, 1, 2, 100, false, true)];
    assert_machine_output(basic_block, "single_instructions", "lw");
}

#[test]
fn lbu() {
    let basic_block = vec![Instruction::new(Opcode::LBU, 1, 2, 100, false, true)];
    assert_machine_output(basic_block, "single_instructions", "lbu");
}

#[test]
fn lhu() {
    let basic_block = vec![Instruction::new(Opcode::LHU, 1, 2, 100, false, true)];
    assert_machine_output(basic_block, "single_instructions", "lhu");
}

#[test]
fn sb() {
    let basic_block = vec![Instruction::new(Opcode::SB, 1, 2, 100, false, true)];
    assert_machine_output(basic_block, "single_instructions", "sb");
}

#[test]
fn sh() {
    let basic_block = vec![Instruction::new(Opcode::SH, 1, 2, 100, false, true)];
    assert_machine_output(basic_block, "single_instructions", "sh");
}

#[test]
fn sw() {
    let basic_block = vec![Instruction::new(Opcode::SW, 1, 2, 100, false, true)];
    assert_machine_output(basic_block, "single_instructions", "sw");
}

#[test]
fn beq() {
    let basic_block = vec![Instruction::new(Opcode::BEQ, 1, 2, 100, false, true)];
    assert_machine_output(basic_block, "single_instructions", "beq");
}

#[test]
fn bne() {
    let basic_block = vec![Instruction::new(Opcode::BNE, 1, 2, 100, false, true)];
    assert_machine_output(basic_block, "single_instructions", "bne");
}

#[test]
fn blt() {
    let basic_block = vec![Instruction::new(Opcode::BLT, 1, 2, 100, false, true)];
    assert_machine_output(basic_block, "single_instructions", "blt");
}

#[test]
fn bge() {
    let basic_block = vec![Instruction::new(Opcode::BGE, 1, 2, 100, false, true)];
    assert_machine_output(basic_block, "single_instructions", "bge");
}

#[test]
fn bltu() {
    let basic_block = vec![Instruction::new(Opcode::BLTU, 1, 2, 100, false, true)];
    assert_machine_output(basic_block, "single_instructions", "bltu");
}

#[test]
fn bgeu() {
    let basic_block = vec![Instruction::new(Opcode::BGEU, 1, 2, 100, false, true)];
    assert_machine_output(basic_block, "single_instructions", "bgeu");
}

#[test]
fn jal() {
    let basic_block = vec![Instruction::new(Opcode::JAL, 1, 0, 100, true, true)];
    assert_machine_output(basic_block, "single_instructions", "jal");
}

#[test]
fn jalr() {
    let basic_block = vec![Instruction::new(Opcode::JALR, 1, 2, 100, false, true)];
    assert_machine_output(basic_block, "single_instructions", "jalr");
}

#[test]
fn auipc() {
    let basic_block = vec![Instruction::new(Opcode::AUIPC, 1, 0, 0x12345, true, true)];
    assert_machine_output(basic_block, "single_instructions", "auipc");
}

#[test]
fn lui() {
    let basic_block = vec![Instruction::new(Opcode::LUI, 1, 0, 0x12345, true, true)];
    assert_machine_output(basic_block, "single_instructions", "lui");
}
