mod common;
use openvm_instructions::instruction::Instruction;
use openvm_stark_sdk::p3_baby_bear::BabyBear;
use powdr_openvm::symbolic_instruction_builder::*;
use test_log::test;

fn assert_machine_output(program: Vec<Instruction<BabyBear>>, test_name: &str) {
    common::apc_builder_utils::assert_machine_output(program, "pseudo_instructions", test_name);
}

// Arithmetic pseudo instructions
#[test]
fn mv() {
    // mv rd, rs1 expands to: addi rd, rs1, 0
    let program = [
        // [x8] = [x5]
        add(8, 5, 0, 0),
    ];
    assert_machine_output(program.to_vec(), "mv");
}

#[test]
fn not() {
    // not rd, rs1 expands to: xori rd, rs1, -1
    // -1 in 24-bit 2's complement is 0xFFFFFF
    let minus_one: u32 = 0xFFFFFF;
    let program = [
        // [x8] = ~[x5]
        xor(8, 5, minus_one, 0),
    ];
    assert_machine_output(program.to_vec(), "not");
}

#[test]
fn neg() {
    // neg rd, rs1 expands to: sub rd, x0, rs1
    let program = [
        // [x8] = -[x5]
        sub(8, 0, 5, 1),
    ];
    assert_machine_output(program.to_vec(), "neg");
}

// Set pseudo instructions
#[test]
fn seqz() {
    // seqz rd, rs1 expands to: sltiu rd, rs1, 1
    // which in our case is: sltu rd, rs1, 1 (with rs2_as = 0 for immediate)
    // This sets rd = 1 if rs1 == 0, else rd = 0
    let program = [
        // [x8] = 1 if [x5] == 0, else 0
        sltu(8, 5, 1, 0),
    ];
    assert_machine_output(program.to_vec(), "seqz");
}

#[test]
fn snez() {
    // snez rd, rs1 expands to: sltu rd, x0, rs1
    let program = [
        // [x8] = 1 if [x5] != 0, else 0
        sltu(8, 0, 5, 1),
    ];
    assert_machine_output(program.to_vec(), "snez");
}

#[test]
fn sltz() {
    // sltz rd, rs1 expands to: slt rd, rs1, x0
    let program = [
        // [x8] = 1 if [x5] < 0 (signed), else 0
        slt(8, 5, 0, 1),
    ];
    assert_machine_output(program.to_vec(), "sltz");
}

#[test]
fn sgtz() {
    // sgtz rd, rs1 expands to: slt rd, x0, rs1
    let program = [
        // [x8] = 1 if [x5] > 0 (signed), else 0
        slt(8, 0, 5, 1),
    ];
    assert_machine_output(program.to_vec(), "sgtz");
}

// Branch pseudo instructions
#[test]
fn beqz() {
    // beqz rs1, offset expands to: beq rs1, x0, offset
    let program = [
        // pc = pc + 8 if [x5] == 0
        beq(5, 0, 8),
    ];
    assert_machine_output(program.to_vec(), "beqz");
}

#[test]
fn bnez() {
    // bnez rs1, offset expands to: bne rs1, x0, offset
    let program = [
        // pc = pc + 8 if [x5] != 0
        bne(5, 0, 8),
    ];
    assert_machine_output(program.to_vec(), "bnez");
}

#[test]
fn blez() {
    // blez rs1, offset expands to: bge x0, rs1, offset
    let program = [
        // pc = pc + 8 if [x5] <= 0 (signed)
        bge(0, 5, 8),
    ];
    assert_machine_output(program.to_vec(), "blez");
}

#[test]
fn bgez() {
    // bgez rs1, offset expands to: bge rs1, x0, offset
    let program = [
        // pc = pc + 8 if [x5] >= 0 (signed)
        bge(5, 0, 8),
    ];
    assert_machine_output(program.to_vec(), "bgez");
}

#[test]
fn bltz() {
    // bltz rs1, offset expands to: blt rs1, x0, offset
    let program = [
        // pc = pc + 8 if [x5] < 0 (signed)
        blt(5, 0, 8),
    ];
    assert_machine_output(program.to_vec(), "bltz");
}

#[test]
fn bgtz() {
    // bgtz rs1, offset expands to: blt x0, rs1, offset
    let program = [
        // pc = pc + 8 if [x5] > 0 (signed)
        blt(0, 5, 8),
    ];
    assert_machine_output(program.to_vec(), "bgtz");
}

// Jump pseudo instructions
#[test]
fn j() {
    // j offset expands to: jal x0, offset
    let program = [
        // pc = pc + 8
        jal(0, 0, 8, 1, 0),
    ];
    assert_machine_output(program.to_vec(), "j");
}

#[test]
fn jr() {
    // jr offset expands to: jal x1, offset
    let program = [
        // pc = pc + 8, [x1] = pc + 4
        jal(1, 0, 8, 1, 0),
    ];
    assert_machine_output(program.to_vec(), "jr");
}

#[test]
fn ret() {
    // ret expands to: jalr x0, x1, 0
    let program = [
        // pc = [x1] + 0
        jalr(0, 1, 0, 1, 0),
    ];
    assert_machine_output(program.to_vec(), "ret");
}

#[test]
fn load_immediate() {
    // [x48] = [x0] + 216 = 216
    let program = [add(48, 0, 216, 0)];
    assert_machine_output(program.to_vec(), "load_immediate");
}
