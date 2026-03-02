mod common;
use openvm_instructions::instruction::Instruction;
use openvm_stark_sdk::p3_baby_bear::BabyBear;
use powdr_autoprecompiles::blocks::BasicBlock;
use powdr_openvm::symbolic_instruction_builder::*;
use test_log::test;

fn assert_machine_output(program: Vec<BasicBlock<Instruction<BabyBear>>>, test_name: &str) {
    common::apc_builder_utils::assert_machine_output(program.into(), "superblocks", test_name);
}

fn bb(
    start_pc: u64,
    instructions: Vec<Instruction<BabyBear>>,
) -> BasicBlock<Instruction<BabyBear>> {
    BasicBlock {
        start_pc,
        instructions,
    }
}

#[test]
fn beq0_fallthrough() {
    // Superblock where the BEQ instruction falls through to the next instruction.
    // This should enforce that x8 != 0.
    let program = [
        bb(0, vec![beq(8, 0, 40)]),
        // PC=4, fallthrough
        bb(4, vec![add(9, 9, 1, 0)]),
    ];

    assert_machine_output(program.to_vec(), "beq0_fallthrough");
}

#[test]
fn beq0_jump() {
    // Superblock where the BEQ instruction jumps to the given address.
    // This should enforce that x8 == 0.
    let program = [
        bb(0, vec![beq(8, 0, 40)]),
        // PC=40, jump taken
        bb(40, vec![add(9, 9, 1, 0)]),
    ];

    assert_machine_output(program.to_vec(), "beq0_jump");
}

#[test]
fn beq_fallthrough() {
    // Superblock where the BEQ instruction falls through to the next instruction.
    // This should enforce that x8 != x10 (x10 holds 33).
    let program = [
        bb(0, vec![add(10, 0, 33, 0), beq(8, 10, 40)]),
        // PC=8, fallthrough (BEQ at PC=4)
        bb(8, vec![add(9, 9, 1, 0)]),
    ];

    assert_machine_output(program.to_vec(), "beq_fallthrough");
}

#[test]
fn beq_jump() {
    // Superblock where the BEQ instruction jumps to the given address.
    // This should enforce that x8 == x10 (x10 holds 33).
    let program = [
        bb(0, vec![add(10, 0, 33, 0), beq(8, 10, 40)]),
        // PC=44, jump taken (BEQ at PC=4 with imm=40 jumps to PC=44)
        bb(44, vec![add(9, 9, 1, 0)]),
    ];

    assert_machine_output(program.to_vec(), "beq_jump");
}

#[test]
fn many_blocks() {
    // Superblock with 3 basic blocks.
    // Constraints should propagate accross the jump instructions:
    // x10 = 10
    // x11 = x10
    // x12 = x11 + 5 = 15
    // x8 = x12 = 15
    let program = [
        bb(
            0,
            vec![
                add(10, 0, 10, 0), // x10 = 10
                bne(10, 11, 100),
            ],
        ),
        // PC=8, BNE fallthrough (x10 = x11)
        bb(
            8,
            vec![
                add(12, 11, 5, 0), // x12 = x11 + 5 = 15 (known after propagation)
                beq(8, 12, 60),    // PC=12, BEQ jump to PC+60=72
            ],
        ),
        // PC=72, BEQ jump (x8 = x12 = 15)
        bb(72, vec![add(9, 9, 1, 0)]),
    ];

    assert_machine_output(program.to_vec(), "many_blocks");
}
