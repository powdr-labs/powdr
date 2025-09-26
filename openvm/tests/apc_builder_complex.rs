mod common;
use openvm_instructions::instruction::Instruction;
use openvm_stark_sdk::p3_baby_bear::BabyBear;
use powdr_openvm::symbolic_instruction_builder::*;
use test_log::test;

fn assert_machine_output(program: Vec<Instruction<BabyBear>>, test_name: &str) {
    common::apc_builder_utils::assert_machine_output(program, "complex", test_name);
}

#[test]
fn guest_top_block() {
    // Top block from `guest` with `--pgo cell`, with 4 instructions:
    // Instruction { opcode: 512, args: [8, 8, 16777200, 1, 0, 0, 0] }
    // Instruction { opcode: 531, args: [4, 8, 12, 1, 2, 1, 0] }
    // Instruction { opcode: 576, args: [4, 0, 0, 1, 0, 0, 0] }
    // Instruction { opcode: 565, args: [4, 4, 1780, 1, 0, 1, 0] }

    let program = [
        add(8, 8, 16777200, 0),
        storew(4, 8, 12, 2, 1, 0),
        auipc(4, 0, 0, 1, 0),
        jalr(4, 4, 1780, 1, 0),
    ];

    assert_machine_output(program.to_vec(), "guest_top_block");
}

#[test]
fn memcpy_block() {
    // AND rd_ptr = 52, rs1_ptr = 44, rs2 = 3, rs2_as = 0
    // SLTU rd_ptr = 52, rs1_ptr = 52, rs2 = 1, rs2_as = 0
    // SLTU rd_ptr = 56, rs1_ptr = 56, rs2 = 1, rs2_as = 0
    // OR rd_ptr = 52, rs1_ptr = 52, rs2 = 56, rs2_as = 1
    // BNE 52 0 248 1 1

    let program = [
        and(52, 44, 3, 0),
        sltu(52, 52, 1, 0),
        sltu(56, 56, 1, 0),
        or(52, 52, 56, 1),
        bne(52, 0, 248),
    ];

    assert_machine_output(program.to_vec(), "memcpy_block");
}

#[test]
fn stack_accesses() {
    // The memory optimizer should realize that [x2 + 24] is accessed twice,
    // with the same value of x2. Therefore, we can reduce it to just one access.
    let program = [
        // Load [x2 + 20] into x8
        loadw(8, 2, 20, 2, 1, 0),
        // Load [x2 + 24] into x9
        loadw(9, 2, 24, 2, 1, 0),
        // Store [x8] into [x2 + 24]
        storew(8, 2, 24, 2, 1, 0),
    ];

    assert_machine_output(program.to_vec(), "stack_accesses");
}

#[test]
fn load_two_bytes_compare() {
    // Block 0x3bc8fc of the Reth benchmark.
    // => Executed 293k times, especially ineffective (1.85x reduction).
    let program = [
        loadb(52, 40, 0, 2, 1, 0),
        loadb(56, 44, 0, 2, 1, 0),
        bne(52, 56, 28),
    ];
    assert_machine_output(program.to_vec(), "load_two_bytes_compare");
}
