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

#[test]
fn memory_copy_block_from_ecc(){
    // Block 0x20a3b0 of the ecc affine hint apc=100
    // => Executed 595.8K times. Effectiveness: 3.39.
//       LOADW rd_rs2_ptr = 60, rs1_ptr = 56, imm = 0, mem_as = 2, needs_write = 1, imm_sign = 0
//   LOADW rd_rs2_ptr = 64, rs1_ptr = 56, imm = 4, mem_as = 2, needs_write = 1, imm_sign = 0
//   LOADW rd_rs2_ptr = 68, rs1_ptr = 56, imm = 8, mem_as = 2, needs_write = 1, imm_sign = 0
//   LOADW rd_rs2_ptr = 20, rs1_ptr = 56, imm = 12, mem_as = 2, needs_write = 1, imm_sign = 0
//   STOREW rd_rs2_ptr = 60, rs1_ptr = 52, imm = 0, mem_as = 2, needs_write = 1, imm_sign = 0
//   STOREW rd_rs2_ptr = 64, rs1_ptr = 52, imm = 4, mem_as = 2, needs_write = 1, imm_sign = 0
//   STOREW rd_rs2_ptr = 68, rs1_ptr = 52, imm = 8, mem_as = 2, needs_write = 1, imm_sign = 0
//   STOREW rd_rs2_ptr = 20, rs1_ptr = 52, imm = 12, mem_as = 2, needs_write = 1, imm_sign = 0
//   ADD rd_ptr = 56, rs1_ptr = 56, rs2 = 16, rs2_as = 0
//   ADD rd_ptr = 48, rs1_ptr = 48, rs2 = 16777200, rs2_as = 0
//   ADD rd_ptr = 52, rs1_ptr = 52, rs2 = 16, rs2_as = 0

    let program =[
        loadw(60, 56, 0, 2, 1, 0),
        loadw(64, 56, 4, 2, 1, 0),
        loadw(68, 56, 8, 2, 1, 0),
        loadw(20, 56, 12, 2, 1, 0),
        storew(60, 52, 0, 2, 1, 0),
        storew(64, 52, 4, 2, 1, 0),
        storew(68, 52, 8, 2, 1, 0),
        storew(20, 52, 12, 2, 1, 0),
        add(56, 56, 16, 0),
        add(48, 48, 16777200, 0),
        add(52, 52, 16, 0),
    ];
    assert_machine_output(program.to_vec(), "memory_copy_block_from_ecc");

}
