mod common;
use openvm_circuit::system::program;
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

// Reth blocks, taken from:
// https://georgwiese.github.io/autoprecompile-analyzer/?data=https%3A%2F%2Fgithub.com%2Fpowdr-labs%2Fbench-results%2Fblob%2Fgh-pages%2Fresults%2F2025-09-25-0815%2Freth%2Fapc_candidates.json

#[test]
fn aligned_memcpy() {
    // Block 0x200a1c of the Reth benchmark.
    // => 1.1B trace cells, executed 986.1K times, effectiveness 3.48x.
    // C code:
    // https://github.com/kraj/musl/blob/d1c1058ee7a61cf86dc0292590e3f7eb09212d70/src/string/memcpy.c#L27-L30
    // RISC-V assembly:
    // https://github.com/openvm-org/openvm/blob/13362dc64fc2ec6f585018b408061bf56e7b7429/crates/toolchain/openvm/src/memcpy.s#L291-L302
    let program = [
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
        bltu(44, 48, -44),
    ];
    assert_machine_output(program.to_vec(), "aligned_memcpy");
}

#[test]
fn unaligned_memcpy() {
    // Block 0x200914 of the Reth benchmark.
    // => 484.1M trace cells, executed 442.9K times, effectiveness 4.61x.
    // C code:
    // https://github.com/kraj/musl/blob/d1c1058ee7a61cf86dc0292590e3f7eb09212d70/src/string/memcpy.c#L23
    // RISC-V assembly:
    // https://github.com/openvm-org/openvm/blob/13362dc64fc2ec6f585018b408061bf56e7b7429/crates/toolchain/openvm/src/memcpy.s#L220-L232
    // Circuit visualization:
    // https://docs.google.com/drawings/d/1JfLRuoWCyAsN3pht27W6UXUgtE_AiNx6r36lf-cAIfs/edit?usp=sharing
    let program = [
        loadb(68, 44, 0, 2, 1, 0),
        add(56, 44, 1, 0),
        add(52, 64, 1, 0),
        storeb(68, 64, 0, 2, 1, 0),
        add(48, 48, 16777215, 0),
        and(44, 60, 3, 0),
        sltu(44, 0, 44, 1),
        sltu(64, 0, 48, 1),
        and(68, 44, 64, 1),
        add(60, 60, 1, 0),
        add(44, 56, 0, 0),
        add(64, 52, 0, 0),
        bne(68, 0, -48),
    ];
    assert_machine_output(program.to_vec(), "unaligned_memcpy");
}

#[test]
fn load_two_bytes_compare() {
    // Block 0x3bc8fc of the Reth benchmark.
    // => 70.3M trace cells, executed 293k times, especially ineffective (1.85x reduction).
    let program = [
        loadb(52, 40, 0, 2, 1, 0),
        loadb(56, 44, 0, 2, 1, 0),
        bne(52, 56, 28),
    ];
    assert_machine_output(program.to_vec(), "load_two_bytes_compare");
}

#[test]
fn store_to_same_address() {
    // Store two different values to the same memory address.
    // The memory optimizer should realize the two memory addresses are the same,
    // and eliminate creating two separate memory columns.
    let program = [storeb(4, 8, 8, 2, 1, 0), storeb(32, 8, 8, 2, 1, 0)];
    assert_machine_output(program.to_vec(), "store_to_same_memory_address");
}

#[test]
fn many_stores_relative_to_same_register() {
    // Many stores to different offsets relative to the same base register.
    // For a real-world example of something similar, see:
    // https://georgwiese.github.io/autoprecompile-analyzer/?data=https%3A%2F%2Fgist.githubusercontent.com%2Fgeorgwiese%2Faa85dcc145f26d37f8f03f9a04665971%2Fraw%2F6ce661ec86302d2fef0282908117c0427d9888db%2Freth_with_labels.json&block=0x260648

    let program = [
        storew(5, 2, 12, 2, 1, 0),
        storew(6, 2, 16, 2, 1, 0),
        storew(7, 2, 20, 2, 1, 0),
    ];
    assert_machine_output(program.to_vec(), "many_stores_relative_to_same_register");
}
