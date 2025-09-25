mod common;
use common::apc_builder_utils::assert_machine_output;
use powdr_openvm::symbolic_instruction_builder::*;
use test_log::test;

// ALU Chip instructions
#[test]
fn single_add_0() {
    let program = [
        // [x8] = [x0] + 5
        add(8, 0, 5, 0),
    ];
    assert_machine_output(program.to_vec(), "single_add_0");
}

#[test]
fn single_sub() {
    let program = [
        // [x8] = [x7] - [x5]
        sub(8, 7, 5, 1),
    ];
    assert_machine_output(program.to_vec(), "single_sub");
}

#[test]
fn single_and_0() {
    let program = [
        // [x8] = [x0] & 5
        and(8, 0, 5, 0),
    ];
    assert_machine_output(program.to_vec(), "single_and_0");
}

#[test]
fn single_xor() {
    let program = [
        // [x8] = [x7] ^ [x5]
        xor(8, 7, 5, 1),
    ];
    assert_machine_output(program.to_vec(), "single_xor");
}

#[test]
fn single_mul() {
    let program = [
        // [x8] = [x7] * [x5]
        mul(8, 7, 5, 1, 0),
    ];
    assert_machine_output(program.to_vec(), "single_mul");
}

// Load/Store Chip instructions
// `needs_write` can be 0 iff `rd=0` for load, but must be 1 if store.
#[test]
fn single_loadw() {
    let program = [
        // Load [x2 + 20]_2 into x8
        loadw(8, 2, 20, 2, 1, 0),
    ];
    assert_machine_output(program.to_vec(), "single_loadw");
}

#[test]
fn single_loadbu() {
    let program = [
        // Load [x2 + 21]_2 into x8
        loadbu(8, 2, 21, 2, 1, 0),
    ];
    assert_machine_output(program.to_vec(), "single_loadbu");
}

#[test]
fn single_loadhu() {
    let program = [
        // Load [x2 + 22]_2 but `needs_write=0`
        loadhu(0, 2, 22, 2, 0, 0),
    ];
    assert_machine_output(program.to_vec(), "single_loadhu");
}

#[test]
fn single_storew() {
    let program = [
        // Store [x8] into [x2 - 4]_2
        storew(8, 2, 4, 2, 1, 1),
    ];
    assert_machine_output(program.to_vec(), "single_storew");
}

#[test]
fn single_storeh() {
    let program = [
        // Store [x8] into [x2 - 6]_2
        storeh(8, 2, 6, 2, 1, 1),
    ];
    assert_machine_output(program.to_vec(), "single_storeh");
}

#[test]
fn single_storeb() {
    let program = [
        // Store [x8] into [x2 + 3]_2
        storeb(8, 2, 3, 2, 1, 0),
    ];
    assert_machine_output(program.to_vec(), "single_storeb");
}

// Load/Store Sign Extend Chip instructions
#[test]
fn single_loadh() {
    let program = [
        // Load [x2 + 6]_2 into x8
        loadh(8, 2, 6, 2, 1, 0),
    ];
    assert_machine_output(program.to_vec(), "single_loadh");
}

#[test]
fn single_loadb() {
    let program = [
        // Load [x2 + 3]_2 into x8 but `needs_write=0`
        loadb(0, 2, 3, 2, 0, 0),
    ];
    assert_machine_output(program.to_vec(), "single_loadb");
}

// Branch Eq Chip instructions
#[test]
fn single_beq() {
    let program = [
        // pc = pc + 2 if x8 == x5
        beq(8, 5, 2),
    ];
    assert_machine_output(program.to_vec(), "single_beq");
}

#[test]
fn single_bne() {
    let program = [
        // pc = pc + 2 if x8 != x5
        bne(8, 5, 2),
    ];
    assert_machine_output(program.to_vec(), "single_bne");
}

// Branch Lt Chip instructions
#[test]
fn single_blt() {
    let program = [
        // pc = pc + 2 if x8 < x5 (signed)
        blt(8, 5, 2),
    ];
    assert_machine_output(program.to_vec(), "single_blt");
}

#[test]
fn single_bltu() {
    let program = [
        // pc = pc + 2 if x8 < x5
        bltu(8, 5, 2),
    ];
    assert_machine_output(program.to_vec(), "single_bltu");
}

#[test]
fn single_bge() {
    let program = [
        // pc = pc + 2 if x8 >= x5 (signed)
        bge(8, 5, 2),
    ];
    assert_machine_output(program.to_vec(), "single_bge");
}

#[test]
fn single_bgeu() {
    let program = [
        // pc = pc + 2 if x8 >= x5
        bgeu(8, 5, 2),
    ];
    assert_machine_output(program.to_vec(), "single_bgeu");
}

// Shift Chip instructions
#[test]
fn single_srl() {
    // Instruction 416 from the largest basic block of the Keccak guest program.
    let program = [srl(68, 40, 25, 0)];
    assert_machine_output(program.to_vec(), "single_srl");
}

#[test]
fn single_sll() {
    // r68 = r40 << 3
    let program = [sll(68, 40, 3, 0)];
    assert_machine_output(program.to_vec(), "single_sll");
}

#[test]
fn single_sll_by_8() {
    // r68 = r40 << 8
    let program = [sll(68, 40, 8, 0)];
    assert_machine_output(program.to_vec(), "single_sll_by_8");
}

#[test]
fn single_sra() {
    // r68 = sign_extend(r40 >> val(R3))
    let program = [sra(68, 40, 3, 1)];
    assert_machine_output(program.to_vec(), "single_sra");
}
