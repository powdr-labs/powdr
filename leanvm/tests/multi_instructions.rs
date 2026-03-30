mod common;

use powdr_autoprecompiles::blocks::BasicBlock;
use powdr_leanvm::instruction::LeanVmInstruction;
use powdr_leanvm::*;
use powdr_number::BabyBearField;
use test_log::test;

fn assert_machine_output(program: Vec<LeanVmInstruction<BabyBearField>>, test_name: &str) {
    let bb = BasicBlock {
        start_pc: 0,
        instructions: program,
    };
    common::assert_machine_output(bb.into(), "multi_instructions", test_name);
}

/// Two ADDs reading the same operand (fp+3).
/// The WOM optimizer should detect that both instructions access m[fp+3]
/// and remove one of the memory bus interactions, replacing it with an
/// equality constraint.
#[test]
fn two_adds_shared_operand() {
    let program = [
        // m[fp+5] = m[fp+3] + m[fp+4]
        add(5, 3, 4, false, false, false),
        // m[fp+6] = m[fp+3] + m[fp+7]
        add(6, 3, 7, false, false, false),
    ];
    assert_machine_output(program.to_vec(), "two_adds_shared_operand");
}

/// ADD followed by MUL, both reading m[fp+3] and m[fp+4].
/// Two shared addresses => WOM should remove 2 memory interactions.
#[test]
fn add_then_mul_shared_operands() {
    let program = [
        // m[fp+5] = m[fp+3] + m[fp+4]
        add(5, 3, 4, false, false, false),
        // m[fp+6] = m[fp+3] * m[fp+4]
        mul(6, 3, 4, false, false, false),
    ];
    assert_machine_output(program.to_vec(), "add_then_mul_shared_operands");
}

/// ADD writing to m[fp+5], then a second ADD reading m[fp+5].
/// The output of the first becomes the input of the second —
/// the WOM optimizer should unify them.
#[test]
fn chained_adds() {
    let program = [
        // m[fp+5] = m[fp+3] + m[fp+4]  (writes addr_A = fp+5)
        add(5, 3, 4, false, false, false),
        // m[fp+6] = m[fp+5] + m[fp+7]  (reads addr_B = fp+5)
        add(6, 5, 7, false, false, false),
    ];
    assert_machine_output(program.to_vec(), "chained_adds");
}

/// Three instructions all reading m[fp+2]: ADD, MUL, DEREF.
/// Should show maximal WOM deduplication.
#[test]
fn three_instructions_same_read() {
    let program = [
        // m[fp+5] = m[fp+2] + m[fp+3]
        add(5, 2, 3, false, false, false),
        // m[fp+6] = m[fp+2] * m[fp+4]
        mul(6, 2, 4, false, false, false),
        // m[m[fp+2] + 1] = m[fp+7]
        deref(2, 1, 7, false, false),
    ];
    assert_machine_output(program.to_vec(), "three_instructions_same_read");
}
