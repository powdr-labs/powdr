mod common;

use std::collections::BTreeMap;

use lean_compiler::{compile_program, ProgramSource};
use lean_vm::{
    Bytecode, Instruction, Label, MemOrConstant, MemOrFpOrConstant, Operation, F,
    NONRESERVED_PROGRAM_INPUT_START,
};
use lean_vm_backend::{PrimeCharacteristicRing, DIGEST_ELEMS};
use test_log::test;

/// Compile a program, extract basic blocks, and snapshot each one.
fn compile_and_snapshot(program: &str, test_prefix: &str) {
    let bytecode = compile_program(&ProgramSource::Raw(program.to_string()));
    let blocks = common::extract_basic_blocks(&bytecode);
    assert!(!blocks.is_empty(), "no basic blocks extracted");
    for (i, bb) in blocks.into_iter().enumerate() {
        let test_name = format!("{test_prefix}_block_{i}");
        common::assert_machine_output(bb.into(), "compiled_programs", &test_name);
    }
}

#[test]
fn simple_arithmetic() {
    compile_and_snapshot(
        r#"
def main():
    a = 3
    b = 5
    c = a + b
    d = a * b
    return
"#,
        "simple_arithmetic",
    );
}

#[test]
fn conditional() {
    compile_and_snapshot(
        r#"
def main():
    x = 10
    y: Mut = 0
    if x == 10:
        y = x + 1
    else:
        y = x * 2
    return
"#,
        "conditional",
    );
}

#[test]
fn loop_unrolled() {
    compile_and_snapshot(
        r#"
def main():
    acc: Mut = 0
    for i in unroll(0, 5):
        acc = acc + i
    return
"#,
        "loop_unrolled",
    );
}

#[test]
fn function_call() {
    compile_and_snapshot(
        r#"
def main():
    a = NONRESERVED_PROGRAM_INPUT_START
    b = a + 8
    c = add_arrays(a, b)
    return

def add_arrays(x, y):
    result = Array(8)
    for i in unroll(0, 8):
        result[i] = x[i] + y[i]
    return result
"#,
        "function_call",
    );
}

/// Build bytecode for u32 add verification program.
///
/// u32 add: c = a + b (wrapping)
/// - a, b, c represented by 16-bit limbs: lo (bits 0-15) and hi (bits 16-31)
///
/// Constraints (from rv32_add.md, with divisor corrected to 2^16):
///   x_1 = a_1 + b_1
///   x_2 = x_1 - c_1
///   carry_1 = x_2 / 65536        (field division)
///   carry_1² = carry_1           (constrains carry to 0 or 1)
///   x_3 = a_2 + b_2
///   x_4 = x_3 + carry_1
///   x_5 = x_4 - c_2
///   carry_2 = x_5 / 65536
///   carry_2² = carry_2
///   range_check(c_1, 2^16)
///   range_check(c_2, 2^16)
///
/// Public inputs: a_1, a_2, b_1, b_2, c_1, c_2
fn build_u32_add_bytecode() -> Bytecode {
    let const_pub_ptr = MemOrConstant::Constant(F::new(NONRESERVED_PROGRAM_INPUT_START as u32));
    let const_65536 = MemOrConstant::Constant(F::new(65536)); // 2^16
    let const_bound = MemOrConstant::Constant(F::new(65535)); // 2^16 - 1

    // Memory layout (fp-relative):
    // fp+0: public input pointer
    // fp+1-4: a_1, a_2, b_1, b_2
    // fp+5-6: c_1, c_2
    // fp+7: x_1 = a_1 + b_1
    // fp+8: x_2 = x_1 - c_1
    // fp+9: carry_1 (assertion: carry_1² written to same cell)
    // fp+10: x_3 = a_2 + b_2
    // fp+11: x_4 = x_3 + carry_1
    // fp+12: x_5 = x_4 - c_2
    // fp+13: carry_2 (assertion: carry_2² written to same cell)
    // fp+14-16: range check aux for c_1
    // fp+17-19: range check aux for c_2

    let instructions = vec![
        // pc=0: halt instruction (jumped to at end)
        Instruction::Jump {
            condition: MemOrConstant::one(),
            label: Label::EndProgram,
            dest: MemOrConstant::Constant(F::ZERO),
            updated_fp: MemOrFpOrConstant::Constant(F::ZERO),
        },
        // === Setup pointer ===
        // pc=1: store public input pointer to fp+0
        Instruction::Computation {
            operation: Operation::Add,
            arg_a: MemOrConstant::zero(),
            arg_c: MemOrFpOrConstant::MemoryAfterFp { offset: 0 },
            res: const_pub_ptr,
        },
        // === Load all inputs from public memory ===
        // pc=2-7: load a_1, a_2, b_1, b_2, c_1, c_2
        Instruction::Deref {
            shift_0: 0,
            shift_1: 0,
            res: MemOrFpOrConstant::MemoryAfterFp { offset: 1 }, // a_1
        },
        Instruction::Deref {
            shift_0: 0,
            shift_1: 1,
            res: MemOrFpOrConstant::MemoryAfterFp { offset: 2 }, // a_2
        },
        Instruction::Deref {
            shift_0: 0,
            shift_1: 2,
            res: MemOrFpOrConstant::MemoryAfterFp { offset: 3 }, // b_1
        },
        Instruction::Deref {
            shift_0: 0,
            shift_1: 3,
            res: MemOrFpOrConstant::MemoryAfterFp { offset: 4 }, // b_2
        },
        Instruction::Deref {
            shift_0: 0,
            shift_1: 4,
            res: MemOrFpOrConstant::MemoryAfterFp { offset: 5 }, // c_1
        },
        Instruction::Deref {
            shift_0: 0,
            shift_1: 5,
            res: MemOrFpOrConstant::MemoryAfterFp { offset: 6 }, // c_2
        },
        // === Low limb constraints ===
        // pc=8: x_1 = a_1 + b_1
        Instruction::Computation {
            operation: Operation::Add,
            arg_a: MemOrConstant::MemoryAfterFp { offset: 1 }, // a_1
            arg_c: MemOrFpOrConstant::MemoryAfterFp { offset: 3 }, // b_1
            res: MemOrConstant::MemoryAfterFp { offset: 7 },   // x_1
        },
        // pc=9: x_2 = x_1 - c_1  (rewrite as: c_1 + x_2 = x_1)
        Instruction::Computation {
            operation: Operation::Add,
            arg_a: MemOrConstant::MemoryAfterFp { offset: 5 }, // c_1
            arg_c: MemOrFpOrConstant::MemoryAfterFp { offset: 8 }, // x_2 (solved)
            res: MemOrConstant::MemoryAfterFp { offset: 7 },   // x_1
        },
        // pc=10: carry_1 = x_2 / 65536  (rewrite as: carry_1 * 65536 = x_2)
        Instruction::Computation {
            operation: Operation::Mul,
            arg_a: const_65536,
            arg_c: MemOrFpOrConstant::MemoryAfterFp { offset: 9 }, // carry_1 (solved)
            res: MemOrConstant::MemoryAfterFp { offset: 8 },       // x_2
        },
        // pc=11: assert carry_1² = carry_1
        Instruction::Computation {
            operation: Operation::Mul,
            arg_a: MemOrConstant::MemoryAfterFp { offset: 9 }, // carry_1
            arg_c: MemOrFpOrConstant::MemoryAfterFp { offset: 9 }, // carry_1
            res: MemOrConstant::MemoryAfterFp { offset: 9 },   // must equal carry_1
        },
        // === High limb constraints ===
        // pc=12: x_3 = a_2 + b_2
        Instruction::Computation {
            operation: Operation::Add,
            arg_a: MemOrConstant::MemoryAfterFp { offset: 2 }, // a_2
            arg_c: MemOrFpOrConstant::MemoryAfterFp { offset: 4 }, // b_2
            res: MemOrConstant::MemoryAfterFp { offset: 10 },  // x_3
        },
        // pc=13: x_4 = x_3 + carry_1
        Instruction::Computation {
            operation: Operation::Add,
            arg_a: MemOrConstant::MemoryAfterFp { offset: 10 }, // x_3
            arg_c: MemOrFpOrConstant::MemoryAfterFp { offset: 9 }, // carry_1
            res: MemOrConstant::MemoryAfterFp { offset: 11 },   // x_4
        },
        // pc=14: x_5 = x_4 - c_2  (rewrite as: c_2 + x_5 = x_4)
        Instruction::Computation {
            operation: Operation::Add,
            arg_a: MemOrConstant::MemoryAfterFp { offset: 6 }, // c_2
            arg_c: MemOrFpOrConstant::MemoryAfterFp { offset: 12 }, // x_5 (solved)
            res: MemOrConstant::MemoryAfterFp { offset: 11 },  // x_4
        },
        // pc=15: carry_2 = x_5 / 65536
        Instruction::Computation {
            operation: Operation::Mul,
            arg_a: const_65536,
            arg_c: MemOrFpOrConstant::MemoryAfterFp { offset: 13 }, // carry_2 (solved)
            res: MemOrConstant::MemoryAfterFp { offset: 12 },       // x_5
        },
        // pc=16: assert carry_2² = carry_2
        Instruction::Computation {
            operation: Operation::Mul,
            arg_a: MemOrConstant::MemoryAfterFp { offset: 13 }, // carry_2
            arg_c: MemOrFpOrConstant::MemoryAfterFp { offset: 13 }, // carry_2
            res: MemOrConstant::MemoryAfterFp { offset: 13 },   // must equal carry_2
        },
        // === Range check c_1 <= 65535 ===
        // pc=17: DEREF proves c_1 < M
        Instruction::Deref {
            shift_0: 5,
            shift_1: 0,
            res: MemOrFpOrConstant::MemoryAfterFp { offset: 14 },
        },
        // pc=18: c_1 + complement = 65535
        Instruction::Computation {
            operation: Operation::Add,
            arg_a: MemOrConstant::MemoryAfterFp { offset: 5 },
            arg_c: MemOrFpOrConstant::MemoryAfterFp { offset: 15 },
            res: const_bound,
        },
        // pc=19: DEREF proves complement < M
        Instruction::Deref {
            shift_0: 15,
            shift_1: 0,
            res: MemOrFpOrConstant::MemoryAfterFp { offset: 16 },
        },
        // === Range check c_2 <= 65535 ===
        // pc=20: DEREF proves c_2 < M
        Instruction::Deref {
            shift_0: 6,
            shift_1: 0,
            res: MemOrFpOrConstant::MemoryAfterFp { offset: 17 },
        },
        // pc=21: c_2 + complement = 65535
        Instruction::Computation {
            operation: Operation::Add,
            arg_a: MemOrConstant::MemoryAfterFp { offset: 6 },
            arg_c: MemOrFpOrConstant::MemoryAfterFp { offset: 18 },
            res: const_bound,
        },
        // pc=22: DEREF proves complement < M
        Instruction::Deref {
            shift_0: 18,
            shift_1: 0,
            res: MemOrFpOrConstant::MemoryAfterFp { offset: 19 },
        },
        // pc=23: halt
        Instruction::Jump {
            condition: MemOrConstant::one(),
            label: Label::EndProgram,
            dest: MemOrConstant::Constant(F::ZERO),
            updated_fp: MemOrFpOrConstant::Constant(F::ZERO),
        },
    ];

    Bytecode {
        instructions: instructions.clone(),
        instructions_multilinear: vec![],
        instructions_multilinear_packed: vec![],
        hints: BTreeMap::new(),
        starting_frame_memory: 20,
        hash: [F::ZERO; DIGEST_ELEMS],
        function_locations: BTreeMap::new(),
        filepaths: BTreeMap::new(),
        source_code: BTreeMap::new(),
        pc_to_location: vec![],
    }
}

#[test]
fn u32_add_programmatic() {
    let bytecode = build_u32_add_bytecode();
    let blocks = common::extract_basic_blocks(&bytecode);
    assert!(!blocks.is_empty(), "no basic blocks extracted");
    for (i, bb) in blocks.into_iter().enumerate() {
        let test_name = format!("u32_add_programmatic_block_{i}");
        common::assert_machine_output(bb.into(), "compiled_programs", &test_name);
    }
}
