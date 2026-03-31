mod common;

use std::collections::BTreeMap;
use std::path::Path;

use lean_compiler::{compile_program, ProgramSource};
use lean_vm::{
    Bytecode, Instruction, Label, MemOrConstant, MemOrFpOrConstant, Operation, F,
    NONRESERVED_PROGRAM_INPUT_START,
};
use lean_vm_backend::{PrimeCharacteristicRing, DIGEST_ELEMS};
use powdr_autoprecompiles::empirical_constraints::EmpiricalConstraints;
use powdr_autoprecompiles::evaluation::AirStats;
use powdr_autoprecompiles::export::ExportOptions;
use powdr_autoprecompiles::{build, VmConfig};
use powdr_leanvm::bus_interaction_handler::LeanVmBusInteractionHandler;
use powdr_leanvm::instruction_handler::LeanVmInstructionHandler;
use powdr_leanvm::{leanvm_bus_map, LeanVmAdapter, DEFAULT_DEGREE_BOUND};
use test_log::test;

/// Compile a program, extract basic blocks, and snapshot each one.
fn compile_and_snapshot(program: &str, test_prefix: &str) {
    let bytecode = compile_program(&ProgramSource::Raw(program.to_string()));
    for instr in &bytecode.instructions {
        println!("{instr}");
    }
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
fn factorial() {
    compile_and_snapshot(
        r#"
def main():
    n = 16
    res = Array(n)
    for i in parallel_range(0, n):
        res[i] = factorial(10000)
    sum: Mut = 0
    for i in range(0, n):
        sum = sum + res[i]
    print(sum)
    return

def factorial(n):
    if n == 0:
        return 1
    else:
        return n * factorial(n - 1)
"#,
        "factorial",
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

#[test]
fn pow() {
    compile_and_snapshot(
        r#"
def main():
    input = NONRESERVED_PROGRAM_INPUT_START
    a = input[0]
    b = a * a
    c = b * b
    d = c * c
    e = d * d

    return
"#,
        "pow",
    );
}

#[test]
fn poseidon1() {
    // Unrolled implementation of Poseidon1
    compile_and_snapshot(
        r#"
WIDTH = 16
HALF_FULL_ROUNDS = 4
PARTIAL_ROUNDS = 20
N_ROUNDS = 28
MDS_COL = [1, 3, 13, 22, 67, 2, 15, 63, 101, 1, 2, 17, 11, 1, 51, 1]

def main():
    input_ptr = NONRESERVED_PROGRAM_INPUT_START
    rc_ptr = input_ptr + WIDTH

    # Allocate state buffers: (N_ROUNDS + 1) * WIDTH
    # buf[0..16] = initial state, buf[16..32] = after round 0, etc.
    buf = Array((N_ROUNDS + 1) * WIDTH)
    for i in unroll(0, WIDTH):
        buf[i] = input_ptr[i]

    # Initial full rounds (rounds 0-3)
    for r in unroll(0, HALF_FULL_ROUNDS):
        full_round(buf + r * WIDTH, buf + (r + 1) * WIDTH, rc_ptr + r * WIDTH)

    # Partial rounds (rounds 4-23)
    for r in unroll(HALF_FULL_ROUNDS, HALF_FULL_ROUNDS + PARTIAL_ROUNDS):
        partial_round(buf + r * WIDTH, buf + (r + 1) * WIDTH, rc_ptr + r * WIDTH)

    # Final full rounds (rounds 24-27)
    for r in unroll(HALF_FULL_ROUNDS + PARTIAL_ROUNDS, N_ROUNDS):
        full_round(buf + r * WIDTH, buf + (r + 1) * WIDTH, rc_ptr + r * WIDTH)

    # Compression: add initial state
    final_ptr = buf + N_ROUNDS * WIDTH
    compressed = Array(WIDTH)
    for i in unroll(0, WIDTH):
        compressed[i] = final_ptr[i] + buf[i]

    # Compute precompile result
    precompile_out = Array(8)
    poseidon16_compress(input_ptr, input_ptr + 8, precompile_out)

    # Assert first 8 elements match
    for i in unroll(0, 8):
        assert compressed[i] == precompile_out[i]

    return

def full_round(in_ptr, out_ptr, rc):
    sboxed = Array(WIDTH)
    for i in unroll(0, WIDTH):
        x = in_ptr[i] + rc[i]
        sboxed[i] = x * x * x
    mds(sboxed, out_ptr)
    return

def partial_round(in_ptr, out_ptr, rc):
    tmp = Array(WIDTH)
    x0 = in_ptr[0] + rc[0]
    tmp[0] = x0 * x0 * x0
    for i in unroll(1, WIDTH):
        tmp[i] = in_ptr[i] + rc[i]
    mds(tmp, out_ptr)
    return

def mds(in_ptr, out_ptr):
    for i in unroll(0, WIDTH):
        acc: Mut = 0
        for j in unroll(0, WIDTH):
            acc = acc + in_ptr[j] * MDS_COL[(WIDTH + i - j) % WIDTH]
        out_ptr[i] = acc
    return
"#,
        "poseidon1",
    );
}

#[test]
#[ignore = "takes too long"]
fn poseidon1_unrolled() {
    // Unrolled implementation of Poseidon1
    compile_and_snapshot(
        r#"
WIDTH = 16
HALF_FULL_ROUNDS = 4
PARTIAL_ROUNDS = 20
N_ROUNDS = 28
MDS_COL = [1, 3, 13, 22, 67, 2, 15, 63, 101, 1, 2, 17, 11, 1, 51, 1]

def main():
    input_ptr = NONRESERVED_PROGRAM_INPUT_START
    rc_ptr = input_ptr + WIDTH

    buf = Array((N_ROUNDS + 1) * WIDTH)
    for i in unroll(0, WIDTH):
        buf[i] = input_ptr[i]

    # Initial full rounds (rounds 0-3)
    for r in unroll(0, HALF_FULL_ROUNDS):
        sboxed = Array(WIDTH)
        for i in unroll(0, WIDTH):
            x = buf[r * WIDTH + i] + rc_ptr[r * WIDTH + i]
            sboxed[i] = x * x * x
        for i in unroll(0, WIDTH):
            acc: Mut = 0
            for j in unroll(0, WIDTH):
                acc = acc + sboxed[j] * MDS_COL[(WIDTH + i - j) % WIDTH]
            buf[(r + 1) * WIDTH + i] = acc

    # Partial rounds (rounds 4-23)
    for r in unroll(HALF_FULL_ROUNDS, HALF_FULL_ROUNDS + PARTIAL_ROUNDS):
        tmp = Array(WIDTH)
        x0 = buf[r * WIDTH] + rc_ptr[r * WIDTH]
        tmp[0] = x0 * x0 * x0
        for i in unroll(1, WIDTH):
            tmp[i] = buf[r * WIDTH + i] + rc_ptr[r * WIDTH + i]
        for i in unroll(0, WIDTH):
            acc: Mut = 0
            for j in unroll(0, WIDTH):
                acc = acc + tmp[j] * MDS_COL[(WIDTH + i - j) % WIDTH]
            buf[(r + 1) * WIDTH + i] = acc

    # Final full rounds (rounds 24-27)
    for r in unroll(HALF_FULL_ROUNDS + PARTIAL_ROUNDS, N_ROUNDS):
        sboxed = Array(WIDTH)
        for i in unroll(0, WIDTH):
            x = buf[r * WIDTH + i] + rc_ptr[r * WIDTH + i]
            sboxed[i] = x * x * x
        for i in unroll(0, WIDTH):
            acc: Mut = 0
            for j in unroll(0, WIDTH):
                acc = acc + sboxed[j] * MDS_COL[(WIDTH + i - j) % WIDTH]
            buf[(r + 1) * WIDTH + i] = acc

    # Compression: add initial state
    compressed = Array(WIDTH)
    for i in unroll(0, WIDTH):
        compressed[i] = buf[N_ROUNDS * WIDTH + i] + buf[i]

    # Compute precompile result
    # precompile_out = Array(8)
    # poseidon16_compress(input_ptr, input_ptr + 8, precompile_out)

    # Assert first 8 elements match
    # for i in unroll(0, 8):
    #     assert compressed[i] == precompile_out[i]

    return
"#,
        "poseidon1_unrolled",
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

#[test]
#[ignore = "slow compilation"]
fn aggregation_bytecode() {
    rec_aggregation::init_aggregation_bytecode();
    let bytecode = rec_aggregation::get_aggregation_bytecode();

    let blocks = common::extract_basic_blocks(bytecode);
    assert!(!blocks.is_empty(), "no basic blocks extracted");

    let degree_bound = DEFAULT_DEGREE_BOUND;
    let instruction_handler = LeanVmInstructionHandler::new(degree_bound);
    let bus_map = leanvm_bus_map();

    let mut csv = String::from(
        "start_pc,num_instructions,columns_before,constraints_before,bus_interactions_before,columns_after,constraints_after,bus_interactions_after\n",
    );

    for bb in &blocks {
        let start_pc = bb.start_pc;
        let num_instructions = bb.instructions.len();

        let vm_config = VmConfig {
            instruction_handler: &instruction_handler,
            bus_interaction_handler: LeanVmBusInteractionHandler,
            bus_map: bus_map.clone(),
        };

        let result = build::<LeanVmAdapter>(
            bb.clone().into(),
            vm_config,
            degree_bound,
            ExportOptions::default(),
            &EmpiricalConstraints::default(),
        );

        match result {
            Ok((apc, pre_opt_stats)) => {
                let post_opt_stats = AirStats::new(apc.machine());
                csv.push_str(&format!(
                    "{start_pc},{num_instructions},{},{},{},{},{},{}\n",
                    pre_opt_stats.main_columns,
                    pre_opt_stats.constraints,
                    pre_opt_stats.bus_interactions,
                    post_opt_stats.main_columns,
                    post_opt_stats.constraints,
                    post_opt_stats.bus_interactions,
                ));
            }
            Err(_) => {
                csv.push_str(&format!("{start_pc},{num_instructions},0,0,0,0,0,0\n"));
            }
        }
    }

    let snapshot_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("apc_snapshots")
        .join("compiled_programs")
        .join("aggregation_bytecode.csv");

    let should_update = std::env::var("UPDATE_EXPECT")
        .map(|v| v.as_str() == "1")
        .unwrap_or(false);

    if snapshot_path.exists() && !should_update {
        let expected = std::fs::read_to_string(&snapshot_path).unwrap();
        pretty_assertions::assert_eq!(
            expected.trim(),
            csv.trim(),
            "CSV does not match. Re-run with UPDATE_EXPECT=1 to update.",
        );
    } else {
        std::fs::create_dir_all(snapshot_path.parent().unwrap()).unwrap();
        std::fs::write(&snapshot_path, &csv).unwrap();
        println!("Snapshot created at {snapshot_path:?}. Re-run to confirm.");
    }
}
