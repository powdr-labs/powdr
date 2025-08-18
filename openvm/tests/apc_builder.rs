use openvm_instructions::instruction::Instruction;
use openvm_sdk::config::SdkVmConfig;
use openvm_stark_sdk::p3_baby_bear::BabyBear;
use powdr_autoprecompiles::evaluation::evaluate_apc;
use powdr_autoprecompiles::{build, BasicBlock, VmConfig};
use powdr_number::BabyBearField;
use powdr_openvm::bus_interaction_handler::OpenVmBusInteractionHandler;
use powdr_openvm::extraction_utils::OriginalVmConfig;
use powdr_openvm::instruction_formatter::openvm_instruction_formatter;
use powdr_openvm::BabyBearOpenVmApcAdapter;
use powdr_openvm::ExtendedVmConfig;
use powdr_openvm::Instr;
use powdr_openvm::DEFAULT_DEGREE_BOUND;
use pretty_assertions::assert_eq;
use std::fs;
use std::path::Path;

// Compiles a basic block to a string, listing the basic block, an evaluation of the APC, and APC constraints.
fn compile(basic_block: Vec<Instruction<BabyBear>>) -> String {
    let sdk_vm_config = SdkVmConfig::builder()
        .system(Default::default())
        .rv32i(Default::default())
        .rv32m(Default::default())
        .io(Default::default())
        .build();

    let ext_vm_config = ExtendedVmConfig { sdk_vm_config };

    let original_config = OriginalVmConfig::new(ext_vm_config);

    let degree_bound = DEFAULT_DEGREE_BOUND;

    let airs = original_config.airs(degree_bound.identities).unwrap();
    let bus_map = original_config.bus_map();

    let vm_config = VmConfig {
        instruction_handler: &airs,
        bus_interaction_handler: OpenVmBusInteractionHandler::<BabyBearField>::default(),
        bus_map: bus_map.clone(),
    };

    let basic_block_str = basic_block
        .iter()
        .map(|inst| format!("  {}", openvm_instruction_formatter(inst)))
        .collect::<Vec<_>>()
        .join("\n");

    let basic_block = BasicBlock {
        statements: basic_block.into_iter().map(Instr).collect(),
        start_pc: 0,
    };

    let apc = build::<BabyBearOpenVmApcAdapter>(basic_block.clone(), vm_config, degree_bound, None)
        .unwrap();
    let apc = apc.machine();

    let evaluation = evaluate_apc(&basic_block.statements, &airs, apc);

    format!(
        "Instructions:\n{basic_block_str}\n\n{evaluation}\n\n{}",
        apc.render(&bus_map)
    )
}

/// Compare `actual` against the contents of the file at `path`.
/// If they differ, write `actual` to the file and fail the test.
fn assert_machine_output(program: Vec<Instruction<BabyBear>>, test_name: &str) {
    let actual = compile(program.to_vec());
    let base = Path::new("tests/apc_builder_outputs");
    let file_path = base.join(format!("{test_name}.txt"));

    let update_expectation = std::env::var("UPDATE_EXPECT")
        .map(|v| v.as_str() == "1")
        .unwrap_or(false);

    let expected = file_path
        .exists()
        .then(|| fs::read_to_string(&file_path).unwrap());

    match (expected, &actual, update_expectation) {
        (Some(expected), _, _) if expected == actual => {
            // Test succeeded.
            return;
        }
        (Some(expected), actual, false) => {
            // The expectation file exists, is different from "actual" and we are
            // not allowed to update it.
            // Test failed.
            assert_eq!(
                expected.trim(),
                actual.trim(),
                "The output of `{test_name}` does not match the expected output. \
                 To overwrite the expected output with the currently generated one, \
                 re-run the test with the environment variable `UPDATE_EXPECT=1` or \
                 delete the file `{test_name}.txt`.",
            );
        }
        _ => {
            // Expectation file does not exist or is different from "actual" and we are allowed to update it.
            fs::create_dir_all(base).unwrap();
            fs::write(&file_path, &actual).unwrap();
            println!("Expected output for `{test_name}` was created. Re-run the test to confirm.");
        }
    }
}

mod single_instruction_tests {
    use crate::assert_machine_output;
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
}

mod complex_tests {
    use crate::assert_machine_output;
    use powdr_openvm::symbolic_instruction_builder::*;
    use test_log::test;

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
}

mod pseudo_instruction_tests {
    use crate::assert_machine_output;
    use powdr_openvm::symbolic_instruction_builder::*;
    use test_log::test;

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
}
