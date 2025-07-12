use openvm_instructions::instruction::Instruction;
use openvm_sdk::config::SdkVmConfig;
use openvm_stark_sdk::p3_baby_bear::BabyBear;
use powdr_autoprecompiles::{build, BasicBlock, DegreeBound, VmConfig};
use powdr_number::BabyBearField;
use powdr_openvm::bus_interaction_handler::OpenVmBusInteractionHandler;
use powdr_openvm::extraction_utils::OriginalVmConfig;
use powdr_openvm::BabyBearOpenVmApcAdapter;
use powdr_openvm::Instr;
use powdr_openvm::{bus_map::default_openvm_bus_map, OPENVM_DEGREE_BOUND, POWDR_OPCODE};
use pretty_assertions::assert_eq;
use std::fs;
use std::path::Path;

// A wrapper that only creates necessary inputs for and then runs powdr_autoprecompile::build
fn compile(program: Vec<Instruction<BabyBear>>) -> String {
    let sdk_vm_config = SdkVmConfig::builder()
        .system(Default::default())
        .rv32i(Default::default())
        .rv32m(Default::default())
        .io(Default::default())
        .build();

    let original_config = OriginalVmConfig::new(sdk_vm_config);

    let airs = original_config.airs().unwrap();
    let bus_map = original_config.bus_map();

    let vm_config = VmConfig {
        instruction_machine_handler: &airs,
        bus_interaction_handler: OpenVmBusInteractionHandler::<BabyBearField>::new(
            default_openvm_bus_map(),
        ),
        bus_map: bus_map.clone(),
    };

    let degree_bound = DegreeBound {
        identities: OPENVM_DEGREE_BOUND,
        bus_interactions: OPENVM_DEGREE_BOUND - 1,
    };

    build::<BabyBearOpenVmApcAdapter>(
        BasicBlock {
            statements: program.into_iter().map(Instr).collect(),
            start_idx: 0,
        },
        vm_config,
        degree_bound,
        POWDR_OPCODE as u32,
        None,
    )
    .unwrap()
    .machine()
    .render(&bus_map)
}

/// Compare `actual` against the contents of the file at `path`.
/// If they differ, write `actual` to the file and fail the test.
fn assert_machine_output(program: Vec<Instruction<BabyBear>>, test_name: &str) {
    let actual = compile(program.to_vec());
    let base = Path::new("tests/apc_builder_outputs");
    let file_path = base.join(format!("{test_name}.txt"));

    match fs::read_to_string(&file_path) {
        Ok(expected) => {
            assert_eq!(
                expected.trim(),
                actual.trim(),
                "The output of `{test_name}` does not match the expected output. \
                 To re-generate the expected output, delete the file `{test_name}.txt` and re-run the test.",
            );
        }
        _ => {
            // Write the new expected output to the file
            fs::create_dir_all(base).unwrap();
            fs::write(&file_path, actual).unwrap();

            println!("Expected output for `{test_name}` was updated. Re-run the test to confirm.");
        }
    }
}

mod single_instruction_tests {
    use crate::assert_machine_output;
    use powdr_openvm::symbolic_instruction_builder::*;

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

    #[test]
    fn guest_top_block() {
        // Top block from `guest` with `--pgo cell`, with 4 instructions:
        // SymbolicInstructionStatement { opcode: 512, args: [8, 8, 16777200, 1, 0, 0, 0] }
        // SymbolicInstructionStatement { opcode: 531, args: [4, 8, 12, 1, 2, 1, 0] }
        // SymbolicInstructionStatement { opcode: 576, args: [4, 0, 0, 1, 0, 0, 0] }
        // SymbolicInstructionStatement { opcode: 565, args: [4, 4, 1780, 1, 0, 1, 0] }

        let program = [
            add(8, 8, 16777200, 0),
            storew(4, 8, 12, 2, 1, 0),
            auipc(4, 0, 0, 1, 0),
            jalr(4, 4, 1780, 1, 0),
        ];

        assert_machine_output(program.to_vec(), "guest_top_block");
    }
}
