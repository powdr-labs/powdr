use openvm_sdk::config::SdkVmConfig;
use powdr_autoprecompiles::{build, DegreeBound, SymbolicInstructionStatement, VmConfig};
use powdr_number::BabyBearField;
use powdr_openvm::bus_interaction_handler::OpenVmBusInteractionHandler;
use powdr_openvm::extraction_utils::OriginalVmConfig;
use powdr_openvm::{bus_map::default_openvm_bus_map, OPENVM_DEGREE_BOUND, POWDR_OPCODE};
use pretty_assertions::assert_eq;
use std::fs;
use std::path::Path;

// A wrapper that only creates necessary inputs for and then runs powdr_autoprecompile::build
fn compile(program: Vec<SymbolicInstructionStatement<BabyBearField>>) -> String {
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

    build(program, vm_config, degree_bound, POWDR_OPCODE as u32)
        .unwrap()
        .machine
        .render(&bus_map)
}

/// Compare `actual` against the contents of the file at `path`.
/// If they differ, write `actual` to the file and fail the test.
fn assert_machine_output(
    program: Vec<SymbolicInstructionStatement<BabyBearField>>,
    test_name: &str,
) {
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

    #[test]
    fn single_add_0() {
        let program = [
            // [x8] = [x0] + 5
            add(8, 0, 5, 0),
        ];
        assert_machine_output(program.to_vec(), "single_add_0");
    }

    #[test]
    fn single_loadw() {
        let program = [
            // Load x2 + 20 into x8
            loadw(8, 2, 20, 2, 1, 0),
        ];
        assert_machine_output(program.to_vec(), "single_loadw");
    }

    #[test]
    fn single_loadbu() {
        let program = [
            // Load x2 + 21 into x8
            loadbu(8, 2, 21, 2, 1, 0),
        ];
        assert_machine_output(program.to_vec(), "single_loadbu");
    }

    #[test]
    fn single_srl() {
        // Instruction 416 from the largest basic block of the Keccak guest program.
        let program = [srl(68, 40, 25, 0)];
        assert_machine_output(program.to_vec(), "single_srl");
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
            blt(4, 4, 1780, 1, 0),
        ];

        assert_machine_output(program.to_vec(), "guest_top_block");
    }
}
