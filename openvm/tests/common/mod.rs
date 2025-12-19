use openvm_sdk::config::SdkVmConfig;
use powdr_openvm::{extraction_utils::OriginalVmConfig, ExtendedVmConfig};
use powdr_openvm_hints_circuit::HintsExtension;

pub fn original_vm_config() -> OriginalVmConfig {
    let sdk_vm_config = SdkVmConfig::builder()
        .system(Default::default())
        .rv32i(Default::default())
        .rv32m(Default::default())
        .io(Default::default())
        .build();

    let ext_vm_config = ExtendedVmConfig {
        sdk: sdk_vm_config,
        hints: HintsExtension,
    };
    OriginalVmConfig::new(ext_vm_config)
}

pub mod apc_builder_utils {
    use openvm_instructions::instruction::Instruction;
    use openvm_stark_sdk::p3_baby_bear::BabyBear;
    use powdr_autoprecompiles::blocks::Block;
    use powdr_autoprecompiles::evaluation::evaluate_apc;
    use powdr_autoprecompiles::{build, VmConfig};
    use powdr_number::BabyBearField;
    use powdr_openvm::bus_interaction_handler::OpenVmBusInteractionHandler;
    use powdr_openvm::instruction_formatter::openvm_instruction_formatter;
    use powdr_openvm::BabyBearOpenVmApcAdapter;
    use powdr_openvm::Instr;
    use powdr_openvm::DEFAULT_DEGREE_BOUND;
    use pretty_assertions::assert_eq;
    use std::fs;
    use std::path::Path;

    use crate::common::original_vm_config;

    // This code is not dead, but somehow the compiler thinks so.
    #[allow(dead_code)]
    pub fn compile(basic_block: Vec<Instruction<BabyBear>>) -> String {
        let original_config = original_vm_config();
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

        let basic_block = Block {
            statements: basic_block.into_iter().map(Instr).collect(),
            // TODO: could we test superblocks here? need to pass as args
            other_pcs: vec![],
            start_pc: 0,
        };

        // Use this env var to output serialized APCs for tests as well.
        let apc_path_var = std::env::var("APC_CBOR_PATH").ok();
        let apc_path: Option<&Path> = apc_path_var.as_deref().map(Path::new);

        let apc = build::<BabyBearOpenVmApcAdapter>(
            basic_block.clone(),
            vm_config,
            degree_bound,
            apc_path,
        )
        .unwrap();
        let apc = apc.machine();

        let evaluation = evaluate_apc(&basic_block.statements, &airs, apc);

        format!(
            "Instructions:\n{basic_block_str}\n\n{evaluation}\n\n{}",
            apc.render(&bus_map)
        )
    }

    // This code is not dead, but somehow the compiler thinks so.
    #[allow(dead_code)]
    pub fn assert_machine_output(
        program: Vec<Instruction<BabyBear>>,
        module_name: &str,
        test_name: &str,
    ) {
        let actual = compile(program.to_vec());

        let expected_path = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("tests")
            .join("apc_snapshots")
            .join(module_name)
            .join(format!("{test_name}.txt"));

        let should_update_expectation = std::env::var("UPDATE_EXPECT")
            .map(|v| v.as_str() == "1")
            .unwrap_or(false);

        let expected = expected_path
            .exists()
            .then(|| fs::read_to_string(&expected_path).unwrap());

        match (expected, should_update_expectation) {
            (Some(expected), _) if expected == actual => {
                // Test succeeded.
            }
            (Some(expected), false) => {
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
                fs::create_dir_all(expected_path.parent().unwrap()).unwrap();
                fs::write(&expected_path, &actual).unwrap();
                println!(
                    "Expected output for `{test_name}` was created. Re-run the test to confirm."
                );
            }
        }
    }
}
