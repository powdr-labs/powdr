pub mod apc_builder_utils {
    use openvm_instructions::instruction::Instruction;
    use openvm_sdk::config::SdkVmConfig;
    use openvm_stark_sdk::p3_baby_bear::BabyBear;
    use powdr_autoprecompiles::blocks::BasicBlock;
    use powdr_autoprecompiles::evaluation::evaluate_apc;
    use powdr_autoprecompiles::{build, VmConfig};
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

    pub fn compile(basic_block: Vec<Instruction<BabyBear>>) -> String {
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

        let apc =
            build::<BabyBearOpenVmApcAdapter>(basic_block.clone(), vm_config, degree_bound, None)
                .unwrap();
        let apc = apc.machine();

        let evaluation = evaluate_apc(&basic_block.statements, &airs, apc);

        format!(
            "Instructions:\n{basic_block_str}\n\n{evaluation}\n\n{}",
            apc.render(&bus_map)
        )
    }

    pub fn assert_machine_output(program: Vec<Instruction<BabyBear>>, test_name: &str) {
        let actual = compile(program.to_vec());
        let base = Path::new("tests/apc_builder_outputs");
        let file_path = base.join(format!("{test_name}.txt"));

        let should_update_expectation = std::env::var("UPDATE_EXPECT")
            .map(|v| v.as_str() == "1")
            .unwrap_or(false);

        let expected = file_path
            .exists()
            .then(|| fs::read_to_string(&file_path).unwrap());

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
                fs::create_dir_all(base).unwrap();
                fs::write(&file_path, &actual).unwrap();
                println!(
                    "Expected output for `{test_name}` was created. Re-run the test to confirm."
                );
            }
        }
    }
}
