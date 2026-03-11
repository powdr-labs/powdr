use openvm_instructions::instruction::Instruction;
use openvm_sdk_config::SdkVmConfig;
use openvm_stark_sdk::p3_baby_bear::BabyBear;
use powdr_autoprecompiles::blocks::SuperBlock;
use powdr_openvm::extraction_utils::OriginalVmConfig;
use powdr_openvm::test_utils;
use powdr_openvm_riscv::{ExtendedVmConfig, RiscvISA};
use powdr_openvm_riscv_hints_circuit::HintsExtension;
use std::path::Path;

pub fn original_vm_config() -> OriginalVmConfig<RiscvISA> {
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
    use super::*;

    // This code is not dead, but somehow the compiler thinks so.
    #[allow(dead_code)]
    pub fn compile(superblock: SuperBlock<Instruction<BabyBear>>) -> String {
        let original_config = original_vm_config();
        test_utils::compile_apc::<RiscvISA>(&original_config, superblock)
    }

    // This code is not dead, but somehow the compiler thinks so.
    #[allow(dead_code)]
    pub fn assert_machine_output(
        program: SuperBlock<Instruction<BabyBear>>,
        module_name: &str,
        test_name: &str,
    ) {
        let snapshot_dir = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("tests")
            .join("apc_snapshots");
        let original_config = original_vm_config();
        test_utils::assert_apc_machine_output::<RiscvISA>(
            &original_config,
            program,
            &snapshot_dir,
            module_name,
            test_name,
        );
    }
}
