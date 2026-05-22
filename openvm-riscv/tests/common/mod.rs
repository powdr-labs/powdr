// Not all test binaries use this submodule, so allow dead_code to avoid
// warnings in binaries that only use `original_vm_config()`.
#[allow(dead_code)]
pub mod apc_builder_utils;

use openvm_sdk_config::SdkVmConfig;
use powdr_openvm::extraction_utils::OriginalVmConfig;
use powdr_openvm_riscv::{ExtendedVmConfig, RiscvISA};
use powdr_openvm_riscv_hints_circuit::HintsExtension;

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
