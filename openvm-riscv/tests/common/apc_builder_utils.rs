use openvm_instructions::instruction::Instruction;
use openvm_stark_sdk::p3_baby_bear::BabyBear;
use powdr_autoprecompiles::blocks::SuperBlock;
use powdr_openvm::test_utils;
use powdr_openvm_riscv::RiscvISA;
use std::path::Path;

pub fn assert_machine_output(
    program: SuperBlock<Instruction<BabyBear>>,
    module_name: &str,
    test_name: &str,
) {
    let snapshot_dir = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("apc_snapshots");
    let original_config = crate::common::original_vm_config();
    test_utils::assert_apc_machine_output::<RiscvISA>(
        &original_config,
        program,
        &snapshot_dir,
        module_name,
        test_name,
    );
}
