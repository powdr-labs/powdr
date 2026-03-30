use powdr_autoprecompiles::blocks::SuperBlock;
use powdr_leanvm::instruction::LeanVmInstruction;
use powdr_leanvm::test_utils;
use powdr_number::BabyBearField;
use std::path::Path;

#[allow(dead_code)]
pub fn assert_machine_output(
    program: SuperBlock<LeanVmInstruction<BabyBearField>>,
    module_name: &str,
    test_name: &str,
) {
    let snapshot_dir = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("apc_snapshots");
    test_utils::assert_apc_machine_output(program, &snapshot_dir, module_name, test_name);
}
