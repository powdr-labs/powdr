use std::path::Path;

use lean_vm::Bytecode;
use powdr_autoprecompiles::adapter::Adapter;
use powdr_autoprecompiles::blocks::{collect_basic_blocks, BasicBlock, SuperBlock};
use powdr_leanvm::instruction::LeanVmInstruction;
use powdr_leanvm::{test_utils, LeanVmAdapter, LeanVmProgram};

#[allow(dead_code)]
pub fn assert_machine_output(
    program: SuperBlock<LeanVmInstruction>,
    module_name: &str,
    test_name: &str,
) {
    let snapshot_dir = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("apc_snapshots");
    test_utils::assert_apc_machine_output(program, &snapshot_dir, module_name, test_name);
}

/// Extract basic blocks from compiled bytecode using the generic block detection.
///
/// Blocks containing disallowed instructions (e.g., `Precompile`) are filtered out.
#[allow(dead_code)]
pub fn extract_basic_blocks(bytecode: &Bytecode) -> Vec<BasicBlock<LeanVmInstruction>> {
    let program = LeanVmProgram::new(bytecode.clone());
    let jumpdest_set = program.jumpdest_set();
    let blocks = collect_basic_blocks::<LeanVmAdapter>(&program, &jumpdest_set);

    // Filter out blocks containing disallowed instructions (they appear as
    // single-instruction blocks from collect_basic_blocks).
    blocks
        .into_iter()
        .filter(|bb| bb.instructions.iter().all(LeanVmAdapter::is_allowed))
        .collect()
}
