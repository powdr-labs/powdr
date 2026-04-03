use itertools::Itertools;
use std::fs;
use std::path::Path;

use crate::bus_interaction_handler::LeanVmBusInteractionHandler;
use crate::instruction::LeanVmInstruction;
use crate::instruction_handler::{LeanVmInstructionHandler, DEFAULT_DEGREE_BOUND};
use crate::{leanvm_bus_map, LeanVmAdapter, LeanVmCustomBusType};
use powdr_autoprecompiles::blocks::SuperBlock;
use powdr_autoprecompiles::bus_map::BusMap;
use powdr_autoprecompiles::empirical_constraints::EmpiricalConstraints;
use powdr_autoprecompiles::evaluation::{evaluate_apc, AirStats};
use powdr_autoprecompiles::export::ExportOptions;
use powdr_autoprecompiles::{build, VmConfig};

/// Result of compiling a superblock into an APC.
pub struct CompileApcResult {
    /// The full snapshot string (instructions + evaluation + machine).
    pub snapshot: String,
    /// Stats before optimization.
    pub pre_opt: AirStats,
    /// Stats after optimization.
    pub post_opt: AirStats,
}

/// Compile a superblock into an APC using shared resources.
pub fn compile_apc_with(
    superblock: SuperBlock<LeanVmInstruction>,
    instruction_handler: &LeanVmInstructionHandler,
    bus_map: &BusMap<LeanVmCustomBusType>,
) -> CompileApcResult {
    let degree_bound = DEFAULT_DEGREE_BOUND;

    let vm_config = VmConfig {
        instruction_handler,
        bus_interaction_handler: LeanVmBusInteractionHandler,
        bus_map: bus_map.clone(),
    };

    let max_pc_digits = superblock.pcs().max().unwrap().max(1).ilog10() as usize + 1;
    let superblock_str = superblock
        .instructions()
        .map(|(pc, inst)| format!("  {pc:>max_pc_digits$}: {inst}"))
        .join("\n");

    let (apc, pre_opt) = build::<LeanVmAdapter>(
        superblock,
        vm_config.clone(),
        degree_bound,
        ExportOptions::default(),
        &EmpiricalConstraints::default(),
    )
    .unwrap();

    let post_opt = AirStats::new(apc.machine());
    let apc_with_stats = evaluate_apc::<LeanVmAdapter>(vm_config.instruction_handler, apc);

    let evaluation = apc_with_stats.evaluation_result();
    let machine = &apc_with_stats.apc().machine;

    let snapshot = format!(
        "Instructions:\n{superblock_str}\n\n{evaluation}\n\n{}",
        machine.render(bus_map)
    );

    CompileApcResult {
        snapshot,
        pre_opt,
        post_opt,
    }
}

/// Compile a superblock into an APC, returning the full snapshot string.
pub fn compile_apc(superblock: SuperBlock<LeanVmInstruction>) -> String {
    let instruction_handler = LeanVmInstructionHandler::new(DEFAULT_DEGREE_BOUND);
    let bus_map = leanvm_bus_map();
    compile_apc_with(superblock, &instruction_handler, &bus_map).snapshot
}

pub fn assert_apc_snapshot(
    actual: &str,
    snapshot_base_dir: &Path,
    module_name: &str,
    test_name: &str,
) {
    let expected_path = snapshot_base_dir
        .join(module_name)
        .join(format!("{test_name}.txt"));

    let should_update = std::env::var("UPDATE_EXPECT")
        .map(|v| v.as_str() == "1")
        .unwrap_or(false);

    let expected = expected_path
        .exists()
        .then(|| fs::read_to_string(&expected_path).unwrap());

    match (expected, should_update) {
        (Some(expected), _) if expected == actual => {}
        (Some(expected), false) => {
            pretty_assertions::assert_eq!(
                expected.trim(),
                actual.trim(),
                "The output of `{test_name}` does not match the expected output. \
                 To overwrite, re-run with `UPDATE_EXPECT=1` or delete `{test_name}.txt`.",
            );
        }
        _ => {
            fs::create_dir_all(expected_path.parent().unwrap()).unwrap();
            fs::write(&expected_path, actual).unwrap();
            println!("Expected output for `{test_name}` was created. Re-run the test to confirm.");
        }
    }
}

pub fn assert_apc_machine_output(
    program: SuperBlock<LeanVmInstruction>,
    snapshot_base_dir: &Path,
    module_name: &str,
    test_name: &str,
) {
    let actual = compile_apc(program);
    assert_apc_snapshot(&actual, snapshot_base_dir, module_name, test_name);
}
