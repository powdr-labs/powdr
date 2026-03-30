use itertools::Itertools;
use std::fs;
use std::path::Path;

use powdr_autoprecompiles::blocks::SuperBlock;
use powdr_autoprecompiles::empirical_constraints::EmpiricalConstraints;
use powdr_autoprecompiles::evaluation::evaluate_apc;
use powdr_autoprecompiles::export::ExportOptions;
use powdr_autoprecompiles::{build, VmConfig};
use powdr_number::BabyBearField;

use crate::bus_interaction_handler::LeanVmBusInteractionHandler;
use crate::instruction::LeanVmInstruction;
use crate::instruction_handler::{LeanVmInstructionHandler, DEFAULT_DEGREE_BOUND};
use crate::{leanvm_bus_map, LeanVmAdapter};

pub fn compile_apc(superblock: SuperBlock<LeanVmInstruction<BabyBearField>>) -> String {
    let degree_bound = DEFAULT_DEGREE_BOUND;
    let instruction_handler = LeanVmInstructionHandler::new(degree_bound);
    let bus_map = leanvm_bus_map();

    let vm_config = VmConfig {
        instruction_handler: &instruction_handler,
        bus_interaction_handler: LeanVmBusInteractionHandler,
        bus_map: bus_map.clone(),
    };

    let max_pc_digits = superblock.pcs().max().unwrap().max(1).ilog10() as usize + 1;
    let superblock_str = superblock
        .instructions()
        .map(|(pc, inst)| format!("  {pc:>max_pc_digits$}: {inst}"))
        .join("\n");

    let apc = build::<LeanVmAdapter>(
        superblock,
        vm_config.clone(),
        degree_bound,
        ExportOptions::default(),
        &EmpiricalConstraints::default(),
    )
    .unwrap();

    let apc_with_stats = evaluate_apc::<LeanVmAdapter>(vm_config.instruction_handler, apc);

    let evaluation = apc_with_stats.evaluation_result();
    let apc = &apc_with_stats.apc().machine;

    format!(
        "Instructions:\n{superblock_str}\n\n{evaluation}\n\n{}",
        apc.render(&bus_map)
    )
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
    program: SuperBlock<LeanVmInstruction<BabyBearField>>,
    snapshot_base_dir: &Path,
    module_name: &str,
    test_name: &str,
) {
    let actual = compile_apc(program);
    assert_apc_snapshot(&actual, snapshot_base_dir, module_name, test_name);
}
