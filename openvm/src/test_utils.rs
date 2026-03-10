use itertools::Itertools;
use openvm_instructions::instruction::Instruction;
use openvm_stark_sdk::p3_baby_bear::BabyBear;
use powdr_autoprecompiles::blocks::SuperBlock;
use powdr_autoprecompiles::empirical_constraints::EmpiricalConstraints;
use powdr_autoprecompiles::evaluation::evaluate_apc;
use powdr_autoprecompiles::export::ExportOptions;
use powdr_autoprecompiles::{build, VmConfig};
use powdr_number::BabyBearField;
use std::fs;
use std::path::Path;

use crate::extraction_utils::OriginalVmConfig;
use crate::isa::OpenVmISA;
use crate::{BabyBearOpenVmApcAdapter, Instr, DEFAULT_DEGREE_BOUND};
use powdr_openvm_bus_interaction_handler::OpenVmBusInteractionHandler;

/// Compile a superblock into an APC snapshot string.
///
/// This builds the APC, evaluates it, and returns a formatted string containing
/// the instructions, evaluation stats, and machine rendering.
pub fn compile_apc<ISA: OpenVmISA>(
    original_config: &OriginalVmConfig<ISA>,
    superblock: SuperBlock<Instruction<BabyBear>>,
) -> String {
    let degree_bound = DEFAULT_DEGREE_BOUND;
    let airs = original_config.airs(degree_bound).unwrap();
    let bus_map = original_config.bus_map();

    let vm_config = VmConfig {
        instruction_handler: &airs,
        bus_interaction_handler: OpenVmBusInteractionHandler::<BabyBearField>::default(),
        bus_map: bus_map.clone(),
    };

    let superblock = superblock.map_instructions(Instr::<BabyBear, ISA>::from);
    // for aligning the output
    let max_pc_digits = superblock.pcs().max().unwrap().max(1).ilog10() as usize + 1;
    let superblock_str = superblock
        .instructions()
        .zip(superblock.pcs())
        .map(|(inst, pc)| {
            format!(
                "  {pc:>max_pc_digits$}: {}",
                ISA::format(&inst.inner)
            )
        })
        .join("\n");

    let export_path = std::env::var("APC_EXPORT_PATH").ok();
    let export_level = std::env::var("APC_EXPORT_LEVEL").ok();

    let apc = build::<BabyBearOpenVmApcAdapter<ISA>>(
        superblock.clone(),
        vm_config.clone(),
        degree_bound,
        ExportOptions::from_env_vars(export_path, export_level, &superblock.start_pcs()),
        &EmpiricalConstraints::default(),
    )
    .unwrap();

    let apc_with_stats =
        evaluate_apc::<BabyBearOpenVmApcAdapter<ISA>>(vm_config.instruction_handler, apc);

    let evaluation = apc_with_stats.evaluation_result();
    let apc = &apc_with_stats.apc().machine;

    format!(
        "Instructions:\n{superblock_str}\n\n{evaluation}\n\n{}",
        apc.render(&bus_map)
    )
}

/// Assert that the APC output for a superblock matches the expected snapshot.
///
/// - `snapshot_base_dir`: The base directory for snapshot files (typically
///   `Path::new(env!("CARGO_MANIFEST_DIR")).join("tests").join("apc_snapshots")`).
/// - `module_name`: Subdirectory within the snapshot dir (e.g., "single_instructions").
/// - `test_name`: Name of the test, used as the snapshot filename (without extension).
///
/// Set the `UPDATE_EXPECT=1` environment variable to update snapshot files.
pub fn assert_apc_snapshot(
    actual: &str,
    snapshot_base_dir: &Path,
    module_name: &str,
    test_name: &str,
) {
    let expected_path = snapshot_base_dir
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
            pretty_assertions::assert_eq!(
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
            fs::write(&expected_path, actual).unwrap();
            println!(
                "Expected output for `{test_name}` was created. Re-run the test to confirm."
            );
        }
    }
}

/// Convenience function combining [`compile_apc`] and [`assert_apc_snapshot`].
pub fn assert_apc_machine_output<ISA: OpenVmISA>(
    original_config: &OriginalVmConfig<ISA>,
    program: SuperBlock<Instruction<BabyBear>>,
    snapshot_base_dir: &Path,
    module_name: &str,
    test_name: &str,
) {
    let actual = compile_apc::<ISA>(original_config, program);
    assert_apc_snapshot(&actual, snapshot_base_dir, module_name, test_name);
}
