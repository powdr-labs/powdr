use std::fmt::Write;
use std::path::Path;

use lean_vm::Bytecode;
use powdr_autoprecompiles::adapter::Adapter;
use powdr_autoprecompiles::blocks::{collect_basic_blocks, BasicBlock, SuperBlock};
use powdr_autoprecompiles::evaluation::AirStats;
use powdr_leanvm::instruction::LeanVmInstruction;
use powdr_leanvm::instruction_handler::LeanVmInstructionHandler;
use powdr_leanvm::{
    leanvm_bus_map, test_utils, LeanVmAdapter, LeanVmProgram, DEFAULT_DEGREE_BOUND,
};
use rayon::prelude::*;

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

/// Assert that the contents of a file match `actual`, or create/update it
/// when `UPDATE_EXPECT=1` is set.
#[allow(dead_code)]
pub fn assert_file_snapshot(path: &Path, actual: &str, label: &str) {
    let should_update = std::env::var("UPDATE_EXPECT")
        .map(|v| v.as_str() == "1")
        .unwrap_or(false);

    if path.exists() && !should_update {
        let expected = std::fs::read_to_string(path).unwrap();
        pretty_assertions::assert_eq!(
            expected.trim(),
            actual.trim(),
            "{label} does not match. Re-run with UPDATE_EXPECT=1 to update.",
        );
    } else {
        std::fs::create_dir_all(path.parent().unwrap()).unwrap();
        std::fs::write(path, actual).unwrap();
        println!("Snapshot for {label} created at {path:?}. Re-run to confirm.");
    }
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

/// Result of building a single basic block into an APC.
#[allow(dead_code)]
pub struct BlockBuildResult {
    pub start_pc: u64,
    pub num_instructions: usize,
    pub pre_opt: AirStats,
    pub post_opt: AirStats,
    /// The full APC snapshot (instructions + evaluation + machine).
    pub snapshot: String,
}

impl BlockBuildResult {
    /// Column effectiveness: columns_before / columns_after.
    #[allow(dead_code)]
    pub fn effectiveness(&self) -> Option<f64> {
        (self.post_opt.main_columns > 0)
            .then(|| self.pre_opt.main_columns as f64 / self.post_opt.main_columns as f64)
    }
}

/// Build all blocks in parallel using `compile_apc_with`, returning stats and
/// the full snapshot for each. Blocks that fail to build are `None`.
#[allow(dead_code)]
pub fn build_blocks_parallel(
    blocks: &[BasicBlock<LeanVmInstruction>],
) -> Vec<Option<BlockBuildResult>> {
    let instruction_handler = LeanVmInstructionHandler::new(DEFAULT_DEGREE_BOUND);
    let bus_map = leanvm_bus_map();

    blocks
        .par_iter()
        .map(|bb| {
            let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                test_utils::compile_apc_with(bb.clone().into(), &instruction_handler, &bus_map)
            }))
            .ok()?;

            Some(BlockBuildResult {
                start_pc: bb.start_pc,
                num_instructions: bb.instructions.len(),
                pre_opt: result.pre_opt,
                post_opt: result.post_opt,
                snapshot: result.snapshot,
            })
        })
        .collect()
}

/// Format a CSV with per-block stats. `id_header` and `id_fn` control the
/// identifier columns that precede the stats columns.
#[allow(dead_code)]
pub fn build_stats_csv(
    blocks: &[BasicBlock<LeanVmInstruction>],
    results: &[Option<BlockBuildResult>],
    id_header: &str,
    id_fn: impl Fn(usize, &BasicBlock<LeanVmInstruction>) -> String,
) -> String {
    let mut csv = format!(
        "{id_header},columns_before,constraints_before,bus_interactions_before,columns_after,constraints_after,bus_interactions_after\n"
    );
    for (i, (bb, r)) in blocks.iter().zip(results).enumerate() {
        let id = id_fn(i, bb);
        match r {
            Some(r) => {
                writeln!(
                    csv,
                    "{id},{},{},{},{},{},{}",
                    r.pre_opt.main_columns,
                    r.pre_opt.constraints,
                    r.pre_opt.bus_interactions,
                    r.post_opt.main_columns,
                    r.post_opt.constraints,
                    r.post_opt.bus_interactions,
                )
                .unwrap();
            }
            None => {
                writeln!(csv, "{id},0,0,0,0,0,0").unwrap();
            }
        }
    }
    csv
}
