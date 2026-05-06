//! On-disk cache of pre-built APCs, keyed by the originating block's start PCs.
//!
//! The cache stores fully-optimized APCs (i.e., the output of [`crate::build`]) so
//! that subsequent runs can skip the expensive constraint optimization stage and
//! jump straight to PGO selection.

use std::path::{Path, PathBuf};

use itertools::Itertools;
use rayon::iter::{IntoParallelIterator, ParallelIterator};

use crate::{
    adapter::{Adapter, AdapterApc},
    blocks::SuperBlock,
    empirical_constraints::EmpiricalConstraints,
    export::{ExportLevel, ExportOptions},
    PowdrConfig,
};

fn cache_file_path(dir: &Path, start_pcs: &[u64]) -> PathBuf {
    let key = start_pcs.iter().join("_");
    dir.join(format!("apc_{key}.cbor"))
}

/// Load a cached APC for the block with the given start PCs.
/// Returns `None` if no cache file exists for this key.
pub fn try_load_apc<A: Adapter>(dir: &Path, start_pcs: &[u64]) -> Option<AdapterApc<A>> {
    let path = cache_file_path(dir, start_pcs);
    if !path.exists() {
        return None;
    }
    let file = std::fs::File::open(&path)
        .unwrap_or_else(|e| panic!("Failed to open APC cache file {}: {e}", path.display()));
    let apc: AdapterApc<A> = serde_cbor::from_reader(std::io::BufReader::new(file))
        .unwrap_or_else(|e| panic!("Failed to deserialize APC from {}: {e}", path.display()));
    Some(apc)
}

/// Persist an APC to the cache directory.
pub fn save_apc<A: Adapter>(dir: &Path, apc: &AdapterApc<A>) {
    std::fs::create_dir_all(dir).expect("Failed to create APC cache directory");
    let path = cache_file_path(dir, &apc.start_pcs());
    let file = std::fs::File::create(&path)
        .unwrap_or_else(|e| panic!("Failed to create APC cache file {}: {e}", path.display()));
    serde_cbor::to_writer(std::io::BufWriter::new(file), apc)
        .unwrap_or_else(|e| panic!("Failed to serialize APC to {}: {e}", path.display()));
}

/// Build APCs for every basic block in `blocks` and write each to
/// `config.apc_candidates_dir_path`. This is the offline step used by the
/// `generate-apcs` CLI command.
pub fn generate_and_cache_apcs<A: Adapter>(
    blocks: Vec<SuperBlock<A::Instruction>>,
    config: &PowdrConfig,
    vm_config: crate::adapter::AdapterVmConfig<A>,
    empirical_constraints: EmpiricalConstraints,
) {
    let output_dir = config
        .apc_candidates_dir_path
        .as_deref()
        .expect("generate_and_cache_apcs requires apc_candidates_dir_path to be set");
    std::fs::create_dir_all(output_dir).expect("Failed to create APC cache directory");

    tracing::info!(
        "Generating and caching APCs for {} blocks into {}",
        blocks.len(),
        output_dir.display()
    );

    let built: usize = blocks
        .into_par_iter()
        .map(|block| {
            let export_options = ExportOptions::new(
                config.apc_candidates_dir_path.clone(),
                &block.start_pcs(),
                ExportLevel::OnlyAPC,
            );
            match crate::build::<A>(
                block.clone(),
                vm_config.clone(),
                config.degree_bound,
                export_options,
                &empirical_constraints,
            ) {
                Ok(apc) => {
                    save_apc::<A>(output_dir, &apc);
                    1
                }
                Err(e) => {
                    tracing::warn!("Skipping APC for block {:?}: {e:?}", block.start_pcs());
                    0
                }
            }
        })
        .sum();

    tracing::info!("Wrote {built} cached APCs to {}", output_dir.display());
}
