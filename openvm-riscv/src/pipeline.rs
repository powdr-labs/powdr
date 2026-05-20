//! Cached `generate-apcs` → `select-apcs` → `setup` pipeline.
//!
//! [`StagedPipeline`] is the shared runner used by both the CLI and external
//! callers (sp1, openvm-eth). Each stage transparently reuses a prior
//! `artifacts_dir` blob; `artifacts_dir = None` disables caching and runs
//! every stage inline.

use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::path::PathBuf;

use powdr_autoprecompiles::adapter::AdapterApcWithStats;
use powdr_autoprecompiles::empirical_constraints::EmpiricalConstraints;
use powdr_autoprecompiles::execution_profile::ExecutionProfile;
use powdr_autoprecompiles::pgo::{default_apc_candidates, pgo_config, PgoType};
use powdr_autoprecompiles::staged_cache::{cached, stage_hash};
use powdr_autoprecompiles::PowdrConfig;
use powdr_openvm::BabyBearOpenVmApcAdapter;

use crate::{
    generate_apcs, select_apcs, setup, CompiledProgram, OriginalCompiledProgram, RiscvISA,
};

/// Output of the generate / select stages.
pub type RankedApcs = Vec<AdapterApcWithStats<BabyBearOpenVmApcAdapter<'static, RiscvISA>>>;

pub struct StagedPipeline {
    guest: OriginalCompiledProgram<'static, RiscvISA>,
    guest_hash: String,
    artifacts_dir: Option<PathBuf>,
}

impl StagedPipeline {
    pub fn new(
        guest: OriginalCompiledProgram<'static, RiscvISA>,
        artifacts_dir: Option<PathBuf>,
    ) -> Self {
        let guest_hash = hash_guest_exe(&guest);
        Self {
            guest,
            guest_hash,
            artifacts_dir,
        }
    }

    pub fn guest(&self) -> &OriginalCompiledProgram<'static, RiscvISA> {
        &self.guest
    }

    pub fn guest_hash(&self) -> &str {
        &self.guest_hash
    }

    pub fn artifacts_dir(&self) -> Option<&std::path::Path> {
        self.artifacts_dir.as_deref()
    }

    /// Build + rank APC candidates, or load the result from the cache.
    ///
    /// `cache_key` is the caller's fingerprint of everything that should
    /// invalidate the cache (config, pgo, max_columns, profile inputs, ...);
    /// it gets mixed with the guest hash. `make_pgo_profile` and
    /// `make_empirical_constraints` only run on a cache miss, so callers may
    /// hide expensive work (RPC fetches, guest execution) behind them. The
    /// `make_pgo_profile` callback owns the policy for how to produce the
    /// profile — typically [`powdr_openvm::execution_profile_from_guest`] for
    /// single-stdin guests, or a custom loop for multi-stdin PGO.
    ///
    /// Applies [`default_apc_candidates`] when `config.apc_candidates` is
    /// unset so the Instruction/None build loop stays bounded.
    pub fn generate_apcs<H: Hash + ?Sized>(
        &self,
        cache_key: &H,
        config: &PowdrConfig,
        pgo: PgoType,
        max_columns: Option<usize>,
        make_pgo_profile: impl FnOnce(&OriginalCompiledProgram<'static, RiscvISA>) -> ExecutionProfile,
        make_empirical_constraints: impl FnOnce() -> EmpiricalConstraints,
    ) -> RankedApcs {
        let hash = self.stage_hash(cache_key);
        cached(self.artifacts_dir.as_deref(), "generate", &hash, || {
            let pgo_cfg = pgo_config(pgo, max_columns, make_pgo_profile(&self.guest));
            let mut config = config.clone();
            if config.apc_candidates.is_none() {
                config.apc_candidates = default_apc_candidates(
                    pgo,
                    config.autoprecompiles,
                    config.skip_autoprecompiles,
                );
            }
            generate_apcs(&self.guest, &config, pgo_cfg, make_empirical_constraints())
        })
    }

    /// Trim a generate-stage ranking to (skip, autoprecompiles), or load
    /// from the cache. `compute_ranked` only runs on a cache miss.
    pub fn select_apcs<H: Hash + ?Sized>(
        &self,
        cache_key: &H,
        config: &PowdrConfig,
        compute_ranked: impl FnOnce() -> RankedApcs,
    ) -> RankedApcs {
        let hash = self.stage_hash(cache_key);
        cached(self.artifacts_dir.as_deref(), "select", &hash, || {
            select_apcs(compute_ranked(), config)
        })
    }

    /// Inject the selected APCs and assemble the final [`CompiledProgram`].
    /// Consumes the pipeline (the guest is moved into `setup`).
    ///
    /// `compute_apcs` receives a borrow of `self` so it can recursively
    /// invoke `select_apcs` / `generate_apcs`, and only runs on a cache miss.
    pub fn setup<H: Hash + ?Sized>(
        self,
        cache_key: &H,
        config: &PowdrConfig,
        compute_apcs: impl FnOnce(&Self) -> RankedApcs,
    ) -> CompiledProgram<RiscvISA> {
        let hash = self.stage_hash(cache_key);
        if let Some(program) = powdr_autoprecompiles::staged_cache::load_cached::<
            CompiledProgram<RiscvISA>,
        >(self.artifacts_dir.as_deref(), "setup", &hash)
        {
            tracing::info!("cache hit: setup/{hash}");
            return program;
        }
        let apcs = compute_apcs(&self);
        let program = setup(self.guest, apcs, config.degree_bound);
        powdr_autoprecompiles::staged_cache::save_cached(
            self.artifacts_dir.as_deref(),
            "setup",
            &hash,
            &program,
        );
        program
    }

    fn stage_hash<H: Hash + ?Sized>(&self, args: &H) -> String {
        stage_hash(args, &self.guest_hash)
    }
}

/// Stable-within-build fingerprint of the transpiled `VmExe`. Captures any
/// guest change (source, deps, toolchain) that would affect downstream
/// stages.
fn hash_guest_exe(guest: &OriginalCompiledProgram<'_, RiscvISA>) -> String {
    let bytes = serde_cbor::to_vec(&*guest.exe).expect("serialize VmExe for hashing");
    let mut hasher = DefaultHasher::new();
    bytes.hash(&mut hasher);
    format!("{:016x}", hasher.finish())
}
