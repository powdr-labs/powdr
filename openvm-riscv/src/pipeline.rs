//! Cached `generate-apcs` → `select-apcs` → `setup` pipeline.
//!
//! [`StagedPipeline`] is the shared runner used by both the CLI and external
//! callers (sp1, openvm-eth). Each stage transparently reuses a prior
//! `artifacts_dir` blob; `artifacts_dir = None` disables caching and runs
//! every stage inline.
//!
//! ## Cache-key model
//!
//! The library hashes everything it can see — the guest fingerprint,
//! [`GenerateConfig`], and the full [`PgoConfig`] (including its opaque
//! `inputs: Vec<u8>` which the caller uses to fingerprint anything hidden
//! behind the `make_*` closures). Both the generate-stage and select-stage
//! cache keys include the same `(generate, pgo_config)` so that changing the
//! PGO strategy can never collide with a stale `select` blob.
//!
//! Generate's hash deliberately excludes the [`SelectConfig`]; select/setup
//! include it. As long as `generate.apc_candidates` doesn't itself depend on the
//! selection size (the [`GenerateConfig::with_select_defaults`] policy for
//! Cell ensures this), an `--apc N` sweep under `--pgo cell` automatically
//! reuses the generate-stage blob.

use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::path::PathBuf;

use powdr_autoprecompiles::adapter::AdapterApcWithStats;
use powdr_autoprecompiles::empirical_constraints::EmpiricalConstraints;
use powdr_autoprecompiles::execution_profile::ExecutionProfile;
use powdr_autoprecompiles::pgo::pgo_data;
use powdr_autoprecompiles::staged_cache::{cached, stage_hash};
use powdr_autoprecompiles::{GenerateConfig, PgoConfig, SelectConfig};
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

    /// Build + rank APC candidates, or load the result from the cache.
    ///
    /// The library hashes `(guest_hash, generate, pgo_config)`. `pgo_config.inputs`
    /// is the caller's serialized form of whatever determines the
    /// `make_*` closure outputs (typically the guest stdin); the closures
    /// receive those bytes back as `&[u8]` so they're pure functions of
    /// their arguments rather than relying on captured state.
    ///
    /// `make_pgo_profile` and `make_empirical_constraints` only run on a
    /// cache miss, so callers may hide expensive work (RPC fetches, guest
    /// execution) behind them.
    pub fn generate_apcs(
        &self,
        generate: &GenerateConfig,
        pgo_config: &PgoConfig,
        make_pgo_profile: impl FnOnce(
            &OriginalCompiledProgram<'static, RiscvISA>,
            &[u8],
        ) -> ExecutionProfile,
        make_empirical_constraints: impl FnOnce(
            &OriginalCompiledProgram<'static, RiscvISA>,
            &[u8],
        ) -> EmpiricalConstraints,
    ) -> RankedApcs {
        let hash = self.generate_hash(generate, pgo_config);
        cached(self.artifacts_dir.as_deref(), "generate", &hash, || {
            let profile = make_pgo_profile(&self.guest, &pgo_config.inputs);
            let pgo = pgo_data(pgo_config.pgo_type, pgo_config.max_columns, profile);
            let empirical = make_empirical_constraints(&self.guest, &pgo_config.inputs);
            generate_apcs(&self.guest, generate, pgo, empirical)
        })
    }

    /// Trim a generate-stage ranking to `select.autoprecompiles` (after
    /// `select.skip`), or load from the cache.
    ///
    /// The library hashes `(guest_hash, generate, pgo_config, select)` —
    /// including `pgo_config` here too means switching `--pgo cell` ↔
    /// `--pgo instruction` (which changes the upstream ranking even though
    /// the local select inputs are identical) never serves a stale blob.
    /// `compute_ranked` only runs on a cache miss; pass it a closure that
    /// invokes [`Self::generate_apcs`] if the upstream blob might also
    /// need rebuilding.
    pub fn select_apcs(
        &self,
        generate: &GenerateConfig,
        pgo_config: &PgoConfig,
        select: SelectConfig,
        compute_ranked: impl FnOnce() -> RankedApcs,
    ) -> RankedApcs {
        let hash = self.select_hash(generate, pgo_config, select);
        cached(self.artifacts_dir.as_deref(), "select", &hash, || {
            select_apcs(compute_ranked(), select)
        })
    }

    /// Inject the selected APCs and assemble the final [`CompiledProgram`].
    /// Consumes the pipeline (the guest is moved into `setup`).
    ///
    /// The library hashes `(guest_hash, generate, pgo_config, select)`.
    /// `compute_apcs` receives a borrow of `self` so it can recursively
    /// invoke [`Self::select_apcs`] / [`Self::generate_apcs`]; only runs
    /// on a cache miss.
    pub fn setup(
        self,
        generate: &GenerateConfig,
        pgo_config: &PgoConfig,
        select: SelectConfig,
        compute_apcs: impl FnOnce(&Self) -> RankedApcs,
    ) -> CompiledProgram<RiscvISA> {
        // Setup's hash uses the same inputs as select (no extra "setup-only"
        // fields exist today). Distinguishing under a different stage name is
        // enough to keep the blobs on disk separate.
        let hash = self.select_hash(generate, pgo_config, select);
        if let Some(program) = powdr_autoprecompiles::staged_cache::load_cached::<
            CompiledProgram<RiscvISA>,
        >(self.artifacts_dir.as_deref(), "setup", &hash)
        {
            tracing::info!("cache hit: setup/{hash}");
            return program;
        }
        let apcs = compute_apcs(&self);
        let program = setup(self.guest, apcs, generate.degree_bound);
        powdr_autoprecompiles::staged_cache::save_cached(
            self.artifacts_dir.as_deref(),
            "setup",
            &hash,
            &program,
        );
        program
    }

    fn generate_hash(&self, generate: &GenerateConfig, pgo_config: &PgoConfig) -> String {
        stage_hash(&(generate, pgo_config), &self.guest_hash)
    }

    fn select_hash(
        &self,
        generate: &GenerateConfig,
        pgo_config: &PgoConfig,
        select: SelectConfig,
    ) -> String {
        stage_hash(&(generate, pgo_config, select), &self.guest_hash)
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
