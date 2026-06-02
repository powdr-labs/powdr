//! Cached `generate-apcs` → `select-apcs` → `setup` pipeline.
//!
//! [`StagedPipeline`] is the shared runner used by both the CLI and external
//! callers (sp1, openvm-eth). Each stage transparently reuses a prior
//! `artifacts_dir` blob; `artifacts_dir = None` disables caching and runs
//! every stage inline.
//!
//! ## Cache-key model
//!
//! The library hashes everything it can see — the guest fingerprint, the
//! per-stage config struct, the PGO type, `max_columns`. The caller supplies
//! `input_fp: &impl Hash`, an opaque fingerprint of anything hidden behind
//! the closures (PGO stdin bytes, block numbers, RPC chain id, ...). Passing
//! `&()` is valid when nothing is hidden.
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
use powdr_autoprecompiles::pgo::{pgo_config, PgoType};
use powdr_autoprecompiles::staged_cache::{cached, stage_hash};
use powdr_autoprecompiles::{GenerateConfig, SelectConfig};
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
    /// The library hashes `(guest, generate, pgo, max_columns, input_fp)` — the
    /// caller never sees the hash. `input_fp` is the only piece the caller
    /// fingerprints: it covers anything hidden behind the `make_*` closures
    /// (PGO stdin contents, RPC chain id, block numbers). Pass `&()` if
    /// nothing is hidden.
    ///
    /// `make_pgo_profile` and `make_empirical_constraints` only run on a
    /// cache miss, so callers may hide expensive work (RPC fetches, guest
    /// execution) behind them. `make_pgo_profile` owns the policy for how to
    /// produce the profile — typically
    /// [`powdr_openvm::execution_profile_from_guest`] for single-stdin
    /// guests, or a custom loop for multi-stdin PGO.
    pub fn generate_apcs<I: Hash + ?Sized>(
        &self,
        generate: &GenerateConfig,
        pgo: PgoType,
        max_columns: Option<usize>,
        input_fp: &I,
        make_pgo_profile: impl FnOnce(&OriginalCompiledProgram<'static, RiscvISA>) -> ExecutionProfile,
        make_empirical_constraints: impl FnOnce() -> EmpiricalConstraints,
    ) -> RankedApcs {
        let hash = self.generate_hash(generate, pgo, max_columns, input_fp);
        cached(self.artifacts_dir.as_deref(), "generate", &hash, || {
            let pgo_cfg = pgo_config(pgo, max_columns, make_pgo_profile(&self.guest));
            generate_apcs(&self.guest, generate, pgo_cfg, make_empirical_constraints())
        })
    }

    /// Trim a generate-stage ranking to `select.autoprecompiles` (after
    /// `select.skip`), or load from the cache.
    ///
    /// The library hashes `(guest, generate, select, input_fp)`. `compute_ranked`
    /// only runs on a cache miss; pass it a closure that invokes
    /// [`Self::generate_apcs`] if the upstream blob might also need rebuilding.
    pub fn select_apcs<I: Hash + ?Sized>(
        &self,
        generate: &GenerateConfig,
        select: SelectConfig,
        input_fp: &I,
        compute_ranked: impl FnOnce() -> RankedApcs,
    ) -> RankedApcs {
        let hash = self.select_hash(generate, select, input_fp);
        cached(self.artifacts_dir.as_deref(), "select", &hash, || {
            select_apcs(compute_ranked(), select)
        })
    }

    /// Inject the selected APCs and assemble the final [`CompiledProgram`].
    /// Consumes the pipeline (the guest is moved into `setup`).
    ///
    /// The library hashes `(guest, generate, select, input_fp)`. `compute_apcs`
    /// receives a borrow of `self` so it can recursively invoke
    /// [`Self::select_apcs`] / [`Self::generate_apcs`]; only runs on a cache
    /// miss.
    pub fn setup<I: Hash + ?Sized>(
        self,
        generate: &GenerateConfig,
        select: SelectConfig,
        input_fp: &I,
        compute_apcs: impl FnOnce(&Self) -> RankedApcs,
    ) -> CompiledProgram<RiscvISA> {
        // Setup's hash uses the same inputs as select (no extra "setup-only"
        // fields exist today). Distinguishing under a different stage name is
        // enough to keep the blobs on disk separate.
        let hash = self.select_hash(generate, select, input_fp);
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

    fn generate_hash<I: Hash + ?Sized>(
        &self,
        generate: &GenerateConfig,
        pgo: PgoType,
        max_columns: Option<usize>,
        input_fp: &I,
    ) -> String {
        stage_hash(&(generate, pgo, max_columns, input_fp), &self.guest_hash)
    }

    fn select_hash<I: Hash + ?Sized>(
        &self,
        generate: &GenerateConfig,
        select: SelectConfig,
        input_fp: &I,
    ) -> String {
        stage_hash(&(generate, select, input_fp), &self.guest_hash)
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
