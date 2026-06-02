//! Cached `generate-apcs` → `select-apcs` → `setup` pipeline.
//!
//! [`StagedPipeline`] is the shared runner used by both the CLI and external
//! callers (e.g. openvm-eth). Each stage transparently reuses a prior
//! `artifacts_dir` blob; `artifacts_dir = None` disables caching and runs
//! every stage inline.

use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::path::PathBuf;

use powdr_autoprecompiles::adapter::AdapterApcWithStats;
use powdr_autoprecompiles::empirical_constraints::EmpiricalConstraints;
use powdr_autoprecompiles::execution_profile::ExecutionProfile;
use powdr_autoprecompiles::pgo::{pgo_data, PgoType};
use powdr_autoprecompiles::staged_cache::{cached, stage_hash};
use powdr_autoprecompiles::PgoData;
use powdr_autoprecompiles::{GenerateConfig, PgoConfig, SelectConfig};
use powdr_openvm::BabyBearOpenVmApcAdapter;

use crate::{
    generate_apcs, select_apcs, setup, CompiledProgram, OriginalCompiledProgram, RiscvISA,
};

/// Output of the generate / select stages.
pub type RankedApcs = Vec<AdapterApcWithStats<BabyBearOpenVmApcAdapter<'static, RiscvISA>>>;

/// Trait alias for the closure that materializes the [`ExecutionProfile`]
/// from the guest + `PgoConfig::inputs`. Kept as a trait so the
/// `StagedPipeline` method signatures aren't dominated by the closure type.
pub trait MakeExecutionProfile:
    Fn(&OriginalCompiledProgram<'static, RiscvISA>, &[u8]) -> ExecutionProfile
{
}
impl<F> MakeExecutionProfile for F where
    F: Fn(&OriginalCompiledProgram<'static, RiscvISA>, &[u8]) -> ExecutionProfile
{
}

/// Trait alias for the closure that materializes [`EmpiricalConstraints`]
/// from the guest + `GenerateConfig` + `PgoConfig::inputs`.
pub trait MakeEmpiricalConstraints:
    Fn(&OriginalCompiledProgram<'static, RiscvISA>, &GenerateConfig, &[u8]) -> EmpiricalConstraints
{
}
impl<F> MakeEmpiricalConstraints for F where
    F: Fn(
        &OriginalCompiledProgram<'static, RiscvISA>,
        &GenerateConfig,
        &[u8],
    ) -> EmpiricalConstraints
{
}

pub struct StagedPipeline<'a> {
    guest: OriginalCompiledProgram<'static, RiscvISA>,
    guest_hash: String,
    artifacts_dir: Option<PathBuf>,
    make_execution_profile: &'a dyn MakeExecutionProfile,
    make_empirical_constraints: &'a dyn MakeEmpiricalConstraints,
}

impl<'a> StagedPipeline<'a> {
    pub fn new(
        guest: OriginalCompiledProgram<'static, RiscvISA>,
        artifacts_dir: Option<PathBuf>,
        make_execution_profile: &'a impl MakeExecutionProfile,
        make_empirical_constraints: &'a impl MakeEmpiricalConstraints,
    ) -> Self {
        let guest_hash = hash_guest_exe(&guest);
        Self {
            guest,
            guest_hash,
            artifacts_dir,
            make_execution_profile,
            make_empirical_constraints,
        }
    }

    pub fn guest(&self) -> &OriginalCompiledProgram<'static, RiscvISA> {
        &self.guest
    }

    /// Build + rank APC candidates (cached).
    pub fn generate_apcs(&self, generate: &GenerateConfig, pgo_config: &PgoConfig) -> RankedApcs {
        let hash = self.generate_hash(generate, pgo_config);
        cached(self.artifacts_dir.as_deref(), "generate", &hash, || {
            // PgoType::None ignores the profile entirely; skip the closure
            // (and any expensive work it'd do, like running the guest) for it.
            let pgo = match pgo_config.pgo_type {
                PgoType::None => PgoData::None,
                pgo_type => {
                    let profile = (self.make_execution_profile)(&self.guest, &pgo_config.inputs);
                    pgo_data(pgo_type, pgo_config.max_columns, profile)
                }
            };
            let empirical =
                (self.make_empirical_constraints)(&self.guest, generate, &pgo_config.inputs);
            generate_apcs(&self.guest, generate, pgo, empirical)
        })
    }

    /// Trim a generate-stage ranking to `select.autoprecompiles`, after
    /// `select.skip` (cached). On a select-stage cache hit, the upstream
    /// generate call is skipped entirely — the recursive
    /// [`Self::generate_apcs`] lives inside the cached closure.
    pub fn select_apcs(
        &self,
        generate: &GenerateConfig,
        pgo_config: &PgoConfig,
        select: SelectConfig,
    ) -> RankedApcs {
        let hash = self.select_hash(generate, pgo_config, select);
        cached(self.artifacts_dir.as_deref(), "select", &hash, move || {
            let ranked = self.generate_apcs(generate, pgo_config);
            select_apcs(ranked, select)
        })
    }

    /// Inject the selected APCs and assemble the final [`CompiledProgram`]
    /// (cached). Consumes the pipeline (the guest is moved into `setup`).
    /// On a setup-stage cache hit, neither select nor generate is consulted.
    pub fn setup(
        self,
        generate: &GenerateConfig,
        pgo_config: &PgoConfig,
        select: SelectConfig,
    ) -> CompiledProgram<RiscvISA> {
        // Setup's hash uses the same inputs as select (no extra "setup-only"
        // fields exist today). Distinguishing under a different stage name is
        // enough to keep the blobs on disk separate.
        let hash = self.select_hash(generate, pgo_config, select);
        let artifacts_dir = self.artifacts_dir.clone();
        let degree_bound = generate.degree_bound;
        cached(artifacts_dir.as_deref(), "setup", &hash, move || {
            let selected_apcs = self.select_apcs(generate, pgo_config, select);
            setup(self.guest, selected_apcs, degree_bound)
        })
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
