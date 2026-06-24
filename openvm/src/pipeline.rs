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

use crate::customize_exe::{generate_apcs, select_apcs, setup, BabyBearOpenVmApcAdapter};
use crate::isa::OpenVmISA;
use crate::program::{CompiledProgram, OriginalCompiledProgram};

/// Output of the generate / select stages.
pub type RankedApcs<ISA> = Vec<AdapterApcWithStats<BabyBearOpenVmApcAdapter<'static, ISA>>>;

/// Trait alias for the closure that materializes the [`ExecutionProfile`]
/// from the guest + `PgoConfig::inputs`. Kept as a trait so the
/// `StagedPipeline` method signatures aren't dominated by the closure type.
pub trait MakePgoProfile<ISA: OpenVmISA>:
    FnOnce(&OriginalCompiledProgram<'static, ISA>, &[u8]) -> ExecutionProfile
{
}
impl<ISA: OpenVmISA, F> MakePgoProfile<ISA> for F where
    F: FnOnce(&OriginalCompiledProgram<'static, ISA>, &[u8]) -> ExecutionProfile
{
}

/// Trait alias for the closure that materializes [`EmpiricalConstraints`]
/// from the guest + `GenerateConfig` + `PgoConfig::inputs`. Callers that
/// don't use optimistic precompiles can pass
/// [`make_default_empirical_constraints`].
pub trait MakeEmpiricalConstraints<ISA: OpenVmISA>:
    FnOnce(&OriginalCompiledProgram<'static, ISA>, &GenerateConfig, &[u8]) -> EmpiricalConstraints
{
}
impl<ISA: OpenVmISA, F> MakeEmpiricalConstraints<ISA> for F where
    F: FnOnce(
        &OriginalCompiledProgram<'static, ISA>,
        &GenerateConfig,
        &[u8],
    ) -> EmpiricalConstraints
{
}

/// A [`MakeEmpiricalConstraints`] that always returns the empty set. Use this
/// when you don't care about optimistic precompiles.
pub fn make_default_empirical_constraints<ISA: OpenVmISA>(
    _guest: &OriginalCompiledProgram<'static, ISA>,
    _generate: &GenerateConfig,
    _inputs: &[u8],
) -> EmpiricalConstraints {
    EmpiricalConstraints::default()
}

pub struct StagedPipeline<ISA: OpenVmISA> {
    guest: OriginalCompiledProgram<'static, ISA>,
    guest_hash: String,
    artifacts_dir: Option<PathBuf>,
}

impl<ISA: OpenVmISA> StagedPipeline<ISA> {
    pub fn new(
        guest: OriginalCompiledProgram<'static, ISA>,
        artifacts_dir: Option<PathBuf>,
    ) -> Self {
        let guest_hash = hash_guest_exe(&guest);
        Self {
            guest,
            guest_hash,
            artifacts_dir,
        }
    }

    pub fn guest(&self) -> &OriginalCompiledProgram<'static, ISA> {
        &self.guest
    }

    /// Build + rank APC candidates (cached).
    pub fn generate_apcs(
        &self,
        generate: &GenerateConfig,
        pgo_config: &PgoConfig,
        make_pgo_profile: impl MakePgoProfile<ISA>,
        make_empirical_constraints: impl MakeEmpiricalConstraints<ISA>,
    ) -> RankedApcs<ISA> {
        let hash = self.generate_hash(generate, pgo_config);
        cached(self.artifacts_dir.as_deref(), "generate", &hash, || {
            let pgo = match pgo_config.pgo_type {
                PgoType::None => PgoData::None,
                pgo_type => {
                    let profile = make_pgo_profile(&self.guest, &pgo_config.inputs);
                    pgo_data(pgo_type, pgo_config.max_columns, profile)
                }
            };
            let empirical = if generate.should_use_optimistic_precompiles {
                make_empirical_constraints(&self.guest, generate, &pgo_config.inputs)
            } else {
                EmpiricalConstraints::default()
            };
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
        make_pgo_profile: impl MakePgoProfile<ISA>,
        make_empirical_constraints: impl MakeEmpiricalConstraints<ISA>,
    ) -> RankedApcs<ISA> {
        let hash = self.select_hash(generate, pgo_config, select);
        cached(self.artifacts_dir.as_deref(), "select", &hash, move || {
            let ranked = self.generate_apcs(
                generate,
                pgo_config,
                make_pgo_profile,
                make_empirical_constraints,
            );
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
        make_pgo_profile: impl MakePgoProfile<ISA>,
        make_empirical_constraints: impl MakeEmpiricalConstraints<ISA>,
    ) -> CompiledProgram<ISA> {
        // Setup's hash uses the same inputs as select (no extra "setup-only"
        // fields exist today). Distinguishing under a different stage name is
        // enough to keep the blobs on disk separate.
        let hash = self.select_hash(generate, pgo_config, select);
        let artifacts_dir = self.artifacts_dir.clone();
        let degree_bound = generate.degree_bound;
        cached(artifacts_dir.as_deref(), "setup", &hash, move || {
            let selected_apcs = self.select_apcs(
                generate,
                pgo_config,
                select,
                make_pgo_profile,
                make_empirical_constraints,
            );
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
fn hash_guest_exe<ISA: OpenVmISA>(guest: &OriginalCompiledProgram<'_, ISA>) -> String {
    let bytes = serde_cbor::to_vec(&*guest.exe).expect("serialize VmExe for hashing");
    let mut hasher = DefaultHasher::new();
    bytes.hash(&mut hasher);
    format!("{:016x}", hasher.finish())
}
