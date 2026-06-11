use std::fmt::Display;
use std::hash::Hash;
use std::iter::once;
use std::marker::PhantomData;
use std::sync::Arc;

use crate::extraction_utils::{get_air_metrics, AirWidthsDiff, OriginalAirs};
use crate::isa::OpenVmISA;
use crate::powdr_extension::chip::PowdrAir;
use crate::program::Prog;
use crate::OriginalCompiledProgram;
use crate::{CompiledProgram, SpecializedConfig};
use itertools::Itertools;
use openvm_circuit::arch::VmState;
use openvm_circuit::system::memory::online::GuestMemory;
use openvm_instructions::instruction::Instruction as OpenVmInstruction;
use openvm_instructions::program::DEFAULT_PC_STEP;
use openvm_instructions::VmOpcode;
use openvm_stark_backend::p3_field::{PrimeCharacteristicRing, PrimeField32};
use openvm_stark_backend::p3_maybe_rayon::prelude::{IntoParallelIterator, ParallelIterator};
use openvm_stark_sdk::p3_baby_bear::BabyBear;
use powdr_autoprecompiles::adapter::{
    Adapter, AdapterApc, AdapterApcWithStats, AdapterUnoptimizedApc, ApcWithStats, PgoAdapter,
};
use powdr_autoprecompiles::blocks::{detect_superblocks, BlockAndStats, Instruction, PcStep};
use powdr_autoprecompiles::empirical_constraints::EmpiricalConstraints;
use powdr_autoprecompiles::execution::ExecutionState;
use powdr_autoprecompiles::execution_profile::ExecutionProfile;
use powdr_autoprecompiles::export::ExportOptions;
use powdr_autoprecompiles::pgo::{ApcCandidate, CellPgo, InstructionPgo, NonePgo, PgoConfig};
use powdr_autoprecompiles::powdr::UniqueReferences;
use powdr_autoprecompiles::DegreeBound;
use powdr_autoprecompiles::PowdrConfig;
use powdr_autoprecompiles::VmConfig;
use powdr_autoprecompiles::{build_unoptimized, optimize_apc, InstructionTemplates};
use powdr_number::{BabyBearField, FieldElement, LargeInt};
use powdr_openvm_bus_interaction_handler::bus_map::OpenVmBusType;
use serde::{Deserialize, Serialize};
use std::io::BufWriter;
use std::path::Path;

use crate::powdr_extension::{PowdrOpcode, PowdrPrecompile};

pub use powdr_openvm_bus_interaction_handler::{
    memory_bus_interaction::OpenVmMemoryBusInteraction, OpenVmBusInteractionHandler,
};

pub const POWDR_OPCODE: usize = 0x10ff;

/// An adapter for the BabyBear OpenVM precompiles.
/// Note: This could be made generic over the field, but the implementation of `Candidate` is BabyBear-specific.
/// The lifetime parameter is used because we use a reference to the `OpenVmProgram` in the `Prog` type.
pub struct BabyBearOpenVmApcAdapter<'a, ISA> {
    _marker: std::marker::PhantomData<&'a ISA>,
}

/// The openvm execution state, used for execution constraint checking
pub struct OpenVmExecutionState<'a, F, ISA> {
    inner: &'a VmState<F, GuestMemory>,
    _marker: PhantomData<ISA>,
}

impl<'a, F: PrimeField32, ISA> From<&'a VmState<F, GuestMemory>>
    for OpenVmExecutionState<'a, F, ISA>
{
    fn from(inner: &'a VmState<F, GuestMemory>) -> Self {
        Self {
            inner,
            _marker: PhantomData,
        }
    }
}
// TODO: This is not tested yet as apc compilation does not currently output any optimistic constraints
impl<'a, F: PrimeField32, ISA: OpenVmISA> ExecutionState for OpenVmExecutionState<'a, F, ISA> {
    type RegisterAddress = ();
    type Value = u32;

    fn pc(&self) -> Self::Value {
        self.inner.pc()
    }

    fn reg(&self, _addr: &Self::RegisterAddress) -> Self::Value {
        unimplemented!("optimistic constraints are currently unused")
    }

    fn value_limb(_value: Self::Value, _limb_index: usize) -> Self::Value {
        unimplemented!("optimistic constraints are currently unused")
    }

    fn global_clk(&self) -> usize {
        unimplemented!("optimistic constraints are currently unused")
    }
}

impl<'a, ISA: OpenVmISA> Adapter for BabyBearOpenVmApcAdapter<'a, ISA> {
    type PowdrField = BabyBearField;
    type Field = BabyBear;
    type InstructionHandler = OriginalAirs<Self::Field, ISA>;
    type BusInteractionHandler = OpenVmBusInteractionHandler<Self::PowdrField>;
    type Program = Prog<'a, Self::Field>;
    type Instruction = Instr<Self::Field, ISA>;
    type MemoryBusInteraction<V: Ord + Clone + Eq + Display + Hash> =
        OpenVmMemoryBusInteraction<Self::PowdrField, V>;
    type CustomBusTypes = OpenVmBusType;
    type ApcStats = OvmApcStats;
    type AirId = String;
    type ExecutionState = OpenVmExecutionState<'a, BabyBear, ISA>;

    fn into_field(e: Self::PowdrField) -> Self::Field {
        openvm_stark_sdk::p3_baby_bear::BabyBear::from_u32(e.to_integer().try_into_u32().unwrap())
    }

    fn from_field(e: Self::Field) -> Self::PowdrField {
        BabyBearField::from(e.as_canonical_u32())
    }

    fn apc_stats(
        apc: Arc<AdapterApc<Self>>,
        instruction_handler: &Self::InstructionHandler,
    ) -> Self::ApcStats {
        let apc_metrics = get_air_metrics(Arc::new(PowdrAir::new(apc.machine.clone())));
        let width_after = apc_metrics.widths;

        // Sum up the metrics for each instruction
        let width_before = apc
            .instructions()
            .map(|instr| {
                instruction_handler
                    .get_instruction_metrics(instr.inner.opcode)
                    .unwrap()
                    .widths
            })
            .sum();

        OvmApcStats::new(AirWidthsDiff::new(width_before, width_after))
    }

    fn is_allowed(instruction: &Self::Instruction) -> bool {
        ISA::allowed_opcodes().contains(&instruction.inner.opcode)
    }

    fn is_branching(instruction: &Self::Instruction) -> bool {
        ISA::branching_opcodes().contains(&instruction.inner.opcode)
    }

    fn try_static_target(
        instruction: (u64, &Self::Instruction),
        previous: Option<(u64, &Self::Instruction)>,
    ) -> Option<u64> {
        ISA::try_static_target(instruction, previous)
    }
}

#[derive(Serialize, Deserialize)]
pub struct Instr<F, ISA> {
    pub inner: OpenVmInstruction<F>,
    _marker: PhantomData<ISA>,
}

impl<F, ISA> From<OpenVmInstruction<F>> for Instr<F, ISA> {
    fn from(value: OpenVmInstruction<F>) -> Self {
        Self {
            inner: value,
            _marker: PhantomData,
        }
    }
}

// TODO: derive, probably the compiler being too conservative here
impl<F, ISA> Clone for Instr<F, ISA>
where
    OpenVmInstruction<F>: Clone,
{
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
            _marker: PhantomData,
        }
    }
}

impl<F: PrimeField32, ISA: OpenVmISA> Display for Instr<F, ISA> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", ISA::format(&self.inner))
    }
}

impl<F, ISA: OpenVmISA> PcStep for Instr<F, ISA> {
    fn pc_step() -> u32 {
        DEFAULT_PC_STEP
    }
}

impl<F: PrimeField32, ISA: OpenVmISA> Instruction<F> for Instr<F, ISA> {
    fn pc_lookup_row(&self, pc: u64) -> Vec<F> {
        let args = [
            self.inner.opcode.to_field(),
            self.inner.a,
            self.inner.b,
            self.inner.c,
            self.inner.d,
            self.inner.e,
            self.inner.f,
            self.inner.g,
        ];
        // The PC lookup row has the format:
        // [pc, opcode, a, b, c, d, e, f, g]
        let pc = F::from_u32(pc.try_into().unwrap());
        once(pc).chain(args).collect()
    }
}

/// Build and select the autoprecompiles for `original_program` according to `pgo_config`.
///
/// This runs the APC generation pipeline up to (but not including) the program-injection
/// and `SpecializedConfig` assembly that [`setup`] performs.
pub fn compile_apcs<'a, ISA: OpenVmISA>(
    original_program: &OriginalCompiledProgram<'a, ISA>,
    config: &PowdrConfig,
    pgo_config: PgoConfig,
    empirical_constraints: EmpiricalConstraints,
) -> Vec<AdapterApcWithStats<BabyBearOpenVmApcAdapter<'a, ISA>>> {
    match pgo_config {
        PgoConfig::Cell(pgo_data, max_total_columns) => {
            let max_total_apc_columns = max_total_columns.map(|max_total_columns| {
                let total_non_apc_columns: usize = original_program
                    .vm_config
                    .chip_inventory_air_metrics()
                    .values()
                    .map(|m| m.total_width())
                    .sum();
                max_total_columns - total_non_apc_columns
            });
            compile_apcs_with_adapter(
                original_program,
                config,
                CellPgo::<_, OpenVmApcCandidate<ISA>>::with_pgo_data_and_max_columns(
                    pgo_data,
                    max_total_apc_columns,
                ),
                empirical_constraints,
            )
        }
        PgoConfig::Instruction(pgo_data) => compile_apcs_with_adapter(
            original_program,
            config,
            InstructionPgo::with_pgo_data(pgo_data),
            empirical_constraints,
        ),
        PgoConfig::None => compile_apcs_with_adapter(
            original_program,
            config,
            NonePgo::default(),
            empirical_constraints,
        ),
    }
}

fn compile_apcs_with_adapter<
    'a,
    ISA: OpenVmISA,
    P: PgoAdapter<Adapter = BabyBearOpenVmApcAdapter<'a, ISA>>,
>(
    original_program: &OriginalCompiledProgram<'a, ISA>,
    config: &PowdrConfig,
    pgo: P,
    empirical_constraints: EmpiricalConstraints,
) -> Vec<AdapterApcWithStats<BabyBearOpenVmApcAdapter<'a, ISA>>> {
    assert_eq!(
        config.optimistic_superblock_max_bb_count, 1,
        "openvm does not support optimistic superblocks"
    );

    let original_config = &original_program.vm_config;
    let airs = original_config.airs(config.degree_bound).expect("Failed to convert the AIR of an OpenVM instruction, even after filtering by the blacklist!");
    let bus_map = original_config.bus_map();

    let vm_config = VmConfig {
        instruction_handler: &airs,
        bus_interaction_handler: OpenVmBusInteractionHandler::new(bus_map.clone()),
        bus_map: bus_map.clone(),
    };

    let symbols = ISA::get_symbol_table(&original_program.linked_program);
    let basic_blocks = original_program.collect_basic_blocks(config.should_use_static_superblocks);
    if tracing::enabled!(tracing::Level::DEBUG) {
        tracing::debug!("Basic blocks sorted by execution count (top 10):");
        for (count, block) in basic_blocks
            .blocks
            .iter()
            .filter_map(|block| Some((pgo.pc_execution_count(block.start_pc)?, block)))
            .sorted_by_key(|(count, _)| *count)
            .rev()
            .take(10)
        {
            let name = symbols
                .try_get_one_or_preceding(block.start_pc)
                .map(|(symbol, offset)| format!("{} + {offset}", symbol))
                .unwrap_or_default();
            tracing::debug!("Basic block (executed {count} times), {name}:\n{block}",);
        }
    }

    let symbols = symbols
        .into_table()
        .into_iter()
        .map(|(key, values)| (key.into(), values))
        .collect();

    let start = std::time::Instant::now();
    let apcs = pgo.filter_blocks_and_create_apcs_with_pgo(
        basic_blocks,
        config,
        vm_config,
        symbols,
        empirical_constraints.apply_pc_threshold(),
    );
    metrics::gauge!("total_apc_gen_time_ms").set(start.elapsed().as_millis() as f64);
    apcs
}

/// Inject the selected APCs into `original_program` and assemble the final [`CompiledProgram`].
///
/// `apcs` is the output of [`compile_apcs`].
pub fn setup<'a, ISA: OpenVmISA>(
    original_program: OriginalCompiledProgram<'a, ISA>,
    apcs: Vec<AdapterApcWithStats<BabyBearOpenVmApcAdapter<'a, ISA>>>,
    degree_bound: DegreeBound,
) -> CompiledProgram<ISA> {
    let original_config = original_program.vm_config;
    let exe = original_program.exe;
    let pc_base = exe.program.pc_base;
    let pc_step = DEFAULT_PC_STEP;
    // We need to clone the program because we need to modify it to add the apc instructions.
    let mut exe = (*exe).clone();
    let program = &mut exe.program;

    tracing::info!("Adjust the program with the autoprecompiles");

    let extensions = apcs
        .into_iter()
        .map(ApcWithStats::into_parts)
        .enumerate()
        .map(|(i, (apc, apc_stats, _))| {
            let opcode = POWDR_OPCODE + i;
            // with optimistic superblocks disabled, start pcs are guaranteed to be all different (single APC per start PC)
            let start_pc = apc.block.start_pcs()[0];
            let start_index: usize = ((start_pc - pc_base as u64) / pc_step as u64)
                .try_into()
                .unwrap();

            // We encode in the program that the prover should execute the apc instruction instead of the original software version.
            // This is only for witgen: the program in the program chip is left unchanged.
            program.add_apc_instruction_at_pc_index(start_index, VmOpcode::from_usize(opcode));

            PowdrPrecompile::new(
                format!("PowdrAutoprecompile_{}", start_pc),
                PowdrOpcode {
                    class_offset: opcode,
                },
                apc,
                apc_stats,
            )
        })
        .collect();

    CompiledProgram {
        exe: Arc::new(exe),
        vm_config: SpecializedConfig::new(original_config, extensions, degree_bound),
    }
}

#[derive(Clone, Serialize, Deserialize)]
pub struct OvmApcStats {
    pub widths: AirWidthsDiff,
}

impl OvmApcStats {
    pub fn new(widths: AirWidthsDiff) -> Self {
        Self { widths }
    }
}

#[derive(Serialize, Deserialize)]
pub struct OpenVmApcCandidate<ISA: OpenVmISA>(
    ApcWithStats<BabyBear, Instr<BabyBear, ISA>, (), u32, OvmApcStats>,
);

impl<'a, ISA: OpenVmISA> ApcCandidate<BabyBearOpenVmApcAdapter<'a, ISA>>
    for OpenVmApcCandidate<ISA>
{
    fn create(apc_with_stats: AdapterApcWithStats<BabyBearOpenVmApcAdapter<'a, ISA>>) -> Self {
        Self(apc_with_stats)
    }

    fn inner(&self) -> &AdapterApcWithStats<BabyBearOpenVmApcAdapter<'a, ISA>> {
        &self.0
    }

    fn into_inner(self) -> AdapterApcWithStats<BabyBearOpenVmApcAdapter<'a, ISA>> {
        self.0
    }

    fn cost_before_opt(&self) -> usize {
        self.0.stats().widths.before.total()
    }

    fn cost_after_opt(&self) -> usize {
        self.0.stats().widths.after.total()
    }

    fn value_per_use(&self) -> usize {
        self.cost_before_opt() - self.cost_after_opt()
    }
}

/// Filename prefix for the per-candidate unoptimized-APC fixtures written by [`dump_apcs`].
/// Exposed so consumers can recognize fixture files in a dump directory.
pub const UNOPT_APC_PREFIX: &str = "unopt_apc_";

fn join_pcs(start_pcs: &[u64]) -> String {
    start_pcs
        .iter()
        .map(|pc| pc.to_string())
        .collect::<Vec<_>>()
        .join("_")
}

fn unopt_apc_file_name(start_pcs: &[u64]) -> String {
    format!("{UNOPT_APC_PREFIX}{}.cbor", join_pcs(start_pcs))
}

/// One row of the `unopt_apcs.json` triage summary.
#[derive(Serialize)]
struct ApcInfoRow {
    start_pcs: Vec<u64>,
    file: String,
    instr_count: usize,
    exec_count: u32,
    before_opt_cols: usize,
    before_opt_constraints: usize,
    before_opt_interactions: usize,
}

fn to_io<E: std::error::Error + Send + Sync + 'static>(e: E) -> std::io::Error {
    std::io::Error::other(e)
}

/// Dump the unoptimized APC for every candidate block into `out_dir`, for offline tooling
/// (e.g. the `block_selection` binary). Self-contained: builds the candidates, serializes
/// them, and writes the index — so external programs don't have to re-implement the
/// generation pipeline. Writes:
/// - `unopt_apc_<start_pcs>.cbor` per candidate — a self-contained [`AdapterUnoptimizedApc`]
///   that [`optimize_unoptimized_apc`] can optimize in isolation, without the guest;
/// - `execution_bb_runs.cbor` — the execution basic-block runs, for replaying selection;
/// - `unopt_apcs.json` — a triage summary (start PCs, exec/instr counts, pre-opt sizes).
///
/// Snapshots are **streamed** to disk as they are built (in parallel), so the large
/// pre-optimization machines don't all sit in memory at once. Requires a profile (candidate
/// detection and execution counts come from `execution_profile`). Returns the number of
/// candidates dumped.
pub fn dump_apcs<'a, ISA: OpenVmISA>(
    original_program: &OriginalCompiledProgram<'a, ISA>,
    config: &PowdrConfig,
    execution_profile: &ExecutionProfile,
    empirical_constraints: EmpiricalConstraints,
    out_dir: &Path,
) -> std::io::Result<usize> {
    assert_eq!(
        config.optimistic_superblock_max_bb_count, 1,
        "openvm does not support optimistic superblocks"
    );

    let original_config = &original_program.vm_config;
    let airs = original_config.airs(config.degree_bound).expect("Failed to convert the AIR of an OpenVM instruction, even after filtering by the blacklist!");
    let bus_map = original_config.bus_map();

    let vm_config = VmConfig {
        instruction_handler: &airs,
        bus_interaction_handler: OpenVmBusInteractionHandler::new(bus_map.clone()),
        bus_map: bus_map.clone(),
    };

    let basic_blocks = original_program.collect_basic_blocks(config.should_use_static_superblocks);
    let exec_blocks = detect_superblocks(config, &execution_profile.pc_list, basic_blocks);
    let empirical_constraints = empirical_constraints.apply_pc_threshold();

    let templates = config.instruction_templates.then(|| {
        InstructionTemplates::new(
            vm_config.bus_interaction_handler.clone(),
            config.degree_bound,
        )
    });

    std::fs::create_dir_all(out_dir)?;

    // Build + serialize each unoptimized APC in parallel, streaming to disk and dropping the
    // large snapshot immediately; collect only the small triage rows.
    let rows = exec_blocks
        .blocks
        .into_par_iter()
        .map(
            |BlockAndStats { block, count }| -> std::io::Result<ApcInfoRow> {
                let instr_count = block.instructions().count();
                let unoptimized_apc = build_unoptimized::<BabyBearOpenVmApcAdapter<'a, ISA>>(
                    block,
                    vm_config.clone(),
                    config.degree_bound,
                    &mut ExportOptions::default(),
                    &empirical_constraints,
                    templates.as_ref(),
                );
                let start_pcs = unoptimized_apc.block.start_pcs();
                let file = unopt_apc_file_name(&start_pcs);
                let writer = BufWriter::new(std::fs::File::create(out_dir.join(&file))?);
                serde_cbor::to_writer(writer, &unoptimized_apc).map_err(to_io)?;
                Ok(ApcInfoRow {
                    start_pcs,
                    file,
                    instr_count,
                    exec_count: count,
                    before_opt_cols: unoptimized_apc.machine.unique_references().count(),
                    before_opt_constraints: unoptimized_apc.machine.constraints.len(),
                    before_opt_interactions: unoptimized_apc.machine.bus_interactions.len(),
                })
            },
        )
        .collect::<std::io::Result<Vec<_>>>()?;

    // Execution basic-block runs, for replaying selection (`block_selection`).
    let runs_writer = BufWriter::new(std::fs::File::create(
        out_dir.join("execution_bb_runs.cbor"),
    )?);
    serde_cbor::to_writer(runs_writer, &exec_blocks.execution_bb_runs).map_err(to_io)?;

    // Triage summary.
    let json = serde_json::to_string_pretty(&rows).map_err(to_io)?;
    std::fs::write(out_dir.join("unopt_apcs.json"), json)?;

    Ok(rows.len())
}

/// The result of optimizing a single [`AdapterUnoptimizedApc`].
pub struct OptimizeApcResult<ISA: OpenVmISA> {
    /// The optimized autoprecompile.
    pub apc: AdapterApc<BabyBearOpenVmApcAdapter<'static, ISA>>,
    /// (unique columns, constraints, bus interactions) before optimization.
    pub before: (usize, usize, usize),
    /// (unique columns, constraints, bus interactions) after optimization.
    pub after: (usize, usize, usize),
}

/// Optimize a single unoptimized APC (the expensive phase of APC generation) in isolation.
///
/// Reconstructs the bus-interaction handler from the unoptimized APC's bus map, so it needs
/// neither the guest program nor the instruction AIRs. Does not compute the full APC
/// stats (those require the guest's instruction handler); reports machine-size deltas.
pub fn optimize_unoptimized_apc<ISA: OpenVmISA>(
    unoptimized_apc: AdapterUnoptimizedApc<BabyBearOpenVmApcAdapter<'static, ISA>>,
) -> Result<OptimizeApcResult<ISA>, powdr_autoprecompiles::constraint_optimizer::Error> {
    let before = (
        unoptimized_apc.machine.unique_references().count(),
        unoptimized_apc.machine.constraints.len(),
        unoptimized_apc.machine.bus_interactions.len(),
    );
    let bus_interaction_handler = OpenVmBusInteractionHandler::new(unoptimized_apc.bus_map.clone());
    let apc = optimize_apc::<BabyBearOpenVmApcAdapter<'static, ISA>>(
        unoptimized_apc,
        bus_interaction_handler,
        &mut ExportOptions::default(),
    )?;
    let after = (
        apc.machine.unique_references().count(),
        apc.machine.constraints.len(),
        apc.machine.bus_interactions.len(),
    );
    Ok(OptimizeApcResult { apc, before, after })
}
