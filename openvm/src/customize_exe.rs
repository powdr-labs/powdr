use std::collections::{BTreeSet, HashMap};

use std::fmt::Display;
use std::hash::Hash;
use std::iter::once;
use std::path::Path;
use std::sync::Arc;

use crate::bus_map::OpenVmBusType;
use crate::extraction_utils::{get_air_metrics, AirWidthsDiff, OriginalAirs, OriginalVmConfig};
use crate::instruction_formatter::openvm_instruction_formatter;
use crate::memory_bus_interaction::OpenVmMemoryBusInteraction;
use crate::opcode::branch_opcodes_bigint_set;
use crate::powdr_extension::chip::PowdrAir;
use crate::utils::UnsupportedOpenVmReferenceError;
use crate::OriginalCompiledProgram;
use crate::PrecompileImplementation;
use crate::{CompiledProgram, SpecializedConfig};
use itertools::Itertools;
use openvm_instructions::instruction::Instruction as OpenVmInstruction;
use openvm_instructions::program::{Program as OpenVmProgram, DEFAULT_PC_STEP};
use openvm_instructions::VmOpcode;
use openvm_stark_backend::{
    interaction::SymbolicInteraction,
    p3_field::{FieldAlgebra, PrimeField32},
};
use openvm_stark_sdk::p3_baby_bear::BabyBear;
use powdr_autoprecompiles::adapter::{
    Adapter, AdapterApc, AdapterApcWithStats, AdapterVmConfig, ApcWithStats, PgoAdapter,
};
use powdr_autoprecompiles::blocks::{collect_basic_blocks, BasicBlock, Instruction, Program};
use powdr_autoprecompiles::evaluation::{evaluate_apc, EvaluationResult};
use powdr_autoprecompiles::expression::try_convert;
use powdr_autoprecompiles::pgo::{ApcCandidateJsonExport, Candidate, KnapsackItem};
use powdr_autoprecompiles::SymbolicBusInteraction;
use powdr_autoprecompiles::VmConfig;
use powdr_autoprecompiles::{Apc, PowdrConfig};
use powdr_number::{BabyBearField, FieldElement, LargeInt};
use powdr_riscv_elf::debug_info::DebugInfo;
use serde::{Deserialize, Serialize};

use crate::bus_interaction_handler::OpenVmBusInteractionHandler;
use crate::{
    powdr_extension::{PowdrOpcode, PowdrPrecompile},
    utils::symbolic_to_algebraic,
};

pub const POWDR_OPCODE: usize = 0x10ff;

/// An adapter for the BabyBear OpenVM precompiles.
/// Note: This could be made generic over the field, but the implementation of `Candidate` is BabyBear-specific.
/// The lifetime parameter is used because we use a reference to the `OpenVmProgram` in the `Prog` type.
pub struct BabyBearOpenVmApcAdapter<'a> {
    _marker: std::marker::PhantomData<&'a ()>,
}

impl<'a> Adapter for BabyBearOpenVmApcAdapter<'a> {
    type PowdrField = BabyBearField;
    type Field = BabyBear;
    type InstructionHandler = OriginalAirs<Self::Field>;
    type BusInteractionHandler = OpenVmBusInteractionHandler<Self::PowdrField>;
    type Program = Prog<'a, Self::Field>;
    type Instruction = Instr<Self::Field>;
    type MemoryBusInteraction<V: Ord + Clone + Eq + Display + Hash> =
        OpenVmMemoryBusInteraction<Self::PowdrField, V>;
    type CustomBusTypes = OpenVmBusType;
    type ApcStats = OvmApcStats;
    type AirId = String;

    fn into_field(e: Self::PowdrField) -> Self::Field {
        openvm_stark_sdk::p3_baby_bear::BabyBear::from_canonical_u32(
            e.to_integer().try_into_u32().unwrap(),
        )
    }

    fn from_field(e: Self::Field) -> Self::PowdrField {
        BabyBearField::from(e.as_canonical_u32())
    }
}

/// A newtype wrapper around `OpenVmProgram` to implement the `Program` trait.
/// This is necessary because we cannot implement a foreign trait for a foreign type.
pub struct Prog<'a, F>(&'a OpenVmProgram<F>);

impl<'a, F> From<&'a OpenVmProgram<F>> for Prog<'a, F> {
    fn from(program: &'a OpenVmProgram<F>) -> Self {
        Prog(program)
    }
}

/// A newtype wrapper around `OpenVmInstruction` to implement the `Instruction` trait.
/// This is necessary because we cannot implement a foreign trait for a foreign type.
#[derive(Clone, Serialize, Deserialize)]
pub struct Instr<F>(pub OpenVmInstruction<F>);

impl<F: PrimeField32> Display for Instr<F> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", openvm_instruction_formatter(&self.0))
    }
}

impl<F: PrimeField32> Instruction<F> for Instr<F> {
    fn pc_lookup_row(&self, pc: Option<u64>) -> Vec<Option<F>> {
        let args = [
            self.0.opcode.to_field(),
            self.0.a,
            self.0.b,
            self.0.c,
            self.0.d,
            self.0.e,
            self.0.f,
            self.0.g,
        ];
        // The PC lookup row has the format:
        // [pc, opcode, a, b, c, d, e, f, g]
        let pc = pc.map(|pc| F::from_canonical_u32(pc.try_into().unwrap()));
        once(pc).chain(args.into_iter().map(Some)).collect()
    }
}

impl<'a, F: PrimeField32> Program<Instr<F>> for Prog<'a, F> {
    fn base_pc(&self) -> u64 {
        self.0.pc_base as u64
    }

    fn pc_step(&self) -> u32 {
        DEFAULT_PC_STEP
    }

    fn instructions(&self) -> Box<dyn Iterator<Item = Instr<F>> + '_> {
        Box::new(
            self.0
                .instructions_and_debug_infos
                .iter()
                .filter_map(|x| x.as_ref().map(|i| Instr(i.0.clone()))),
        )
    }

    fn length(&self) -> u32 {
        self.0.instructions_and_debug_infos.len() as u32
    }
}

pub fn customize<'a, P: PgoAdapter<Adapter = BabyBearOpenVmApcAdapter<'a>>>(
    OriginalCompiledProgram { exe, vm_config }: OriginalCompiledProgram,
    labels: &BTreeSet<u32>,
    debug_info: &DebugInfo,
    config: PowdrConfig,
    implementation: PrecompileImplementation,
    pgo: P,
) -> CompiledProgram {
    let original_config = OriginalVmConfig::new(vm_config.clone());
    let airs = original_config.airs(config.degree_bound.identities).expect("Failed to convert the AIR of an OpenVM instruction, even after filtering by the blacklist!");
    let bus_map = original_config.bus_map();

    let jumpdest_set = add_extra_targets(
        &exe.program,
        labels.clone(),
        exe.program.pc_base,
        DEFAULT_PC_STEP,
    );

    let program = Prog(&exe.program);

    let range_tuple_checker_sizes = vm_config.sdk.rv32m.unwrap().range_tuple_checker_sizes;
    let vm_config = VmConfig {
        instruction_handler: &airs,
        bus_interaction_handler: OpenVmBusInteractionHandler::new(
            bus_map.clone(),
            range_tuple_checker_sizes,
        ),
        bus_map: bus_map.clone(),
    };

    // Convert the jump destinations to u64 for compatibility with the `collect_basic_blocks` function.
    let jumpdest_set = jumpdest_set
        .iter()
        .map(|&x| x as u64)
        .collect::<BTreeSet<_>>();

    let blocks = collect_basic_blocks::<BabyBearOpenVmApcAdapter>(&program, &jumpdest_set, &airs);
    tracing::info!(
        "Got {} basic blocks from `collect_basic_blocks`",
        blocks.len()
    );
    if tracing::enabled!(tracing::Level::DEBUG) {
        tracing::debug!("Basic blocks sorted by execution count (top 10):");
        for (count, block) in blocks
            .iter()
            .filter_map(|block| Some((pgo.pc_execution_count(block.start_pc)?, block)))
            .sorted_by_key(|(count, _)| *count)
            .rev()
            .take(10)
        {
            let name = debug_info
                .symbols
                .try_get_one_or_preceding(block.start_pc)
                .map(|(symbol, offset)| format!("{} + {offset}", rustc_demangle::demangle(symbol)))
                .unwrap_or_default();
            tracing::debug!("Basic block (executed {count} times), {name}:\n{block}",);
        }
    }

    let labels = debug_info
        .symbols
        .table()
        .iter()
        .map(|(addr, names)| {
            (
                *addr as u64,
                names
                    .iter()
                    .map(|name| rustc_demangle::demangle(name).to_string())
                    .collect(),
            )
        })
        .collect();

    let start = std::time::Instant::now();
    let apcs = pgo.filter_blocks_and_create_apcs_with_pgo(blocks, &config, vm_config, labels);
    metrics::gauge!("total_apc_gen_time_ms").set(start.elapsed().as_millis() as f64);

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
        .map(|(i, (apc, apc_stats))| {
            let opcode = POWDR_OPCODE + i;
            let start_index = ((apc.start_pc() - pc_base as u64) / pc_step as u64)
                .try_into()
                .unwrap();

            // We encode in the program that the prover should execute the apc instruction instead of the original software version.
            // This is only for witgen: the program in the program chip is left unchanged.
            program.add_apc_instruction_at_pc_index(start_index, VmOpcode::from_usize(opcode));

            PowdrPrecompile::new(
                format!("PowdrAutoprecompile_{}", apc.start_pc()),
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
        vm_config: SpecializedConfig::new(
            original_config,
            extensions,
            implementation,
            config.degree_bound.identities,
        ),
    }
}

/// Besides the base RISCV-V branching instructions, the bigint extension adds two more branching
/// instruction classes over BranchEqual and BranchLessThan.
/// Those instructions have the form <INSTR rs0 rs1 target_offset ...>, where target_offset is the
/// relative jump we're interested in.
/// This means that for a given program address A containing the instruction above,
/// we add A + target_offset as a target as well.
fn add_extra_targets<F: PrimeField32>(
    program: &OpenVmProgram<F>,
    mut labels: BTreeSet<u32>,
    base_pc: u32,
    pc_step: u32,
) -> BTreeSet<u32> {
    let branch_opcodes_bigint = branch_opcodes_bigint_set();
    let new_labels = program
        .instructions_and_debug_infos
        .iter()
        .enumerate()
        .filter_map(|(i, instr)| {
            let instr = instr.as_ref().unwrap().0.clone();
            let adjusted_pc = base_pc + (i as u32) * pc_step;
            let op = instr.opcode;
            branch_opcodes_bigint
                .contains(&op)
                .then_some(adjusted_pc + instr.c.as_canonical_u32())
        });
    labels.extend(new_labels);

    labels
}

pub fn openvm_bus_interaction_to_powdr<F: PrimeField32>(
    interaction: &SymbolicInteraction<F>,
    columns: &[Arc<String>],
) -> Result<SymbolicBusInteraction<F>, UnsupportedOpenVmReferenceError> {
    let id = interaction.bus_index as u64;

    let mult = try_convert(symbolic_to_algebraic(&interaction.count, columns))?;
    let args = interaction
        .message
        .iter()
        .map(|e| try_convert(symbolic_to_algebraic(e, columns)))
        .collect::<Result<_, _>>()?;

    Ok(SymbolicBusInteraction { id, mult, args })
}

#[derive(Serialize, Deserialize)]
pub struct OpenVmApcCandidate<F, I> {
    apc: Arc<Apc<F, I>>,
    execution_frequency: usize,
    widths: AirWidthsDiff,
    stats: EvaluationResult,
}

#[derive(Clone, Serialize, Deserialize)]
pub struct OvmApcStats {
    pub widths: AirWidthsDiff,
}

impl OvmApcStats {
    fn new(widths: AirWidthsDiff) -> Self {
        Self { widths }
    }
}

impl<'a> Candidate<BabyBearOpenVmApcAdapter<'a>> for OpenVmApcCandidate<BabyBear, Instr<BabyBear>> {
    fn create(
        apc: Arc<AdapterApc<BabyBearOpenVmApcAdapter<'a>>>,
        pgo_program_pc_count: &HashMap<u64, u32>,
        vm_config: AdapterVmConfig<BabyBearOpenVmApcAdapter>,
        max_degree: usize,
    ) -> Self {
        let apc_metrics = get_air_metrics(Arc::new(PowdrAir::new(apc.clone())), max_degree);
        let width_after = apc_metrics.widths;

        let width_before = apc
            .block
            .statements
            .iter()
            .map(|instr| {
                vm_config
                    .instruction_handler
                    .get_instruction_metrics(instr.0.opcode)
                    .unwrap()
                    .widths
            })
            .sum();

        let stats = evaluate_apc(
            &apc.block.statements,
            vm_config.instruction_handler,
            apc.machine(),
        );

        let execution_frequency =
            *pgo_program_pc_count.get(&apc.block.start_pc).unwrap_or(&0) as usize;

        Self {
            apc,
            execution_frequency,
            widths: AirWidthsDiff::new(width_before, width_after),
            stats,
        }
    }

    /// Return a JSON export of the APC candidate.
    fn to_json_export(&self, apc_candidates_dir_path: &Path) -> ApcCandidateJsonExport {
        ApcCandidateJsonExport {
            execution_frequency: self.execution_frequency,
            original_block: BasicBlock {
                start_pc: self.apc.block.start_pc,
                statements: self
                    .apc
                    .block
                    .statements
                    .iter()
                    .map(ToString::to_string)
                    .collect(),
            },
            stats: self.stats,
            width_before: self.widths.before.total(),
            value: self.value(),
            cost_before: self.widths.before.total() as f64,
            cost_after: self.widths.after.total() as f64,
            apc_candidate_file: apc_candidates_dir_path
                .join(format!("apc_{}.cbor", self.apc.start_pc()))
                .display()
                .to_string(),
        }
    }

    fn into_apc_and_stats(self) -> AdapterApcWithStats<BabyBearOpenVmApcAdapter<'a>> {
        ApcWithStats::from(self.apc).with_stats(OvmApcStats::new(self.widths))
    }
}

impl<P, I> OpenVmApcCandidate<P, I> {
    fn cells_saved_per_row(&self) -> usize {
        // The number of cells saved per row is the difference between the width before and after the APC.
        self.widths.columns_saved().total()
    }
}

impl<P, I> KnapsackItem for OpenVmApcCandidate<P, I> {
    fn cost(&self) -> usize {
        self.widths.after.total()
    }

    fn value(&self) -> usize {
        // For an APC which is called once and saves 1 cell, this would be 1.
        let value = self
            .execution_frequency
            .checked_mul(self.cells_saved_per_row())
            .unwrap();
        // We need `value()` to be much larger than `cost()` to avoid ties when ranking by `value() / cost()`
        // Therefore, we scale it up by a constant factor.
        value.checked_mul(1000).unwrap()
    }

    fn tie_breaker(&self) -> usize {
        self.apc.start_pc() as usize
    }
}
