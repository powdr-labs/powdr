use std::collections::{BTreeSet, HashMap};

use std::path::Path;
use std::sync::Arc;

use crate::extraction_utils::{get_air_metrics, AirWidthsDiff, OriginalAirs, OriginalVmConfig};
use crate::instruction_formatter::openvm_instruction_formatter;
use crate::opcode::{branch_opcodes_bigint_set, branch_opcodes_set};
use crate::powdr_extension::chip::PowdrAir;
use crate::utils::UnsupportedOpenVmReferenceError;
use crate::OriginalCompiledProgram;
use crate::PrecompileImplementation;
use crate::{CompiledProgram, SpecializedConfig};
use itertools::Itertools;
use openvm_instructions::instruction::Instruction as OpenVmInstruction;
use openvm_instructions::program::Program as OpenVmProgram;
use openvm_instructions::VmOpcode;
use openvm_stark_backend::{
    interaction::SymbolicInteraction,
    p3_field::{FieldAlgebra, PrimeField32},
};
use openvm_stark_sdk::p3_baby_bear::BabyBear;
use powdr_autoprecompiles::adapter::{Adapter, AdapterApc};
use powdr_autoprecompiles::blocks::{collect_basic_blocks, Instruction, Program};
use powdr_autoprecompiles::blocks::{generate_apcs_with_pgo, Candidate, KnapsackItem, PgoConfig};
use powdr_autoprecompiles::expression::try_convert;
use powdr_autoprecompiles::SymbolicBusInteraction;
use powdr_autoprecompiles::{Apc, PowdrConfig, SymbolicInstructionStatement};
use powdr_autoprecompiles::{BasicBlock, VmConfig};
use powdr_number::{BabyBearField, FieldElement, LargeInt};
use powdr_riscv_elf::debug_info::DebugInfo;
use serde::{Deserialize, Serialize};

use crate::bus_interaction_handler::OpenVmBusInteractionHandler;
use crate::{
    powdr_extension::{OriginalInstruction, PowdrOpcode, PowdrPrecompile},
    utils::symbolic_to_algebraic,
};

pub const POWDR_OPCODE: usize = 0x10ff;

#[derive(Debug)]
pub enum Error {
    AutoPrecompileError,
}

impl From<powdr_autoprecompiles::constraint_optimizer::Error> for Error {
    fn from(_e: powdr_autoprecompiles::constraint_optimizer::Error) -> Self {
        Error::AutoPrecompileError
    }
}

/// An adapter for the BabyBear OpenVM precompiles.
/// Note: This could be made generic over the field, but the implementation of `Candidate` is BabyBear-specific.
/// The lifetime parameter is used because we use a reference to the `OpenVmProgram` in the `Prog` type.
pub struct BabyBearOpenVmApcAdapter<'a> {
    _marker: std::marker::PhantomData<&'a ()>,
}

impl<'a> Adapter for BabyBearOpenVmApcAdapter<'a> {
    type PowdrField = BabyBearField;
    type Field = BabyBear;
    type InstructionMachineHandler = OriginalAirs<Self::Field>;
    type BusInteractionHandler = OpenVmBusInteractionHandler<Self::PowdrField>;
    type Candidate = OpenVmApcCandidate<Self::Field, Instr<Self::Field>>;
    type Program = Prog<'a, Self::Field>;
    type Instruction = Instr<Self::Field>;

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

/// A newtype wrapper around `OpenVmInstruction` to implement the `Instruction` trait.
/// This is necessary because we cannot implement a foreign trait for a foreign type.
#[derive(Clone, Serialize, Deserialize)]
pub struct Instr<F>(pub OpenVmInstruction<F>);

impl<F: PrimeField32> Instruction<F> for Instr<F> {
    fn opcode(&self) -> usize {
        self.0.opcode.as_usize()
    }

    fn into_symbolic_instruction(self) -> SymbolicInstructionStatement<F> {
        SymbolicInstructionStatement {
            opcode: self.0.opcode.to_field(),
            args: vec![
                self.0.a, self.0.b, self.0.c, self.0.d, self.0.e, self.0.f, self.0.g,
            ],
        }
    }
}

impl<'a, F: PrimeField32> Program<Instr<F>> for Prog<'a, F> {
    fn base_pc(&self) -> u64 {
        self.0.pc_base as u64
    }

    fn pc_step(&self) -> u32 {
        self.0.step
    }

    fn instructions(&self) -> Box<dyn Iterator<Item = Instr<F>> + '_> {
        Box::new(
            self.0
                .instructions_and_debug_infos
                .iter()
                .filter_map(|x| x.as_ref().map(|i| Instr(i.0.clone()))),
        )
    }
}

pub fn customize(
    OriginalCompiledProgram {
        mut exe,
        sdk_vm_config,
    }: OriginalCompiledProgram,
    labels: &BTreeSet<u32>,
    debug_info: &DebugInfo,
    config: PowdrConfig,
    implementation: PrecompileImplementation,
    pgo_config: PgoConfig,
) -> CompiledProgram {
    let original_config = OriginalVmConfig::new(sdk_vm_config.clone());
    let airs = original_config.airs().expect("Failed to convert the AIR of an OpenVM instruction, even after filtering by the blacklist!");
    let bus_map = original_config.bus_map();

    let opcodes_allowlist = airs.allow_list();

    let labels = add_extra_targets(
        &exe.program,
        labels.clone(),
        exe.program.pc_base,
        exe.program.step,
    );

    let program = Prog(&exe.program);

    let vm_config = VmConfig {
        instruction_machine_handler: &airs,
        bus_interaction_handler: OpenVmBusInteractionHandler::new(bus_map.clone()),
        bus_map: bus_map.clone(),
    };

    let max_total_apc_columns: Option<usize> = match pgo_config {
        PgoConfig::Cell(_, max_total_columns) => max_total_columns.map(|max_total_columns| {
            let total_non_apc_columns = original_config
                .chip_inventory_air_metrics()
                .values()
                .map(|m| m.total_width())
                .sum::<usize>();
            max_total_columns - total_non_apc_columns
        }),
        PgoConfig::Instruction(_) | PgoConfig::None => None,
    };

    // Convert the labels to u64 for compatibility with the `collect_basic_blocks` function.
    let labels = labels.iter().map(|&x| x as u64).collect::<BTreeSet<_>>();

    let blocks = collect_basic_blocks::<BabyBearOpenVmApcAdapter>(
        &program,
        &labels,
        &opcodes_allowlist,
        &branch_opcodes_set(),
    );
    tracing::info!(
        "Got {} basic blocks from `collect_basic_blocks`",
        blocks.len()
    );
    if tracing::enabled!(tracing::Level::DEBUG) {
        tracing::debug!("Basic blocks sorted by execution count (top 10):");
        for (count, block) in blocks
            .iter()
            .filter_map(|block| {
                Some((
                    pgo_config.pc_offset_execution_count(block.start_idx as u32)?,
                    block,
                ))
            })
            .sorted_by_key(|(count, _)| *count)
            .rev()
            .take(10)
        {
            let name = debug_info
                .symbols
                .try_get_one_or_preceding(
                    block.start_address(exe.program.pc_base, exe.program.step),
                )
                .map(|(symbol, offset)| format!("{} + {offset}", rustc_demangle::demangle(symbol)))
                .unwrap_or_default();
            tracing::debug!(
                "Basic block (executed {count} times), {name}:\n{}",
                block.pretty_print(|n| openvm_instruction_formatter(&n.0))
            );
        }
    }

    let blocks = blocks
        .into_iter()
        .filter(|b| {
            b.statements
                .iter()
                .all(|instr| opcodes_allowlist.contains(&instr.opcode()))
        })
        .collect::<Vec<_>>();

    let apcs = generate_apcs_with_pgo::<BabyBearOpenVmApcAdapter>(
        blocks,
        &config,
        max_total_apc_columns,
        pgo_config,
        vm_config,
    );

    let program = &mut exe.program.instructions_and_debug_infos;

    let noop = OpenVmInstruction {
        opcode: VmOpcode::from_usize(0xdeadaf),
        a: BabyBear::ZERO,
        b: BabyBear::ZERO,
        c: BabyBear::ZERO,
        d: BabyBear::ZERO,
        e: BabyBear::ZERO,
        f: BabyBear::ZERO,
        g: BabyBear::ZERO,
    };

    tracing::info!("Adjust the program with the autoprecompiles");

    let extensions = apcs
        .into_iter()
        .map(
            |(
                Apc {
                    block,
                    opcode,
                    machine,
                    subs,
                },
                apc_stats,
            )| {
                let new_instr = OpenVmInstruction {
                    opcode: VmOpcode::from_usize(opcode as usize),
                    a: BabyBear::ZERO,
                    b: BabyBear::ZERO,
                    c: BabyBear::ZERO,
                    d: BabyBear::ZERO,
                    e: BabyBear::ZERO,
                    f: BabyBear::ZERO,
                    g: BabyBear::ZERO,
                };

                let pc = block.start_idx;
                let n_acc = block.statements.len();
                let (acc, new_instrs): (Vec<_>, Vec<_>) = program[pc..pc + n_acc]
                    .iter()
                    .enumerate()
                    .map(|(i, x)| {
                        let instr = x.as_ref().unwrap();
                        let instr = instr.0.clone();
                        if i == 0 {
                            (instr, new_instr.clone())
                        } else {
                            (instr, noop.clone())
                        }
                    })
                    .collect();

                let new_instrs = new_instrs.into_iter().map(|x| Some((x, None)));

                let len_before = program.len();
                program.splice(pc..pc + n_acc, new_instrs);
                assert_eq!(program.len(), len_before);

                let is_valid_column = machine
                    .main_columns()
                    .find(|c| &*c.name == "is_valid")
                    .unwrap();

                PowdrPrecompile::new(
                    format!("PowdrAutoprecompile_{opcode}"),
                    PowdrOpcode {
                        class_offset: opcode as usize,
                    },
                    machine,
                    acc.into_iter()
                        .zip_eq(subs)
                        .map(|(instruction, subs)| OriginalInstruction::new(instruction, subs))
                        .collect(),
                    is_valid_column,
                    apc_stats,
                )
            },
        )
        .collect();

    CompiledProgram {
        exe,
        vm_config: SpecializedConfig::new(original_config, extensions, implementation),
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
            let op = instr.opcode.as_usize();
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
    apc: Apc<F, I>,
    execution_frequency: usize,
    widths: AirWidthsDiff,
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
    type JsonExport = OpenVmApcCandidateJsonExport<Instr<BabyBear>>;
    type ApcStats = OvmApcStats;

    fn create(
        apc: AdapterApc<BabyBearOpenVmApcAdapter<'a>>,
        pgo_program_idx_count: &HashMap<u32, u32>,
        vm_config: VmConfig<OriginalAirs<BabyBear>, OpenVmBusInteractionHandler<BabyBearField>>,
    ) -> Self {
        let apc_metrics = get_air_metrics(Arc::new(PowdrAir::new(apc.machine().clone())));
        let width_after = apc_metrics.widths;

        let width_before = apc
            .block
            .statements
            .iter()
            .map(|instr| {
                vm_config
                    .instruction_machine_handler
                    .get_instruction_metrics(instr.opcode())
                    .unwrap()
                    .widths
            })
            .sum();

        let execution_frequency = *pgo_program_idx_count
            .get(&(apc.block.start_idx as u32))
            .unwrap_or(&0) as usize;

        Self {
            apc,
            execution_frequency,
            widths: AirWidthsDiff::new(width_before, width_after),
        }
    }

    /// Return a JSON export of the APC candidate.
    fn to_json_export(
        &self,
        apc_candidates_dir_path: &Path,
    ) -> OpenVmApcCandidateJsonExport<Instr<BabyBear>> {
        OpenVmApcCandidateJsonExport {
            opcode: self.apc.opcode,
            execution_frequency: self.execution_frequency,
            original_block: self.apc.block.clone(),
            total_width_before: self.widths.before.total(),
            total_width_after: self.widths.after.total(),
            apc_candidate_file: apc_candidates_dir_path
                .join(format!("apc_{}.cbor", self.apc.opcode))
                .display()
                .to_string(),
        }
    }

    fn into_apc_and_stats(self) -> (AdapterApc<BabyBearOpenVmApcAdapter<'a>>, Self::ApcStats) {
        (self.apc, OvmApcStats::new(self.widths))
    }
}

#[derive(Serialize, Deserialize)]
pub struct OpenVmApcCandidateJsonExport<I> {
    // opcode
    opcode: u32,
    // execution_frequency
    execution_frequency: usize,
    // original instructions
    original_block: BasicBlock<I>,
    // total width before optimisation
    total_width_before: usize,
    // total width after optimisation
    total_width_after: usize,
    // path to the apc candidate file
    apc_candidate_file: String,
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
        self.apc.opcode as usize
    }
}
