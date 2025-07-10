use std::collections::{BTreeSet, HashMap};

use std::path::Path;
use std::sync::Arc;

use crate::extraction_utils::{get_air_metrics, OriginalVmConfig};
use crate::instruction_formatter::openvm_instruction_formatter;
use crate::opcode::{branch_opcodes_bigint_set, branch_opcodes_set};
use crate::powdr_extension::chip::PowdrAir;
use crate::utils::UnsupportedOpenVmReferenceError;
use crate::IntoOpenVm;
use crate::OpenVmField;
use crate::OriginalCompiledProgram;
use crate::{CompiledProgram, SpecializedConfig};
use itertools::Itertools;
use openvm_instructions::instruction::Instruction;
use openvm_instructions::program::Program as OpenVmProgram;
use openvm_instructions::VmOpcode;
use openvm_stark_backend::{
    interaction::SymbolicInteraction,
    p3_field::{FieldAlgebra, PrimeField32},
};
use powdr_autoprecompiles::blocks::{collect_basic_blocks, Program};
use powdr_autoprecompiles::blocks::{generate_apcs_with_pgo, Candidate, KnapsackItem, PgoConfig};
use powdr_autoprecompiles::constraint_optimizer::IsBusStateful;
use powdr_autoprecompiles::expression::try_convert;
use powdr_autoprecompiles::{Apc, InstructionMachineHandler};
use powdr_autoprecompiles::{BasicBlock, VmConfig};
use powdr_autoprecompiles::{SymbolicBusInteraction, SymbolicInstructionStatement};
use powdr_constraint_solver::constraint_system::BusInteractionHandler;
use powdr_number::{BabyBearField, FieldElement};
use serde::{Deserialize, Serialize};

use crate::bus_interaction_handler::OpenVmBusInteractionHandler;
use crate::{
    powdr_extension::{OriginalInstruction, PowdrOpcode, PowdrPrecompile},
    utils::symbolic_to_algebraic,
};

pub const POWDR_OPCODE: usize = 0x10ff;

use crate::PowdrOpenVmConfig;

#[derive(Debug)]
pub enum Error {
    AutoPrecompileError,
}

impl From<powdr_autoprecompiles::constraint_optimizer::Error> for Error {
    fn from(_e: powdr_autoprecompiles::constraint_optimizer::Error) -> Self {
        Error::AutoPrecompileError
    }
}

pub fn customize(
    OriginalCompiledProgram {
        mut exe,
        sdk_vm_config,
    }: OriginalCompiledProgram,
    labels: &BTreeSet<u32>,
    config: PowdrOpenVmConfig,
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

    let program = Program::new(
        exe.program
            .instructions_and_debug_infos
            .iter()
            .map(|o| o.as_ref().unwrap().0.clone())
            .map(|instr| SymbolicInstructionStatement {
                opcode: instr.opcode.as_usize(),
                args: [
                    instr.a, instr.b, instr.c, instr.d, instr.e, instr.f, instr.g,
                ]
                .iter()
                .map(|f| BabyBearField::from_openvm_field(*f))
                .collect(),
            })
            .collect_vec(),
        exe.program.pc_base,
        exe.program.step,
    );

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

    let blocks = collect_basic_blocks(&program, &labels, &opcodes_allowlist, &branch_opcodes_set());
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
            tracing::debug!(
                "Basic block (executed {count} times):\n{}",
                block.pretty_print(openvm_instruction_formatter)
            );
        }
    }

    let blocks = blocks
        .into_iter()
        .filter(|b| {
            b.statements
                .iter()
                .all(|instr| opcodes_allowlist.contains(&instr.opcode))
        })
        .collect::<Vec<_>>();

    let apcs = generate_apcs_with_pgo::<OpenVmApcCandidate<_>, _, _, _>(
        blocks,
        &config.core,
        max_total_apc_columns,
        pgo_config,
        vm_config,
    );

    let program = &mut exe.program.instructions_and_debug_infos;

    let noop = Instruction {
        opcode: VmOpcode::from_usize(0xdeadaf),
        a: OpenVmField::<BabyBearField>::ZERO,
        b: OpenVmField::<BabyBearField>::ZERO,
        c: OpenVmField::<BabyBearField>::ZERO,
        d: OpenVmField::<BabyBearField>::ZERO,
        e: OpenVmField::<BabyBearField>::ZERO,
        f: OpenVmField::<BabyBearField>::ZERO,
        g: OpenVmField::<BabyBearField>::ZERO,
    };

    tracing::info!("Adjust the program with the autoprecompiles");

    let extensions = apcs
        .into_iter()
        .map(
            |Apc {
                 block,
                 opcode,
                 machine,
                 subs,
             }| {
                let new_instr = Instruction {
                    opcode: VmOpcode::from_usize(opcode as usize),
                    a: OpenVmField::<BabyBearField>::ZERO,
                    b: OpenVmField::<BabyBearField>::ZERO,
                    c: OpenVmField::<BabyBearField>::ZERO,
                    d: OpenVmField::<BabyBearField>::ZERO,
                    e: OpenVmField::<BabyBearField>::ZERO,
                    f: OpenVmField::<BabyBearField>::ZERO,
                    g: OpenVmField::<BabyBearField>::ZERO,
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
                )
            },
        )
        .collect();

    CompiledProgram {
        exe,
        vm_config: SpecializedConfig::new(original_config, extensions, config.implementation),
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

pub fn openvm_bus_interaction_to_powdr<F: PrimeField32, P: FieldElement>(
    interaction: &SymbolicInteraction<F>,
    columns: &[Arc<String>],
) -> Result<SymbolicBusInteraction<P>, UnsupportedOpenVmReferenceError> {
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
struct OpenVmApcCandidate<P> {
    apc: Apc<P>,
    execution_frequency: usize,
    width_before: usize,
    width_after: usize,
}

impl<
        I: InstructionMachineHandler<BabyBearField> + Clone + Send + Sync,
        B: BusInteractionHandler<BabyBearField> + Clone + Send + Sync + IsBusStateful<BabyBearField>,
    > Candidate<BabyBearField, I, B> for OpenVmApcCandidate<BabyBearField>
{
    type JsonExport = OpenVmApcCandidateJsonExport<BabyBearField>;

    fn create(
        apc: Apc<BabyBearField>,
        pgo_program_idx_count: &HashMap<u32, u32>,
        vm_config: VmConfig<I, B>,
    ) -> Self {
        let apc_metrics = get_air_metrics(Arc::new(PowdrAir::new(apc.machine().clone())));
        let width_after = apc_metrics.widths.total();

        let width_before: usize = apc
            .block
            .statements
            .iter()
            .map(|instr| {
                vm_config
                    .instruction_machine_handler
                    .get_instruction_metrics(instr.opcode)
                    .unwrap()
                    .widths
                    .total()
            })
            .sum();

        let execution_frequency = *pgo_program_idx_count
            .get(&(apc.block.start_idx as u32))
            .unwrap_or(&0) as usize;

        Self {
            apc,
            execution_frequency,
            width_before,
            width_after,
        }
    }

    /// Return a JSON export of the APC candidate.
    fn to_json_export(
        &self,
        apc_candidates_dir_path: &Path,
    ) -> OpenVmApcCandidateJsonExport<BabyBearField> {
        OpenVmApcCandidateJsonExport {
            opcode: self.apc.opcode,
            execution_frequency: self.execution_frequency,
            original_block: self.apc.block.clone(),
            total_width_before: self.width_before,
            total_width_after: self.width_after,
            apc_candidate_file: apc_candidates_dir_path
                .join(format!("apc_{}.cbor", self.apc.opcode))
                .display()
                .to_string(),
        }
    }

    fn into_apc(self) -> Apc<BabyBearField> {
        self.apc
    }
}

#[derive(Serialize, Deserialize)]
struct OpenVmApcCandidateJsonExport<P> {
    // opcode
    opcode: u32,
    // execution_frequency
    execution_frequency: usize,
    // original instructions
    original_block: BasicBlock<P>,
    // total width before optimisation
    total_width_before: usize,
    // total width after optimisation
    total_width_after: usize,
    // path to the apc candidate file
    apc_candidate_file: String,
}

impl<P> OpenVmApcCandidate<P> {
    fn cells_saved_per_row(&self) -> usize {
        // The number of cells saved per row is the difference between the width before and after the APC.
        self.width_before - self.width_after
    }
}

impl<P> KnapsackItem for OpenVmApcCandidate<P> {
    fn cost(&self) -> usize {
        self.width_after
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
