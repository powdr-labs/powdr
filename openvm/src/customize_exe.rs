use std::collections::{BTreeSet, HashMap};

use std::io::BufWriter;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};

use crate::extraction_utils::OriginalVmConfig;
use crate::instruction_formatter::openvm_instruction_formatter;
use crate::opcode::{branch_opcodes_bigint_set, branch_opcodes_set};
use crate::utils::{fractional_knapsack, KnapsackItem, UnsupportedOpenVmReferenceError};
use crate::IntoOpenVm;
use crate::OpenVmField;
use crate::OriginalCompiledProgram;
use crate::{CompiledProgram, SpecializedConfig};
use itertools::Itertools;
use openvm_instructions::instruction::Instruction;
use openvm_instructions::program::Program as OpenVmProgram;
use openvm_instructions::VmOpcode;
use openvm_stark_backend::p3_maybe_rayon::prelude::*;
use openvm_stark_backend::{
    interaction::SymbolicInteraction,
    p3_field::{FieldAlgebra, PrimeField32},
};
use powdr_autoprecompiles::basic_blocks::{collect_basic_blocks, Program};
use powdr_autoprecompiles::constraint_optimizer::IsBusStateful;
use powdr_autoprecompiles::expression::try_convert;
use powdr_autoprecompiles::{AirMetrics, Apc, DegreeBound, InstructionMachineHandler};
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

pub const OPENVM_DEGREE_BOUND: usize = 5;

pub const POWDR_OPCODE: usize = 0x10ff;

use crate::{PgoConfig, PowdrConfig};

#[derive(Debug)]
pub enum Error {
    AutoPrecompileError,
}

impl From<powdr_autoprecompiles::constraint_optimizer::Error> for Error {
    fn from(_e: powdr_autoprecompiles::constraint_optimizer::Error) -> Self {
        Error::AutoPrecompileError
    }
}

fn generate_apcs_with_pgo<
    P: FieldElement,
    I: InstructionMachineHandler<P> + Clone + Send + Sync,
    B: BusInteractionHandler<P> + Clone + IsBusStateful<P> + Send + Sync,
>(
    blocks: Vec<BasicBlock<P>>,
    config: &PowdrConfig,
    max_total_apc_columns: Option<usize>,
    pgo_config: PgoConfig,
    vm_config: powdr_autoprecompiles::VmConfig<I, B>,
) -> Vec<Apc<P>> {
    // sort basic blocks by:
    // 1. if PgoConfig::Cell, cost = frequency * cells_saved_per_row
    // 2. if PgoConfig::Instruction, cost = frequency * number_of_instructions
    // 3. if PgoConfig::None, cost = number_of_instructions
    let res = match pgo_config {
        PgoConfig::Cell(pgo_program_idx_count, _) => {
            create_apcs_with_cell_pgo::<OpenVmApcCandidate<P, I, B>, _, _, _>(
                blocks,
                pgo_program_idx_count,
                config,
                max_total_apc_columns,
                vm_config,
            )
        }
        PgoConfig::Instruction(pgo_program_idx_count) => {
            create_apcs_with_instruction_pgo(blocks, pgo_program_idx_count, config, vm_config)
        }
        PgoConfig::None => create_apcs_with_no_pgo(blocks, config, vm_config),
    };

    assert!(res.len() <= config.autoprecompiles as usize);

    res
}

pub fn customize(
    OriginalCompiledProgram {
        mut exe,
        sdk_vm_config,
    }: OriginalCompiledProgram,
    labels: &BTreeSet<u32>,
    config: PowdrConfig,
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

    let apcs = generate_apcs_with_pgo(
        blocks,
        &config,
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

// Only used for PgoConfig::Instruction and PgoConfig::None,
// because PgoConfig::Cell caches all APCs in sorting stage.
fn create_apcs_for_all_blocks<
    P: FieldElement,
    I: InstructionMachineHandler<P> + Clone + Send + Sync,
    B: BusInteractionHandler<P> + Clone + IsBusStateful<P> + Send + Sync,
>(
    blocks: Vec<BasicBlock<P>>,
    powdr_config: &PowdrConfig,
    vm_config: VmConfig<I, B>,
) -> Vec<Apc<P>> {
    let n_acc = powdr_config.autoprecompiles as usize;
    tracing::info!("Generating {n_acc} autoprecompiles in parallel");

    blocks
        .into_par_iter()
        .skip(powdr_config.skip_autoprecompiles as usize)
        .take(n_acc)
        .enumerate()
        .map(|(index, block)| {
            tracing::debug!(
                "Accelerating block of length {} and start idx {}",
                block.statements.len(),
                block.start_idx
            );

            tracing::debug!(
                "Acc block: {}",
                block.pretty_print(openvm_instruction_formatter)
            );

            let apc_opcode = POWDR_OPCODE + index;

            powdr_autoprecompiles::build(
                block,
                vm_config.clone(),
                powdr_config.degree_bound,
                apc_opcode as u32,
                powdr_config.apc_candidates_dir_path.as_deref(),
            )
            .unwrap()
        })
        .collect()
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
struct OpenVmApcCandidate<P, I, B> {
    apc: Apc<P>,
    execution_frequency: usize,
    width_before: usize,
    width_after: usize,
    _phantom: std::marker::PhantomData<(I, B)>,
}

impl<
        P: FieldElement,
        I: InstructionMachineHandler<P> + Clone + Send + Sync,
        B: BusInteractionHandler<P> + Clone + Send + Sync + IsBusStateful<P>,
    > Candidate<P, I, B> for OpenVmApcCandidate<P, I, B>
{
    type JsonExport = ApcCandidateJsonExport<P>;

    fn create(
        apc: Apc<P>,
        opcode: usize,
        degree_bound: DegreeBound,
        pgo_program_idx_count: &HashMap<u32, u32>,
        apc_candidates_dir_path: &Option<PathBuf>,
        vm_config: VmConfig<I, B>,
    ) -> Self {
        OpenVmApcCandidate::create(
            apc,
            opcode,
            degree_bound,
            pgo_program_idx_count,
            apc_candidates_dir_path,
            vm_config,
        )
    }

    fn to_json_export(&self, apc_candidates_dir_path: &Path) -> ApcCandidateJsonExport<P> {
        self.to_json_export(apc_candidates_dir_path)
    }

    fn into_apc(self) -> Apc<P> {
        self.apc
    }
}

#[derive(Serialize, Deserialize)]
struct ApcCandidateJsonExport<P> {
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

impl<
        P: FieldElement,
        I: InstructionMachineHandler<P> + Clone + Send + Sync,
        B: BusInteractionHandler<P> + Clone + IsBusStateful<P> + Send + Sync,
    > OpenVmApcCandidate<P, I, B>
{
    /// Try to create an autoprecompile candidate from a block.
    pub fn create(
        apc: Apc<P>,
        opcode: usize,
        degree_bound: DegreeBound,
        pgo_program_idx_count: &HashMap<u32, u32>,
        apc_candidates_dir_path: &Option<PathBuf>,
        vm_config: VmConfig<I, B>,
    ) -> Self {
        let apc_metrics: AirMetrics = unimplemented!();
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
            _phantom: std::marker::PhantomData,
        }
    }

    /// Return a JSON export of the APC candidate.
    fn to_json_export(&self, apc_candidates_dir_path: &Path) -> ApcCandidateJsonExport<P> {
        ApcCandidateJsonExport {
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
}

impl<P, I, B> OpenVmApcCandidate<P, I, B> {
    fn cells_saved_per_row(&self) -> usize {
        // The number of cells saved per row is the difference between the width before and after the APC.
        self.width_before - self.width_after
    }
}

impl<P, I, B> KnapsackItem for OpenVmApcCandidate<P, I, B> {
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

trait Candidate<P, I, B>: Sized {
    type JsonExport: Serialize + for<'de> Deserialize<'de> + Send + Sync;

    /// Try to create an autoprecompile candidate from a block.
    fn create(
        apc: Apc<P>,
        opcode: usize,
        degree_bound: DegreeBound,
        pgo_program_idx_count: &HashMap<u32, u32>,
        apc_candidates_dir_path: &Option<PathBuf>,
        vm_config: VmConfig<I, B>,
    ) -> Self;

    /// Return a JSON export of the APC candidate.
    fn to_json_export(&self, apc_candidates_dir_path: &Path) -> Self::JsonExport;

    fn into_apc(self) -> Apc<P>;
}

// Note: This function can lead to OOM since it generates the apc for many blocks.
fn create_apcs_with_cell_pgo<
    C: KnapsackItem + Candidate<P, I, B> + Send + Sync,
    P: FieldElement,
    I: InstructionMachineHandler<P> + Clone + Send + Sync,
    B: BusInteractionHandler<P> + Clone + IsBusStateful<P> + Send + Sync,
>(
    mut blocks: Vec<BasicBlock<P>>,
    pgo_program_idx_count: HashMap<u32, u32>,
    config: &PowdrConfig,
    max_total_apc_columns: Option<usize>,
    vm_config: VmConfig<I, B>,
) -> Vec<Apc<P>> {
    // drop any block whose start index cannot be found in pc_idx_count,
    // because a basic block might not be executed at all.
    // Also only keep basic blocks with more than one original instruction.
    blocks.retain(|b| {
        pgo_program_idx_count.contains_key(&(b.start_idx as u32)) && b.statements.len() > 1
    });

    tracing::debug!(
        "Retained {} basic blocks after filtering by pc_idx_count",
        blocks.len()
    );

    // generate apc for all basic blocks and only cache the ones we eventually use
    // calculate number of trace cells saved per row for each basic block to sort them by descending cost
    let max_cache = (config.autoprecompiles + config.skip_autoprecompiles) as usize;
    tracing::info!(
        "Generating autoprecompiles for all ({}) basic blocks in parallel and caching costliest {}",
        blocks.len(),
        max_cache,
    );

    let apc_candidates = Arc::new(Mutex::new(vec![]));

    // map–reduce over blocks into a single BinaryHeap<ApcCandidate<P>> capped at max_cache
    let res = fractional_knapsack(
        blocks.into_par_iter().enumerate().filter_map(|(i, block)| {
            let apc = powdr_autoprecompiles::build(
                block.clone(),
                vm_config.clone(),
                config.degree_bound,
                (POWDR_OPCODE + i) as u32,
                config.apc_candidates_dir_path.as_deref(),
            )
            .ok()?;
            let candidate = C::create(
                apc,
                POWDR_OPCODE + i,
                config.degree_bound,
                &pgo_program_idx_count,
                &config.apc_candidates_dir_path,
                vm_config.clone(),
            );
            if let Some(apc_candidates_dir_path) = &config.apc_candidates_dir_path {
                let json_export = candidate.to_json_export(apc_candidates_dir_path);
                apc_candidates.lock().unwrap().push(json_export);
            }
            Some(candidate)
        }),
        max_cache,
        max_total_apc_columns,
    )
    .skip(config.skip_autoprecompiles as usize)
    .map(C::into_apc)
    .collect();

    // Write the APC candidates JSON to disk if the directory is specified.
    if let Some(apc_candidates_dir_path) = &config.apc_candidates_dir_path {
        let apc_candidates_json_file = apc_candidates.lock().unwrap();
        let json_path = apc_candidates_dir_path.join("apc_candidates.json");
        let file = std::fs::File::create(&json_path)
            .expect("Failed to create file for APC candidates JSON");
        serde_json::to_writer(BufWriter::new(file), &*apc_candidates_json_file)
            .expect("Failed to write APC candidates JSON to file");
    }

    res
}

fn create_apcs_with_instruction_pgo<
    P: FieldElement,
    I: InstructionMachineHandler<P> + Clone + Send + Sync,
    B: BusInteractionHandler<P> + Clone + IsBusStateful<P> + Send + Sync,
>(
    mut blocks: Vec<BasicBlock<P>>,
    pgo_program_idx_count: HashMap<u32, u32>,
    config: &PowdrConfig,
    vm_config: VmConfig<I, B>,
) -> Vec<Apc<P>> {
    // drop any block whose start index cannot be found in pc_idx_count,
    // because a basic block might not be executed at all.
    // Also only keep basic blocks with more than one original instruction.
    blocks.retain(|b| {
        pgo_program_idx_count.contains_key(&(b.start_idx as u32)) && b.statements.len() > 1
    });

    tracing::debug!(
        "Retained {} basic blocks after filtering by pc_idx_count",
        blocks.len()
    );

    // cost = cells_saved_per_row
    blocks.sort_by(|a, b| {
        let a_cnt = pgo_program_idx_count[&(a.start_idx as u32)];
        let b_cnt = pgo_program_idx_count[&(b.start_idx as u32)];
        (b_cnt * (b.statements.len() as u32)).cmp(&(a_cnt * (a.statements.len() as u32)))
    });

    // Debug print blocks by descending cost
    for block in &blocks {
        let start_idx = block.start_idx;
        let frequency = pgo_program_idx_count[&(start_idx as u32)];
        let number_of_instructions = block.statements.len();
        let value = frequency * number_of_instructions as u32;

        tracing::debug!(
            "Basic block start_idx: {start_idx}, value: {value}, frequency: {frequency}, number_of_instructions: {number_of_instructions}",
        );
    }

    create_apcs_for_all_blocks(blocks, config, vm_config)
}

fn create_apcs_with_no_pgo<
    P: FieldElement,
    I: InstructionMachineHandler<P> + Clone + Send + Sync,
    B: BusInteractionHandler<P> + Clone + IsBusStateful<P> + Send + Sync,
>(
    mut blocks: Vec<BasicBlock<P>>,
    config: &PowdrConfig,
    vm_config: VmConfig<I, B>,
) -> Vec<Apc<P>> {
    // cost = number_of_original_instructions
    blocks.sort_by(|a, b| b.statements.len().cmp(&a.statements.len()));

    // Debug print blocks by descending cost
    for block in &blocks {
        let start_idx = block.start_idx;
        tracing::debug!(
            "Basic block start_idx: {}, number_of_instructions: {}",
            start_idx,
            block.statements.len(),
        );
    }

    create_apcs_for_all_blocks(blocks, config, vm_config)
}
