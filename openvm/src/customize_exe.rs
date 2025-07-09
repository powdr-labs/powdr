use std::collections::{BTreeSet, HashMap};

use std::io::BufWriter;
use std::path::Path;
use std::sync::{Arc, Mutex};

use crate::extraction_utils::{get_air_metrics, OriginalAirs, OriginalVmConfig};
use crate::opcode::{branch_opcodes_bigint_set, branch_opcodes_set};
use crate::powdr_extension::chip::PowdrAir;
use crate::utils::{fractional_knapsack, KnapsackItem, UnsupportedOpenVmReferenceError};
use crate::IntoOpenVm;
use crate::OpenVmField;
use crate::OriginalCompiledProgram;
use crate::{CompiledProgram, SpecializedConfig};
use itertools::Itertools;
use openvm_instructions::instruction::Instruction;
use openvm_instructions::program::Program;
use openvm_instructions::VmOpcode;
use openvm_stark_backend::p3_maybe_rayon::prelude::*;
use openvm_stark_backend::{
    interaction::SymbolicInteraction,
    p3_field::{FieldAlgebra, PrimeField32},
};
use powdr_autoprecompiles::expression::try_convert;
use powdr_autoprecompiles::{
    bus_map::BusMap, SymbolicBusInteraction, SymbolicInstructionStatement,
};
use powdr_autoprecompiles::{Apc, DegreeBound};
use powdr_autoprecompiles::{SymbolicBlock, VmConfig};
use powdr_number::{BabyBearField, FieldElement};
use serde::{Deserialize, Serialize};

use crate::bus_interaction_handler::OpenVmBusInteractionHandler;
use crate::instruction_formatter::openvm_instruction_formatter;
use crate::{
    powdr_extension::{OriginalInstruction, PowdrOpcode, PowdrPrecompile},
    utils::symbolic_to_algebraic,
};

pub const OPENVM_DEGREE_BOUND: usize = 5;

// TODO: read this from program
const OPENVM_INIT_PC: u32 = 0x0020_0800;

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

fn generate_apcs_with_pgo(
    blocks: Vec<BasicBlock<OpenVmField<BabyBearField>>>,
    airs: &OriginalAirs<BabyBearField>,
    bus_map: &BusMap,
    config: &PowdrConfig,
    original_config: &OriginalVmConfig,
    pgo_config: PgoConfig,
) -> Vec<Apc<BabyBearField>> {
    // sort basic blocks by:
    // 1. if PgoConfig::Cell, cost = frequency * cells_saved_per_row
    // 2. if PgoConfig::Instruction, cost = frequency * number_of_instructions
    // 3. if PgoConfig::None, cost = number_of_instructions
    let res = match pgo_config {
        PgoConfig::Cell(pgo_program_idx_count, max_total_columns) => create_apcs_with_cell_pgo(
            blocks,
            pgo_program_idx_count,
            max_total_columns,
            airs,
            config,
            original_config,
            bus_map,
        ),
        PgoConfig::Instruction(pgo_program_idx_count) => {
            create_apcs_with_instruction_pgo(blocks, pgo_program_idx_count, airs, config, bus_map)
        }
        PgoConfig::None => create_apcs_with_no_pgo(blocks, airs, config, bus_map),
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

    let labels = add_extra_targets(&exe.program, labels.clone());

    let blocks = collect_basic_blocks(
        &exe.program,
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
                .all(|instr| opcodes_allowlist.contains(&instr.opcode.as_usize()))
        })
        .collect::<Vec<_>>();

    let blocks_with_apcs = generate_apcs_with_pgo(
        blocks,
        &airs,
        &bus_map,
        &config,
        &original_config,
        pgo_config,
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

    let extensions = blocks_with_apcs
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

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BasicBlock<F> {
    pub start_idx: usize,
    pub statements: Vec<Instruction<F>>,
}

impl<F: PrimeField32> BasicBlock<F> {
    fn pretty_print(&self, instr_formatter: impl Fn(&Instruction<F>) -> String) -> String {
        format!("BasicBlock(start_idx: {}, statements: [\n", self.start_idx)
            + &self
                .statements
                .iter()
                .enumerate()
                .map(|(i, instr)| format!("   instr {i:>3}:   {}", instr_formatter(instr)))
                .collect::<Vec<_>>()
                .join("\n")
            + "\n])"
    }
}

pub fn collect_basic_blocks<F: PrimeField32>(
    program: &Program<F>,
    labels: &BTreeSet<u32>,
    opcode_allowlist: &BTreeSet<usize>,
    branch_opcodes: &BTreeSet<usize>,
) -> Vec<BasicBlock<F>> {
    let mut blocks = Vec::new();
    let mut curr_block = BasicBlock {
        start_idx: 0,
        statements: Vec::new(),
    };
    for (i, instr) in program.instructions_and_debug_infos.iter().enumerate() {
        let instr = instr.as_ref().unwrap().0.clone();
        let adjusted_pc = OPENVM_INIT_PC + (i as u32) * 4;
        let is_target = labels.contains(&adjusted_pc);
        let is_branch = branch_opcodes.contains(&instr.opcode.as_usize());

        // If this opcode cannot be in an apc, we make sure it's alone in a BB.
        if !opcode_allowlist.contains(&instr.opcode.as_usize()) {
            // If not empty, push the current block.
            if !curr_block.statements.is_empty() {
                blocks.push(curr_block);
            }
            // Push the instruction itself
            blocks.push(BasicBlock {
                start_idx: i,
                statements: vec![instr.clone()],
            });
            // Skip the instrucion and start a new block from the next instruction.
            curr_block = BasicBlock {
                start_idx: i + 1,
                statements: Vec::new(),
            };
        } else {
            // If the instruction is a target, we need to close the previous block
            // as is if not empty and start a new block from this instruction.
            if is_target {
                if !curr_block.statements.is_empty() {
                    blocks.push(curr_block);
                }
                curr_block = BasicBlock {
                    start_idx: i,
                    statements: Vec::new(),
                };
            }
            curr_block.statements.push(instr.clone());
            // If the instruction is a branch, we need to close this block
            // with this instruction and start a new block from the next one.
            if is_branch {
                blocks.push(curr_block); // guaranteed to be non-empty because an instruction was just pushed
                curr_block = BasicBlock {
                    start_idx: i + 1,
                    statements: Vec::new(),
                };
            }
        }
    }

    if !curr_block.statements.is_empty() {
        blocks.push(curr_block);
    }

    blocks
}

/// Besides the base RISCV-V branching instructions, the bigint extension adds two more branching
/// instruction classes over BranchEqual and BranchLessThan.
/// Those instructions have the form <INSTR rs0 rs1 target_offset ...>, where target_offset is the
/// relative jump we're interested in.
/// This means that for a given program address A containing the instruction above,
/// we add A + target_offset as a target as well.
fn add_extra_targets<F: PrimeField32>(
    program: &Program<F>,
    mut labels: BTreeSet<u32>,
) -> BTreeSet<u32> {
    let branch_opcodes_bigint = branch_opcodes_bigint_set();
    let new_labels = program
        .instructions_and_debug_infos
        .iter()
        .enumerate()
        .filter_map(|(i, instr)| {
            let instr = instr.as_ref().unwrap().0.clone();
            let adjusted_pc = OPENVM_INIT_PC + (i as u32) * 4;
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
fn create_apcs_for_all_blocks<P: IntoOpenVm>(
    blocks: Vec<BasicBlock<OpenVmField<P>>>,
    powdr_config: &PowdrConfig,
    airs: &OriginalAirs<P>,
    bus_map: &BusMap,
) -> Vec<Apc<P>> {
    let n_acc = powdr_config.autoprecompiles as usize;
    tracing::info!("Generating {n_acc} autoprecompiles in parallel");

    blocks
        .into_par_iter()
        .skip(powdr_config.skip_autoprecompiles as usize)
        .take(n_acc)
        .enumerate()
        .map(|(index, acc_block)| {
            tracing::debug!(
                "Accelerating block of length {} and start idx {}",
                acc_block.statements.len(),
                acc_block.start_idx
            );

            tracing::debug!(
                "Acc block: {}",
                acc_block.pretty_print(openvm_instruction_formatter)
            );

            let apc_opcode = POWDR_OPCODE + index;

            generate_autoprecompile(
                &acc_block,
                airs,
                apc_opcode,
                bus_map,
                powdr_config.degree_bound,
            )
            .unwrap()
        })
        .collect()
}

// OpenVM relevant bus ids:
// 0: execution bridge -> [pc, timestamp]
// 1: memory -> [address space, pointer, data, timestamp, 1]
// 2: pc lookup -> [...]
// 3: range tuple -> [col, bits]
// 5: bitwise xor ->
//    [a, b, 0, 0] byte range checks for a and b
//    [a, b, c, 1] c = xor(a, b)
fn generate_autoprecompile<P: IntoOpenVm>(
    block: &BasicBlock<OpenVmField<P>>,
    airs: &OriginalAirs<P>,
    apc_opcode: usize,
    bus_map: &BusMap,
    degree_bound: DegreeBound,
) -> Result<Apc<P>, Error> {
    tracing::debug!(
        "Generating autoprecompile for block at index {}",
        block.start_idx
    );
    let block = SymbolicBlock {
        statements: block
            .statements
            .iter()
            .map(|instr| SymbolicInstructionStatement {
                opcode: instr.opcode.as_usize(),
                args: [
                    instr.a, instr.b, instr.c, instr.d, instr.e, instr.f, instr.g,
                ]
                .iter()
                .map(|f| P::from_openvm_field(*f))
                .collect(),
            })
            .collect(),
        start_idx: block.start_idx,
    };

    let vm_config = VmConfig {
        instruction_machine_handler: airs,
        bus_interaction_handler: OpenVmBusInteractionHandler::new(bus_map.clone()),
        bus_map: bus_map.clone(),
    };

    let apc = powdr_autoprecompiles::build(block, vm_config, degree_bound, apc_opcode as u32)?;

    // Check that substitution values are unique over all instructions
    assert!(apc.subs().iter().flatten().all_unique());

    tracing::debug!(
        "Done generating autoprecompile for block at index {}",
        apc.block.start_idx
    );

    Ok(apc)
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
struct ApcCandidate<P> {
    apc: Apc<P>,
    execution_frequency: usize,
    width_before: usize,
    width_after: usize,
}

#[derive(Serialize, Deserialize)]
struct ApcCandidateJsonExport<P> {
    // opcode
    opcode: u32,
    // execution_frequency
    execution_frequency: usize,
    // original instructions
    original_block: SymbolicBlock<P>,
    // total width before optimisation
    total_width_before: usize,
    // total width after optimisation
    total_width_after: usize,
    // path to the apc candidate file
    apc_candidate_file: String,
}

impl ApcCandidate<BabyBearField> {
    /// Try to create an autoprecompile candidate from a block.
    #[allow(clippy::too_many_arguments)]
    pub fn try_create(
        block: BasicBlock<OpenVmField<BabyBearField>>,
        airs: &OriginalAirs<BabyBearField>,
        opcode: usize,
        bus_map: &BusMap,
        degree_bound: DegreeBound,
        pgo_program_idx_count: &HashMap<u32, u32>,
    ) -> Option<Self> {
        let apc = generate_autoprecompile(&block, airs, opcode, bus_map, degree_bound).ok()?;

        let apc_metrics = get_air_metrics(Arc::new(PowdrAir::new(apc.machine().clone())));
        let width_after = apc_metrics.widths.total();

        let width_before: usize = block
            .statements
            .iter()
            .map(|instr| {
                airs.get_instruction_metrics(instr.opcode.as_usize())
                    .unwrap()
                    .widths
                    .total()
            })
            .sum();

        let execution_frequency = *pgo_program_idx_count
            .get(&(block.start_idx as u32))
            .unwrap_or(&0) as usize;

        let candidate = Self {
            apc,
            execution_frequency,
            width_before,
            width_after,
        };

        Some(candidate)
    }

    /// Save the candidate to disk.
    fn save_to_disk(
        &self,
        apc_candidates_dir_path: &Path,
        apc_candidates_json_file: Arc<Mutex<Vec<ApcCandidateJsonExport<BabyBearField>>>>,
    ) {
        let ser_path = apc_candidates_dir_path
            .join(format!("apc_candidate_{}", self.apc.opcode))
            .with_extension("cbor");
        apc_candidates_json_file
            .lock()
            .unwrap()
            .push(ApcCandidateJsonExport {
                opcode: self.apc.opcode,
                execution_frequency: self.execution_frequency,
                original_block: self.apc.block.clone(),
                total_width_before: self.width_before,
                total_width_after: self.width_after,
                apc_candidate_file: ser_path.display().to_string(),
            });
        std::fs::create_dir_all(apc_candidates_dir_path)
            .expect("Failed to create directory for APC candidates");
        let file =
            std::fs::File::create(&ser_path).expect("Failed to create file for APC candidate");
        serde_cbor::to_writer(file, &self).expect("Failed to write APC candidate to file");
    }
}

impl<P> ApcCandidate<P> {
    fn cells_saved_per_row(&self) -> usize {
        // The number of cells saved per row is the difference between the width before and after the APC.
        self.width_before - self.width_after
    }
}

impl<P> KnapsackItem for ApcCandidate<P> {
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

// Note: This function can lead to OOM since it generates the apc for many blocks.
fn create_apcs_with_cell_pgo(
    mut blocks: Vec<BasicBlock<OpenVmField<BabyBearField>>>,
    pgo_program_idx_count: HashMap<u32, u32>,
    max_total_columns: Option<usize>,
    airs: &OriginalAirs<BabyBearField>,
    config: &PowdrConfig,
    original_config: &OriginalVmConfig,
    bus_map: &BusMap,
) -> Vec<Apc<BabyBearField>> {
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

    let max_total_apc_columns: Option<usize> = max_total_columns.map(|max_total_columns| {
        let total_non_apc_columns = original_config
            .chip_inventory_air_metrics()
            .values()
            .map(|m| m.total_width())
            .sum::<usize>();
        max_total_columns - total_non_apc_columns
    });

    let apc_candidates = Arc::new(Mutex::new(vec![]));

    // map–reduce over blocks into a single BinaryHeap<ApcCandidate<P>> capped at max_cache
    let res = fractional_knapsack(
        blocks.into_par_iter().enumerate().filter_map(|(i, block)| {
            ApcCandidate::try_create(
                block,
                airs,
        POWDR_OPCODE + i,
                bus_map,
                config.degree_bound,
                &pgo_program_idx_count,
            ).inspect(|candidate| {
                if let Some(apc_candidates_dir_path) = &config.apc_candidates_dir_path {
                    candidate.save_to_disk(apc_candidates_dir_path, apc_candidates.clone());
                }
            })
        }),
        max_cache,
        max_total_apc_columns,
    )
    .skip(config.skip_autoprecompiles as usize)
    .map(|c| {
        tracing::debug!(
            "Basic block start_idx: {}, cost adjusted value: {}, frequency: {}, cells_saved_per_row: {}",
            c.apc.block.start_idx,
            c.value() / c.cost(),
            c.execution_frequency,
            c.cells_saved_per_row(),
        );

        c.apc
    })
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

fn create_apcs_with_instruction_pgo<P: IntoOpenVm>(
    mut blocks: Vec<BasicBlock<OpenVmField<P>>>,
    pgo_program_idx_count: HashMap<u32, u32>,
    airs: &OriginalAirs<P>,
    config: &PowdrConfig,
    bus_map: &BusMap,
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

    create_apcs_for_all_blocks(blocks, config, airs, bus_map)
}

fn create_apcs_with_no_pgo<P: IntoOpenVm>(
    mut blocks: Vec<BasicBlock<OpenVmField<P>>>,
    airs: &OriginalAirs<P>,
    config: &PowdrConfig,
    bus_map: &BusMap,
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

    create_apcs_for_all_blocks(blocks, config, airs, bus_map)
}
