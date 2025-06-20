use std::cmp::Ordering;
use std::collections::{BTreeSet, BinaryHeap, HashMap};
use std::sync::Arc;

use crate::extraction_utils::{OriginalAirs, OriginalVmConfig};
use crate::opcode::{branch_opcodes_bigint_set, branch_opcodes_set, instruction_allowlist};
use crate::utils::UnsupportedOpenVmReferenceError;
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
use powdr_autoprecompiles::powdr::UniqueReferences;
use powdr_autoprecompiles::DegreeBound;
use powdr_autoprecompiles::VmConfig;
use powdr_autoprecompiles::{
    bus_map::BusMap, SymbolicBusInteraction, SymbolicInstructionStatement, SymbolicMachine,
};
use powdr_number::{BabyBearField, FieldElement};

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

#[derive(Clone, Debug)]
pub struct CachedAutoPrecompile<F> {
    apc_opcode: usize,
    autoprecompile: SymbolicMachine<F>,
    subs: Vec<Vec<u64>>,
}

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

    // If we use PgoConfig::Cell, which creates APC for all eligible basic blocks,
    // `apc_cache` will be populated to be used later when we select which basic blocks to accelerate.
    // Otherwise, `apc_cache` will remain empty, and we will generate APC on the fly when we select which basic blocks to accelerate.
    let mut apc_cache = HashMap::new();
    let opcodes_allowlist = instruction_allowlist();

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

    let mut blocks = blocks
        .into_iter()
        .filter(|b| {
            b.statements
                .iter()
                .all(|instr| opcodes_allowlist.contains(&instr.opcode.as_usize()))
        })
        .collect::<Vec<_>>();

    // sort basic blocks by:
    // 1. if PgoConfig::Cell, cost = frequency * cells_saved_per_row
    // 2. if PgoConfig::Instruction, cost = frequency * number_of_instructions
    // 3. if PgoConfig::None, cost = number_of_instructions
    match pgo_config {
        PgoConfig::Cell(pgo_program_idx_count) => {
            sort_blocks_by_pgo_cell_cost_and_cache_apc(
                &mut blocks,
                &mut apc_cache,
                pgo_program_idx_count,
                &airs,
                config.clone(),
                bus_map.clone(),
                &opcodes_allowlist,
            );
        }
        PgoConfig::Instruction(pgo_program_idx_count) => {
            sort_blocks_by_pgo_instruction_cost(&mut blocks, pgo_program_idx_count);
            cache_apc_for_all_acc_blocks(&mut apc_cache, &config, &blocks, &airs, bus_map);
        }
        PgoConfig::None => {
            sort_blocks_by_length(&mut blocks);
            cache_apc_for_all_acc_blocks(&mut apc_cache, &config, &blocks, &airs, bus_map);
        }
    };

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

    let mut extensions = Vec::new();
    let n_acc = config.autoprecompiles as usize;
    let n_skip = config.skip_autoprecompiles as usize;

    tracing::info!("Adjust the program with the autoprecompiles");

    // now the blocks have been sorted by cost
    for acc_block in blocks.iter().skip(n_skip).take(n_acc) {
        let CachedAutoPrecompile {
            apc_opcode,
            autoprecompile,
            subs,
        } = apc_cache.remove(&acc_block.start_idx).unwrap();

        let new_instr = Instruction {
            opcode: VmOpcode::from_usize(apc_opcode),
            a: OpenVmField::<BabyBearField>::ZERO,
            b: OpenVmField::<BabyBearField>::ZERO,
            c: OpenVmField::<BabyBearField>::ZERO,
            d: OpenVmField::<BabyBearField>::ZERO,
            e: OpenVmField::<BabyBearField>::ZERO,
            f: OpenVmField::<BabyBearField>::ZERO,
            g: OpenVmField::<BabyBearField>::ZERO,
        };

        let pc = acc_block.start_idx;
        let n_acc = acc_block.statements.len();
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

        let is_valid_column = autoprecompile
            .unique_references()
            .find(|c| &*c.name == "is_valid")
            .unwrap();

        extensions.push(PowdrPrecompile::new(
            format!("PowdrAutoprecompile_{apc_opcode}"),
            PowdrOpcode {
                class_offset: apc_opcode,
            },
            autoprecompile,
            acc.into_iter()
                .zip_eq(subs)
                .map(|(instruction, subs)| OriginalInstruction::new(instruction, subs))
                .collect(),
            is_valid_column,
        ));
    }

    CompiledProgram {
        exe,
        vm_config: SpecializedConfig::new(original_config, extensions, config.implementation),
    }
}

#[derive(Debug, Clone)]
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

fn generate_apc_cache<P: IntoOpenVm>(
    block: &BasicBlock<OpenVmField<P>>,
    airs: &OriginalAirs<P>,
    apc_opcode: usize,
    bus_map: BusMap,
    degree_bound: DegreeBound,
) -> Result<CachedAutoPrecompile<P>, Error> {
    let (autoprecompile, subs) =
        generate_autoprecompile(block, airs, apc_opcode, bus_map, degree_bound)?;

    Ok(CachedAutoPrecompile {
        apc_opcode,
        autoprecompile,
        subs,
    })
}

// Only used for PgoConfig::Instruction and PgoConfig::None,
// because PgoConfig::Cell caches all APCs in sorting stage.
fn cache_apc_for_all_acc_blocks<P: IntoOpenVm>(
    apc_cache: &mut HashMap<usize, CachedAutoPrecompile<P>>,
    powdr_config: &PowdrConfig,
    blocks: &[BasicBlock<OpenVmField<P>>],
    airs: &OriginalAirs<P>,
    bus_map: BusMap,
) {
    let n_acc = powdr_config.autoprecompiles as usize;
    tracing::info!("Generating {n_acc} autoprecompiles in parallel");

    let apcs = blocks
        .par_iter()
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

            (
                acc_block.start_idx,
                generate_apc_cache(
                    acc_block,
                    airs,
                    apc_opcode,
                    bus_map.clone(),
                    powdr_config.degree_bound,
                )
                .unwrap(),
            )
        })
        .collect::<Vec<_>>();

    apc_cache.extend(apcs);
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
    bus_map: BusMap,
    degree_bound: DegreeBound,
) -> Result<(SymbolicMachine<P>, Vec<Vec<u64>>), Error> {
    tracing::debug!(
        "Generating autoprecompile for block at index {}",
        block.start_idx
    );
    let program = block
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
        .collect();

    let vm_config = VmConfig {
        instruction_machine_handler: airs,
        bus_interaction_handler: OpenVmBusInteractionHandler::new(bus_map.clone()),
        bus_map,
    };

    let (precompile, subs) =
        powdr_autoprecompiles::build(program, vm_config, degree_bound, apc_opcode as u32)?;

    // Check that substitution values are unique over all instructions
    assert!(subs.iter().flatten().all_unique());

    tracing::debug!(
        "Done generating autoprecompile for block at index {}",
        block.start_idx
    );

    Ok((precompile, subs))
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

// Note: This function can lead to OOM since it generates the apc for all blocks
fn sort_blocks_by_pgo_cell_cost_and_cache_apc<P: IntoOpenVm>(
    blocks: &mut Vec<BasicBlock<OpenVmField<P>>>,
    apc_cache: &mut HashMap<usize, CachedAutoPrecompile<P>>,
    pgo_program_idx_count: HashMap<u32, u32>,
    airs: &OriginalAirs<P>,
    config: PowdrConfig,
    bus_map: BusMap,
    opcode_allowlist: &BTreeSet<usize>,
) {
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

    // store air width by opcode, so that we don't repetitively calculate them later
    // filter out opcodes that contain next references in their air, because they are not supported yet in apc
    let air_width_by_opcode = airs.air_width_per_opcode(opcode_allowlist);

    // generate apc for all basic blocks and only cache the ones we eventually use
    // calculate number of trace cells saved per row for each basic block to sort them by descending cost
    let max_cache = (config.autoprecompiles + config.skip_autoprecompiles) as usize;
    tracing::info!(
        "Generating autoprecompiles for all ({}) basic blocks in parallel and caching costliest {}",
        blocks.len(),
        max_cache,
    );

    // each generated apc becomes a candidate for caching
    // it is only ordered by cost, but carries all needed data for modifying the blocks, extending the cache, and debug print
    struct ApcCandidate<P: IntoOpenVm> {
        block: BasicBlock<OpenVmField<P>>,
        apc_cache_entry: CachedAutoPrecompile<P>,
        apc_cost: usize,
        execution_frequency: usize,
        cells_saved_per_row: usize,
    }

    impl<P: IntoOpenVm> PartialEq for ApcCandidate<P> {
        fn eq(&self, other: &Self) -> bool {
            self.apc_cost == other.apc_cost
        }
    }

    impl<P: IntoOpenVm> Eq for ApcCandidate<P> {}

    impl<P: IntoOpenVm> PartialOrd for ApcCandidate<P> {
        fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
            Some(self.cmp(other))
        }
    }

    // order apc candidates by reverse cost, so that our BinaryHeap pops the smallest costed apc candidate first
    impl<P: IntoOpenVm> Ord for ApcCandidate<P> {
        fn cmp(&self, other: &Self) -> Ordering {
            other.apc_cost.cmp(&self.apc_cost)
        }
    }

    // map–reduce over blocks into a single BinaryHeap<ApcCandidate<P>> capped at max_cache
    // returned caches and blocks are ordered by descending cost already because they are unzipped from the min heap
    let (new_apc_cache, retained_blocks_with_stats): (
        HashMap<usize, CachedAutoPrecompile<_>>,
        Vec<_>,
    ) = blocks
        .par_iter()
        .enumerate()
        .filter_map(|(i, block)| {
            // try to create apc for a candidate block
            let apc_cache_entry = generate_apc_cache(
                block,
                airs,
                POWDR_OPCODE + i,
                bus_map.clone(),
                config.degree_bound,
            )
            .ok()?; // if apc creation fails, filter out this candidate block

            // compute cost and cells_saved_per_row
            let apc_cells_per_row = apc_cache_entry.autoprecompile.unique_references().count();
            let orig_cells_per_row: usize = block
                .statements
                .iter()
                .map(|instr| air_width_by_opcode[&instr.opcode])
                .sum();
            let cells_saved_per_row = orig_cells_per_row - apc_cells_per_row;
            let execution_frequency = *pgo_program_idx_count
                .get(&(block.start_idx as u32))
                .unwrap_or(&0) as usize;
            let apc_cost = cells_saved_per_row * execution_frequency;

            // build a 1-element heap
            let mut heap = BinaryHeap::new();
            heap.push(ApcCandidate {
                block: block.clone(),
                apc_cache_entry,
                apc_cost,
                execution_frequency,
                cells_saved_per_row,
            });
            Some(heap)
        })
        .reduce(
            // identity: empty heap
            BinaryHeap::new,
            // merge two heaps, pruning back to max_cache
            |mut heap_acc, mut heap| {
                for apc_candidate in heap.drain() {
                    heap_acc.push(apc_candidate);
                    if heap_acc.len() > max_cache {
                        heap_acc.pop();
                    }
                }
                heap_acc
            },
        )
        .into_iter()
        .map(
            |ApcCandidate {
                 block,
                 apc_cache_entry,
                 apc_cost,
                 execution_frequency,
                 cells_saved_per_row,
             }| {
                (
                    (block.start_idx, apc_cache_entry),
                    (block, apc_cost, execution_frequency, cells_saved_per_row),
                )
            },
        )
        .unzip();

    apc_cache.extend(new_apc_cache);

    // debug print blocks by descending cost
    for (block, apc_cost, execution_frequency, cells_saved_per_row) in
        retained_blocks_with_stats.iter()
    {
        tracing::debug!(
            "Basic block start_idx: {}, cost: {}, frequency: {}, cells_saved_per_row: {}",
            block.start_idx,
            apc_cost,
            execution_frequency,
            cells_saved_per_row,
        );
    }

    let retained_blocks: Vec<BasicBlock<<P as IntoOpenVm>::Field>> = retained_blocks_with_stats
        .into_iter()
        .map(|(block, _, _, _)| block)
        .collect();

    blocks.clear(); // have to clear first and extend because &mut doesn't allow reassignment
    blocks.extend(retained_blocks);
}

fn sort_blocks_by_pgo_instruction_cost<F: PrimeField32>(
    blocks: &mut Vec<BasicBlock<F>>,
    pgo_program_idx_count: HashMap<u32, u32>,
) {
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
    for block in blocks {
        let start_idx = block.start_idx;
        let count = pgo_program_idx_count[&(start_idx as u32)];
        let cost = count * (block.statements.len() as u32);

        tracing::debug!(
            "Basic block start_idx: {}, cost: {}, frequency: {}, number_of_instructions: {}",
            start_idx,
            cost,
            count,
            block.statements.len(),
        );
    }
}

fn sort_blocks_by_length<F: PrimeField32>(blocks: &mut Vec<BasicBlock<F>>) {
    // cost = number_of_original_instructions
    blocks.sort_by(|a, b| b.statements.len().cmp(&a.statements.len()));

    // Debug print blocks by descending cost
    for block in blocks {
        let start_idx = block.start_idx;
        tracing::debug!(
            "Basic block start_idx: {}, number_of_instructions: {}",
            start_idx,
            block.statements.len(),
        );
    }
}
