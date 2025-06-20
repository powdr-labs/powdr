use std::collections::HashSet;
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::sync::Arc;

use crate::extraction_utils::OriginalVmConfig;
use crate::opcode::ALL_OPCODES;
use crate::utils::UnsupportedOpenVmReferenceError;
use crate::IntoOpenVm;
use crate::OpenVmField;
use crate::OriginalCompiledProgram;
use crate::{CompiledProgram, SpecializedConfig};
use itertools::Itertools;
use openvm_bigint_transpiler::{Rv32BranchEqual256Opcode, Rv32BranchLessThan256Opcode};
use openvm_instructions::instruction::Instruction;
use openvm_instructions::program::Program;
use openvm_instructions::{LocalOpcode, VmOpcode};
use openvm_rv32im_transpiler::Rv32HintStoreOpcode;
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
    // tally opcode frequency
    if let PgoConfig::Cell(pgo_program_idx_count) = &pgo_config {
        tracing::debug!("pc count: {:#?}", pgo_program_idx_count);
        let mut tally = HashMap::new();
        exe.program.instructions_and_debug_infos.iter()
            .enumerate()
            .for_each(|(i, instr)| {
                let instr = instr.as_ref().unwrap().0.clone();
                if let Some(count) = pgo_program_idx_count.get(&(i as u32)) {
                    tally.entry(instr.opcode.as_usize())
                        .and_modify(|e| *e += count)
                        .or_insert(*count);
                }
            });
        let mut tally = tally.into_iter().collect::<Vec<_>>();
        tally.sort_by(|a, b| b.1.cmp(&a.1)); // sort by frequency descending

        fn first_part_str(s: String) -> String {
            s.split_whitespace()
             .next()
             .unwrap_or("")
             .to_string()
        }

        // print the tally
        println!("Opcode frequency tally");
        tally.iter().for_each(|(opcode, count)| {
            let dummy_instruction = Instruction { 
                opcode: VmOpcode::from_usize(*opcode),
                a: OpenVmField::<BabyBearField>::ZERO,
                b: OpenVmField::<BabyBearField>::ZERO,
                c: OpenVmField::<BabyBearField>::ZERO,
                d: OpenVmField::<BabyBearField>::ZERO,
                e: OpenVmField::<BabyBearField>::ZERO,
                f: OpenVmField::<BabyBearField>::ZERO,
                g: OpenVmField::<BabyBearField>::ZERO,
            };
            println!("{}: {}", first_part_str(openvm_instruction_formatter(&dummy_instruction)), count);
        });
    }

    panic!();


    let original_config = OriginalVmConfig::new(sdk_vm_config.clone());
    let airs = original_config.airs().expect("Failed to convert the AIR of an OpenVM instruction, even after filtering by the blacklist!");
    let bus_map = original_config.bus_map();

    // If we use PgoConfig::Cell, which creates APC for all eligible basic blocks,
    // `apc_cache` will be populated to be used later when we select which basic blocks to accelerate.
    // Otherwise, `apc_cache` will remain empty, and we will generate APC on the fly when we select which basic blocks to accelerate.
    let mut apc_cache = HashMap::new();
    let opcodes_allowlist = instruction_allowlist();

    let labels = add_extra_targets(&exe.program, labels.clone());
    let branch_opcodes_set = branch_opcodes_set();

    let blocks = collect_basic_blocks(
        &exe.program,
        &labels,
        &opcodes_allowlist,
        &branch_opcodes_set,
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

// Allowed opcodes = ALL_OPCODES (which includes bigint branch opcodes) - HINT_STOREW - HINT_BUFFER
pub fn instruction_allowlist() -> HashSet<usize> {
    let hint_storew = Rv32HintStoreOpcode::HINT_STOREW.global_opcode().as_usize();
    let hint_buffer = Rv32HintStoreOpcode::HINT_BUFFER.global_opcode().as_usize();

    // Filter out HINT_STOREW and HINT_BUFFER, which contain next references that don't work with apc
    ALL_OPCODES
        .iter()
        .map(|&op| op as usize)
        .filter(|&op| op != hint_storew && op != hint_buffer)
        .collect()
}

fn branch_opcodes() -> Vec<usize> {
    let mut opcodes = vec![
        openvm_rv32im_transpiler::BranchEqualOpcode::BEQ
            .global_opcode()
            .as_usize(),
        openvm_rv32im_transpiler::BranchEqualOpcode::BNE
            .global_opcode()
            .as_usize(),
        openvm_rv32im_transpiler::BranchLessThanOpcode::BLT
            .global_opcode()
            .as_usize(),
        openvm_rv32im_transpiler::BranchLessThanOpcode::BLTU
            .global_opcode()
            .as_usize(),
        openvm_rv32im_transpiler::BranchLessThanOpcode::BGE
            .global_opcode()
            .as_usize(),
        openvm_rv32im_transpiler::BranchLessThanOpcode::BGEU
            .global_opcode()
            .as_usize(),
        openvm_rv32im_transpiler::Rv32JalLuiOpcode::JAL
            .global_opcode()
            .as_usize(),
        openvm_rv32im_transpiler::Rv32JalrOpcode::JALR
            .global_opcode()
            .as_usize(),
    ];

    opcodes.extend(branch_opcodes_bigint());

    opcodes
}

fn branch_opcodes_bigint() -> Vec<usize> {
    vec![
        // The instructions below are structs so we cannot call `global_opcode()` on them without
        // an instnace, so we manually build the global opcodes.
        Rv32BranchEqual256Opcode::CLASS_OFFSET
            + openvm_rv32im_transpiler::BranchEqualOpcode::BEQ.local_usize(),
        Rv32BranchEqual256Opcode::CLASS_OFFSET
            + openvm_rv32im_transpiler::BranchEqualOpcode::BNE.local_usize(),
        Rv32BranchLessThan256Opcode::CLASS_OFFSET
            + openvm_rv32im_transpiler::BranchLessThanOpcode::BLT.local_usize(),
        Rv32BranchLessThan256Opcode::CLASS_OFFSET
            + openvm_rv32im_transpiler::BranchLessThanOpcode::BLTU.local_usize(),
        Rv32BranchLessThan256Opcode::CLASS_OFFSET
            + openvm_rv32im_transpiler::BranchLessThanOpcode::BGE.local_usize(),
        Rv32BranchLessThan256Opcode::CLASS_OFFSET
            + openvm_rv32im_transpiler::BranchLessThanOpcode::BGEU.local_usize(),
    ]
}

fn branch_opcodes_set() -> BTreeSet<usize> {
    branch_opcodes().into_iter().collect()
}

fn branch_opcodes_bigint_set() -> BTreeSet<usize> {
    branch_opcodes_bigint().into_iter().collect()
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
    opcode_allowlist: &HashSet<usize>,
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
    airs: &BTreeMap<usize, SymbolicMachine<P>>,
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
    airs: &BTreeMap<usize, SymbolicMachine<P>>,
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
    airs: &BTreeMap<usize, SymbolicMachine<P>>,
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
        instruction_machines: airs,
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
    airs: &BTreeMap<usize, SymbolicMachine<P>>,
    config: PowdrConfig,
    bus_map: BusMap,
    opcode_allowlist: &HashSet<usize>,
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
    let air_width_by_opcode = airs
        .iter()
        .filter(|&(i, _)| (opcode_allowlist.contains(i)))
        .map(|(i, air)| (*i, air.unique_references().count()))
        .collect::<HashMap<_, _>>();

    // generate apc and cache it for all basic blocks
    // calculate number of trace cells saved per row for each basic block to sort them by descending cost
    tracing::info!(
        "Generating autoprecompiles for all ({}) basic blocks in parallel",
        blocks.len()
    );

    let (cells_saved_per_row_by_bb, new_apc_cache): (
        HashMap<_, _>,
        HashMap<usize, CachedAutoPrecompile<P>>,
    ) = blocks
        .par_iter()
        .enumerate()
        .filter_map(|(i, acc_block)| {
            let apc_cache_entry = generate_apc_cache(
                acc_block,
                airs,
                POWDR_OPCODE + i,
                bus_map.clone(),
                config.degree_bound,
            )
            .ok()?;

            // calculate cells saved per row
            let apc_cells_per_row = apc_cache_entry.autoprecompile.unique_references().count();
            let original_cells_per_row: usize = acc_block
                .statements
                .iter()
                .map(|instruction| {
                    air_width_by_opcode
                        .get(&instruction.opcode.as_usize())
                        .unwrap()
                })
                .sum();
            let cells_saved_per_row = original_cells_per_row - apc_cells_per_row;
            assert!(cells_saved_per_row > 0);
            tracing::debug!(
                "Basic block start_idx: {}, cells saved per row: {}",
                acc_block.start_idx,
                cells_saved_per_row
            );

            Some((
                (acc_block.start_idx, cells_saved_per_row),
                (acc_block.start_idx, apc_cache_entry),
            ))
        })
        .unzip();

    apc_cache.extend(new_apc_cache);

    // filter out the basic blocks where the apc generation errored out
    blocks.retain(|b| cells_saved_per_row_by_bb.contains_key(&b.start_idx));

    // cost = frequency * cells_saved_per_row
    blocks.sort_by(|a, b| {
        let a_cells_saved = cells_saved_per_row_by_bb[&a.start_idx];
        let b_cells_saved = cells_saved_per_row_by_bb[&b.start_idx];

        let a_cnt = pgo_program_idx_count[&(a.start_idx as u32)];
        let b_cnt = pgo_program_idx_count[&(b.start_idx as u32)];

        (b_cells_saved * b_cnt as usize).cmp(&(a_cells_saved * a_cnt as usize))
    });

    // Debug print blocks by descending cost
    for block in blocks {
        let start_idx = block.start_idx;
        let cells_saved = cells_saved_per_row_by_bb[&start_idx];
        let count = pgo_program_idx_count[&(start_idx as u32)];
        let cost = count * cells_saved as u32;

        tracing::debug!(
            "Basic block start_idx: {}, cost: {}, frequency: {}, cells_saved_per_row: {}",
            start_idx,
            cost,
            count,
            cells_saved,
        );
    }
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
