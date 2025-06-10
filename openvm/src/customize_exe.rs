use std::collections::{BTreeMap, BTreeSet, HashMap};

use crate::IntoOpenVm;
use crate::OpenVmField;
use itertools::Itertools;
use openvm_algebra_transpiler::{Fp2Opcode, Rv32ModularArithmeticOpcode};
use openvm_bigint_transpiler::{Rv32BranchEqual256Opcode, Rv32BranchLessThan256Opcode};
use openvm_ecc_transpiler::Rv32WeierstrassOpcode;
use openvm_instructions::{exe::VmExe, instruction::Instruction, program::Program, VmOpcode};
use openvm_instructions::{LocalOpcode, SystemOpcode};
use openvm_keccak256_transpiler::Rv32KeccakOpcode;
use openvm_rv32im_transpiler::{Rv32HintStoreOpcode, Rv32LoadStoreOpcode};
use openvm_sdk::config::SdkVmConfig;
use openvm_sha256_transpiler::Rv32Sha256Opcode;
use openvm_stark_backend::{
    interaction::SymbolicInteraction,
    p3_field::{FieldAlgebra, PrimeField32},
};
use powdr_autoprecompiles::powdr::UniqueColumns;
use powdr_autoprecompiles::DegreeBound;
use powdr_autoprecompiles::VmConfig;
use powdr_autoprecompiles::{
    BusMap, SymbolicBusInteraction, SymbolicInstructionStatement, SymbolicMachine,
};
use powdr_number::FieldElement;

use crate::bus_interaction_handler::OpenVmBusInteractionHandler;
use crate::instruction_formatter::openvm_instruction_formatter;
use crate::{
    powdr_extension::{OriginalInstruction, PowdrExtension, PowdrOpcode, PowdrPrecompile},
    utils::symbolic_to_algebraic,
};

pub const OPENVM_DEGREE_BOUND: usize = 5;

// TODO: read this from program
const OPENVM_INIT_PC: u32 = 0x0020_0800;

const POWDR_OPCODE: usize = 0x10ff;

type CachedAutoPrecompile<F> = (
    usize,              // powdr opcode
    SymbolicMachine<F>, // autoprecompile
    Vec<Vec<u64>>,      // poly id substitution of original columns
);

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

pub fn customize<P: IntoOpenVm>(
    mut exe: VmExe<OpenVmField<P>>,
    base_config: SdkVmConfig,
    labels: &BTreeSet<u32>,
    airs: &BTreeMap<usize, SymbolicMachine<P>>,
    config: PowdrConfig,
    pgo_config: PgoConfig,
) -> (VmExe<OpenVmField<P>>, PowdrExtension<P>) {
    // If we use PgoConfig::Cell, which creates APC for all eligible basic blocks,
    // `apc_cache` will be populated to be used later when we select which basic blocks to accelerate.
    // Otherwise, `apc_cache` will remain empty, and we will generate APC on the fly when we select which basic blocks to accelerate.
    let mut apc_cache = HashMap::new();

    // The following opcodes shall never be accelerated and therefore always put in its own basic block.
    // Currently this contains OpenVm opcodes: Rv32HintStoreOpcode::HINT_STOREW (0x260) and Rv32HintStoreOpcode::HINT_BUFFER (0x261)
    // which are the only two opcodes from the Rv32HintStore, the air responsible for reading host states via stdin.
    // We don't want these opcodes because they create air constraints with next references, which powdr-openvm does not support yet.
    let opcodes_no_apc = vec![
        Rv32HintStoreOpcode::HINT_STOREW.global_opcode().as_usize(), // contain next references that don't work with apc
        Rv32HintStoreOpcode::HINT_BUFFER.global_opcode().as_usize(), // contain next references that don't work with apc
        Rv32LoadStoreOpcode::LOADB.global_opcode().as_usize(),
        Rv32LoadStoreOpcode::LOADH.global_opcode().as_usize(),
        Rv32WeierstrassOpcode::EC_ADD_NE.global_opcode().as_usize(),
        Rv32WeierstrassOpcode::SETUP_EC_ADD_NE
            .global_opcode()
            .as_usize(),
        Rv32WeierstrassOpcode::EC_DOUBLE.global_opcode().as_usize(),
        Rv32WeierstrassOpcode::SETUP_EC_DOUBLE
            .global_opcode()
            .as_usize(),
        Rv32WeierstrassOpcode::EC_ADD_NE.global_opcode().as_usize() + 4,
        Rv32WeierstrassOpcode::SETUP_EC_ADD_NE
            .global_opcode()
            .as_usize()
            + 4,
        Rv32WeierstrassOpcode::EC_DOUBLE.global_opcode().as_usize() + 4,
        Rv32WeierstrassOpcode::SETUP_EC_DOUBLE
            .global_opcode()
            .as_usize()
            + 4,
        Rv32KeccakOpcode::KECCAK256.global_opcode().as_usize(),
        Rv32Sha256Opcode::SHA256.global_opcode().as_usize(),
        Rv32ModularArithmeticOpcode::ADD.global_opcode().as_usize(),
        Rv32ModularArithmeticOpcode::SUB.global_opcode().as_usize(),
        Rv32ModularArithmeticOpcode::SETUP_ADDSUB
            .global_opcode()
            .as_usize(),
        Rv32ModularArithmeticOpcode::MUL.global_opcode().as_usize(),
        Rv32ModularArithmeticOpcode::DIV.global_opcode().as_usize(),
        Rv32ModularArithmeticOpcode::SETUP_MULDIV
            .global_opcode()
            .as_usize(),
        Rv32ModularArithmeticOpcode::IS_EQ
            .global_opcode()
            .as_usize(),
        Rv32ModularArithmeticOpcode::SETUP_ISEQ
            .global_opcode()
            .as_usize(),
        Fp2Opcode::ADD.global_opcode().as_usize(),
        Fp2Opcode::SUB.global_opcode().as_usize(),
        Fp2Opcode::SETUP_ADDSUB.global_opcode().as_usize(),
        Fp2Opcode::MUL.global_opcode().as_usize(),
        Fp2Opcode::DIV.global_opcode().as_usize(),
        Fp2Opcode::SETUP_MULDIV.global_opcode().as_usize(),
        SystemOpcode::PHANTOM.global_opcode().as_usize(),
        SystemOpcode::TERMINATE.global_opcode().as_usize(),
        // TODO clean this up
        0x510, // not sure yet what this is
        0x513, // not sure yet what this is
        0x516, // not sure yet what this is
        0x51c, // not sure yet what this is
        0x523, // not sure yet what this is
        0x526, // not sure yet what this is
        1024,
        1025,
        1028,
        1033,
        1104,
        1030,
        1033,
        1027,
        1029,
    ];

    let labels = add_extra_targets(&exe.program, labels.clone());
    let branch_opcodes_set = branch_opcodes_set();

    let mut blocks =
        collect_basic_blocks(&exe.program, &labels, &opcodes_no_apc, &branch_opcodes_set);
    tracing::info!(
        "Got {} basic blocks from `collect_basic_blocks`",
        blocks.len()
    );

    // print start index of all retained blocks
    blocks.iter().enumerate().for_each(|(i, block)| {
        tracing::info!("Block {}: start_idx {}", i, block.start_idx);
    });

    // sort basic blocks by:
    // 1. if PgoConfig::Cell, cost = frequency * cells_saved_per_row
    // 2. if PgoConfig::Instruction, cost = frequency * number_of_instructions
    // 3. if PgoConfig::None, cost = number_of_instructions
    match pgo_config {
        PgoConfig::Cell(pgo_program_idx_count) => {
            sort_blocks_by_pgo_cell_cost(
                &mut blocks,
                &mut apc_cache,
                pgo_program_idx_count,
                airs,
                config.clone(),
                &opcodes_no_apc,
            );
        }
        PgoConfig::Instruction(pgo_program_idx_count) => {
            sort_blocks_by_pgo_instruction_cost(&mut blocks, pgo_program_idx_count);
        }
        PgoConfig::None => {
            sort_blocks_by_length(&mut blocks);
        }
    };

    let program = &mut exe.program.instructions_and_debug_infos;

    let noop = Instruction {
        opcode: VmOpcode::from_usize(0xdeadaf),
        a: OpenVmField::<P>::ZERO,
        b: OpenVmField::<P>::ZERO,
        c: OpenVmField::<P>::ZERO,
        d: OpenVmField::<P>::ZERO,
        e: OpenVmField::<P>::ZERO,
        f: OpenVmField::<P>::ZERO,
        g: OpenVmField::<P>::ZERO,
    };

    let mut extensions = Vec::new();
    let n_acc = config.autoprecompiles as usize;
    let n_skip = config.skip_autoprecompiles as usize;
    tracing::info!("Generating {n_acc} autoprecompiles");

    // now the blocks have been sorted by cost
    for (i, acc_block) in blocks.iter().skip(n_skip).take(n_acc).enumerate() {
        tracing::debug!(
            "Accelerating block {i} of length {} and start idx {}",
            acc_block.statements.len(),
            acc_block.start_idx
        );

        tracing::debug!(
            "Acc block: {}",
            acc_block.pretty_print(openvm_instruction_formatter)
        );

        // Lookup if an APC is already cached by PgoConfig::Cell and generate the APC if not
        let (apc_opcode, autoprecompile, subs) =
            apc_cache.remove(&acc_block.start_idx).unwrap_or_else(|| {
                generate_apc_cache(
                    acc_block,
                    airs,
                    POWDR_OPCODE + i,
                    config.bus_map.clone(),
                    config.degree_bound,
                )
                .expect("Failed to generate autoprecompile")
            });

        let new_instr = Instruction {
            opcode: VmOpcode::from_usize(apc_opcode),
            a: OpenVmField::<P>::ZERO,
            b: OpenVmField::<P>::ZERO,
            c: OpenVmField::<P>::ZERO,
            d: OpenVmField::<P>::ZERO,
            e: OpenVmField::<P>::ZERO,
            f: OpenVmField::<P>::ZERO,
            g: OpenVmField::<P>::ZERO,
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
            .unique_columns()
            .find(|c| c.name == "is_valid")
            .unwrap();

        let opcodes_in_acc = acc
            .iter()
            .map(|x| x.opcode.as_usize())
            .unique()
            .collect_vec();

        extensions.push(PowdrPrecompile::new(
            format!("PowdrAutoprecompile_{i}"),
            PowdrOpcode {
                class_offset: apc_opcode,
            },
            autoprecompile,
            acc.into_iter()
                .zip_eq(subs)
                .map(|(instruction, subs)| OriginalInstruction::new(instruction, subs))
                .collect(),
            airs.iter()
                .filter(|(i, _)| opcodes_in_acc.contains(*i))
                .map(|(i, air)| (*i, air.clone()))
                .collect(),
            is_valid_column,
        ));
    }

    (
        exe,
        PowdrExtension::new(
            extensions,
            base_config,
            config.implementation,
            config.bus_map,
        ),
    )
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
        openvm_rv32im_transpiler::Rv32JalLuiOpcode::LUI
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
    opcodes_no_apc: &[usize],
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
        if opcodes_no_apc.contains(&instr.opcode.as_usize()) {
            // If not empty, push the current block.
            if !curr_block.statements.is_empty() {
                blocks.push(curr_block);
            }
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

    Ok((apc_opcode, autoprecompile, subs))
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
    columns: &[String],
) -> SymbolicBusInteraction<P> {
    let id = interaction.bus_index as u64;

    let mult = symbolic_to_algebraic(&interaction.count, columns);
    let args = interaction
        .message
        .iter()
        .map(|e| symbolic_to_algebraic(e, columns))
        .collect();

    SymbolicBusInteraction { id, mult, args }
}

fn sort_blocks_by_pgo_cell_cost<P: IntoOpenVm>(
    blocks: &mut Vec<BasicBlock<OpenVmField<P>>>,
    apc_cache: &mut HashMap<usize, CachedAutoPrecompile<P>>,
    pgo_program_idx_count: HashMap<u32, u32>,
    airs: &BTreeMap<usize, SymbolicMachine<P>>,
    config: PowdrConfig,
    opcodes_no_apc: &[usize],
) {
    // drop any block whose start index cannot be found in pc_idx_count,
    // because a basic block might not be executed at all.
    // Also only keep basic blocks with more than one original instruction.
    blocks.retain(|b| {
        pgo_program_idx_count.contains_key(&(b.start_idx as u32)) && b.statements.len() > 1
    });

    tracing::info!(
        "Retained {} basic blocks after filtering by pc_idx_count",
        blocks.len()
    );

    // store air width by opcode, so that we don't repetitively calculate them later
    // filter out opcodes that contain next references in their air, because they are not supported yet in apc
    let air_width_by_opcode = airs
        .iter()
        .filter(|&(i, _)| (!opcodes_no_apc.contains(i)))
        .map(|(i, air)| (*i, air.unique_columns().count()))
        .collect::<HashMap<_, _>>();

    // generate apc and cache it for all basic blocks
    // calculate number of trace cells saved per row for each basic block to sort them by descending cost
    let cells_saved_per_row_by_bb: HashMap<_, _> = blocks
        .iter()
        .enumerate()
        .filter_map(|(i, acc_block)| {
            let apc_cache_entry = generate_apc_cache(
                acc_block,
                airs,
                POWDR_OPCODE + i,
                config.bus_map.clone(),
                config.degree_bound,
            )
            .ok()?;
            apc_cache.insert(acc_block.start_idx, apc_cache_entry.clone());

            // calculate cells saved per row
            let apc_cells_per_row = apc_cache_entry.1.unique_columns().count();
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
            tracing::info!(
                "Basic block start_idx: {}, cells saved per row: {}",
                acc_block.start_idx,
                cells_saved_per_row
            );

            Some((acc_block.start_idx, cells_saved_per_row))
        })
        .collect();

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

        tracing::info!(
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

    tracing::info!(
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

        tracing::info!(
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
        tracing::info!(
            "Basic block start_idx: {}, number_of_instructions: {}",
            start_idx,
            block.statements.len(),
        );
    }
}
