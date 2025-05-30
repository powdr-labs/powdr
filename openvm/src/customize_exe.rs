use std::collections::{BTreeMap, BTreeSet, HashMap};

use itertools::Itertools;
use openvm::platform::print;
use openvm_algebra_transpiler::{Fp2Opcode, Rv32ModularArithmeticOpcode};
use openvm_ecc_transpiler::Rv32WeierstrassOpcode;
use openvm_instructions::{exe::VmExe, instruction::Instruction, program::Program, VmOpcode};
use openvm_instructions::{LocalOpcode, SystemOpcode};
use openvm_keccak256_transpiler::Rv32KeccakOpcode;
use openvm_rv32im_transpiler::{Rv32HintStoreOpcode, Rv32LoadStoreOpcode};
use openvm_sdk::config::SdkVmConfig;
use openvm_sha256_transpiler::Rv32Sha256Opcode;
use openvm_stark_backend::{interaction::SymbolicInteraction, p3_field::PrimeField32};
use powdr_ast::analyzed::AlgebraicExpression;
use powdr_autoprecompiles::powdr::UniqueColumns;
use powdr_autoprecompiles::{
    InstructionKind, SymbolicBusInteraction, SymbolicConstraint, SymbolicInstructionStatement,
    SymbolicMachine,
};
use powdr_number::FieldElement;

use crate::bus_interaction_handler::{BusMap, OpenVmBusInteractionHandler};
use crate::instruction_formatter::openvm_instruction_formatter;
use crate::utils::{to_ovm_field, to_powdr_field};
use crate::{
    powdr_extension::{OriginalInstruction, PowdrExtension, PowdrOpcode, PowdrPrecompile},
    utils::symbolic_to_algebraic,
};

pub const OPENVM_DEGREE_BOUND: usize = 5;

const POWDR_OPCODE: usize = 0x10ff;

use crate::PowdrConfig;

pub enum Error {
    AutoPrecompileError,
}

impl From<powdr_autoprecompiles::constraint_optimizer::Error> for Error {
    fn from(e: powdr_autoprecompiles::constraint_optimizer::Error) -> Self {
        Error::AutoPrecompileError
    }
}

pub fn customize<F: PrimeField32>(
    mut exe: VmExe<F>,
    base_config: SdkVmConfig,
    labels: &BTreeSet<u32>,
    airs: &BTreeMap<usize, SymbolicMachine<powdr_number::BabyBearField>>,
    config: PowdrConfig,
    pc_idx_count: Option<HashMap<u32, u32>>,
) -> (VmExe<F>, PowdrExtension<F>) {
    // The following opcodes shall never be accelerated and therefore always put in its own basic block.
    // Currently this contains OpenVm opcodes: Rv32HintStoreOpcode::HINT_STOREW (0x260) and Rv32HintStoreOpcode::HINT_BUFFER (0x261)
    // which are the only two opcodes from the Rv32HintStore, the air responsible for reading host states via stdin.
    // We don't want these opcodes because they create air constraints with next references, which powdr-openvm does not support yet.
    let opcode_next_reference = [
        Rv32HintStoreOpcode::HINT_STOREW.global_opcode().as_usize(), // contain next references that don't work with apc
        Rv32HintStoreOpcode::HINT_BUFFER.global_opcode().as_usize(), // contain next references that don't work with apc
    ];

    let mut opcodes_no_apc = vec![
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
        0x510, // not sure yet what this is
        0x513, // not sure yet what this is
        0x516, // not sure yet what this is
        0x51c, // not sure yet what this is
        0x523, // not sure yet what this is
        0x526, // not sure yet what this is
    ];
    opcodes_no_apc.extend(opcode_next_reference.iter());

    let mut blocks = collect_basic_blocks(&exe.program, labels, &opcodes_no_apc);
    tracing::info!("Got {} basic blocks", blocks.len());

    if let Some(pgo_program_idx_count) = pc_idx_count.clone() {
        // first, drop any block whose start index cannot be found in pc_idx_count,
        // because a basic block might not be executed at all.
        // Also only keep basic blocks with more than one original instruction.
        blocks.retain(|b| {
            pgo_program_idx_count.contains_key(&(b.start_idx as u32)) && b.statements.len() > 1
        });
    }

    tracing::info!(
        "Retained {} basic blocks after filtering by pc_idx_count",
        blocks.len()
    );

    // print start index of all retained blocks
    blocks.iter().enumerate().for_each(|(i, block)| {
        tracing::info!("Block {}: start_idx {}", i, block.start_idx);
    });

    // store air width by opcode, so that we don't repetitively calculate them later
    // filter out opcodes that contain next references in their air, because they are not supported yet in apc
    let air_width_by_opcode = airs
        .iter()
        .filter(|&(i, _)| (!opcode_next_reference.contains(i)))
        .map(|(i, air)| (*i, air.unique_columns().count()))
        .collect::<HashMap<_, _>>();

    // generate apc and cache it for all basic blocks
    // calculate number of trace cells saved per row for each basic block to sort them by descending cost
    let (cells_saved_per_row_by_bb, mut apcs): (HashMap<_, _>, HashMap<_, (_, _, _)>) = blocks
        .iter()
        .enumerate()
        .filter_map(|(i, acc_block)| {
            let apc_opcode = POWDR_OPCODE + i;
            let (autoprecompile, subs) = match generate_autoprecompile::<
                F,
                powdr_number::BabyBearField,
            >(
                acc_block, airs, apc_opcode, config.bus_map.clone()
            ) {
                Err(_) => {
                    return None;
                }
                Ok((autoprecompile, subs)) => (autoprecompile, subs),
            };
            // calculate cells saved per row
            let apc_cells_per_row = autoprecompile.unique_columns().count();
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
            tracing::info!(
                "Basic block start_idx: {}, cells saved per row: {}",
                acc_block.start_idx,
                cells_saved_per_row
            );
            assert!(cells_saved_per_row > 0);
            Some((
                (acc_block.start_idx, cells_saved_per_row),
                (acc_block.start_idx, (apc_opcode, autoprecompile, subs)),
            ))
        })
        .unzip();

    // filter out the basic blocks where the apc generation errored out
    blocks.retain(|b| cells_saved_per_row_by_bb.contains_key(&b.start_idx));

    // sort basic blocks by:
    // 1. if pc_idx_count (frequency) is provided, cost = frequency * cells_saved_per_row
    // 2. if pc_idx_count (frequency) is not provided, cost = cells_saved_per_row
    if let Some(pgo_program_idx_count) = pc_idx_count.clone() {
        blocks.sort_by(|a, b| {
            let a_cells_saved = cells_saved_per_row_by_bb[&a.start_idx];
            let b_cells_saved = cells_saved_per_row_by_bb[&b.start_idx];

            let a_cnt = pgo_program_idx_count[&(a.start_idx as u32)];
            let b_cnt = pgo_program_idx_count[&(b.start_idx as u32)];

            (b_cells_saved * b_cnt as usize).cmp(&(a_cells_saved * a_cnt as usize))
        });
    } else {
        blocks.sort_by(|a, b| {
            let a_cells_saved = cells_saved_per_row_by_bb[&(a.start_idx)];
            let b_cells_saved = cells_saved_per_row_by_bb[&(b.start_idx)];
            b_cells_saved.cmp(&a_cells_saved)
        });
    }

    // print sorted by descending cost
    // block start_idx | cost = (optional) frequency * cells_saved_per_row | (optional) frequency | cells_saved_per_row
    for block in &blocks {
        let start_idx = block.start_idx;
        let cells_saved = cells_saved_per_row_by_bb[&start_idx];
        if let Some(pgo_program_idx_count) = pc_idx_count.clone() {
            let count = pgo_program_idx_count[&(start_idx as u32)];
            let cost = count * cells_saved as u32;

            tracing::info!(
                "Basic block start_idx: {}, cost: {}, frequency: {}, cells_saved_per_row: {}",
                start_idx,
                cost,
                count,
                cells_saved,
            );
        } else {
            tracing::info!(
                "Basic block start_idx: {}, cells_saved_per_row: {}",
                start_idx,
                cells_saved
            );
        }
    }

    let program = &mut exe.program.instructions_and_debug_infos;

    let noop = Instruction {
        opcode: VmOpcode::from_usize(0xdeadaf),
        a: F::ZERO,
        b: F::ZERO,
        c: F::ZERO,
        d: F::ZERO,
        e: F::ZERO,
        f: F::ZERO,
        g: F::ZERO,
    };

    let mut extensions = Vec::new();
    let n_acc = config.autoprecompiles as usize;
    let n_skip = config.skip_autoprecompiles as usize;
    tracing::info!("Generating {n_acc} autoprecompiles");

    // now the blocks have been sorted by cost
    for (i, acc_block) in blocks.iter().skip(n_skip).take(n_acc).enumerate() {
        // use apc generated earlier
        let (apc_opcode, autoprecompile, subs) = apcs.remove(&acc_block.start_idx).unwrap();

        tracing::debug!(
            "Accelerating block {i} of length {} and start idx {}",
            acc_block.statements.len(),
            acc_block.start_idx
        );

        tracing::debug!(
            "Acc block: {}",
            acc_block.pretty_print(openvm_instruction_formatter)
        );

        let new_instr = Instruction {
            opcode: VmOpcode::from_usize(apc_opcode),
            a: F::ZERO,
            b: F::ZERO,
            c: F::ZERO,
            d: F::ZERO,
            e: F::ZERO,
            f: F::ZERO,
            g: F::ZERO,
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
            transpose_symbolic_machine(autoprecompile),
            acc.into_iter()
                .zip_eq(subs)
                .map(|(instruction, subs)| OriginalInstruction::new(instruction, subs))
                .collect(),
            airs.iter()
                .filter(|(i, _)| opcodes_in_acc.contains(*i))
                .map(|(i, air)| (*i, transpose_symbolic_machine(air.clone())))
                .collect(),
            is_valid_column,
        ));
    }

    (exe, PowdrExtension::new(extensions, base_config))
}

// TODO collect properly from opcode enums
const BRANCH_OPCODES: [usize; 9] = [
    0x220, 0x221, 0x225, 0x226, 0x227, 0x228, 0x230, 0x231, 0x235,
];
pub fn is_jump(instruction: &VmOpcode) -> bool {
    let opcode = instruction.as_usize();
    BRANCH_OPCODES.contains(&opcode)
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
) -> Vec<BasicBlock<F>> {
    let mut blocks = Vec::new();
    let mut curr_block = BasicBlock {
        start_idx: 0,
        statements: Vec::new(),
    };
    let init_pc = 0x0020_0800;
    for (i, instr) in program.instructions_and_debug_infos.iter().enumerate() {
        let instr = instr.as_ref().unwrap().0.clone();
        let adjusted_pc = init_pc + (i as u32) * 4;
        let is_target = labels.contains(&adjusted_pc);
        let is_branch = is_jump(&instr.opcode);

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
            // as is if not empty and start a new block from this instruction.
            if is_target {
                if !curr_block.statements.is_empty() {
                    blocks.push(curr_block);
                }
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

// OpenVM relevant bus ids:
// 0: execution bridge -> [pc, timestamp]
// 1: memory -> [address space, pointer, data, timestamp, 1]
// 2: pc lookup -> [...]
// 3: range tuple -> [col, bits]
// 5: bitwise xor ->
//    [a, b, 0, 0] byte range checks for a and b
//    [a, b, c, 1] c = xor(a, b)
fn generate_autoprecompile<F: PrimeField32, P: FieldElement>(
    block: &BasicBlock<F>,
    airs: &BTreeMap<usize, SymbolicMachine<P>>,
    apc_opcode: usize,
    bus_map: BusMap,
    degree_bound: usize,
) -> Result<(SymbolicMachine<P>, Vec<Vec<u64>>), Error> {
    tracing::debug!(
        "Generating autoprecompile for block at index {}",
        block.start_idx
    );
    let mut instruction_kind = BTreeMap::new();
    let mut instruction_machines = BTreeMap::new();
    let program = block
        .statements
        .iter()
        .map(|instr| {
            let instr_name = format!("{}", instr.opcode);

            let symb_machine = airs.get(&instr.opcode.as_usize()).unwrap();

            let symb_instr = SymbolicInstructionStatement {
                name: instr_name.clone(),
                opcode: instr.opcode.as_usize(),
                args: [
                    instr.a, instr.b, instr.c, instr.d, instr.e, instr.f, instr.g,
                ]
                .iter()
                .map(|f| to_powdr_field::<F, P>(*f))
                .collect(),
            };

            if is_jump(&instr.opcode) {
                instruction_kind.insert(instr_name.clone(), InstructionKind::ConditionalBranch);
            } else {
                instruction_kind.insert(instr_name.clone(), InstructionKind::Normal);
            };

            instruction_machines.insert(instr_name.clone(), symb_machine.clone());

            symb_instr
        })
        .collect();

    let (precompile, subs) = powdr_autoprecompiles::build(
        program,
        instruction_kind,
        instruction_machines,
        OpenVmBusInteractionHandler::new(bus_map),
        degree_bound,
        apc_opcode as u32,
    )?;

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

// Transpose an algebraic expression from the powdr field to openvm field
fn transpose_algebraic_expression<F: PrimeField32, P: FieldElement>(
    expr: AlgebraicExpression<P>,
) -> AlgebraicExpression<F> {
    match expr {
        AlgebraicExpression::Number(n) => AlgebraicExpression::Number(to_ovm_field(n)),
        AlgebraicExpression::Reference(reference) => AlgebraicExpression::Reference(reference),
        AlgebraicExpression::PublicReference(reference) => {
            AlgebraicExpression::PublicReference(reference)
        }
        AlgebraicExpression::Challenge(challenge) => AlgebraicExpression::Challenge(challenge),
        AlgebraicExpression::BinaryOperation(algebraic_binary_operation) => {
            let left = transpose_algebraic_expression(*algebraic_binary_operation.left);
            let right = transpose_algebraic_expression(*algebraic_binary_operation.right);
            AlgebraicExpression::BinaryOperation(powdr_ast::analyzed::AlgebraicBinaryOperation {
                left: Box::new(left),
                right: Box::new(right),
                op: algebraic_binary_operation.op,
            })
        }
        AlgebraicExpression::UnaryOperation(algebraic_unary_operation) => {
            AlgebraicExpression::UnaryOperation(powdr_ast::analyzed::AlgebraicUnaryOperation {
                op: algebraic_unary_operation.op,
                expr: Box::new(transpose_algebraic_expression(
                    *algebraic_unary_operation.expr,
                )),
            })
        }
    }
}

// Transpose a symbolic machine from the powdr field to openvm field
fn transpose_symbolic_machine<F: PrimeField32, P: FieldElement>(
    machine: SymbolicMachine<P>,
) -> SymbolicMachine<F> {
    let constraints = machine
        .constraints
        .into_iter()
        .map(|constraint| SymbolicConstraint {
            expr: transpose_algebraic_expression(constraint.expr),
        })
        .collect();
    let bus_interactions = machine
        .bus_interactions
        .into_iter()
        .map(|interaction| SymbolicBusInteraction {
            id: interaction.id,
            mult: transpose_algebraic_expression(interaction.mult.clone()),
            args: interaction
                .args
                .iter()
                .map(|arg| transpose_algebraic_expression(arg.clone()))
                .collect(),
        })
        .collect();

    SymbolicMachine {
        constraints,
        bus_interactions,
    }
}
