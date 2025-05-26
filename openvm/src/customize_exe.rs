use std::collections::{BTreeMap, BTreeSet, HashMap};

use itertools::Itertools;
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
    Autoprecompiles, BusInteractionKind, InstructionKind, SymbolicBusInteraction,
    SymbolicConstraint, SymbolicInstructionStatement, SymbolicMachine,
};
use powdr_number::{FieldElement, LargeInt};

use crate::bus_interaction_handler::OpenVmBusInteractionHandler;
use crate::instruction_formatter::openvm_instruction_formatter;
use crate::{
    powdr_extension::{OriginalInstruction, PowdrExtension, PowdrOpcode, PowdrPrecompile},
    utils::symbolic_to_algebraic,
};

const OPENVM_DEGREE_BOUND: usize = 5;

const POWDR_OPCODE: usize = 0x10ff;

pub fn customize<F: PrimeField32>(
    mut exe: VmExe<F>,
    base_config: SdkVmConfig,
    labels: &BTreeSet<u32>,
    airs: &BTreeMap<usize, SymbolicMachine<powdr_number::BabyBearField>>,
    autoprecompiles: usize,
    skip: usize,
    pc_idx_count: Option<HashMap<u32, u32>>,
) -> (VmExe<F>, PowdrExtension<F>) {
    // The following opcodes shall never be accelerated and therefore always put in its own basic block.
    // Currently this contains OpenVm opcodes: Rv32HintStoreOpcode::HINT_STOREW (0x260) and Rv32HintStoreOpcode::HINT_BUFFER (0x261)
    // which are the only two opcodes from the Rv32HintStore, the air responsible for reading host states via stdin.
    // We don't want these opcodes because they create air constraints with next references, which powdr-openvm does not support yet.
    let opcodes_no_apc = vec![
        Rv32HintStoreOpcode::HINT_STOREW.global_opcode().as_usize(),
        Rv32HintStoreOpcode::HINT_BUFFER.global_opcode().as_usize(),
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

    let mut blocks = collect_basic_blocks(&exe.program, labels, &opcodes_no_apc);
    tracing::info!("Got {} basic blocks", blocks.len());

    if let Some(pgo_program_idx_count) = pc_idx_count {
        // sort the blocks by block_len * frequency (the count of start_idx in pgo_program_idx_count)
        blocks.sort_by(|a, b| {
            // not all start index of a basic block can be found in pc_idx_count, because a basic block might not be executed at all
            // in this case, they will just default to 0
            let a_count = pgo_program_idx_count
                .get(&(a.start_idx as u32))
                .unwrap_or(&0);
            let b_count = pgo_program_idx_count
                .get(&(b.start_idx as u32))
                .unwrap_or(&0);

            let a_opcode_no_apc = if !a.statements.is_empty() {
                opcodes_no_apc.contains(&a.statements[0].opcode.as_usize())
            } else {
                true
            };
            let b_opcode_no_apc = if !b.statements.is_empty() {
                opcodes_no_apc.contains(&b.statements[0].opcode.as_usize())
            } else {
                true
            };

            // if a basic block starts with an opcode that is in opcodes_no_apc, put it at the bottom of the list of blocks to order
            // otherwise, order by descending cost = instruction count * execution frequency
            match (a_opcode_no_apc, b_opcode_no_apc) {
                (true, false) => std::cmp::Ordering::Greater,
                (false, true) => std::cmp::Ordering::Less,
                _ => (b_count * (b.statements.len() as u32))
                    .cmp(&(a_count * (a.statements.len() as u32))),
            }
        });

        // print block start_idx, cost = block_len * frequency, block_len, and frequency, sorted by descending cost
        for block in &blocks {
            let start_idx = block.start_idx;
            let block_len = block.statements.len();
            let count = pgo_program_idx_count.get(&(start_idx as u32)).unwrap_or(&0);
            let cost = count * (block_len as u32);
            tracing::info!(
                "Basic block start_idx: {}, cost: {}, block_len: {}, frequency: {}",
                start_idx,
                cost,
                block_len,
                count
            );
        }
    } else {
        // if pgo option is not set, sort by descending order of block length
        blocks.sort_by(|a, b| (b.statements.len()).cmp(&a.statements.len()));
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
    let n_acc = autoprecompiles;
    let n_skip = skip;
    tracing::info!("Generating {n_acc} autoprecompiles");

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

        let apc_opcode = POWDR_OPCODE + i;
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

        let pc = acc_block.start_idx as usize;
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

        let (autoprecompile, subs) =
            generate_autoprecompile::<F, powdr_number::BabyBearField>(acc_block, airs, apc_opcode);

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
    pub start_idx: u64,
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
            // Push the current block and start a new one from this instruction.
            blocks.push(curr_block);
            curr_block = BasicBlock {
                start_idx: i as u64,
                statements: Vec::new(),
            };
            // Add the instruction and push the block
            curr_block.statements.push(instr.clone());
            blocks.push(curr_block);
            // Start a new block from the next instruction.
            curr_block = BasicBlock {
                start_idx: (i + 1) as u64,
                statements: Vec::new(),
            };
        } else {
            // If the instruction is a target, we need to close the previous block
            // as is and start a new block from this instruction.
            if is_target {
                blocks.push(curr_block);
                curr_block = BasicBlock {
                    start_idx: i as u64,
                    statements: Vec::new(),
                };
            }
            curr_block.statements.push(instr.clone());
            // If the instruction is a branch, we need to close this block
            // with this instruction and start a new block from the next one.
            if is_branch {
                blocks.push(curr_block);
                curr_block = BasicBlock {
                    start_idx: (i + 1) as u64,
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
) -> (SymbolicMachine<P>, Vec<Vec<u64>>) {
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

    let autoprecompiles = Autoprecompiles {
        program,
        instruction_kind,
        instruction_machines,
    };

    let (precompile, subs) = autoprecompiles.build(
        OpenVmBusInteractionHandler::default(),
        OPENVM_DEGREE_BOUND,
        apc_opcode as u32,
    );

    // Check that substitution values are unique over all instructions
    assert!(subs.iter().flatten().all_unique());

    tracing::debug!(
        "Done generating autoprecompile for block at index {}",
        block.start_idx
    );

    (precompile, subs)
}

pub fn openvm_bus_interaction_to_powdr<F: PrimeField32, P: FieldElement>(
    interaction: &SymbolicInteraction<F>,
    columns: &[String],
) -> SymbolicBusInteraction<P> {
    // TODO
    let kind = BusInteractionKind::Send;
    // let kind = match interaction.interaction_type {
    //     InteractionType::Send => BusInteractionKind::Send,
    //     InteractionType::Receive => BusInteractionKind::Receive,
    // };
    let id = interaction.bus_index as u64;

    let mult = symbolic_to_algebraic(&interaction.count, columns);
    let args = interaction
        .message
        .iter()
        .map(|e| symbolic_to_algebraic(e, columns))
        .collect();

    SymbolicBusInteraction {
        kind,
        id,
        mult,
        args,
    }
}

fn to_powdr_field<F: PrimeField32, P: FieldElement>(f: F) -> P {
    f.as_canonical_u32().into()
}

fn to_ovm_field<F: PrimeField32, P: FieldElement>(f: P) -> F {
    F::from_canonical_u32(f.to_integer().try_into_u32().unwrap())
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
            kind: interaction.kind,
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
