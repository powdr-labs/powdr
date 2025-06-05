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
use powdr_ast::analyzed::{AlgebraicExpression, AlgebraicReference, PolyID, PolynomialType};
use powdr_ast::parsed::visitor::ExpressionVisitable;
use powdr_autoprecompiles::powdr::UniqueColumns;
use powdr_autoprecompiles::{
    InstructionKind, SymbolicBusInteraction, SymbolicInstructionStatement, SymbolicMachine,
};
use powdr_number::FieldElement;

use crate::bus_interaction_handler::{BusMap, OpenVmBusInteractionHandler};
use crate::instruction_formatter::openvm_instruction_formatter;
use crate::{
    powdr_extension::{OriginalInstruction, PowdrExtension, PowdrOpcode, PowdrPrecompile, PowdrStackedPrecompile},
    utils::symbolic_to_algebraic,
};

pub const OPENVM_DEGREE_BOUND: usize = 5;

// TODO: read this from program
const OPENVM_INIT_PC: u32 = 0x0020_0800;

const POWDR_OPCODE: usize = 0x10ff;

use crate::PowdrConfig;

pub fn customize<P: IntoOpenVm>(
    mut exe: VmExe<OpenVmField<P>>,
    base_config: SdkVmConfig,
    labels: &BTreeSet<u32>,
    airs: &BTreeMap<usize, SymbolicMachine<P>>,
    config: PowdrConfig,
    pc_idx_count: Option<HashMap<u32, u32>>,
) -> (VmExe<OpenVmField<P>>, PowdrExtension<P>) {
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
    tracing::info!("Got {} basic blocks", blocks.len());

    if let Some(pgo_program_idx_count) = pc_idx_count {
        // first, drop any block whose start index cannot be found in pc_idx_count,
        // because a basic block might not be executed at all.
        // Also only keep basic blocks with more than one original instruction.
        blocks.retain(|b| {
            pgo_program_idx_count.contains_key(&(b.start_idx as u32)) && b.statements.len() > 1
        });

        // then, sort the blocks by block_len * execution frequency (the count of start_idx in pgo_program_idx_count)
        blocks.sort_by(|a, b| {
            let a_cnt = pgo_program_idx_count[&(a.start_idx as u32)];
            let b_cnt = pgo_program_idx_count[&(b.start_idx as u32)];
            (b_cnt * (b.statements.len() as u32)).cmp(&(a_cnt * (a.statements.len() as u32)))
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
            a: OpenVmField::<P>::ZERO,
            b: OpenVmField::<P>::ZERO,
            c: OpenVmField::<P>::ZERO,
            d: OpenVmField::<P>::ZERO,
            e: OpenVmField::<P>::ZERO,
            f: OpenVmField::<P>::ZERO,
            g: OpenVmField::<P>::ZERO,
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

        let (autoprecompile, subs) = generate_autoprecompile(
            acc_block,
            airs,
            apc_opcode,
            config.bus_map.clone(),
            config.degree_bound,
            &branch_opcodes_set,
        );

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

    let extensions = air_stacking(extensions);
    (
        exe,
        PowdrExtension::new(extensions, base_config, config.implementation),
    )
}

/// make PolyIDs start from zero, sequential and compact
fn compact_ids<P: IntoOpenVm>(pcp: &mut PowdrPrecompile<P>) {
    let mut curr_id = 0;
    let mut id_map: HashMap<u64, u64> = Default::default();
    let mut new_poly_id = |old_id: u64| {
        *id_map.entry(old_id).or_insert_with(|| {
            let id = curr_id;
            // println!("remapping poly id {} to {}", old_id, curr_id);
            curr_id += 1;
            id
        })
    };
    pcp.machine.pre_visit_expressions_mut(&mut |expr| {
        if let AlgebraicExpression::Reference(r) = expr {
            if r.poly_id.ptype == PolynomialType::Committed {
                r.poly_id.id = new_poly_id(r.poly_id.id);
            }
        }
    });
    pcp.original_instructions.iter_mut().for_each(|instr| {
        instr.subs.iter_mut().for_each(|sub| {
            *sub = new_poly_id(*sub);
        });
    });

    pcp.is_valid_column.id.id = new_poly_id(pcp.is_valid_column.id.id);
}

fn merge_bus_interactions_simple<P: IntoOpenVm>(interactions: Vec<Vec<SymbolicBusInteraction<P>>>) -> Vec<SymbolicBusInteraction<P>> {
    interactions.into_iter().flatten().collect_vec()
}

fn merge_bus_interactions<P: IntoOpenVm>(interactions_by_machine: Vec<Vec<SymbolicBusInteraction<P>>>) -> Vec<SymbolicBusInteraction<P>> {
    let mut interactions_by_bus: HashMap<_, Vec<Vec<SymbolicBusInteraction<P>>>> = Default::default();

    for interactions in interactions_by_machine {
        let interactions_by_bus_this_machine = interactions.into_iter()
            // we group by bus id and number of args
            .into_group_map_by(|interaction| (interaction.id, interaction.args.len()));
        for (k,v) in interactions_by_bus_this_machine {
            let e = interactions_by_bus.entry(k).or_default();
            e.push(v);
        }
    }
    let mut to_merge = vec![];
    for interactions_per_machine in interactions_by_bus.into_values() {
        // this is doing a "zip longest" among the interactions from each machine
        // (for a given bus), and saving the elements that were picked
        // together to be merged later
        let mut idx = 0;
        loop {
            let mut items = vec![];
            for machine_interactions in interactions_per_machine.iter() {
                if let Some(item) = machine_interactions.get(idx) {
                    items.push(item.clone());
                }
            }
            if items.is_empty() {
                break;
            }
            idx += 1;
            to_merge.push(items);
        }
    }

    to_merge.into_iter().map(|interactions| {
        // assert_eq!(interactions.iter().map(|i| &i.kind).unique().count(), 1);

        // add multiplicities together
        let mut mult = interactions[0].mult.clone();
        for mult2 in interactions.iter().skip(1).map(|i| i.mult.clone()) {
            mult = mult + mult2;
        }

        // add args together
        let mut args = interactions[0].args.clone();
        for args2 in interactions.iter().skip(1).map(|i| i.args.clone()) {
            args = args.into_iter().zip_eq(args2).map(|(a1, a2)| a1 + a2).collect();
        }

        SymbolicBusInteraction {
            id: interactions[0].id,
            mult,
            args,
        }
    }).collect_vec()
}

// perform air stacking of multiple precompiles into stacked precompiles.
fn air_stacking<P: IntoOpenVm>(
    mut extensions: Vec<PowdrPrecompile<P>>,
) -> Vec<PowdrStackedPrecompile<P>> {
    assert!(!extensions.is_empty());

    extensions.iter_mut().for_each(compact_ids);

    // create apc groups by number of columns
    let mut groups: HashMap<usize, Vec<PowdrPrecompile<P>>> = Default::default();
    for pcp in extensions {
        let idx = f64::log(pcp.machine.unique_columns().count() as f64, 2.0).floor() as usize;
        groups.entry(idx).or_default().push(pcp);
    }

    let mut result = vec![];

    for mut extensions in groups.into_values() {
        // if there is only one precompile in the group, we can just return it as a stacked precompile
        if extensions.len() == 1 {
            println!("not stacking precompile");
            result.push(PowdrStackedPrecompile {
                machine: SymbolicMachine {
                    constraints: extensions[0].machine.constraints.clone(),
                    bus_interactions: extensions[0].machine.bus_interactions.clone(),
                },
                precompiles: extensions
                    .into_iter()
                    .map(|p| (p.opcode.clone(), p))
                    .collect(),
            });
            continue;
        }

        let mut stacked_constraints = vec![];
        let mut interactions_by_machine = vec![];

        println!("stacking {} precompiles", extensions.len());

        // take the max id in all pcps and add 1.
        let is_valid_start = 1 + extensions.iter()
            .flat_map(|pcp| pcp.original_instructions.iter().flat_map(|instr| instr.subs.iter()))
            .max()
            .unwrap();


        let mut is_valid_sum: Option<AlgebraicExpression<P>> = None;

        for (idx, pcp) in extensions.iter_mut().enumerate() {
            // is_valid columns cannot be shared between precompiles. Here we do
            // their remapping into exclusive columns.
            let is_valid_new_id = is_valid_start + idx as u64;

            println!("\tpcp width: {}", pcp.machine.unique_columns().count());

            // remap is_valid column in constraints and interactions
            let mut remapped = pcp.machine.clone();
            remapped.pre_visit_expressions_mut(&mut |expr| {
                if let AlgebraicExpression::Reference(r) = expr {
                    assert!(r.poly_id.id <= is_valid_start);
                    if r.poly_id == pcp.is_valid_column.id {
                        // we assume each pcp to have a specific column named "is_valid"
                        assert!(r.name == "is_valid");
                        r.poly_id.id = is_valid_new_id;
                        r.name = format!("is_valid_{}", pcp.opcode.global_opcode().as_usize());
                    } else {
                        // we need to rename columns here because `unique_columns` relies on both `Column::name` and `Column::id`
                        r.name = format!("col_{}", r.poly_id.id);
                    }
                }
            });

            // set the is valid column in the original precompile
            pcp.is_valid_column.id.id = is_valid_new_id;

            let is_valid = AlgebraicExpression::Reference(AlgebraicReference {
                name: format!("is_valid_{}", pcp.opcode.global_opcode().as_usize()),
                poly_id: PolyID {
                    id: is_valid_new_id,
                    ptype: PolynomialType::Committed,
                },
                next: false,
            });

            // guard interaction payloads so they can be merged later
            remapped.bus_interactions.iter_mut().for_each(|interaction| {
                interaction.args.iter_mut().for_each(|arg| {
                    *arg = arg.clone() * is_valid.clone();
                });
            });


            is_valid_sum = is_valid_sum
                .map(|sum| sum + is_valid.clone())
                .or_else(|| Some(is_valid.clone()));

            stacked_constraints.extend(remapped.constraints);
            interactions_by_machine.push(remapped.bus_interactions);
        }

        // enforce only one is_valid is active
        let one = AlgebraicExpression::Number(P::ONE);
        let one_hot_is_valid = (one - is_valid_sum.clone().unwrap()) * is_valid_sum.unwrap();

        stacked_constraints.push(one_hot_is_valid.into());

        println!("interaction count before: {}", interactions_by_machine.iter().flatten().count());
        // let stacked_interactions = merge_bus_interactions_simple(interactions_by_machine);
        let stacked_interactions = merge_bus_interactions(interactions_by_machine);

        let machine = SymbolicMachine {
            constraints: stacked_constraints,
            bus_interactions: stacked_interactions,
        };

        println!("interaction count: {}", machine.bus_interactions.len());

        println!("stacked width: {}", machine.unique_columns().count());

        result.push(PowdrStackedPrecompile {
            precompiles: extensions
                .into_iter()
                .map(|p| (p.opcode.clone(), p))
                .collect(),
            machine,
        });
    }

    result
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
                start_idx: (i + 1) as u64,
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
                    start_idx: i as u64,
                    statements: Vec::new(),
                };
            }
            curr_block.statements.push(instr.clone());
            // If the instruction is a branch, we need to close this block
            // with this instruction and start a new block from the next one.
            if is_branch {
                blocks.push(curr_block); // guaranteed to be non-empty because an instruction was just pushed
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
    degree_bound: usize,
    branch_opcodes: &BTreeSet<usize>,
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
                .map(|f| P::from_openvm_field(*f))
                .collect(),
            };

            if branch_opcodes.contains(&instr.opcode.as_usize()) {
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
        // chip stacking needs to add guards to bus arguments also, so we restrict the optimizer by 1 degree here.
        degree_bound - 1,
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
    let id = interaction.bus_index as u64;

    let mult = symbolic_to_algebraic(&interaction.count, columns);
    let args = interaction
        .message
        .iter()
        .map(|e| symbolic_to_algebraic(e, columns))
        .collect();

    SymbolicBusInteraction { id, mult, args }
}
