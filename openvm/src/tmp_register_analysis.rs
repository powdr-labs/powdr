use std::collections::{BTreeMap, HashMap, HashSet};

use openvm_instructions::instruction::Instruction;
use openvm_stark_backend::p3_field::PrimeField32;
use powdr_autoprecompiles::blocks::BasicBlock;

use crate::{instruction_formatter::openvm_instruction_formatter, opcode::*, Instr};

fn f_to_i64<F: PrimeField32>(f: &F) -> i64 {
    let u = f.as_canonical_u32();
    if u < F::ORDER_U32 / 2 {
        u as i64
    } else {
        (u as i64) - (F::ORDER_U32 as i64)
    }
}

fn possible_targets<F: PrimeField32>(
    instruction: &Instruction<F>,
    instruction_pc: u64,
    pc_step: u64,
) -> Vec<u64> {
    let opcode = instruction.opcode.as_usize();

    if opcode == OPCODE_JALR {
        // For JALR, we don't know the target statically.
        return vec![];
    }

    if BRANCH_OPCODES_BIGINT.contains(&opcode) || BRANCH_OPCODES.contains(&opcode) {
        // All other branch instructions conditionally add `c` to the current PC,
        // and otherwise increment the PC by the default step.
        let offset = f_to_i64(&instruction.c);
        return [
            instruction_pc + pc_step,
            (instruction_pc as i64 + offset) as u64,
        ]
        .into_iter()
        .collect();
    }
    [instruction_pc + pc_step].into_iter().collect()
}

pub fn control_flow_graph<F: PrimeField32>(
    basic_blocks: &[BasicBlock<Instr<F>>],
    pc_step: u64,
) -> HashMap<u64, Vec<u64>> {
    let known_targets = basic_blocks
        .iter()
        .map(|block| block.start_pc)
        .collect::<HashSet<_>>();
    basic_blocks
        .iter()
        .map(|block| {
            let id = block.start_pc;
            let last_pc = id + (block.statements.len() as u64 - 1) * pc_step;
            let last_instr = block.statements.last().unwrap();
            let targets = possible_targets(&last_instr.0, last_pc, pc_step);
            for target in &targets {
                assert!(
                    known_targets.contains(target),
                    "Unknown target {target} from block starting at {id}"
                );
            }
            (id, targets)
        })
        .collect()
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RegisterAccessType {
    Read,
    Write,
    Unused,
}

pub fn print_register_access_type_stats<F: PrimeField32>(basic_blocks: &[BasicBlock<Instr<F>>]) {
    let mut global_state = [RegisterAccessType::Unused; 32];
    for basic_block in basic_blocks {
        let access_types = register_access_types(basic_block);
        for (i, access_type) in access_types.iter().enumerate() {
            if *access_type == RegisterAccessType::Read {
                // Read overrides any previous state.
                global_state[i] = RegisterAccessType::Read;
            } else if *access_type == RegisterAccessType::Write {
                // Write only overrides Unused.
                if global_state[i] == RegisterAccessType::Unused {
                    global_state[i] = RegisterAccessType::Write;
                }
            }
        }
        let written_regs = access_types
            .iter()
            .enumerate()
            .filter(|(_, t)| **t == RegisterAccessType::Write)
            .map(|(i, _)| i)
            .collect::<Vec<_>>();
        tracing::info!(
            "Block at {} writes to: {:?}",
            basic_block.start_pc,
            written_regs
        );
    }
    let unused = global_state
        .iter()
        .enumerate()
        .filter(|(_, &t)| t == RegisterAccessType::Unused)
        .map(|(i, _)| i)
        .collect::<Vec<_>>();
    tracing::info!("Registers never accessed: {unused:?}");
    let only_written = global_state
        .iter()
        .enumerate()
        .filter(|(_, &t)| t == RegisterAccessType::Write)
        .map(|(i, _)| i)
        .collect::<Vec<_>>();
    tracing::info!("Registers only written to, never read: {only_written:?}");
}

fn register_access_types<F: PrimeField32>(
    basic_block: &BasicBlock<Instr<F>>,
) -> [RegisterAccessType; 32] {
    let mut access_types = [RegisterAccessType::Unused; 32];
    for instr in &basic_block.statements {
        let (reads, writes) = instruction_reads_writes(&instr.0);
        for r in reads {
            assert!(r < 32);
            let r = r as usize;
            match access_types[r] {
                RegisterAccessType::Unused => access_types[r] = RegisterAccessType::Read,
                RegisterAccessType::Read => {}
                RegisterAccessType::Write => {}
            }
        }
        for w in writes {
            assert!(w < 32);
            let w = w as usize;
            match access_types[w] {
                RegisterAccessType::Unused => access_types[w] = RegisterAccessType::Write,
                RegisterAccessType::Read => {}
                RegisterAccessType::Write => {}
            }
        }
    }
    access_types
}

fn instruction_reads_writes<F: PrimeField32>(instruction: &Instruction<F>) -> (Vec<u32>, Vec<u32>) {
    let opcode = instruction.opcode.as_usize();
    let (reads, writes) = match opcode {
        // ALU instructions
        OPCODE_ADD | OPCODE_SUB | OPCODE_XOR | OPCODE_OR | OPCODE_AND | OPCODE_SLL | OPCODE_SRL
        | OPCODE_SRA | OPCODE_SLT | OPCODE_SLTU | OPCODE_MUL | OPCODE_MULH | OPCODE_MULHSU
        | OPCODE_MULHU | OPCODE_DIV | OPCODE_DIVU | OPCODE_REM | OPCODE_REMU => {
            let writes = [instruction.a.as_canonical_u32()].into_iter().collect();
            let rs2_is_reg = instruction.e.is_one();
            let reads = [instruction.b.as_canonical_u32()]
                .into_iter()
                .chain(rs2_is_reg.then_some(instruction.c.as_canonical_u32()))
                .collect();
            (reads, writes)
        }
        // Load instructions
        OPCODE_LOADW | OPCODE_LOADBU | OPCODE_LOADHU | OPCODE_LOADB | OPCODE_LOADH => {
            let writes = [instruction.a.as_canonical_u32()].into_iter().collect();
            let reads = [instruction.b.as_canonical_u32()].into_iter().collect();
            (reads, writes)
        }
        // Store instructions
        OPCODE_STOREW | OPCODE_STOREH | OPCODE_STOREB => {
            let writes = vec![];
            assert!(instruction.f.is_one());
            let reads = [
                instruction.a.as_canonical_u32(),
                instruction.b.as_canonical_u32(),
            ]
            .into_iter()
            .collect();
            (reads, writes)
        }
        // Branch instructions
        OPCODE_BEQ | OPCODE_BNE | OPCODE_BLT | OPCODE_BLTU | OPCODE_BGE | OPCODE_BGEU => {
            let writes = vec![];
            let reads = [
                instruction.a.as_canonical_u32(),
                instruction.b.as_canonical_u32(),
            ]
            .into_iter()
            .collect();
            (reads, writes)
        }
        OPCODE_JAL => {
            let writes = [instruction.a.as_canonical_u32()].into_iter().collect();
            let reads = vec![];
            (reads, writes)
        }
        OPCODE_JALR => {
            let writes = [instruction.a.as_canonical_u32()].into_iter().collect();
            let reads = [instruction.b.as_canonical_u32()].into_iter().collect();
            (reads, writes)
        }
        OPCODE_LUI | OPCODE_AUIPC => {
            let writes = [instruction.a.as_canonical_u32()].into_iter().collect();
            let reads = vec![];
            (reads, writes)
        }
        OPCODE_HINT_STOREW => {
            let writes = vec![];
            let reads = [instruction.b.as_canonical_u32()].into_iter().collect();
            (reads, writes)
        }
        OPCODE_HINT_BUFFER => {
            let writes = vec![];
            let reads = [
                instruction.a.as_canonical_u32(),
                instruction.b.as_canonical_u32(),
            ]
            .into_iter()
            .collect();
            (reads, writes)
        }
        // TERMINATE
        0 => (vec![], vec![]),
        // PHANTOM
        1 => {
            // TODO: Not sure what should happen here.
            tracing::error!(
                "Unhandled PHANTOM instruction: {}",
                openvm_instruction_formatter(instruction)
            );
            (vec![], vec![])
        }
        _ => {
            // Probably manual precompiles.
            tracing::error!(
                "Unhandled opcode {opcode} in register_accesses, instruction: {}",
                openvm_instruction_formatter(instruction)
            );
            (vec![], vec![])
        }
    };
    let normalize_reg = |r: u32| {
        assert!(
            r % 4 == 0 && r / 4 < 32,
            "Register {r} is not a valid register number, instruction: {}",
            openvm_instruction_formatter(instruction)
        );
        r / 4
    };
    let reads = reads.into_iter().map(normalize_reg).collect();
    let writes = writes.into_iter().map(normalize_reg).collect();
    (reads, writes)
}

pub fn find_tmp_registers<F: PrimeField32>(basic_blocks: &[BasicBlock<Instr<F>>], pc_step: u64) {
    let graph = control_flow_graph(basic_blocks, pc_step);
    let mut register_access_types = basic_blocks
        .iter()
        .map(|block| (block.start_pc, register_access_types(block)))
        .collect::<BTreeMap<_, _>>();
    let block_ids = register_access_types.keys().copied().collect::<Vec<_>>();
    for i in 0.. {
        let mut changed = false;
        if i % 100 == 0 {
            tracing::info!("Iteration {i} of register access type fixpoint");
        }
        for block_id in &block_ids {
            // Propagate register access types from successors to predecessors.
            let successors = graph.get(block_id).unwrap();
            let mut successor_access_types = [RegisterAccessType::Unused; 32];
            let mut all_writes = [true; 32];
            for succ in successors {
                let succ_access_types = register_access_types.get(succ).unwrap();
                for (r, succ_access_type) in succ_access_types.iter().enumerate() {
                    match *succ_access_type {
                        RegisterAccessType::Read => {
                            // Read overrides any previous state.
                            successor_access_types[r] = RegisterAccessType::Read;
                            all_writes[r] = false;
                        }
                        RegisterAccessType::Write => {}
                        RegisterAccessType::Unused => {
                            all_writes[r] = false;
                        }
                    }
                }
            }

            for r in 0..32 {
                if all_writes[r] {
                    // If all successors write, then we can consider it a write.
                    successor_access_types[r] = RegisterAccessType::Write;
                }
            }

            for (r, succ) in successor_access_types.iter().enumerate() {
                let current = register_access_types.get(block_id).unwrap()[r];
                if current == RegisterAccessType::Unused && *succ != RegisterAccessType::Unused {
                    // Unused becomes whatever the successor is.
                    changed = true;
                    register_access_types.get_mut(block_id).unwrap()[r] = *succ;
                }
            }
        }
        if !changed {
            tracing::info!("Reached fixpoint after {i} iterations");
            break;
        }
    }
    for (block_id, access_types) in &register_access_types {
        let written_regs = access_types
            .iter()
            .enumerate()
            .filter(|(_, t)| **t == RegisterAccessType::Write)
            .map(|(i, _)| i)
            .collect::<Vec<_>>();
        let removable_regs = written_regs
            .iter()
            .copied()
            .filter(|r| {
                // We can remove it if all successor have it as write.
                let successors = graph.get(block_id).unwrap();
                successors.iter().all(|succ| {
                    let succ_access_types = register_access_types.get(succ).unwrap();
                    succ_access_types[*r] == RegisterAccessType::Write
                })
            })
            .collect::<Vec<_>>();
        tracing::info!(
            "Block at {block_id} writes to: {written_regs:?} (removable: {removable_regs:?})",
        );
    }
}
