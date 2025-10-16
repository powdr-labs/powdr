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
    auipc_address: Option<(u8, u32)>,
) -> Vec<u64> {
    let opcode = instruction.opcode.as_usize();

    if opcode == OPCODE_JALR {
        // If the instruction just before was AUIPC, we can compute the target.
        let rs1: u8 = instruction.b.as_canonical_u32().try_into().unwrap();
        match auipc_address {
            Some((rd, address)) if rd == rs1 => {
                tracing::debug!("Detected AUIPC + JALR!");
                let imm = instruction.c.as_canonical_u32();
                let imm_sign = instruction.g.as_canonical_u32();
                let imm_extended = imm + imm_sign * 0xffff0000;
                let address = address.wrapping_add(imm_extended) & !1;
                return vec![instruction_pc + pc_step, address as u64];
            }
            // We don't know the target statically.
            _ => return vec![],
        }
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

/// If the second last instruction is an AUIPC, return the target address it computes.
/// Returns Some((rd, address)), or None.
fn auipc_address<F: PrimeField32>(basic_block: &BasicBlock<Instr<F>>) -> Option<(u8, u32)> {
    if basic_block.statements.len() < 2 {
        return None;
    }
    let second_last = &basic_block.statements[basic_block.statements.len() - 2].0;
    if second_last.opcode.as_usize() != OPCODE_AUIPC {
        return None;
    }

    let pc = basic_block.start_pc + (basic_block.statements.len() as u64 - 2) * 4;
    let pc: u32 = pc.try_into().unwrap();
    let imm = second_last.c.as_canonical_u32();
    let address = pc.wrapping_add((imm as u32) << 8);

    Some((
        second_last.a.as_canonical_u32().try_into().unwrap(),
        address,
    ))
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
            let auipc_address = auipc_address(block);
            let targets = possible_targets(&last_instr.0, last_pc, pc_step, auipc_address);
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
    /// The register is read.
    /// If the register is read in the current block, stores the ID of the current block.
    /// Otherwise, stores the next hop on a path to a reading block.
    Read(u64),
    Write,
    Unused,
}

/// Finds the register access types, looking at a basic block in isolation.
///
/// Finds the first instruction that accesses the register. Then:
/// - If it reads, return Read (even if it also writes in the same instruction). The block ID is stored.
/// - If it only writes, return Write.
/// - If it is never accessed, return Unused.
fn register_access_types<F: PrimeField32>(
    basic_block: &BasicBlock<Instr<F>>,
) -> [RegisterAccessType; 32] {
    let mut access_types = [RegisterAccessType::Unused; 32];
    for instr in &basic_block.statements {
        let (reads, writes) = instruction_reads_writes(&instr.0);
        for r in reads {
            assert!(r < 32);
            let r = r as usize;
            if access_types[r] == RegisterAccessType::Unused {
                access_types[r] = RegisterAccessType::Read(basic_block.start_pc);
            }
        }
        for w in writes {
            assert!(w < 32);
            let w = w as usize;
            if access_types[w] == RegisterAccessType::Unused {
                access_types[w] = RegisterAccessType::Write;
            }
        }
    }
    access_types
}

/// Returns the registers read and written by the instruction.
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

/// Given a *non-empty* list of access types, compute the intersection, meaning:
/// - If *any* item reads, return Read. The block ID is taken from the first reading item.
/// - If *all* items write, return Write.
/// - Otherwise, return Unused.
fn intersect_access_types<'a>(
    all_access_types: impl Iterator<Item = (u64, &'a [RegisterAccessType; 32])>,
) -> [RegisterAccessType; 32] {
    let mut result = [RegisterAccessType::Unused; 32];
    let mut all_writes = [true; 32];
    let mut is_empty = true;
    for (block_id, access_types) in all_access_types {
        is_empty = false;
        for (r, access_type) in access_types.iter().enumerate() {
            match *access_type {
                RegisterAccessType::Read(_) => {
                    // Read overrides any previous state.
                    result[r] = RegisterAccessType::Read(block_id);
                    all_writes[r] = false;
                }
                RegisterAccessType::Write => {}
                RegisterAccessType::Unused => {
                    all_writes[r] = false;
                }
            }
        }
    }

    if is_empty {
        panic!("intersect_access_types called with empty iterator");
    }

    for r in 0..32 {
        if all_writes[r] {
            // If all blocks write, then we can consider it a write.
            result[r] = RegisterAccessType::Write;
        }
    }
    result
}

pub fn find_tmp_registers<F: PrimeField32>(basic_blocks: &[BasicBlock<Instr<F>>], pc_step: u64) {
    let graph = control_flow_graph(basic_blocks, pc_step);
    let block_ids = graph.keys().copied().collect::<Vec<_>>();

    // Register access types for each block in isolation
    let register_access_types = basic_blocks
        .iter()
        .map(|block| (block.start_pc, register_access_types(block)))
        .collect::<BTreeMap<_, _>>();

    // Check whether any register has the property that it is never read.
    // This is not used later, but interesting to know.
    let mut never_read = [true; 32];
    for access_types in register_access_types.values() {
        for (r, access_type) in access_types.iter().enumerate() {
            if matches!(access_type, RegisterAccessType::Read(_)) {
                never_read[r] = false;
            }
        }
    }
    for (r, never_read) in never_read.iter().enumerate() {
        if *never_read {
            tracing::info!("Register {r} is never read in any block");
        }
    }

    // Fixpoint iteration to propagate register access types through the control flow graph.
    // A read or write here means that either this block or one of its successors reads/writes the register.
    let mut register_access_types_with_succ = register_access_types.clone();
    tracing::info!("Block IDs: {block_ids:?}");
    for i in 0.. {
        let mut changed = false;
        if i % 100 == 0 {
            tracing::info!("Iteration {i} of register access type fixpoint");
        }
        for block_id in &block_ids {
            // Propagate register access types from successors to predecessors.
            let successors = graph.get(block_id).unwrap();
            let successor_access_types = if successors.is_empty() {
                // We don't know anything, assume the worst case (all registers are read).
                [RegisterAccessType::Read(*block_id); 32]
            } else {
                intersect_access_types(
                    successors
                        .iter()
                        .map(|succ| (*succ, register_access_types_with_succ.get(succ).unwrap())),
                )
            };

            for (r, access_type) in successor_access_types.iter().enumerate() {
                let current = register_access_types_with_succ.get(block_id).unwrap()[r];
                if current == RegisterAccessType::Unused
                    && *access_type != RegisterAccessType::Unused
                {
                    // Unused becomes whatever the successor is.
                    changed = true;
                    register_access_types_with_succ.get_mut(block_id).unwrap()[r] = *access_type;
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
                // We can remove it if no successor reads.
                // If the successor list is empty (JALR), we cannot remove it.
                let successors = graph.get(block_id).unwrap();
                !successors.is_empty()
                    && !successors.iter().any(|succ| {
                        let succ_access_types = register_access_types_with_succ.get(succ).unwrap();
                        matches!(succ_access_types[*r], RegisterAccessType::Read(_))
                    })
            })
            .collect::<Vec<_>>();
        tracing::info!(
            "Block at 0x{block_id:x} writes to: {written_regs:?}, removable: {} ({removable_regs:?})",
            removable_regs.len()
        );
        for r in &written_regs {
            if !removable_regs.contains(r) {
                // Explain why we cannot remove it by printing a path to a reading block.
                let successors = graph.get(block_id).unwrap();
                if successors.is_empty() {
                    tracing::info!("  Register {r} might be read after dynamic jump (JALR)");
                    continue;
                }

                // Pick the first reading successor (which must exist)
                let first_reading_successor = *successors
                    .iter()
                    .find(|succ| {
                        let succ_access_types = register_access_types_with_succ.get(succ).unwrap();
                        matches!(succ_access_types[*r], RegisterAccessType::Read(_))
                    })
                    .unwrap();

                let mut path = [*block_id, first_reading_successor]
                    .into_iter()
                    .collect::<Vec<_>>();

                loop {
                    let last = *path.last().unwrap();
                    let RegisterAccessType::Read(reading_successor) =
                        register_access_types_with_succ.get(&last).unwrap()[*r]
                    else {
                        unreachable!()
                    };

                    if reading_successor == last {
                        // We have reached the block that reads the register.
                        break;
                    }

                    path.push(reading_successor);
                }
                let path = path
                    .into_iter()
                    .map(|block_id| format!("0x{block_id:x}"))
                    .collect::<Vec<_>>();
                tracing::info!(
                    "  Register {r} might be read in the future via the following path: {path:?}"
                );
            }
        }
    }
}
