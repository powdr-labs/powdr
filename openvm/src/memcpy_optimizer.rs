use std::collections::BTreeMap;
use std::sync::Arc;

use openvm_instructions::instruction::Instruction as OpenVmInstruction;
use openvm_instructions::program::DEFAULT_PC_STEP;
use openvm_instructions::VmOpcode;
use openvm_stark_backend::p3_field::{FieldAlgebra, PrimeField32};
use openvm_stark_sdk::p3_baby_bear::BabyBear;
use powdr_autoprecompiles::blocks::PcStep;

use crate::{isa::OpenVmISA, Instr, OriginalCompiledProgram};

// OpenVM opcode constants for RISC-V
const OPCODE_ADD: usize = 0x200;
const OPCODE_AND: usize = 0x204;
const OPCODE_OR: usize = 0x203;
const OPCODE_AUIPC: usize = 0x240;
const OPCODE_JALR: usize = 0x235;
const OPCODE_BEQ: usize = 0x220;
const OPCODE_BNE: usize = 0x221;

// Load/Store opcodes
const OPCODE_LOADW: usize = 0x210;
const OPCODE_LOADBU: usize = 0x211;
const OPCODE_STOREW: usize = 0x213;
const OPCODE_STOREB: usize = 0x215;

// Register pointers: reg_ptr = 4 * register_number
const A0_REG_PTR: u32 = 40; // a0 = x10 (dst)
const A1_REG_PTR: u32 = 44; // a1 = x11 (src)
const A2_REG_PTR: u32 = 48; // a2 = x12 (temp for alignment)
const A3_REG_PTR: u32 = 52; // a3 = x13 (temp for data)
const A4_REG_PTR: u32 = 56; // a4 = x14 (temp for alignment)
const RA_REG_PTR: u32 = 4; // ra = x1
const X0_REG_PTR: u32 = 0; // x0 = zero

const MAX_MEMCPY_LENGTH: u64 = 128;

type F = BabyBear;

fn f(v: u32) -> F {
    F::from_canonical_u32(v)
}

/// Reconstruct the call target from an AUIPC+JALR pair within a basic block.
fn auipc_jalr_target(
    block: &[Instr<BabyBear, impl OpenVmISA>],
    block_start_pc: u64,
    pc_step: u32,
) -> Option<u64> {
    if block.len() < 2 {
        return None;
    }
    let jalr = block.last()?;
    if jalr.inner.opcode.as_usize() != OPCODE_JALR {
        return None;
    }
    let jalr_rs1 = jalr.inner.b;

    for (i, instr) in block.iter().enumerate().rev().skip(1) {
        if instr.inner.opcode.as_usize() == OPCODE_AUIPC && instr.inner.a == jalr_rs1 {
            let auipc_pc = block_start_pc + (i as u64) * pc_step as u64;
            let auipc_upper = (instr.inner.c.as_canonical_u32() as u64) << 8;
            let reg_val = auipc_pc + auipc_upper;
            let jalr_c = jalr.inner.c.as_canonical_u32() as u64;
            let jalr_signed_negative = jalr.inner.g.as_canonical_u32() != 0;
            let target = if jalr_signed_negative {
                reg_val.wrapping_sub(jalr_c)
            } else {
                reg_val.wrapping_add(jalr_c)
            };
            return Some(target);
        }
        if instr.inner.a == jalr_rs1 {
            break;
        }
    }
    None
}

/// Try to determine the constant value of register a2 at the point of the JALR call.
fn resolve_a2_constant(block: &[Instr<BabyBear, impl OpenVmISA>]) -> Option<u64> {
    for instr in block.iter().rev().skip(1) {
        let opcode = instr.inner.opcode.as_usize();
        let dst = instr.inner.a.as_canonical_u32();
        if dst != A2_REG_PTR {
            continue;
        }
        match opcode {
            OPCODE_ADD
                if instr.inner.b.as_canonical_u32() == 0
                    && instr.inner.e.as_canonical_u32() == 0 =>
            {
                let c_val = instr.inner.c.as_canonical_u32();
                if c_val < 0x800 {
                    return Some(c_val as u64);
                } else {
                    return None;
                }
            }
            _ => return None,
        }
    }
    None
}

// ---- Instruction builders ----

fn alu(opcode: usize, rd: u32, rs1: u32, rs2_or_imm: u32, rs2_as: u32) -> OpenVmInstruction<F> {
    OpenVmInstruction {
        opcode: VmOpcode::from_usize(opcode),
        a: f(rd),
        b: f(rs1),
        c: f(rs2_or_imm),
        d: F::ONE,
        e: f(rs2_as),
        f: F::ZERO,
        g: F::ZERO,
    }
}

fn addi(rd: u32, rs1: u32, imm: u32) -> OpenVmInstruction<F> {
    alu(OPCODE_ADD, rd, rs1, imm, 0)
}

fn andi(rd: u32, rs1: u32, imm: u32) -> OpenVmInstruction<F> {
    alu(OPCODE_AND, rd, rs1, imm, 0)
}

fn or_reg(rd: u32, rs1: u32, rs2: u32) -> OpenVmInstruction<F> {
    alu(OPCODE_OR, rd, rs1, rs2, 1)
}

fn load_store(
    opcode: usize,
    rd_rs2: u32,
    rs1: u32,
    imm: u32,
    imm_sign: u32,
) -> OpenVmInstruction<F> {
    OpenVmInstruction {
        opcode: VmOpcode::from_usize(opcode),
        a: f(rd_rs2),
        b: f(rs1),
        c: f(imm),
        d: F::ONE,
        e: f(2),   // mem_as = main memory
        f: F::ONE, // needs_write
        g: f(imm_sign),
    }
}

fn loadw(rd: u32, rs1: u32, imm: u32) -> OpenVmInstruction<F> {
    load_store(OPCODE_LOADW, rd, rs1, imm, 0)
}

fn loadbu(rd: u32, rs1: u32, imm: u32) -> OpenVmInstruction<F> {
    load_store(OPCODE_LOADBU, rd, rs1, imm, 0)
}

fn storew(rs2: u32, rs1: u32, imm: u32) -> OpenVmInstruction<F> {
    load_store(OPCODE_STOREW, rs2, rs1, imm, 0)
}

fn storeb(rs2: u32, rs1: u32, imm: u32) -> OpenVmInstruction<F> {
    load_store(OPCODE_STOREB, rs2, rs1, imm, 0)
}

fn bne(rs1: u32, rs2: u32, offset: i32) -> OpenVmInstruction<F> {
    branch(OPCODE_BNE, rs1, rs2, offset)
}

fn beq(rs1: u32, rs2: u32, offset: i32) -> OpenVmInstruction<F> {
    branch(OPCODE_BEQ, rs1, rs2, offset)
}

fn branch(opcode: usize, rs1: u32, rs2: u32, offset: i32) -> OpenVmInstruction<F> {
    let imm = if offset >= 0 {
        f(offset as u32)
    } else {
        -f((-offset) as u32)
    };
    OpenVmInstruction {
        opcode: VmOpcode::from_usize(opcode),
        a: f(rs1),
        b: f(rs2),
        c: imm,
        d: F::ONE,
        e: F::ONE,
        f: F::ZERO,
        g: F::ZERO,
    }
}

fn jalr_ret() -> OpenVmInstruction<F> {
    OpenVmInstruction {
        opcode: VmOpcode::from_usize(OPCODE_JALR),
        a: f(X0_REG_PTR),
        b: f(RA_REG_PTR),
        c: F::ZERO,
        d: F::ONE,
        e: F::ZERO,
        f: F::ZERO,
        g: F::ZERO,
    }
}

/// Generate head bytes (LOADBU/STOREB), word copies (LOADW/STOREW), and tail bytes.
/// `head_bytes` is the number of bytes to copy before the first word-aligned offset.
fn generate_head_word_tail(head_bytes: u32, total_length: u32) -> Vec<OpenVmInstruction<F>> {
    let remaining = total_length - head_bytes;
    let num_words = remaining / 4;
    let tail = remaining % 4;

    let mut instrs = Vec::new();
    // Head bytes
    for i in 0..head_bytes {
        instrs.push(loadbu(A3_REG_PTR, A1_REG_PTR, i));
        instrs.push(storeb(A3_REG_PTR, A0_REG_PTR, i));
    }
    // Word copies
    for w in 0..num_words {
        let offset = head_bytes + w * 4;
        instrs.push(loadw(A3_REG_PTR, A1_REG_PTR, offset));
        instrs.push(storew(A3_REG_PTR, A0_REG_PTR, offset));
    }
    // Tail bytes
    for i in 0..tail {
        let offset = head_bytes + num_words * 4 + i;
        instrs.push(loadbu(A3_REG_PTR, A1_REG_PTR, offset));
        instrs.push(storeb(A3_REG_PTR, A0_REG_PTR, offset));
    }
    instrs
}

/// Generate a specialized memcpy RISC-V routine for a known constant length.
///
/// Five execution paths based on pointer alignment:
///   1. Both word-aligned: LOADW/STOREW for all words + byte tail
///   2. Same misalignment (off by 1): 3 head bytes + LOADW/STOREW + tail
///   3. Same misalignment (off by 2): 2 head bytes + LOADW/STOREW + tail
///   4. Same misalignment (off by 3): 1 head byte + LOADW/STOREW + tail
///   5. Different misalignment: byte-by-byte LOADBU/STOREB fallback
///
/// When src and dst have the same alignment (mod 4), copying a few head bytes
/// makes both pointers word-aligned, enabling efficient word copies for the bulk
/// of the data. This is the common case for allocator-returned pointers.
fn generate_specialized_memcpy(length: u32) -> Vec<OpenVmInstruction<F>> {
    // For very short copies, just do bytes — no alignment check overhead
    if length < 4 {
        let mut instrs = Vec::new();
        for i in 0..length {
            instrs.push(loadbu(A3_REG_PTR, A1_REG_PTR, i));
            instrs.push(storeb(A3_REG_PTR, A0_REG_PTR, i));
        }
        instrs.push(jalr_ret());
        return instrs;
    }

    let mut instrs = Vec::new();
    let pc_step = DEFAULT_PC_STEP as i32;

    // -- Alignment check --
    // ANDI a2, a1, 3        (src & 3)
    // ANDI a4, a0, 3        (dst & 3)
    // OR   a3, a2, a4       (either misaligned?)
    // BNE  a3, x0, not_both_aligned
    instrs.push(andi(A2_REG_PTR, A1_REG_PTR, 3));
    instrs.push(andi(A4_REG_PTR, A0_REG_PTR, 3));
    instrs.push(or_reg(A3_REG_PTR, A2_REG_PTR, A4_REG_PTR));
    let bne_not_both_idx = instrs.len();
    instrs.push(bne(A3_REG_PTR, X0_REG_PTR, 0)); // placeholder

    // -- Path 1: Both aligned --
    instrs.extend(generate_head_word_tail(0, length));
    instrs.push(jalr_ret());

    // -- Not both aligned: check same vs different misalignment --
    let not_both_aligned_idx = instrs.len();
    // BNE a2, a4, byte_path  (different misalignment → byte fallback)
    let bne_diff_idx = instrs.len();
    instrs.push(bne(A2_REG_PTR, A4_REG_PTR, 0)); // placeholder

    // Same misalignment: dispatch by alignment value (a2 is 1, 2, or 3)
    // LI a3, 1; BEQ a2, a3, off_1
    instrs.push(addi(A3_REG_PTR, X0_REG_PTR, 1));
    let beq_off1_idx = instrs.len();
    instrs.push(beq(A2_REG_PTR, A3_REG_PTR, 0)); // placeholder
                                                 // LI a3, 2; BEQ a2, a3, off_2
    instrs.push(addi(A3_REG_PTR, X0_REG_PTR, 2));
    let beq_off2_idx = instrs.len();
    instrs.push(beq(A2_REG_PTR, A3_REG_PTR, 0)); // placeholder
                                                 // Fall through: off by 3 → copy 1 head byte to align

    // -- Path 4: Same misalignment, off by 3 → 1 head byte --
    let off3_idx = instrs.len();
    let _ = off3_idx; // used only for documentation
    instrs.extend(generate_head_word_tail(1, length));
    instrs.push(jalr_ret());

    // -- Path 3: Same misalignment, off by 2 → 2 head bytes --
    let off2_idx = instrs.len();
    instrs.extend(generate_head_word_tail(2, length));
    instrs.push(jalr_ret());

    // -- Path 2: Same misalignment, off by 1 → 3 head bytes --
    let off1_idx = instrs.len();
    instrs.extend(generate_head_word_tail(3, length));
    instrs.push(jalr_ret());

    // -- Path 5: Different misalignment → byte-by-byte --
    let byte_path_idx = instrs.len();
    for i in 0..length {
        instrs.push(loadbu(A3_REG_PTR, A1_REG_PTR, i));
        instrs.push(storeb(A3_REG_PTR, A0_REG_PTR, i));
    }
    instrs.push(jalr_ret());

    // -- Patch branch offsets --
    instrs[bne_not_both_idx] = bne(
        A3_REG_PTR,
        X0_REG_PTR,
        (not_both_aligned_idx as i32 - bne_not_both_idx as i32) * pc_step,
    );
    instrs[bne_diff_idx] = bne(
        A2_REG_PTR,
        A4_REG_PTR,
        (byte_path_idx as i32 - bne_diff_idx as i32) * pc_step,
    );
    instrs[beq_off1_idx] = beq(
        A2_REG_PTR,
        A3_REG_PTR,
        (off1_idx as i32 - beq_off1_idx as i32) * pc_step,
    );
    instrs[beq_off2_idx] = beq(
        A2_REG_PTR,
        A3_REG_PTR,
        (off2_idx as i32 - beq_off2_idx as i32) * pc_step,
    );

    instrs
}

/// Compute AUIPC c field and JALR c/g fields for a call from `from_pc` to `to_pc`.
/// The AUIPC is at `from_pc` and JALR at `from_pc + pc_step`.
fn encode_auipc_jalr(from_pc: u64, to_pc: u64) -> (F, F, F) {
    // target = from_pc + (auipc_upper << 8) + jalr_offset
    // where auipc_upper << 8 is the upper 20 bits contribution
    let diff = to_pc as i64 - from_pc as i64;
    // Split into upper 20 bits and lower 12 bits, with sign extension handling
    let mut upper = diff >> 12;
    let mut lower = diff & 0xFFF;
    // If lower is negative when sign-extended (bit 11 set), adjust
    if lower >= 0x800 {
        upper += 1;
        lower -= 0x1000;
    }
    let auipc_c = f((upper as u32) << 4); // stored as (imm & 0xfffff000) >> 8
    let (jalr_c, jalr_g) = if lower >= 0 {
        (f(lower as u32), F::ZERO)
    } else {
        (f((-lower) as u32), F::ONE)
    };
    (auipc_c, jalr_c, jalr_g)
}

pub fn optimize<ISA: OpenVmISA>(
    original_program: OriginalCompiledProgram<ISA>,
) -> OriginalCompiledProgram<ISA> {
    let symbols = ISA::get_symbol_table(&original_program.linked_program);
    let blocks = original_program.collect_basic_blocks();

    let pc_step = <Instr<BabyBear, ISA> as PcStep>::pc_step();
    let branching = ISA::branching_opcodes();

    // Find memcpy function address
    let memcpy_pc = symbols
        .table()
        .iter()
        .find(|(_, names)| names.iter().any(|n| n.contains("memcpy")));

    let Some((memcpy_pc, names)) = memcpy_pc else {
        tracing::info!("memcpy_optimizer: no memcpy symbol found");
        return original_program;
    };
    let memcpy_pc: u64 = *memcpy_pc as u64;
    tracing::info!(
        "Using memcpy at PC 0x{:x} with symbols {}",
        memcpy_pc,
        names.join(", ")
    );

    // Collect call sites: for each block calling memcpy with constant length,
    // record (auipc_index_in_program, length).
    // We need the index of the AUIPC instruction in the program to patch it later.
    let mut calls_by_length: BTreeMap<u64, Vec<u64>> = BTreeMap::new();
    for block in &blocks {
        let Some(last_instr) = block.instructions.last() else {
            continue;
        };
        if !branching.contains(&last_instr.inner.opcode) {
            continue;
        }
        if last_instr.inner.opcode.as_usize() == OPCODE_JALR {
            if let Some(target_pc) = auipc_jalr_target(&block.instructions, block.start_pc, pc_step)
            {
                if target_pc == memcpy_pc {
                    if let Some(len) = resolve_a2_constant(&block.instructions) {
                        if len <= MAX_MEMCPY_LENGTH {
                            // Find the PC of the AUIPC instruction for this call
                            let jalr_rs1 = last_instr.inner.b;
                            for (i, instr) in block.instructions.iter().enumerate().rev().skip(1) {
                                if instr.inner.opcode.as_usize() == OPCODE_AUIPC
                                    && instr.inner.a == jalr_rs1
                                {
                                    let auipc_pc = block.start_pc + (i as u64) * pc_step as u64;
                                    calls_by_length.entry(len).or_default().push(auipc_pc);
                                    break;
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    if calls_by_length.is_empty() {
        tracing::info!("memcpy_optimizer: no eligible memcpy calls found");
        return original_program;
    }

    tracing::info!(
        "memcpy_optimizer: memcpy at PC 0x{:x}, call sites by length: {:?}",
        memcpy_pc,
        calls_by_length
            .iter()
            .map(|(len, sites)| (*len, sites.len()))
            .collect::<Vec<_>>()
    );

    // Clone exe out of Arc so we can mutate it
    let mut exe = (*original_program.exe).clone();
    let pc_base = exe.program.pc_base as u64;

    // Generate specialized routines and append to program, recording their start PCs
    let mut routine_pcs: BTreeMap<u64, u64> = BTreeMap::new(); // length -> start PC

    for &length in calls_by_length.keys() {
        let routine = generate_specialized_memcpy(length as u32);
        let routine_start_index = exe.program.instructions_and_debug_infos.len();
        let routine_start_pc = pc_base + (routine_start_index as u64) * (DEFAULT_PC_STEP as u64);

        tracing::info!(
            "memcpy_optimizer: appending specialized memcpy for length={} at PC 0x{:x} ({} instructions)",
            length,
            routine_start_pc,
            routine.len()
        );

        for instr in routine {
            exe.program
                .instructions_and_debug_infos
                .push(Some((instr, None)));
        }

        routine_pcs.insert(length, routine_start_pc);
    }

    // Patch call sites: rewrite AUIPC+JALR to point to the specialized routine
    for (&length, auipc_pcs) in &calls_by_length {
        let target_pc = routine_pcs[&length];
        for &auipc_pc in auipc_pcs {
            let auipc_index = ((auipc_pc - pc_base) / DEFAULT_PC_STEP as u64) as usize;
            let jalr_index = auipc_index + 1;

            let (auipc_c, jalr_c, jalr_g) = encode_auipc_jalr(auipc_pc, target_pc);

            // Patch AUIPC: update c field
            if let Some(Some((ref mut instr, _))) = exe
                .program
                .instructions_and_debug_infos
                .get_mut(auipc_index)
            {
                instr.c = auipc_c;
            }

            // Patch JALR: update c and g fields
            if let Some(Some((ref mut instr, _))) =
                exe.program.instructions_and_debug_infos.get_mut(jalr_index)
            {
                instr.c = jalr_c;
                instr.g = jalr_g;
            }
        }
    }

    tracing::info!(
        "memcpy_optimizer: patched {} call sites across {} lengths",
        calls_by_length.values().map(|v| v.len()).sum::<usize>(),
        calls_by_length.len()
    );

    OriginalCompiledProgram::new(
        Arc::new(exe),
        original_program.vm_config,
        original_program.linked_program,
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Minimal RISC-V interpreter for the instruction subset used by specialized memcpy.
    struct Interpreter {
        regs: [u32; 32],
        mem: Vec<u8>,
        pc: u32,
    }

    /// BabyBear modulus
    const P: u32 = 0x78000001;

    impl Interpreter {
        fn new(mem_size: usize) -> Self {
            Self {
                regs: [0; 32],
                mem: vec![0; mem_size],
                pc: 0,
            }
        }

        fn reg(&self, ptr: u32) -> u32 {
            let idx = (ptr / 4) as usize;
            if idx == 0 {
                0
            } else {
                self.regs[idx]
            }
        }

        fn set_reg(&mut self, ptr: u32, val: u32) {
            let idx = (ptr / 4) as usize;
            if idx != 0 {
                self.regs[idx] = val;
            }
        }

        fn effective_addr(&self, base_ptr: u32, imm: u32, sign: u32) -> u32 {
            let base = self.reg(base_ptr);
            if sign == 0 {
                base.wrapping_add(imm)
            } else {
                base.wrapping_sub(imm)
            }
        }

        fn run(&mut self, instrs: &[OpenVmInstruction<F>], max_steps: usize) {
            let pc_step = DEFAULT_PC_STEP;
            for step in 0..max_steps {
                let idx = self.pc as usize / pc_step as usize;
                assert!(
                    idx < instrs.len(),
                    "PC out of bounds: pc={}, idx={}, len={} at step {}",
                    self.pc,
                    idx,
                    instrs.len(),
                    step
                );
                let instr = &instrs[idx];
                let opcode = instr.opcode.as_usize();
                let a = instr.a.as_canonical_u32();
                let b = instr.b.as_canonical_u32();
                let c_raw = instr.c.as_canonical_u32();
                let e = instr.e.as_canonical_u32();
                let g = instr.g.as_canonical_u32();

                match opcode {
                    OPCODE_ADD | OPCODE_AND | OPCODE_OR => {
                        let rs1_val = self.reg(b);
                        let operand = if e == 0 { c_raw } else { self.reg(c_raw) };
                        let result = match opcode {
                            OPCODE_ADD => rs1_val.wrapping_add(operand),
                            OPCODE_AND => rs1_val & operand,
                            OPCODE_OR => rs1_val | operand,
                            _ => unreachable!(),
                        };
                        self.set_reg(a, result);
                        self.pc += pc_step;
                    }
                    OPCODE_LOADW => {
                        let addr = self.effective_addr(b, c_raw, g);
                        assert!(
                            addr % 4 == 0,
                            "Unaligned LOADW at addr 0x{:x} (step {})",
                            addr,
                            step
                        );
                        let a_idx = addr as usize;
                        let val = u32::from_le_bytes([
                            self.mem[a_idx],
                            self.mem[a_idx + 1],
                            self.mem[a_idx + 2],
                            self.mem[a_idx + 3],
                        ]);
                        self.set_reg(a, val);
                        self.pc += pc_step;
                    }
                    OPCODE_LOADBU => {
                        let addr = self.effective_addr(b, c_raw, g);
                        let val = self.mem[addr as usize] as u32;
                        self.set_reg(a, val);
                        self.pc += pc_step;
                    }
                    OPCODE_STOREW => {
                        let addr = self.effective_addr(b, c_raw, g);
                        assert!(
                            addr % 4 == 0,
                            "Unaligned STOREW at addr 0x{:x} (step {})",
                            addr,
                            step
                        );
                        let val = self.reg(a);
                        let bytes = val.to_le_bytes();
                        let a_idx = addr as usize;
                        self.mem[a_idx..a_idx + 4].copy_from_slice(&bytes);
                        self.pc += pc_step;
                    }
                    OPCODE_STOREB => {
                        let addr = self.effective_addr(b, c_raw, g);
                        let val = (self.reg(a) & 0xFF) as u8;
                        self.mem[addr as usize] = val;
                        self.pc += pc_step;
                    }
                    OPCODE_BEQ | OPCODE_BNE => {
                        let rs1_val = self.reg(a);
                        let rs2_val = self.reg(b);
                        let take = match opcode {
                            OPCODE_BEQ => rs1_val == rs2_val,
                            OPCODE_BNE => rs1_val != rs2_val,
                            _ => unreachable!(),
                        };
                        if take {
                            // Decode signed offset from BabyBear field element
                            let offset: i32 = if c_raw > P / 2 {
                                -(P.wrapping_sub(c_raw) as i32)
                            } else {
                                c_raw as i32
                            };
                            self.pc = (self.pc as i32 + offset) as u32;
                        } else {
                            self.pc += pc_step;
                        }
                    }
                    OPCODE_JALR => {
                        // JALR with rd=x0 is a return — end execution
                        return;
                    }
                    _ => panic!("Unknown opcode: 0x{:x} at step {}", opcode, step),
                }
            }
            panic!("Max steps exceeded ({})", max_steps);
        }
    }

    #[test]
    fn test_specialized_memcpy_all_alignments() {
        // Test a variety of lengths including edge cases
        let lengths: Vec<u32> = vec![0, 1, 2, 3, 4, 5, 7, 8, 12, 15, 16, 31, 32, 48, 63, 64, 128];

        for &length in &lengths {
            let routine = generate_specialized_memcpy(length);

            for src_align in 0..4u32 {
                for dst_align in 0..4u32 {
                    // Place src and dst far enough apart to never overlap,
                    // and offset from a base aligned address to test alignment.
                    let src_base = 256 + src_align;
                    let dst_base = 768 + dst_align;

                    let mut interp = Interpreter::new(1024);

                    // Fill entire memory with a sentinel value (0xAA)
                    // so we can detect any stray writes
                    interp.mem.fill(0xAA);

                    // Fill src with a deterministic non-zero pattern (distinct from sentinel)
                    for i in 0..length {
                        interp.mem[(src_base + i) as usize] =
                            ((i.wrapping_mul(7).wrapping_add(13 + length)) & 0x7F) as u8;
                    }

                    // Set argument registers
                    interp.set_reg(A0_REG_PTR, dst_base);
                    interp.set_reg(A1_REG_PTR, src_base);

                    // Run the routine
                    interp.run(&routine, 10_000);

                    // Verify every byte was copied correctly
                    for i in 0..length {
                        let expected = interp.mem[(src_base + i) as usize];
                        let actual = interp.mem[(dst_base + i) as usize];
                        assert_eq!(
                            actual, expected,
                            "Mismatch at byte {} for length={}, src_align={}, dst_align={}: \
                             expected 0x{:02x}, got 0x{:02x}",
                            i, length, src_align, dst_align, expected, actual
                        );
                    }

                    // Verify no stray writes: check a guard zone around the destination
                    let guard = 16;
                    let check_start = dst_base.saturating_sub(guard);
                    let check_end = (dst_base + length + guard).min(1024);
                    for addr in check_start..check_end {
                        if addr >= dst_base && addr < dst_base + length {
                            continue; // inside the copied region, already verified
                        }
                        assert_eq!(
                            interp.mem[addr as usize],
                            0xAA,
                            "Stray write at addr {} (dst=[{}..{})) for \
                             length={}, src_align={}, dst_align={}",
                            addr,
                            dst_base,
                            dst_base + length,
                            length,
                            src_align,
                            dst_align
                        );
                    }
                }
            }
        }
    }
}
