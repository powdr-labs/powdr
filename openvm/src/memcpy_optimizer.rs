use std::collections::BTreeMap;
use std::path::Path;

use openvm_transpiler::elf::Elf;
use powdr_riscv_elf::debug_info::SymbolTable;
use rustc_demangle::demangle;

const MAX_MEMCPY_LENGTH: u32 = 128;

/// Parse the base address (lowest executable segment vaddr) from a raw ELF binary.
pub fn parse_pc_base(elf_path: &Path) -> u32 {
    parse_pc_base_data(&std::fs::read(elf_path).expect("Failed to read ELF binary"))
}

/// Parse the base address (lowest executable segment vaddr) from a raw ELF binary.
pub fn parse_pc_base_data(data: &[u8]) -> u32 {
    assert!(data.len() > 52, "ELF too small");
    assert_eq!(&data[0..4], b"\x7fELF", "Not an ELF file");
    assert_eq!(data[4], 1, "Expected 32-bit ELF"); // EI_CLASS = ELFCLASS32

    let e_phoff = u32::from_le_bytes(data[28..32].try_into().unwrap()) as usize;
    let e_phentsize = u16::from_le_bytes(data[42..44].try_into().unwrap()) as usize;
    let e_phnum = u16::from_le_bytes(data[44..46].try_into().unwrap()) as usize;

    let mut min_vaddr = u32::MAX;
    for i in 0..e_phnum {
        let off = e_phoff + i * e_phentsize;
        let p_type = u32::from_le_bytes(data[off..off + 4].try_into().unwrap());
        let p_vaddr = u32::from_le_bytes(data[off + 8..off + 12].try_into().unwrap());
        let p_flags = u32::from_le_bytes(data[off + 24..off + 28].try_into().unwrap());
        // PT_LOAD = 1, PF_X = 1
        if p_type == 1 && (p_flags & 1) != 0 && p_vaddr < min_vaddr {
            min_vaddr = p_vaddr;
        }
    }
    assert_ne!(min_vaddr, u32::MAX, "No executable segment found");
    min_vaddr
}

// RISC-V register numbers
const X0: u32 = 0;
const X1: u32 = 1; // ra
const X5: u32 = 5; // t0
const X6: u32 = 6; // t1
const X7: u32 = 7; // t2
const X10: u32 = 10; // a0 (dst / ptr1)
const X11: u32 = 11; // a1 (src / ptr2)
const X12: u32 = 12; // a2 (length)
const X13: u32 = 13; // a3 (temp)
const X14: u32 = 14; // a4 (temp)
const X28: u32 = 28; // t3
const X29: u32 = 29; // t4

// RISC-V instruction encoding helpers

/// Encode ADDI rd, rs1, imm (I-type)
fn rv_addi(rd: u32, rs1: u32, imm: i32) -> u32 {
    let imm = (imm as u32) & 0xFFF;
    ((imm << 20) | (rs1 << 15)) | (rd << 7) | 0x13
}

/// Encode ANDI rd, rs1, imm (I-type)
fn rv_andi(rd: u32, rs1: u32, imm: i32) -> u32 {
    let imm = (imm as u32) & 0xFFF;
    (imm << 20) | (rs1 << 15) | (0b111 << 12) | (rd << 7) | 0x13
}

/// Encode OR rd, rs1, rs2 (R-type)
fn rv_or(rd: u32, rs1: u32, rs2: u32) -> u32 {
    (rs2 << 20) | (rs1 << 15) | (0b110 << 12) | (rd << 7) | 0x33
}

/// Encode SUB rd, rs1, rs2 (R-type)
fn rv_sub(rd: u32, rs1: u32, rs2: u32) -> u32 {
    (0b0100000 << 25) | (rs2 << 20) | (rs1 << 15) | (0b000 << 12) | (rd << 7) | 0x33
}

/// Encode SRLI rd, rs1, shamt (I-type shift)
fn rv_srli(rd: u32, rs1: u32, shamt: u32) -> u32 {
    ((shamt & 0x1F) << 20) | (rs1 << 15) | (0b101 << 12) | (rd << 7) | 0x13
}

/// Encode JAL rd, offset (J-type, offset in bytes)
fn rv_jal(rd: u32, offset: i32) -> u32 {
    let imm = offset as u32;
    let bit_20 = (imm >> 20) & 1;
    let bits_10_1 = (imm >> 1) & 0x3FF;
    let bit_11 = (imm >> 11) & 1;
    let bits_19_12 = (imm >> 12) & 0xFF;
    (bit_20 << 31) | (bits_10_1 << 21) | (bit_11 << 20) | (bits_19_12 << 12) | (rd << 7) | 0x6F
}

/// Encode LW rd, imm(rs1) (I-type)
fn rv_lw(rd: u32, rs1: u32, imm: i32) -> u32 {
    let imm = (imm as u32) & 0xFFF;
    (imm << 20) | (rs1 << 15) | (0b010 << 12) | (rd << 7) | 0x03
}

/// Encode LBU rd, imm(rs1) (I-type)
fn rv_lbu(rd: u32, rs1: u32, imm: i32) -> u32 {
    let imm = (imm as u32) & 0xFFF;
    (imm << 20) | (rs1 << 15) | (0b100 << 12) | (rd << 7) | 0x03
}

/// Encode SW rs2, imm(rs1) (S-type)
fn rv_sw(rs2: u32, rs1: u32, imm: i32) -> u32 {
    let imm = (imm as u32) & 0xFFF;
    let imm_11_5 = (imm >> 5) & 0x7F;
    let imm_4_0 = imm & 0x1F;
    (imm_11_5 << 25) | (rs2 << 20) | (rs1 << 15) | (0b010 << 12) | (imm_4_0 << 7) | 0x23
}

/// Encode SB rs2, imm(rs1) (S-type)
fn rv_sb(rs2: u32, rs1: u32, imm: i32) -> u32 {
    let imm = (imm as u32) & 0xFFF;
    let imm_11_5 = (imm >> 5) & 0x7F;
    let imm_4_0 = imm & 0x1F;
    ((imm_11_5 << 25) | (rs2 << 20) | (rs1 << 15)) | (imm_4_0 << 7) | 0x23
}

/// Encode BEQ rs1, rs2, offset (B-type, offset in bytes)
fn rv_beq(rs1: u32, rs2: u32, offset: i32) -> u32 {
    rv_branch(0b000, rs1, rs2, offset)
}

/// Encode BNE rs1, rs2, offset (B-type, offset in bytes)
fn rv_bne(rs1: u32, rs2: u32, offset: i32) -> u32 {
    rv_branch(0b001, rs1, rs2, offset)
}

/// Encode BLTU rs1, rs2, offset (B-type, offset in bytes, unsigned less-than)
fn rv_bltu(rs1: u32, rs2: u32, offset: i32) -> u32 {
    rv_branch(0b110, rs1, rs2, offset)
}

fn rv_branch(funct3: u32, rs1: u32, rs2: u32, offset: i32) -> u32 {
    let imm = (offset as u32) & 0x1FFE; // bits [12:1], bit 0 always 0
    let bit_12 = (imm >> 12) & 1;
    let bit_11 = (imm >> 11) & 1;
    let bits_10_5 = (imm >> 5) & 0x3F;
    let bits_4_1 = (imm >> 1) & 0xF;
    (bit_12 << 31)
        | (bits_10_5 << 25)
        | (rs2 << 20)
        | (rs1 << 15)
        | (funct3 << 12)
        | (bits_4_1 << 8)
        | (bit_11 << 7)
        | 0x63
}

/// Encode JALR rd, rs1, imm (I-type)
fn rv_jalr(rd: u32, rs1: u32, imm: i32) -> u32 {
    let imm = (imm as u32) & 0xFFF;
    ((imm << 20) | (rs1 << 15)) | (rd << 7) | 0x67
}

/// Encode JALR x0, x1, 0 (return)
fn rv_ret() -> u32 {
    rv_jalr(X0, X1, 0)
}

/// Encode AUIPC rd, imm (U-type, imm is the upper 20 bits already shifted)
fn rv_auipc(rd: u32, imm: u32) -> u32 {
    (imm & 0xFFFFF000) | (rd << 7) | 0x17
}

// ---- RISC-V instruction decoding helpers ----

/// Extract opcode field (bits [6:0]) from a raw RISC-V instruction
fn rv_opcode(insn: u32) -> u32 {
    insn & 0x7F
}

/// Extract rd field (bits [11:7])
fn rv_rd(insn: u32) -> u32 {
    (insn >> 7) & 0x1F
}

/// Extract rs1 field (bits [19:15])
fn rv_rs1(insn: u32) -> u32 {
    (insn >> 15) & 0x1F
}

/// Extract funct3 field (bits [14:12])
fn rv_funct3(insn: u32) -> u32 {
    (insn >> 12) & 0x7
}

/// Decode I-type immediate (sign-extended, bits [31:20])
fn rv_imm_i(insn: u32) -> i32 {
    (insn as i32) >> 20
}

/// Decode U-type immediate (bits [31:12], already shifted)
fn rv_imm_u(insn: u32) -> u32 {
    insn & 0xFFFFF000
}

/// Decode J-type immediate (JAL): bits [31|30:21|20|19:12] → sign-extended 21-bit offset
fn rv_imm_j(insn: u32) -> i32 {
    let insn = insn as i32;
    let imm20 = (insn >> 31) & 1; // bit 31 → imm[20]
    let imm10_1 = (insn >> 21) & 0x3FF; // bits 30:21 → imm[10:1]
    let imm11 = (insn >> 20) & 1; // bit 20 → imm[11]
    let imm19_12 = (insn >> 12) & 0xFF; // bits 19:12 → imm[19:12]
    let raw = (imm20 << 20) | (imm19_12 << 12) | (imm11 << 11) | (imm10_1 << 1);
    // Sign-extend from bit 20
    (raw << 11) >> 11
}

/// Check if instruction is AUIPC (opcode 0x17)
fn is_auipc(insn: u32) -> bool {
    rv_opcode(insn) == 0x17
}

/// Check if instruction is JALR (opcode 0x67, funct3 0x0)
fn is_jalr(insn: u32) -> bool {
    rv_opcode(insn) == 0x67 && rv_funct3(insn) == 0
}

/// Check if instruction is ADDI (opcode 0x13, funct3 0x0)
fn is_addi(insn: u32) -> bool {
    rv_opcode(insn) == 0x13 && rv_funct3(insn) == 0
}

/// Reconstruct call target from AUIPC+JALR pair at raw RISC-V level.
/// `auipc_addr` is the byte address of the AUIPC instruction.
fn rv_auipc_jalr_target(auipc_insn: u32, jalr_insn: u32, auipc_addr: u32) -> u32 {
    let upper = rv_imm_u(auipc_insn); // already has lower 12 bits zeroed
    let lower = rv_imm_i(jalr_insn); // sign-extended 12-bit immediate
    auipc_addr.wrapping_add(upper).wrapping_add(lower as u32)
}

/// Check if instruction is LUI (opcode 0x37)
fn is_lui(insn: u32) -> bool {
    rv_opcode(insn) == 0x37
}

/// Try to resolve the constant value loaded into a register by scanning
/// backwards from `end_idx` (exclusive). Handles:
///   - ADDI rd, x0, imm  (small constants)
///   - LUI rd, upper  followed later by  ADDI rd, rd, lower
/// Returns None if the value cannot be statically determined.
/// `max_value`: if Some, reject constants above this threshold.
fn rv_resolve_reg_constant(
    instructions: &[u32],
    end_idx: usize,
    reg: u32,
    max_value: Option<u32>,
) -> Option<u32> {
    let start = end_idx.saturating_sub(16);
    for i in (start..end_idx).rev() {
        let insn = instructions[i];
        let rd = rv_rd(insn);
        if rd != reg {
            continue;
        }
        if is_addi(insn) && rv_rs1(insn) == X0 {
            // li reg, small_imm  →  addi reg, x0, imm
            let imm = rv_imm_i(insn);
            if imm < 0 {
                return None;
            }
            let val = imm as u32;
            if let Some(max) = max_value {
                if val > max {
                    return None;
                }
            }
            return Some(val);
        }
        if is_addi(insn) && rv_rs1(insn) == reg {
            // addi reg, reg, lower — look further back for LUI reg, upper
            let lower = rv_imm_i(insn);
            for j in (start..i).rev() {
                let insn2 = instructions[j];
                if rv_rd(insn2) != reg {
                    continue;
                }
                if is_lui(insn2) {
                    let upper = rv_imm_u(insn2);
                    let val = upper.wrapping_add(lower as u32);
                    if let Some(max) = max_value {
                        if val > max {
                            return None;
                        }
                    }
                    return Some(val);
                }
                // Some other write to reg before we found LUI — give up
                return None;
            }
            return None;
        }
        // Any other write to the register — give up
        return None;
    }
    None
}

/// Try to resolve the constant value loaded into register a2 (x12) by scanning
/// backwards from `end_idx` (exclusive) in the instruction stream.
/// Scans at most 16 instructions back. Rejects values above MAX_MEMCPY_LENGTH.
fn rv_resolve_a2_constant(instructions: &[u32], end_idx: usize) -> Option<u32> {
    rv_resolve_reg_constant(instructions, end_idx, X12, Some(MAX_MEMCPY_LENGTH))
}

/// Scan all call sites to `target_addr` and report how many have a
/// compile-time constant in a2 (x12) vs. dynamic values.
fn analyze_a2_constants(
    instructions: &[u32],
    pc_base: u32,
    target_addr: u32,
    label: &str,
) {
    let mut total_calls = 0u32;
    let mut constant_calls = 0u32;
    let mut constant_histogram: BTreeMap<u32, u32> = BTreeMap::new();

    for i in 0..instructions.len().saturating_sub(1) {
        let insn0 = instructions[i];
        let insn1 = instructions[i + 1];
        if !is_auipc(insn0) || !is_jalr(insn1) {
            continue;
        }
        let auipc_rd = rv_rd(insn0);
        if rv_rs1(insn1) != auipc_rd || rv_rd(insn1) != X1 {
            continue;
        }
        let auipc_addr = pc_base + (i as u32) * 4;
        let target = rv_auipc_jalr_target(insn0, insn1, auipc_addr);
        if target != target_addr {
            continue;
        }

        total_calls += 1;
        // No max_value cap — we want to see all constants.
        if let Some(val) = rv_resolve_reg_constant(instructions, i, X12, None) {
            constant_calls += 1;
            *constant_histogram.entry(val).or_default() += 1;
        }
    }

    if total_calls == 0 {
        println!("{label}: no call sites found");
        return;
    }

    let dynamic_calls = total_calls - constant_calls;
    let pct = (constant_calls as f64 / total_calls as f64) * 100.0;
    println!(
        "{label}: {constant_calls}/{total_calls} call sites ({pct:.1}%) have compile-time constant size, \
         {dynamic_calls} dynamic"
    );
    if !constant_histogram.is_empty() {
        let mut by_count: Vec<(u32, u32)> = constant_histogram.into_iter().collect();
        by_count.sort_by(|a, b| b.1.cmp(&a.1));
        println!("{label}: constant sizes (size × count):");
        for (val, count) in &by_count {
            println!("{label}:   {val} × {count}");
        }
    }
}

// ---- RISC-V routine generation ----

/// Generate head bytes + word copies + tail bytes as raw RISC-V instructions.
fn rv_generate_head_word_tail(head_bytes: u32, total_length: u32) -> Vec<u32> {
    let remaining = total_length - head_bytes;
    let num_words = remaining / 4;
    let tail = remaining % 4;

    let mut instrs = Vec::new();
    for i in 0..head_bytes {
        instrs.push(rv_lbu(X13, X11, i as i32));
        instrs.push(rv_sb(X13, X10, i as i32));
    }
    for w in 0..num_words {
        let offset = (head_bytes + w * 4) as i32;
        instrs.push(rv_lw(X13, X11, offset));
        instrs.push(rv_sw(X13, X10, offset));
    }
    for i in 0..tail {
        let offset = (head_bytes + num_words * 4 + i) as i32;
        instrs.push(rv_lbu(X13, X11, offset));
        instrs.push(rv_sb(X13, X10, offset));
    }
    instrs
}

/// Generate a specialized memcpy as raw RISC-V u32 instructions for a known constant length.
/// Branch offsets are in bytes (standard RISC-V convention).
fn rv_generate_specialized_memcpy(length: u32) -> Vec<u32> {
    if length == 0 {
        return vec![rv_ret()];
    }

    if length < 4 {
        let mut instrs = Vec::new();
        for i in 0..length {
            instrs.push(rv_lbu(X13, X11, i as i32));
            instrs.push(rv_sb(X13, X10, i as i32));
        }
        instrs.push(rv_ret());
        return instrs;
    }

    rv_generate_alignment_dispatched_copy(length, false)
}

/// Generate backward copy: same layout as forward (head bytes, words, tail bytes)
/// but copied in reverse order for memmove overlap safety.
fn rv_generate_backward_head_word_tail(head_bytes: u32, total_length: u32) -> Vec<u32> {
    let remaining = total_length - head_bytes;
    let num_words = remaining / 4;
    let tail = remaining % 4;

    let mut instrs = Vec::new();
    // Tail bytes (at the end), backward
    for i in (0..tail).rev() {
        let offset = (head_bytes + num_words * 4 + i) as i32;
        instrs.push(rv_lbu(X13, X11, offset));
        instrs.push(rv_sb(X13, X10, offset));
    }
    // Words, backward
    for w in (0..num_words).rev() {
        let offset = (head_bytes + w * 4) as i32;
        instrs.push(rv_lw(X13, X11, offset));
        instrs.push(rv_sw(X13, X10, offset));
    }
    // Head bytes, backward
    for i in (0..head_bytes).rev() {
        instrs.push(rv_lbu(X13, X11, i as i32));
        instrs.push(rv_sb(X13, X10, i as i32));
    }
    instrs
}

/// Generate an alignment-dispatched copy block (forward or backward).
/// Returns the instructions for the block.
fn rv_generate_alignment_dispatched_copy(length: u32, backward: bool) -> Vec<u32> {
    let gen_copy = if backward {
        rv_generate_backward_head_word_tail
    } else {
        rv_generate_head_word_tail
    };

    let mut instrs = Vec::new();

    // Alignment check
    instrs.push(rv_andi(X12, X11, 3)); // src & 3
    instrs.push(rv_andi(X14, X10, 3)); // dst & 3
    instrs.push(rv_or(X13, X12, X14)); // either misaligned?
    let bne_not_both_idx = instrs.len();
    instrs.push(0); // placeholder BNE

    // Path 1: Both aligned
    instrs.extend(gen_copy(0, length));
    instrs.push(rv_ret());

    // Not both aligned
    let not_both_aligned_idx = instrs.len();
    let bne_diff_idx = instrs.len();
    instrs.push(0); // placeholder BNE

    // Same misalignment dispatch
    instrs.push(rv_addi(X13, X0, 1));
    let beq_off1_idx = instrs.len();
    instrs.push(0); // placeholder BEQ
    instrs.push(rv_addi(X13, X0, 2));
    let beq_off2_idx = instrs.len();
    instrs.push(0); // placeholder BEQ

    // off by 3 → 1 head byte
    instrs.extend(gen_copy(1, length));
    instrs.push(rv_ret());

    // off by 2 → 2 head bytes
    let off2_idx = instrs.len();
    instrs.extend(gen_copy(2, length));
    instrs.push(rv_ret());

    // off by 1 → 3 head bytes
    let off1_idx = instrs.len();
    instrs.extend(gen_copy(3, length));
    instrs.push(rv_ret());

    // Byte-by-byte fallback
    let byte_path_idx = instrs.len();
    let range: Vec<u32> = if backward {
        (0..length).rev().collect()
    } else {
        (0..length).collect()
    };
    for i in range {
        instrs.push(rv_lbu(X13, X11, i as i32));
        instrs.push(rv_sb(X13, X10, i as i32));
    }
    instrs.push(rv_ret());

    // Patch branch offsets
    instrs[bne_not_both_idx] = rv_bne(
        X13,
        X0,
        ((not_both_aligned_idx as i32) - (bne_not_both_idx as i32)) * 4,
    );
    instrs[bne_diff_idx] = rv_bne(
        X12,
        X14,
        ((byte_path_idx as i32) - (bne_diff_idx as i32)) * 4,
    );
    instrs[beq_off1_idx] = rv_beq(X12, X13, ((off1_idx as i32) - (beq_off1_idx as i32)) * 4);
    instrs[beq_off2_idx] = rv_beq(X12, X13, ((off2_idx as i32) - (beq_off2_idx as i32)) * 4);

    instrs
}

/// Generate a specialized memmove as raw RISC-V u32 instructions for a known constant length.
/// Unlike memcpy, memmove handles overlapping src/dst by choosing copy direction.
fn rv_generate_specialized_memmove(length: u32) -> Vec<u32> {
    if length == 0 {
        return vec![rv_ret()];
    }

    if length < 4 {
        let mut instrs = Vec::new();
        // Direction check: if dst < src, forward copy is safe
        let bltu_fwd_idx = instrs.len();
        instrs.push(0); // placeholder BLTU

        // Backward byte copy (dst >= src)
        for i in (0..length).rev() {
            instrs.push(rv_lbu(X13, X11, i as i32));
            instrs.push(rv_sb(X13, X10, i as i32));
        }
        instrs.push(rv_ret());

        // Forward byte copy (dst < src)
        let forward_start = instrs.len();
        for i in 0..length {
            instrs.push(rv_lbu(X13, X11, i as i32));
            instrs.push(rv_sb(X13, X10, i as i32));
        }
        instrs.push(rv_ret());

        instrs[bltu_fwd_idx] =
            rv_bltu(X10, X11, ((forward_start - bltu_fwd_idx) as i32) * 4);
        return instrs;
    }

    let mut instrs = Vec::new();

    // Direction check: if dst < src, jump to forward copy
    let bltu_fwd_idx = instrs.len();
    instrs.push(0); // placeholder BLTU

    // Backward path (dst >= src)
    instrs.extend(rv_generate_alignment_dispatched_copy(length, true));

    // Forward path (dst < src)
    let forward_start = instrs.len();
    instrs.extend(rv_generate_alignment_dispatched_copy(length, false));

    instrs[bltu_fwd_idx] =
        rv_bltu(X10, X11, ((forward_start - bltu_fwd_idx) as i32) * 4);

    instrs
}

// ---- memcmp optimization ----

/// Generate the "word_diff" epilogue: given two differing words in X5(t0) and
/// X6(t1), find the first differing byte (little-endian) and return
/// a0 = (byte_from_ptr1 - byte_from_ptr2) as the memcmp result.
/// Uses X7(t2) and X28(t3) as temporaries.
fn rv_generate_word_diff_epilogue() -> Vec<u32> {
    let mut instrs = Vec::new();
    // byte 0 (bits [7:0])
    instrs.push(rv_andi(X7, X5, 0xFF));
    instrs.push(rv_andi(X28, X6, 0xFF));
    let bne0_idx = instrs.len();
    instrs.push(0); // placeholder BNE → diff_done
    // byte 1 (bits [15:8])
    instrs.push(rv_srli(X7, X5, 8));
    instrs.push(rv_andi(X7, X7, 0xFF));
    instrs.push(rv_srli(X28, X6, 8));
    instrs.push(rv_andi(X28, X28, 0xFF));
    let bne1_idx = instrs.len();
    instrs.push(0); // placeholder BNE → diff_done
    // byte 2 (bits [23:16])
    instrs.push(rv_srli(X7, X5, 16));
    instrs.push(rv_andi(X7, X7, 0xFF));
    instrs.push(rv_srli(X28, X6, 16));
    instrs.push(rv_andi(X28, X28, 0xFF));
    let bne2_idx = instrs.len();
    instrs.push(0); // placeholder BNE → diff_done
    // byte 3 — must be the differing one
    instrs.push(rv_srli(X7, X5, 24));
    instrs.push(rv_srli(X28, X6, 24));
    // diff_done: a0 = t2 - t3
    let diff_done_idx = instrs.len();
    instrs.push(rv_sub(X10, X7, X28));
    instrs.push(rv_ret());
    // patch BNEs
    instrs[bne0_idx] =
        rv_bne(X7, X28, ((diff_done_idx as i32) - (bne0_idx as i32)) * 4);
    instrs[bne1_idx] =
        rv_bne(X7, X28, ((diff_done_idx as i32) - (bne1_idx as i32)) * 4);
    instrs[bne2_idx] =
        rv_bne(X7, X28, ((diff_done_idx as i32) - (bne2_idx as i32)) * 4);
    instrs
}

/// Generate a specialized memcmp for a known constant length.
/// a0 = ptr1, a1 = ptr2, a2 = length (already set by caller, not used).
/// Returns a0 = comparison result (<0, 0, >0).
fn rv_generate_specialized_memcmp(length: u32) -> Vec<u32> {
    if length == 0 {
        // return 0
        return vec![rv_addi(X10, X0, 0), rv_ret()];
    }

    if length < 4 {
        // Pure byte-by-byte: short enough that word tricks don't help.
        let mut instrs = Vec::new();
        for i in 0..length {
            instrs.push(rv_lbu(X5, X10, i as i32)); // t0 = ptr1[i]
            instrs.push(rv_lbu(X6, X11, i as i32)); // t1 = ptr2[i]
            let bne_idx = instrs.len();
            instrs.push(0); // placeholder BNE → diff
            // continue to next byte (fall through)
            // We'll patch to jump forward to a shared diff return
            // but we need to know where diff_done is first.
            // Actually, for simplicity, push a diff sequence per pair.
            // Let's instead collect bne indices and patch later.
            let _ = bne_idx; // handled below
        }
        // equal: return 0
        let equal_idx = instrs.len();
        instrs.push(rv_addi(X10, X0, 0));
        instrs.push(rv_ret());
        // byte_diff: return t0 - t1
        let byte_diff_idx = instrs.len();
        instrs.push(rv_sub(X10, X5, X6));
        instrs.push(rv_ret());

        // Patch BNE placeholders (every third instruction starting at index 2)
        for i in 0..length {
            let bne_idx = (i * 3 + 2) as usize;
            instrs[bne_idx] =
                rv_bne(X5, X6, ((byte_diff_idx as i32) - (bne_idx as i32)) * 4);
        }
        // The equal path is reached by falling through all BNEs — it's already
        // right after the last byte pair at equal_idx, but the last iteration
        // falls through into equal_idx. Let's verify: last byte pair ends at
        // index (length-1)*3 + 2 + 1 = length*3, and equal_idx = length*3. ✓
        let _ = equal_idx;
        return instrs;
    }

    // length >= 4: alignment-dispatched word compare
    let num_words = length / 4;
    let tail = length % 4;

    let mut instrs = Vec::new();

    // Check alignment: (a0 | a1) & 3
    instrs.push(rv_or(X5, X10, X11));       // t0 = ptr1 | ptr2
    instrs.push(rv_andi(X5, X5, 3));        // t0 = (ptr1 | ptr2) & 3
    let bne_to_byte_idx = instrs.len();
    instrs.push(0); // placeholder BNE t0, x0 → byte_loop

    // ---- Word-aligned path ----
    // Compare words at known offsets. On mismatch, jump to word_diff epilogue.
    let word_diff_jump_indices: Vec<usize> = (0..num_words)
        .map(|w| {
            let off = (w * 4) as i32;
            instrs.push(rv_lw(X5, X10, off));   // t0 = *(ptr1 + off)
            instrs.push(rv_lw(X6, X11, off));   // t1 = *(ptr2 + off)
            let idx = instrs.len();
            instrs.push(0); // placeholder BNE → word_diff
            idx
        })
        .collect();

    // Tail bytes (after words)
    let tail_bne_indices: Vec<usize> = (0..tail)
        .map(|i| {
            let off = (num_words * 4 + i) as i32;
            instrs.push(rv_lbu(X5, X10, off)); // t0 = ptr1[off]
            instrs.push(rv_lbu(X6, X11, off)); // t1 = ptr2[off]
            let idx = instrs.len();
            instrs.push(0); // placeholder BNE → byte_diff
            idx
        })
        .collect();

    // Equal: return 0
    instrs.push(rv_addi(X10, X0, 0));
    instrs.push(rv_ret());

    // word_diff epilogue: t0 and t1 have differing words
    let word_diff_idx = instrs.len();
    instrs.extend(rv_generate_word_diff_epilogue());

    // byte_diff: return t0 - t1 (for tail bytes)
    let byte_diff_idx = instrs.len();
    instrs.push(rv_sub(X10, X5, X6));
    instrs.push(rv_ret());

    // ---- Byte-by-byte fallback (unaligned) ----
    // Use a loop with a2 as counter (already = length from caller).
    let byte_loop_idx = instrs.len();
    // if a2 == 0, equal
    let beq_done_idx = instrs.len();
    instrs.push(0); // placeholder BEQ a2, x0 → equal_unaligned
    instrs.push(rv_lbu(X5, X10, 0));  // t0 = *ptr1
    instrs.push(rv_lbu(X6, X11, 0));  // t1 = *ptr2
    let bne_loop_diff_idx = instrs.len();
    instrs.push(0); // placeholder BNE → byte_diff_unaligned
    instrs.push(rv_addi(X10, X10, 1));  // ptr1++
    instrs.push(rv_addi(X11, X11, 1));  // ptr2++
    instrs.push(rv_addi(X12, X12, -1)); // a2--
    let j_loop_idx = instrs.len();
    instrs.push(0); // placeholder J → byte_loop
    // equal_unaligned: return 0
    let equal_unaligned_idx = instrs.len();
    instrs.push(rv_addi(X10, X0, 0));
    instrs.push(rv_ret());
    // byte_diff_unaligned: return t0 - t1
    let byte_diff_unaligned_idx = instrs.len();
    instrs.push(rv_sub(X10, X5, X6));
    instrs.push(rv_ret());

    // ---- Patch all branch placeholders ----
    instrs[bne_to_byte_idx] =
        rv_bne(X5, X0, ((byte_loop_idx as i32) - (bne_to_byte_idx as i32)) * 4);
    for &idx in &word_diff_jump_indices {
        instrs[idx] =
            rv_bne(X5, X6, ((word_diff_idx as i32) - (idx as i32)) * 4);
    }
    for &idx in &tail_bne_indices {
        instrs[idx] =
            rv_bne(X5, X6, ((byte_diff_idx as i32) - (idx as i32)) * 4);
    }
    instrs[beq_done_idx] =
        rv_beq(X12, X0, ((equal_unaligned_idx as i32) - (beq_done_idx as i32)) * 4);
    instrs[bne_loop_diff_idx] =
        rv_bne(X5, X6, ((byte_diff_unaligned_idx as i32) - (bne_loop_diff_idx as i32)) * 4);
    instrs[j_loop_idx] =
        rv_jal(X0, ((byte_loop_idx as i32) - (j_loop_idx as i32)) * 4);

    instrs
}

/// Generate a generic optimized memcmp (loop-based, word-aligned when possible).
/// Used for call sites where the length is not a compile-time constant.
/// a0 = ptr1, a1 = ptr2, a2 = length.
/// Returns a0 = comparison result.
fn rv_generate_optimized_memcmp_loop() -> Vec<u32> {
    let mut instrs = Vec::new();

    // If length == 0, return 0
    let beq_zero_idx = instrs.len();
    instrs.push(0); // placeholder BEQ a2, x0 → return_zero

    // Check alignment: (a0 | a1) & 3
    instrs.push(rv_or(X5, X10, X11));  // t0 = ptr1 | ptr2
    instrs.push(rv_andi(X5, X5, 3));   // t0 &= 3
    let bne_unaligned_idx = instrs.len();
    instrs.push(0); // placeholder BNE t0, x0 → byte_loop

    // ---- Word-aligned path ----
    // X13 = number of full words (a2 >> 2)
    instrs.push(rv_srli(X13, X12, 2));
    // X14 = tail bytes (a2 & 3)
    instrs.push(rv_andi(X14, X12, 3));

    // word_loop:
    let word_loop_idx = instrs.len();
    let beq_word_done_idx = instrs.len();
    instrs.push(0); // placeholder BEQ X13, x0 → tail_loop
    instrs.push(rv_lw(X5, X10, 0));   // t0 = *ptr1
    instrs.push(rv_lw(X6, X11, 0));   // t1 = *ptr2
    let bne_word_diff_idx = instrs.len();
    instrs.push(0); // placeholder BNE → word_diff
    instrs.push(rv_addi(X10, X10, 4));  // ptr1 += 4
    instrs.push(rv_addi(X11, X11, 4));  // ptr2 += 4
    instrs.push(rv_addi(X13, X13, -1)); // word_count--
    let j_word_loop_idx = instrs.len();
    instrs.push(0); // placeholder J → word_loop

    // tail_loop: compare remaining X14 bytes
    let tail_loop_idx = instrs.len();
    let beq_tail_done_idx = instrs.len();
    instrs.push(0); // placeholder BEQ X14, x0 → return_zero
    instrs.push(rv_lbu(X5, X10, 0));
    instrs.push(rv_lbu(X6, X11, 0));
    let bne_tail_diff_idx = instrs.len();
    instrs.push(0); // placeholder BNE → byte_diff
    instrs.push(rv_addi(X10, X10, 1));
    instrs.push(rv_addi(X11, X11, 1));
    instrs.push(rv_addi(X14, X14, -1));
    let j_tail_loop_idx = instrs.len();
    instrs.push(0); // placeholder J → tail_loop

    // return_zero:
    let return_zero_idx = instrs.len();
    instrs.push(rv_addi(X10, X0, 0));
    instrs.push(rv_ret());

    // word_diff: extract first differing byte from t0, t1
    let word_diff_idx = instrs.len();
    instrs.extend(rv_generate_word_diff_epilogue());

    // byte_diff: return t0 - t1
    let byte_diff_idx = instrs.len();
    instrs.push(rv_sub(X10, X5, X6));
    instrs.push(rv_ret());

    // ---- Unaligned byte-by-byte fallback loop ----
    // Uses a2 as counter directly.
    let byte_loop_idx = instrs.len();
    let beq_byte_done_idx = instrs.len();
    instrs.push(0); // placeholder BEQ a2, x0 → return_zero
    instrs.push(rv_lbu(X5, X10, 0));
    instrs.push(rv_lbu(X6, X11, 0));
    let bne_byte_diff_idx = instrs.len();
    instrs.push(0); // placeholder BNE → byte_diff
    instrs.push(rv_addi(X10, X10, 1));
    instrs.push(rv_addi(X11, X11, 1));
    instrs.push(rv_addi(X12, X12, -1));
    let j_byte_loop_idx = instrs.len();
    instrs.push(0); // placeholder J → byte_loop

    // ---- Patch all branches ----
    instrs[beq_zero_idx] =
        rv_beq(X12, X0, ((return_zero_idx as i32) - (beq_zero_idx as i32)) * 4);
    instrs[bne_unaligned_idx] =
        rv_bne(X5, X0, ((byte_loop_idx as i32) - (bne_unaligned_idx as i32)) * 4);
    instrs[beq_word_done_idx] =
        rv_beq(X13, X0, ((tail_loop_idx as i32) - (beq_word_done_idx as i32)) * 4);
    instrs[bne_word_diff_idx] =
        rv_bne(X5, X6, ((word_diff_idx as i32) - (bne_word_diff_idx as i32)) * 4);
    instrs[j_word_loop_idx] =
        rv_jal(X0, ((word_loop_idx as i32) - (j_word_loop_idx as i32)) * 4);
    instrs[beq_tail_done_idx] =
        rv_beq(X14, X0, ((return_zero_idx as i32) - (beq_tail_done_idx as i32)) * 4);
    instrs[bne_tail_diff_idx] =
        rv_bne(X5, X6, ((byte_diff_idx as i32) - (bne_tail_diff_idx as i32)) * 4);
    instrs[j_tail_loop_idx] =
        rv_jal(X0, ((tail_loop_idx as i32) - (j_tail_loop_idx as i32)) * 4);
    instrs[beq_byte_done_idx] =
        rv_beq(X12, X0, ((return_zero_idx as i32) - (beq_byte_done_idx as i32)) * 4);
    instrs[bne_byte_diff_idx] =
        rv_bne(X5, X6, ((byte_diff_idx as i32) - (bne_byte_diff_idx as i32)) * 4);
    instrs[j_byte_loop_idx] =
        rv_jal(X0, ((byte_loop_idx as i32) - (j_byte_loop_idx as i32)) * 4);

    instrs
}

/// Scan ALL call sites to `target_addr`, partitioned into constant-length
/// (within MAX_MEMCPY_LENGTH) and dynamic-length groups.
/// Returns (constant_calls: BTreeMap<length, Vec<index>>, dynamic_calls: Vec<index>).
/// Debug helper: scan for all AUIPC+JALR pairs and JAL instructions that target `target_addr`,
/// printing diagnostics about what's found and what's filtered out.
fn debug_scan_calls(instructions: &[u32], pc_base: u32, target_addr: u32, label: &str) {
    let mut auipc_jalr_found = 0u32;
    let mut jal_found = 0u32;
    let mut auipc_jalr_rd_mismatch = 0u32;
    let mut auipc_jalr_not_x1 = 0u32;

    for i in 0..instructions.len().saturating_sub(1) {
        let insn0 = instructions[i];
        let insn1 = instructions[i + 1];

        // Check AUIPC+JALR pattern
        if is_auipc(insn0) && is_jalr(insn1) {
            let auipc_rd = rv_rd(insn0);
            let jalr_rs1 = rv_rs1(insn1);
            let jalr_rd = rv_rd(insn1);
            let auipc_addr = pc_base + (i as u32) * 4;
            let target = rv_auipc_jalr_target(insn0, insn1, auipc_addr);
            if target == target_addr {
                auipc_jalr_found += 1;
                if jalr_rs1 != auipc_rd {
                    auipc_jalr_rd_mismatch += 1;
                }
                if jalr_rd != X1 {
                    auipc_jalr_not_x1 += 1;
                    println!(
                        "  {label}: AUIPC+JALR at 0x{auipc_addr:x} targets 0x{target:x}, \
                         but JALR rd={jalr_rd} (not x1/ra)"
                    );
                }
            }
        }

        // Check JAL (J-type) pattern
        if rv_opcode(insn0) == 0x6F {
            let jal_addr = pc_base + (i as u32) * 4;
            let imm = rv_imm_j(insn0);
            let target = jal_addr.wrapping_add(imm as u32);
            if target == target_addr {
                jal_found += 1;
                let rd = rv_rd(insn0);
                println!(
                    "  {label}: JAL at 0x{jal_addr:x} targets 0x{target:x}, rd={rd}"
                );
            }
        }
    }

    println!(
        "  {label} call scan: AUIPC+JALR targeting 0x{target_addr:x}: {auipc_jalr_found} \
         (rd mismatch: {auipc_jalr_rd_mismatch}, not-x1: {auipc_jalr_not_x1}), \
         JAL targeting: {jal_found}"
    );
}

fn scan_all_call_sites(
    instructions: &[u32],
    pc_base: u32,
    target_addr: u32,
) -> (BTreeMap<u32, Vec<usize>>, Vec<usize>) {
    let mut by_length: BTreeMap<u32, Vec<usize>> = BTreeMap::new();
    let mut dynamic: Vec<usize> = Vec::new();

    for i in 0..instructions.len().saturating_sub(1) {
        let insn0 = instructions[i];
        let insn1 = instructions[i + 1];
        if !is_auipc(insn0) || !is_jalr(insn1) {
            continue;
        }
        let auipc_rd = rv_rd(insn0);
        if rv_rs1(insn1) != auipc_rd || rv_rd(insn1) != X1 {
            continue;
        }
        let auipc_addr = pc_base + (i as u32) * 4;
        let target = rv_auipc_jalr_target(insn0, insn1, auipc_addr);
        if target != target_addr {
            continue;
        }
        match rv_resolve_reg_constant(instructions, i, X12, Some(MAX_MEMCPY_LENGTH)) {
            Some(length) => by_length.entry(length).or_default().push(i),
            None => dynamic.push(i),
        }
    }
    (by_length, dynamic)
}

/// Encode AUIPC+JALR pair for a call from `from_addr` to `to_addr`.
/// Returns (auipc_insn, jalr_insn) using register `rd` as the intermediate.
fn rv_encode_call(from_addr: u32, to_addr: u32, rd: u32) -> (u32, u32) {
    let diff = to_addr.wrapping_sub(from_addr) as i32;
    let mut upper = diff >> 12;
    let lower = diff & 0xFFF;
    // If lower would be sign-extended as negative, adjust upper
    if lower >= 0x800 {
        upper += 1;
    }
    let auipc = rv_auipc(rd, (upper as u32) << 12);
    let jalr = rv_jalr(X1, rd, diff - (upper << 12));
    (auipc, jalr)
}

/// Scan for call sites to `target_addr` with constant a2, returning a map from length to call-site indices.
fn scan_call_sites(
    instructions: &[u32],
    pc_base: u32,
    target_addr: u32,
) -> BTreeMap<u32, Vec<usize>> {
    let mut calls_by_length: BTreeMap<u32, Vec<usize>> = BTreeMap::new();
    for i in 0..instructions.len().saturating_sub(1) {
        let insn0 = instructions[i];
        let insn1 = instructions[i + 1];
        if !is_auipc(insn0) || !is_jalr(insn1) {
            continue;
        }
        let auipc_rd = rv_rd(insn0);
        if rv_rs1(insn1) != auipc_rd || rv_rd(insn1) != X1 {
            continue;
        }
        let auipc_addr = pc_base + (i as u32) * 4;
        let target = rv_auipc_jalr_target(insn0, insn1, auipc_addr);
        if target != target_addr {
            continue;
        }
        if let Some(length) = rv_resolve_a2_constant(instructions, i) {
            if length <= MAX_MEMCPY_LENGTH {
                calls_by_length.entry(length).or_default().push(i);
            }
        }
    }
    calls_by_length
}

/// Generate specialized routines, append to ELF, and patch call sites.
fn generate_and_patch(
    elf: &mut Elf,
    pc_base: u32,
    calls_by_length: &BTreeMap<u32, Vec<usize>>,
    generate_routine: fn(u32) -> Vec<u32>,
    label: &str,
) {
    let mut routine_addrs: BTreeMap<u32, u32> = BTreeMap::new();
    let original_len = elf.instructions.len();

    for &length in calls_by_length.keys() {
        let routine = generate_routine(length);
        let routine_start_idx = elf.instructions.len();
        let routine_start_addr = pc_base + (routine_start_idx as u32) * 4;

        println!(
            "{}: length={} at addr 0x{:x} ({} instructions)",
            label,
            length,
            routine_start_addr,
            routine.len()
        );

        elf.instructions.extend(routine);
        routine_addrs.insert(length, routine_start_addr);
    }

    for (&length, auipc_indices) in calls_by_length {
        let target_addr = routine_addrs[&length];
        for &auipc_idx in auipc_indices {
            let auipc_addr = pc_base + (auipc_idx as u32) * 4;
            let rd = rv_rd(elf.instructions[auipc_idx]);
            let (new_auipc, new_jalr) = rv_encode_call(auipc_addr, target_addr, rd);

            let verify_target = rv_auipc_jalr_target(new_auipc, new_jalr, auipc_addr);
            assert_eq!(
                verify_target, target_addr,
                "AUIPC+JALR encoding mismatch at 0x{:x}: got 0x{:x}, expected 0x{:x}",
                auipc_addr, verify_target, target_addr
            );

            elf.instructions[auipc_idx] = new_auipc;
            elf.instructions[auipc_idx + 1] = new_jalr;
        }
    }

    println!(
        "{}: patched {} call sites across {} lengths, {} -> {} instructions",
        label,
        calls_by_length.values().map(|v| v.len()).sum::<usize>(),
        calls_by_length.len(),
        original_len,
        elf.instructions.len()
    );
}

/// Find a symbol address by substring match on the demangled name.
/// Returns (address, joined name string).
fn find_symbol(symbols: &SymbolTable, needle: &str) -> Option<(u32, String)> {
    symbols
        .table()
        .iter()
        .find(|(_, names): &(&u32, &Vec<String>)| {
            names
                .iter()
                .any(|n| n.contains(needle) || format!("{:#}", demangle(n)).contains(needle))
        })
        .map(|(&addr, names)| (addr, names.join(", ")))
}

/// Optimize an ELF by replacing memcpy/memmove calls with constant-length arguments
/// with specialized unrolled routines. Modifies `elf.instructions` in place.
/// `pc_base` is the byte address of the first instruction (lowest executable segment vaddr).
pub fn optimize_elf(elf: &mut Elf, symbols: &SymbolTable, pc_base: u32) {
    let elf_end = pc_base + (elf.instructions.len() as u32) * 4;
    println!(
        "memcpy_optimizer: pc_base=0x{pc_base:x}, {} instructions (range 0x{pc_base:x}..0x{elf_end:x})",
        elf.instructions.len()
    );
    // Process memcpy
    if let Some((memcpy_addr, names_str)) = find_symbol(symbols, "memcpy") {
        println!(
            "memcpy_optimizer: memcpy at addr 0x{:x} ({})",
            memcpy_addr, names_str
        );
        let calls = scan_call_sites(&elf.instructions, pc_base, memcpy_addr);
        if calls.is_empty() {
            println!("memcpy_optimizer: no eligible memcpy calls found");
        } else {
            println!(
                "memcpy_optimizer: memcpy call sites by length: {:?}",
                calls
                    .iter()
                    .map(|(len, sites)| (*len, sites.len()))
                    .collect::<Vec<_>>()
            );
            generate_and_patch(
                elf,
                pc_base,
                &calls,
                rv_generate_specialized_memcpy,
                "memcpy_optimizer",
            );
        }
    } else {
        println!("memcpy_optimizer: no memcpy symbol found");
    }

    // Process memmove
    if let Some((memmove_addr, names_str)) = find_symbol(symbols, "memmove") {
        println!(
            "memcpy_optimizer: memmove at addr 0x{:x} ({})",
            memmove_addr, names_str
        );
        let calls = scan_call_sites(&elf.instructions, pc_base, memmove_addr);
        if calls.is_empty() {
            println!("memcpy_optimizer: no eligible memmove calls found");
        } else {
            println!(
                "memcpy_optimizer: memmove call sites by length: {:?}",
                calls
                    .iter()
                    .map(|(len, sites)| (*len, sites.len()))
                    .collect::<Vec<_>>()
            );
            generate_and_patch(
                elf,
                pc_base,
                &calls,
                rv_generate_specialized_memmove,
                "memmove_optimizer",
            );
        }
    } else {
        println!("memcpy_optimizer: no memmove symbol found");
    }

    // Analyze and patch memcmp call sites
    if let Some((memcmp_addr, names_str)) = find_symbol(symbols, "memcmp") {
        println!(
            "memcpy_optimizer: memcmp at addr 0x{memcmp_addr:x} ({names_str})"
        );
        let elf_end_addr = pc_base + (elf.instructions.len() as u32) * 4;
        println!(
            "memcpy_optimizer: ELF range 0x{pc_base:x}..0x{elf_end_addr:x} ({} insns)",
            elf.instructions.len()
        );
        debug_scan_calls(&elf.instructions, pc_base, memcmp_addr, "memcmp");
        analyze_a2_constants(&elf.instructions, pc_base, memcmp_addr, "memcmp");

        let (const_calls, dyn_calls) =
            scan_all_call_sites(&elf.instructions, pc_base, memcmp_addr);

        // Patch constant-length call sites with specialized unrolled memcmp
        if !const_calls.is_empty() {
            println!(
                "memcpy_optimizer: memcmp constant-length call sites: {:?}",
                const_calls
                    .iter()
                    .map(|(len, sites)| (*len, sites.len()))
                    .collect::<Vec<_>>()
            );
            generate_and_patch(
                elf,
                pc_base,
                &const_calls,
                rv_generate_specialized_memcmp,
                "memcmp_optimizer",
            );
        }

        // Patch dynamic-length call sites with generic loop-based word memcmp
        if !dyn_calls.is_empty() {
            let routine = rv_generate_optimized_memcmp_loop();
            let routine_addr = pc_base + (elf.instructions.len() as u32) * 4;
            println!(
                "memcmp_optimizer: generic loop at 0x{routine_addr:x} ({} insns), \
                 patching {} dynamic call sites",
                routine.len(),
                dyn_calls.len(),
            );
            elf.instructions.extend(routine);

            for &auipc_idx in &dyn_calls {
                let auipc_addr = pc_base + (auipc_idx as u32) * 4;
                let rd = rv_rd(elf.instructions[auipc_idx]);
                let (new_auipc, new_jalr) = rv_encode_call(auipc_addr, routine_addr, rd);

                let verify = rv_auipc_jalr_target(new_auipc, new_jalr, auipc_addr);
                assert_eq!(
                    verify, routine_addr,
                    "AUIPC+JALR encoding mismatch at 0x{auipc_addr:x}"
                );

                elf.instructions[auipc_idx] = new_auipc;
                elf.instructions[auipc_idx + 1] = new_jalr;
            }
        }

        if const_calls.is_empty() && dyn_calls.is_empty() {
            println!("memcpy_optimizer: no memcmp call sites found");
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Minimal RISC-V interpreter for raw u32 instructions.
    struct RvInterpreter {
        regs: [u32; 32],
        mem: Vec<u8>,
        pc: u32, // byte address
    }

    impl RvInterpreter {
        fn new(mem_size: usize) -> Self {
            Self {
                regs: [0; 32],
                mem: vec![0; mem_size],
                pc: 0,
            }
        }

        fn reg(&self, r: u32) -> u32 {
            if r == 0 {
                0
            } else {
                self.regs[r as usize]
            }
        }

        fn set_reg(&mut self, r: u32, val: u32) {
            if r != 0 {
                self.regs[r as usize] = val;
            }
        }

        fn run(&mut self, instrs: &[u32], max_steps: usize) {
            let trace = std::env::var("MEMCPY_TRACE").is_ok();
            for step in 0..max_steps {
                let idx = (self.pc / 4) as usize;
                if idx >= instrs.len() {
                    panic!(
                        "PC out of bounds: pc=0x{:x}, idx={}, len={}",
                        self.pc,
                        idx,
                        instrs.len()
                    );
                }
                let insn = instrs[idx];
                let opcode = rv_opcode(insn);
                let rd = rv_rd(insn);
                let rs1 = rv_rs1(insn);
                let funct3 = rv_funct3(insn);

                if trace {
                    eprintln!(
                        "  step {:4}: pc=0x{:04x} insn=0x{:08x}",
                        step, self.pc, insn
                    );
                }

                match opcode {
                    0x13 => {
                        // I-type ALU: ADDI, ANDI, ORI, SRLI, etc.
                        let imm = rv_imm_i(insn);
                        let rs1_val = self.reg(rs1);
                        let result = match funct3 {
                            0b000 => rs1_val.wrapping_add(imm as u32), // ADDI
                            0b101 => rs1_val >> ((imm as u32) & 0x1F), // SRLI (funct7=0)
                            0b111 => rs1_val & (imm as u32),           // ANDI
                            0b110 => rs1_val | (imm as u32),           // ORI
                            _ => panic!("Unknown I-type ALU funct3: {funct3}"),
                        };
                        self.set_reg(rd, result);
                        self.pc += 4;
                    }
                    0x33 => {
                        // R-type ALU
                        let rs2 = (insn >> 20) & 0x1F;
                        let funct7 = insn >> 25;
                        let rs1_val = self.reg(rs1);
                        let rs2_val = self.reg(rs2);
                        let result = match (funct3, funct7) {
                            (0b000, 0b0000000) => rs1_val.wrapping_add(rs2_val), // ADD
                            (0b000, 0b0100000) => rs1_val.wrapping_sub(rs2_val), // SUB
                            (0b110, _) => rs1_val | rs2_val,                     // OR
                            (0b111, _) => rs1_val & rs2_val,                     // AND
                            _ => panic!("Unknown R-type funct3={funct3} funct7={funct7}"),
                        };
                        self.set_reg(rd, result);
                        self.pc += 4;
                    }
                    0x03 => {
                        // Loads
                        let imm = rv_imm_i(insn);
                        let addr = self.reg(rs1).wrapping_add(imm as u32);
                        let val = match funct3 {
                            0b010 => {
                                // LW
                                assert!(addr.is_multiple_of(4), "Unaligned LW at 0x{:x}", addr);
                                let a = addr as usize;
                                u32::from_le_bytes([
                                    self.mem[a],
                                    self.mem[a + 1],
                                    self.mem[a + 2],
                                    self.mem[a + 3],
                                ])
                            }
                            0b100 => {
                                // LBU
                                self.mem[addr as usize] as u32
                            }
                            _ => panic!("Unknown load funct3: {}", funct3),
                        };
                        self.set_reg(rd, val);
                        self.pc += 4;
                    }
                    0x23 => {
                        // Stores (S-type)
                        let rs2 = (insn >> 20) & 0x1F;
                        let imm_4_0 = (insn >> 7) & 0x1F;
                        let imm_11_5 = (insn >> 25) & 0x7F;
                        let imm = ((imm_11_5 << 5) | imm_4_0) as i32;
                        // Sign-extend from 12 bits
                        let imm = if imm & 0x800 != 0 { imm | !0xFFF } else { imm };
                        let addr = self.reg(rs1).wrapping_add(imm as u32);
                        let val = self.reg(rs2);
                        match funct3 {
                            0b010 => {
                                // SW
                                assert!(addr.is_multiple_of(4), "Unaligned SW at 0x{:x}", addr);
                                let a = addr as usize;
                                self.mem[a..a + 4].copy_from_slice(&val.to_le_bytes());
                            }
                            0b000 => {
                                // SB
                                self.mem[addr as usize] = (val & 0xFF) as u8;
                            }
                            _ => panic!("Unknown store funct3: {}", funct3),
                        }
                        self.pc += 4;
                    }
                    0x63 => {
                        // B-type branches
                        let rs2 = (insn >> 20) & 0x1F;
                        let rs1_val = self.reg(rs1);
                        let rs2_val = self.reg(rs2);
                        let take = match funct3 {
                            0b000 => rs1_val == rs2_val, // BEQ
                            0b001 => rs1_val != rs2_val, // BNE
                            0b110 => rs1_val < rs2_val,  // BLTU
                            0b111 => rs1_val >= rs2_val, // BGEU
                            _ => panic!("Unknown branch funct3: {}", funct3),
                        };
                        if take {
                            // Decode B-type immediate
                            let bit_11 = (insn >> 7) & 1;
                            let bits_4_1 = (insn >> 8) & 0xF;
                            let bits_10_5 = (insn >> 25) & 0x3F;
                            let bit_12 = (insn >> 31) & 1;
                            let offset = (bit_12 << 12)
                                | (bit_11 << 11)
                                | (bits_10_5 << 5)
                                | (bits_4_1 << 1);
                            // Sign-extend from 13 bits
                            let offset = if bit_12 != 0 {
                                offset | !0x1FFF
                            } else {
                                offset
                            };
                            self.pc = self.pc.wrapping_add(offset);
                        } else {
                            self.pc += 4;
                        }
                    }
                    0x67 => {
                        // JALR — return when rd=x0
                        if rd == 0 {
                            return;
                        }
                        let imm = rv_imm_i(insn);
                        let target = (self.reg(rs1).wrapping_add(imm as u32)) & !1;
                        self.set_reg(rd, self.pc + 4);
                        self.pc = target;
                    }
                    0x6F => {
                        // JAL rd, offset (J-type)
                        let bits_19_12 = (insn >> 12) & 0xFF;
                        let bit_11 = (insn >> 20) & 1;
                        let bits_10_1 = (insn >> 21) & 0x3FF;
                        let bit_20 = (insn >> 31) & 1;
                        let offset = (bit_20 << 20)
                            | (bits_19_12 << 12)
                            | (bit_11 << 11)
                            | (bits_10_1 << 1);
                        // Sign-extend from 21 bits
                        let offset = if bit_20 != 0 {
                            offset | !0x1FFFFF
                        } else {
                            offset
                        };
                        self.set_reg(rd, self.pc + 4);
                        self.pc = self.pc.wrapping_add(offset);
                    }
                    _ => panic!("Unknown opcode: 0x{:02x} at pc=0x{:x}", opcode, self.pc),
                }
            }
            panic!("Max steps exceeded ({})", max_steps);
        }
    }

    #[test]
    fn test_specialized_memcpy_all_alignments() {
        let lengths: Vec<u32> = vec![0, 1, 2, 3, 4, 5, 7, 8, 12, 15, 16, 31, 32, 48, 63, 64, 128];

        for &length in &lengths {
            eprintln!("=== Generating routine for length={} ===", length);
            let routine = rv_generate_specialized_memcpy(length);
            eprintln!("  routine has {} instructions", routine.len());

            for src_align in 0..4u32 {
                for dst_align in 0..4u32 {
                    eprintln!(
                        "  Testing length={}, src_align={}, dst_align={}",
                        length, src_align, dst_align
                    );
                    let src_base = 256 + src_align;
                    let dst_base = 768 + dst_align;

                    let mut interp = RvInterpreter::new(1024);
                    interp.mem.fill(0xAA);

                    for i in 0..length {
                        interp.mem[(src_base + i) as usize] =
                            ((i.wrapping_mul(7).wrapping_add(13 + length)) & 0x7F) as u8;
                    }

                    interp.set_reg(X10, dst_base); // a0 = dst
                    interp.set_reg(X11, src_base); // a1 = src

                    interp.run(&routine, 10_000);

                    // Verify copy correctness
                    for i in 0..length {
                        let expected = interp.mem[(src_base + i) as usize];
                        let actual = interp.mem[(dst_base + i) as usize];
                        assert_eq!(
                            actual, expected,
                            "Mismatch at byte {} for length={}, src_align={}, dst_align={}",
                            i, length, src_align, dst_align
                        );
                    }

                    // Verify no stray writes
                    let guard = 16;
                    let check_start = dst_base.saturating_sub(guard);
                    let check_end = (dst_base + length + guard).min(1024);
                    for addr in check_start..check_end {
                        if addr >= dst_base && addr < dst_base + length {
                            continue;
                        }
                        assert_eq!(
                            interp.mem[addr as usize], 0xAA,
                            "Stray write at addr {} for length={}, src_align={}, dst_align={}",
                            addr, length, src_align, dst_align
                        );
                    }
                }
            }
        }
    }

    #[test]
    fn test_rv_encoding_roundtrip() {
        // Verify AUIPC+JALR encoding produces correct target
        let from = 0x10000u32;
        let targets = [0x20000, 0x10100, 0x10800, 0x0FF00, 0x30ABC, 0x10004];
        for &to in &targets {
            let (auipc, jalr) = rv_encode_call(from, to, 5);
            let decoded = rv_auipc_jalr_target(auipc, jalr, from);
            assert_eq!(
                decoded, to,
                "Encoding roundtrip failed: from=0x{:x}, to=0x{:x}, got=0x{:x}",
                from, to, decoded
            );
        }
    }

    #[test]
    fn test_specialized_memmove_non_overlapping() {
        // Test memmove with non-overlapping regions (should behave like memcpy)
        let lengths: Vec<u32> = vec![0, 1, 2, 3, 4, 5, 7, 8, 12, 15, 16, 31, 32, 48, 63, 64, 128];

        for &length in &lengths {
            let routine = rv_generate_specialized_memmove(length);

            for src_align in 0..4u32 {
                for dst_align in 0..4u32 {
                    // dst < src (forward path)
                    {
                        let src_base = 768 + src_align;
                        let dst_base = 256 + dst_align;

                        let mut interp = RvInterpreter::new(1024);
                        interp.mem.fill(0xAA);

                        for i in 0..length {
                            interp.mem[(src_base + i) as usize] =
                                ((i.wrapping_mul(7).wrapping_add(13 + length)) & 0x7F) as u8;
                        }

                        interp.set_reg(X10, dst_base);
                        interp.set_reg(X11, src_base);
                        interp.run(&routine, 10_000);

                        for i in 0..length {
                            let expected = interp.mem[(src_base + i) as usize];
                            let actual = interp.mem[(dst_base + i) as usize];
                            assert_eq!(
                                actual, expected,
                                "Forward mismatch at byte {} for length={}, src_align={}, dst_align={}",
                                i, length, src_align, dst_align
                            );
                        }
                    }

                    // dst > src (backward path)
                    {
                        let src_base = 256 + src_align;
                        let dst_base = 768 + dst_align;

                        let mut interp = RvInterpreter::new(1024);
                        interp.mem.fill(0xAA);

                        for i in 0..length {
                            interp.mem[(src_base + i) as usize] =
                                ((i.wrapping_mul(7).wrapping_add(13 + length)) & 0x7F) as u8;
                        }

                        interp.set_reg(X10, dst_base);
                        interp.set_reg(X11, src_base);
                        interp.run(&routine, 10_000);

                        for i in 0..length {
                            let expected = interp.mem[(src_base + i) as usize];
                            let actual = interp.mem[(dst_base + i) as usize];
                            assert_eq!(
                                actual, expected,
                                "Backward mismatch at byte {} for length={}, src_align={}, dst_align={}",
                                i, length, src_align, dst_align
                            );
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_specialized_memmove_overlapping() {
        // Test memmove with overlapping regions where dst > src
        let lengths: Vec<u32> = vec![4, 5, 7, 8, 12, 15, 16, 31, 32, 48, 63, 64, 128];

        for &length in &lengths {
            let routine = rv_generate_specialized_memmove(length);

            // Test various overlap amounts
            for overlap in [1, 2, 3, 4, length / 2, length - 1] {
                if overlap == 0 || overlap >= length {
                    continue;
                }
                let src_base = 256u32;
                let dst_base = src_base + (length - overlap);

                // Prepare source data
                let src_data: Vec<u8> = (0..length)
                    .map(|i| ((i.wrapping_mul(7).wrapping_add(13 + length)) & 0xFF) as u8)
                    .collect();

                let mut interp = RvInterpreter::new(1024);
                interp.mem.fill(0xAA);
                for i in 0..length {
                    interp.mem[(src_base + i) as usize] = src_data[i as usize];
                }

                interp.set_reg(X10, dst_base);
                interp.set_reg(X11, src_base);
                interp.run(&routine, 10_000);

                // Verify: dst should contain the original src data
                for i in 0..length {
                    let expected = src_data[i as usize];
                    let actual = interp.mem[(dst_base + i) as usize];
                    assert_eq!(
                        actual, expected,
                        "Overlap mismatch at byte {} for length={}, overlap={}",
                        i, length, overlap
                    );
                }
            }
        }
    }

    /// Reference memcmp implementation for testing.
    fn ref_memcmp(a: &[u8], b: &[u8]) -> i32 {
        for (x, y) in a.iter().zip(b.iter()) {
            if x != y {
                return (*x as i32) - (*y as i32);
            }
        }
        0
    }

    #[test]
    fn test_specialized_memcmp_all_alignments() {
        let lengths: Vec<u32> = vec![0, 1, 2, 3, 4, 5, 7, 8, 12, 15, 16, 31, 32, 48, 63, 64, 128];

        for &length in &lengths {
            eprintln!("=== Generating memcmp routine for length={length} ===");
            let routine = rv_generate_specialized_memcmp(length);
            eprintln!("  routine has {} instructions", routine.len());

            for src_align in 0..4u32 {
                for dst_align in 0..4u32 {
                    // Test 1: equal buffers
                    {
                        let ptr1 = 256 + src_align;
                        let ptr2 = 768 + dst_align;
                        let mut interp = RvInterpreter::new(1024);
                        for i in 0..length {
                            let v = ((i.wrapping_mul(7).wrapping_add(13)) & 0xFF) as u8;
                            interp.mem[(ptr1 + i) as usize] = v;
                            interp.mem[(ptr2 + i) as usize] = v;
                        }
                        interp.set_reg(X10, ptr1);
                        interp.set_reg(X11, ptr2);
                        interp.set_reg(X12, length);
                        interp.run(&routine, 50_000);
                        let result = interp.reg(X10) as i32;
                        assert_eq!(
                            result, 0,
                            "Expected equal (0) for length={length}, align=({src_align},{dst_align})"
                        );
                    }

                    // Test 2: first buffer less than second (difference at various positions)
                    if length > 0 {
                        for diff_pos in [0, length / 2, length - 1] {
                            let ptr1 = 256 + src_align;
                            let ptr2 = 768 + dst_align;
                            let mut interp = RvInterpreter::new(1024);
                            for i in 0..length {
                                let v = 0x42u8;
                                interp.mem[(ptr1 + i) as usize] = v;
                                interp.mem[(ptr2 + i) as usize] = v;
                            }
                            interp.mem[(ptr1 + diff_pos) as usize] = 0x10;
                            interp.mem[(ptr2 + diff_pos) as usize] = 0x50;
                            let a_buf: Vec<u8> = (0..length)
                                .map(|i| interp.mem[(ptr1 + i) as usize])
                                .collect();
                            let b_buf: Vec<u8> = (0..length)
                                .map(|i| interp.mem[(ptr2 + i) as usize])
                                .collect();
                            let expected = ref_memcmp(&a_buf, &b_buf);

                            interp.set_reg(X10, ptr1);
                            interp.set_reg(X11, ptr2);
                            interp.set_reg(X12, length);
                            interp.run(&routine, 50_000);
                            let result = interp.reg(X10) as i32;
                            assert!(
                                (result < 0) == (expected < 0) && (result > 0) == (expected > 0),
                                "Sign mismatch for length={length}, align=({src_align},{dst_align}), \
                                 diff_pos={diff_pos}: got {result}, expected {expected}"
                            );
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_optimized_memcmp_loop() {
        let routine = rv_generate_optimized_memcmp_loop();
        eprintln!("Generic memcmp loop: {} instructions", routine.len());

        let lengths: Vec<u32> = vec![0, 1, 2, 3, 4, 5, 7, 8, 12, 15, 16, 31, 32, 48, 63, 64, 128];

        for &length in &lengths {
            for src_align in 0..4u32 {
                for dst_align in 0..4u32 {
                    // Equal
                    {
                        let ptr1 = 256 + src_align;
                        let ptr2 = 768 + dst_align;
                        let mut interp = RvInterpreter::new(1024);
                        for i in 0..length {
                            let v = ((i.wrapping_mul(11).wrapping_add(7)) & 0xFF) as u8;
                            interp.mem[(ptr1 + i) as usize] = v;
                            interp.mem[(ptr2 + i) as usize] = v;
                        }
                        interp.set_reg(X10, ptr1);
                        interp.set_reg(X11, ptr2);
                        interp.set_reg(X12, length);
                        interp.run(&routine, 50_000);
                        assert_eq!(
                            interp.reg(X10) as i32,
                            0,
                            "Expected equal for loop length={length}, align=({src_align},{dst_align})"
                        );
                    }

                    // Difference at middle
                    if length > 0 {
                        let diff_pos = length / 2;
                        let ptr1 = 256 + src_align;
                        let ptr2 = 768 + dst_align;
                        let mut interp = RvInterpreter::new(1024);
                        for i in 0..length {
                            interp.mem[(ptr1 + i) as usize] = 0x42;
                            interp.mem[(ptr2 + i) as usize] = 0x42;
                        }
                        interp.mem[(ptr1 + diff_pos) as usize] = 0x80;
                        interp.mem[(ptr2 + diff_pos) as usize] = 0x20;

                        interp.set_reg(X10, ptr1);
                        interp.set_reg(X11, ptr2);
                        interp.set_reg(X12, length);
                        interp.run(&routine, 50_000);
                        let result = interp.reg(X10) as i32;
                        assert!(
                            result > 0,
                            "Expected positive for loop length={length}, \
                             align=({src_align},{dst_align}): got {result}"
                        );
                    }
                }
            }
        }
    }
}
