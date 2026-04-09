use std::collections::BTreeMap;
use std::path::Path;

use openvm_transpiler::elf::Elf;
use powdr_riscv_elf::debug_info::SymbolTable;

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
const X10: u32 = 10; // a0 (dst)
const X11: u32 = 11; // a1 (src)
const X12: u32 = 12; // a2 (temp)
const X13: u32 = 13; // a3 (temp)
const X14: u32 = 14; // a4 (temp)

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

/// Try to resolve the constant value loaded into register a2 (x12) by scanning
/// backwards from `end_idx` (exclusive) in the instruction stream.
/// Handles ADDI x12, x0, imm pattern. Scans at most 16 instructions back.
fn rv_resolve_a2_constant(instructions: &[u32], end_idx: usize) -> Option<u32> {
    let start = end_idx.saturating_sub(16);
    for i in (start..end_idx).rev() {
        let insn = instructions[i];
        let rd = rv_rd(insn);
        if rd != X12 {
            continue;
        }
        if is_addi(insn) && rv_rs1(insn) == X0 {
            let imm = rv_imm_i(insn);
            if imm >= 0 && imm <= MAX_MEMCPY_LENGTH as i32 {
                return Some(imm as u32);
            }
            return None;
        }
        // Any other write to x12 — give up
        return None;
    }
    None
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

    let mut instrs = Vec::new();

    // Alignment check
    instrs.push(rv_andi(X12, X11, 3)); // src & 3
    instrs.push(rv_andi(X14, X10, 3)); // dst & 3
    instrs.push(rv_or(X13, X12, X14)); // either misaligned?
    let bne_not_both_idx = instrs.len();
    instrs.push(0); // placeholder BNE

    // Path 1: Both aligned
    instrs.extend(rv_generate_head_word_tail(0, length));
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

    // Path 4: off by 3 → 1 head byte
    instrs.extend(rv_generate_head_word_tail(1, length));
    instrs.push(rv_ret());

    // Path 3: off by 2 → 2 head bytes
    let off2_idx = instrs.len();
    instrs.extend(rv_generate_head_word_tail(2, length));
    instrs.push(rv_ret());

    // Path 2: off by 1 → 3 head bytes
    let off1_idx = instrs.len();
    instrs.extend(rv_generate_head_word_tail(3, length));
    instrs.push(rv_ret());

    // Path 5: byte-by-byte
    let byte_path_idx = instrs.len();
    for i in 0..length {
        instrs.push(rv_lbu(X13, X11, i as i32));
        instrs.push(rv_sb(X13, X10, i as i32));
    }
    instrs.push(rv_ret());

    // Patch branch offsets (in bytes: offset = (target_idx - branch_idx) * 4)
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

/// Optimize an ELF by replacing memcpy calls with constant-length arguments
/// with specialized unrolled routines. Modifies `elf.instructions` in place.
/// `pc_base` is the byte address of the first instruction (lowest executable segment vaddr).
pub fn optimize_elf(elf: &mut Elf, symbols: &SymbolTable, pc_base: u32) {
    // Find memcpy function address
    let memcpy_entry = symbols
        .table()
        .iter()
        .find(|(_, names): &(&u32, &Vec<String>)| names.iter().any(|n| n.contains("memcpy")));

    let Some((&memcpy_addr, names)) = memcpy_entry else {
        println!("memcpy_optimizer: no memcpy symbol found");
        return;
    };
    let names_str = names.join(", ");
    println!(
        "memcpy_optimizer: memcpy at addr 0x{:x} ({})",
        memcpy_addr, names_str
    );

    let instructions = &elf.instructions;

    // Scan for AUIPC+JALR pairs targeting memcpy with constant a2
    let mut calls_by_length: BTreeMap<u32, Vec<usize>> = BTreeMap::new(); // length -> [auipc_index]

    for i in 0..instructions.len().saturating_sub(1) {
        let insn0 = instructions[i];
        let insn1 = instructions[i + 1];
        if !is_auipc(insn0) || !is_jalr(insn1) {
            continue;
        }
        // Check JALR uses AUIPC's rd and writes to ra (x1)
        let auipc_rd = rv_rd(insn0);
        if rv_rs1(insn1) != auipc_rd || rv_rd(insn1) != X1 {
            continue;
        }
        let auipc_addr = pc_base + (i as u32) * 4;
        let target = rv_auipc_jalr_target(insn0, insn1, auipc_addr);
        if target != memcpy_addr {
            continue;
        }
        // Found a call to memcpy — check for constant a2
        if let Some(length) = rv_resolve_a2_constant(instructions, i) {
            if length <= MAX_MEMCPY_LENGTH {
                calls_by_length.entry(length).or_default().push(i);
            }
        }
    }

    if calls_by_length.is_empty() {
        println!("memcpy_optimizer: no eligible memcpy calls found");
        return;
    }

    println!(
        "memcpy_optimizer: call sites by length: {:?}",
        calls_by_length
            .iter()
            .map(|(len, sites)| (*len, sites.len()))
            .collect::<Vec<_>>()
    );

    // Generate specialized routines and append to instructions
    let mut routine_addrs: BTreeMap<u32, u32> = BTreeMap::new(); // length -> start addr
    let original_len = elf.instructions.len();

    for &length in calls_by_length.keys() {
        let routine = rv_generate_specialized_memcpy(length);
        let routine_start_idx = elf.instructions.len();
        let routine_start_addr = pc_base + (routine_start_idx as u32) * 4;

        println!(
            "memcpy_optimizer: length={} at addr 0x{:x} ({} instructions)",
            length,
            routine_start_addr,
            routine.len()
        );

        elf.instructions.extend(routine);
        routine_addrs.insert(length, routine_start_addr);
    }

    // Patch call sites
    for (&length, auipc_indices) in &calls_by_length {
        // if length == 32 || length == 64 {
        //     continue;
        // }
        let target_addr = routine_addrs[&length];
        for &auipc_idx in auipc_indices {
            let auipc_addr = pc_base + (auipc_idx as u32) * 4;
            let rd = rv_rd(elf.instructions[auipc_idx]);
            let (new_auipc, new_jalr) = rv_encode_call(auipc_addr, target_addr, rd);

            // Verify before patching
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
        "memcpy_optimizer: patched {} call sites across {} lengths, {} -> {} instructions",
        calls_by_length.values().map(|v| v.len()).sum::<usize>(),
        calls_by_length.len(),
        original_len,
        elf.instructions.len()
    );
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
                        // I-type ALU: ADDI, ANDI, ORI, etc.
                        let imm = rv_imm_i(insn);
                        let rs1_val = self.reg(rs1);
                        let result = match funct3 {
                            0b000 => rs1_val.wrapping_add(imm as u32), // ADDI
                            0b111 => rs1_val & (imm as u32),           // ANDI
                            0b110 => rs1_val | (imm as u32),           // ORI
                            _ => panic!("Unknown I-type ALU funct3: {}", funct3),
                        };
                        self.set_reg(rd, result);
                        self.pc += 4;
                    }
                    0x33 => {
                        // R-type ALU
                        let rs2 = (insn >> 20) & 0x1F;
                        let rs1_val = self.reg(rs1);
                        let rs2_val = self.reg(rs2);
                        let result = match funct3 {
                            0b000 => rs1_val.wrapping_add(rs2_val), // ADD
                            0b110 => rs1_val | rs2_val,             // OR
                            0b111 => rs1_val & rs2_val,             // AND
                            _ => panic!("Unknown R-type funct3: {}", funct3),
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
}
