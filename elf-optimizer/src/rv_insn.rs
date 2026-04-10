//! RISC-V instruction encoding, decoding, and predicate helpers.

// RISC-V register numbers
pub(crate) const X0: u32 = 0;
pub(crate) const X1: u32 = 1; // ra
pub(crate) const X5: u32 = 5; // t0
pub(crate) const X6: u32 = 6; // t1
pub(crate) const X7: u32 = 7; // t2
pub(crate) const X10: u32 = 10; // a0 (dst / ptr1)
pub(crate) const X11: u32 = 11; // a1 (src / ptr2)
pub(crate) const X12: u32 = 12; // a2 (length)
pub(crate) const X13: u32 = 13; // a3 (temp)
pub(crate) const X14: u32 = 14; // a4 (temp)
pub(crate) const X28: u32 = 28; // t3
#[allow(dead_code)]
pub(crate) const X29: u32 = 29; // t4

// ---- Encoding helpers ----

/// Encode ADDI rd, rs1, imm (I-type)
pub(crate) fn rv_addi(rd: u32, rs1: u32, imm: i32) -> u32 {
    let imm = (imm as u32) & 0xFFF;
    ((imm << 20) | (rs1 << 15)) | (rd << 7) | 0x13
}

/// Encode ANDI rd, rs1, imm (I-type)
pub(crate) fn rv_andi(rd: u32, rs1: u32, imm: i32) -> u32 {
    let imm = (imm as u32) & 0xFFF;
    (imm << 20) | (rs1 << 15) | (0b111 << 12) | (rd << 7) | 0x13
}

/// Encode OR rd, rs1, rs2 (R-type)
pub(crate) fn rv_or(rd: u32, rs1: u32, rs2: u32) -> u32 {
    (rs2 << 20) | (rs1 << 15) | (0b110 << 12) | (rd << 7) | 0x33
}

/// Encode SUB rd, rs1, rs2 (R-type)
pub(crate) fn rv_sub(rd: u32, rs1: u32, rs2: u32) -> u32 {
    (0b0100000 << 25) | (rs2 << 20) | (rs1 << 15) | (rd << 7) | 0x33
}

/// Encode SRLI rd, rs1, shamt (I-type shift)
pub(crate) fn rv_srli(rd: u32, rs1: u32, shamt: u32) -> u32 {
    ((shamt & 0x1F) << 20) | (rs1 << 15) | (0b101 << 12) | (rd << 7) | 0x13
}

/// Encode JAL rd, offset (J-type, offset in bytes)
pub(crate) fn rv_jal(rd: u32, offset: i32) -> u32 {
    let imm = offset as u32;
    let bit_20 = (imm >> 20) & 1;
    let bits_10_1 = (imm >> 1) & 0x3FF;
    let bit_11 = (imm >> 11) & 1;
    let bits_19_12 = (imm >> 12) & 0xFF;
    (bit_20 << 31) | (bits_10_1 << 21) | (bit_11 << 20) | (bits_19_12 << 12) | (rd << 7) | 0x6F
}

/// Encode LW rd, imm(rs1) (I-type)
pub(crate) fn rv_lw(rd: u32, rs1: u32, imm: i32) -> u32 {
    let imm = (imm as u32) & 0xFFF;
    (imm << 20) | (rs1 << 15) | (0b010 << 12) | (rd << 7) | 0x03
}

/// Encode LBU rd, imm(rs1) (I-type)
pub(crate) fn rv_lbu(rd: u32, rs1: u32, imm: i32) -> u32 {
    let imm = (imm as u32) & 0xFFF;
    (imm << 20) | (rs1 << 15) | (0b100 << 12) | (rd << 7) | 0x03
}

/// Encode SW rs2, imm(rs1) (S-type)
pub(crate) fn rv_sw(rs2: u32, rs1: u32, imm: i32) -> u32 {
    let imm = (imm as u32) & 0xFFF;
    let imm_11_5 = (imm >> 5) & 0x7F;
    let imm_4_0 = imm & 0x1F;
    (imm_11_5 << 25) | (rs2 << 20) | (rs1 << 15) | (0b010 << 12) | (imm_4_0 << 7) | 0x23
}

/// Encode SB rs2, imm(rs1) (S-type)
pub(crate) fn rv_sb(rs2: u32, rs1: u32, imm: i32) -> u32 {
    let imm = (imm as u32) & 0xFFF;
    let imm_11_5 = (imm >> 5) & 0x7F;
    let imm_4_0 = imm & 0x1F;
    ((imm_11_5 << 25) | (rs2 << 20) | (rs1 << 15)) | (imm_4_0 << 7) | 0x23
}

/// Encode BEQ rs1, rs2, offset (B-type, offset in bytes)
pub(crate) fn rv_beq(rs1: u32, rs2: u32, offset: i32) -> u32 {
    rv_branch(0b000, rs1, rs2, offset)
}

/// Encode BNE rs1, rs2, offset (B-type, offset in bytes)
pub(crate) fn rv_bne(rs1: u32, rs2: u32, offset: i32) -> u32 {
    rv_branch(0b001, rs1, rs2, offset)
}

/// Encode BLTU rs1, rs2, offset (B-type, offset in bytes, unsigned less-than)
pub(crate) fn rv_bltu(rs1: u32, rs2: u32, offset: i32) -> u32 {
    rv_branch(0b110, rs1, rs2, offset)
}

pub(crate) fn rv_branch(funct3: u32, rs1: u32, rs2: u32, offset: i32) -> u32 {
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
pub(crate) fn rv_jalr(rd: u32, rs1: u32, imm: i32) -> u32 {
    let imm = (imm as u32) & 0xFFF;
    ((imm << 20) | (rs1 << 15)) | (rd << 7) | 0x67
}

/// Encode JALR x0, x1, 0 (return)
pub(crate) fn rv_ret() -> u32 {
    rv_jalr(X0, X1, 0)
}

/// Encode AUIPC rd, imm (U-type, imm is the upper 20 bits already shifted)
pub(crate) fn rv_auipc(rd: u32, imm: u32) -> u32 {
    (imm & 0xFFFFF000) | (rd << 7) | 0x17
}

// ---- Decoding helpers ----

/// Extract opcode field (bits [6:0]) from a raw RISC-V instruction
pub(crate) fn rv_opcode(insn: u32) -> u32 {
    insn & 0x7F
}

/// Extract rd field (bits [11:7])
pub(crate) fn rv_rd(insn: u32) -> u32 {
    (insn >> 7) & 0x1F
}

/// Extract rs1 field (bits [19:15])
pub(crate) fn rv_rs1(insn: u32) -> u32 {
    (insn >> 15) & 0x1F
}

/// Extract funct3 field (bits [14:12])
pub(crate) fn rv_funct3(insn: u32) -> u32 {
    (insn >> 12) & 0x7
}

/// Decode I-type immediate (sign-extended, bits [31:20])
pub(crate) fn rv_imm_i(insn: u32) -> i32 {
    (insn as i32) >> 20
}

/// Decode U-type immediate (bits [31:12], already shifted)
pub(crate) fn rv_imm_u(insn: u32) -> u32 {
    insn & 0xFFFFF000
}

/// Decode J-type immediate (JAL): bits [31|30:21|20|19:12] → sign-extended 21-bit offset
pub(crate) fn rv_imm_j(insn: u32) -> i32 {
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
pub(crate) fn is_auipc(insn: u32) -> bool {
    rv_opcode(insn) == 0x17
}

/// Check if instruction is JALR (opcode 0x67, funct3 0x0)
pub(crate) fn is_jalr(insn: u32) -> bool {
    rv_opcode(insn) == 0x67 && rv_funct3(insn) == 0
}

/// Check if instruction is ADDI (opcode 0x13, funct3 0x0)
pub(crate) fn is_addi(insn: u32) -> bool {
    rv_opcode(insn) == 0x13 && rv_funct3(insn) == 0
}

/// Reconstruct call target from AUIPC+JALR pair at raw RISC-V level.
/// `auipc_addr` is the byte address of the AUIPC instruction.
pub(crate) fn rv_auipc_jalr_target(auipc_insn: u32, jalr_insn: u32, auipc_addr: u32) -> u32 {
    let upper = rv_imm_u(auipc_insn); // already has lower 12 bits zeroed
    let lower = rv_imm_i(jalr_insn); // sign-extended 12-bit immediate
    auipc_addr.wrapping_add(upper).wrapping_add(lower as u32)
}

/// Decode S-type immediate (sign-extended 12-bit, for SB/SH/SW)
pub(crate) fn rv_imm_s(insn: u32) -> i32 {
    let imm_4_0 = (insn >> 7) & 0x1F;
    let imm_11_5 = (insn >> 25) & 0x7F;
    let raw = (imm_11_5 << 5) | imm_4_0;
    if raw & 0x800 != 0 {
        (raw | 0xFFFFF000) as i32
    } else {
        raw as i32
    }
}

/// Extract rs2 field (bits [24:20])
pub(crate) fn rv_rs2(insn: u32) -> u32 {
    (insn >> 20) & 0x1F
}

/// Check if instruction is LBU (opcode 0x03, funct3 = 4)
pub(crate) fn is_lbu(insn: u32) -> bool {
    rv_opcode(insn) == 0x03 && rv_funct3(insn) == 0b100
}

/// Check if instruction is SB (opcode 0x23, funct3 = 0)
pub(crate) fn is_sb(insn: u32) -> bool {
    rv_opcode(insn) == 0x23 && rv_funct3(insn) == 0b000
}

/// Check if instruction is LUI (opcode 0x37)
pub(crate) fn is_lui(insn: u32) -> bool {
    rv_opcode(insn) == 0x37
}
