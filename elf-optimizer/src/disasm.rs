/// RISC-V register ABI names.
const REG_NAMES: [&str; 32] = [
    "zero", "ra", "sp", "gp", "tp", "t0", "t1", "t2", "s0", "s1", "a0", "a1", "a2", "a3", "a4",
    "a5", "a6", "a7", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9", "s10", "s11", "t3", "t4",
    "t5", "t6",
];

pub(crate) fn reg_name(r: u32) -> &'static str {
    REG_NAMES.get(r as usize).unwrap_or(&"?")
}

/// Simple RISC-V disassembler for the most common instruction formats.
pub(crate) fn disasm(insn: u32, addr: u32) -> String {
    let opcode = insn & 0x7F;
    let rd = (insn >> 7) & 0x1F;
    let rs1 = (insn >> 15) & 0x1F;
    let rs2 = (insn >> 20) & 0x1F;
    let funct3 = (insn >> 12) & 0x7;
    let funct7 = (insn >> 25) & 0x7F;
    let imm_i = (insn as i32) >> 20;
    let imm_s = ((((insn >> 25) & 0x7F) as i32) << 5) | (((insn >> 7) & 0x1F) as i32);
    let imm_s = (imm_s << 20) >> 20; // sign extend
    let imm_u = insn & 0xFFFFF000;

    match opcode {
        0x37 => format!("lui     {}, 0x{:x}", reg_name(rd), imm_u >> 12),
        0x17 => format!("auipc   {}, 0x{:x}", reg_name(rd), imm_u >> 12),
        0x6F => {
            // JAL
            let imm20 = ((insn >> 31) & 1) as i32;
            let imm10_1 = ((insn >> 21) & 0x3FF) as i32;
            let imm11 = ((insn >> 20) & 1) as i32;
            let imm19_12 = ((insn >> 12) & 0xFF) as i32;
            let offset = (imm20 << 20) | (imm19_12 << 12) | (imm11 << 11) | (imm10_1 << 1);
            let offset = (offset << 11) >> 11; // sign extend
            let target = (addr as i32).wrapping_add(offset) as u32;
            if rd == 0 {
                format!("j       0x{target:08x}")
            } else {
                format!("jal     {}, 0x{target:08x}", reg_name(rd))
            }
        }
        0x67 => {
            // JALR
            if rd == 0 && rs1 == 1 && imm_i == 0 {
                "ret".to_string()
            } else {
                format!("jalr    {}, {}({})", reg_name(rd), imm_i, reg_name(rs1))
            }
        }
        0x63 => {
            // Branch
            let imm12 = ((insn >> 31) & 1) as i32;
            let imm10_5 = ((insn >> 25) & 0x3F) as i32;
            let imm4_1 = ((insn >> 8) & 0xF) as i32;
            let imm11 = ((insn >> 7) & 1) as i32;
            let offset = (imm12 << 12) | (imm11 << 11) | (imm10_5 << 5) | (imm4_1 << 1);
            let offset = (offset << 19) >> 19;
            let target = (addr as i32).wrapping_add(offset) as u32;
            let op = match funct3 {
                0 => "beq",
                1 => "bne",
                4 => "blt",
                5 => "bge",
                6 => "bltu",
                7 => "bgeu",
                _ => "b??",
            };
            format!(
                "{:<8}{}, {}, 0x{target:08x}",
                op,
                reg_name(rs1),
                reg_name(rs2)
            )
        }
        0x03 => {
            // Load
            let op = match funct3 {
                0 => "lb",
                1 => "lh",
                2 => "lw",
                4 => "lbu",
                5 => "lhu",
                _ => "l??",
            };
            format!("{:<8}{}, {}({})", op, reg_name(rd), imm_i, reg_name(rs1))
        }
        0x23 => {
            // Store
            let op = match funct3 {
                0 => "sb",
                1 => "sh",
                2 => "sw",
                _ => "s??",
            };
            format!("{:<8}{}, {}({})", op, reg_name(rs2), imm_s, reg_name(rs1))
        }
        0x13 => {
            // I-type ALU
            let op = match funct3 {
                0 => "addi",
                1 => "slli",
                2 => "slti",
                3 => "sltiu",
                4 => "xori",
                5 => {
                    if funct7 == 0x20 {
                        "srai"
                    } else {
                        "srli"
                    }
                }
                6 => "ori",
                7 => "andi",
                _ => "???i",
            };
            if funct3 == 0 && rs1 == 0 && rd != 0 {
                format!("li      {}, {}", reg_name(rd), imm_i)
            } else if funct3 == 0 && imm_i == 0 && rd != 0 {
                format!("mv      {}, {}", reg_name(rd), reg_name(rs1))
            } else if funct3 == 0 && rd == 0 && rs1 == 0 && imm_i == 0 {
                "nop".to_string()
            } else {
                format!("{:<8}{}, {}, {}", op, reg_name(rd), reg_name(rs1), imm_i)
            }
        }
        0x33 => {
            // R-type ALU
            let op = match (funct3, funct7) {
                (0, 0) => "add",
                (0, 0x20) => "sub",
                (1, 0) => "sll",
                (2, 0) => "slt",
                (3, 0) => "sltu",
                (4, 0) => "xor",
                (5, 0) => "srl",
                (5, 0x20) => "sra",
                (6, 0) => "or",
                (7, 0) => "and",
                (0, 1) => "mul",
                (1, 1) => "mulh",
                (2, 1) => "mulhsu",
                (3, 1) => "mulhu",
                (4, 1) => "div",
                (5, 1) => "divu",
                (6, 1) => "rem",
                (7, 1) => "remu",
                _ => "???",
            };
            format!(
                "{:<8}{}, {}, {}",
                op,
                reg_name(rd),
                reg_name(rs1),
                reg_name(rs2)
            )
        }
        0x0F => "fence".to_string(),
        0x73 => {
            if insn == 0x00000073 {
                "ecall".to_string()
            } else if insn == 0x00100073 {
                "ebreak".to_string()
            } else {
                format!("system  0x{insn:08x}")
            }
        }
        _ => format!(".word   0x{insn:08x}"),
    }
}
