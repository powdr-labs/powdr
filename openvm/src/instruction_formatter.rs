use crate::opcode::*;
use openvm_instructions::VmOpcode;

pub fn openvm_opcode_formatter(opcode: &VmOpcode) -> String {
    // Opcodes taken from:
    // https://github.com/openvm-org/openvm/blob/v1.0.0/extensions/rv32im/transpiler/src/instructions.rs
    match opcode.as_usize() {
        // Rv32BaseAluChip opcodes
        OPCODE_ADD => "ADD".to_string(),
        OPCODE_SUB => "SUB".to_string(),
        OPCODE_XOR => "XOR".to_string(),
        OPCODE_OR => "OR".to_string(),
        OPCODE_AND => "AND".to_string(),
        // Rv32ShiftChip opcodes
        OPCODE_SLL => "SLL".to_string(),
        OPCODE_SRL => "SRL".to_string(),
        OPCODE_SRA => "SRA".to_string(),
        // Rv32LessThanChip opcodes
        OPCODE_SLT => "SLT".to_string(),
        OPCODE_SLTU => "SLTU".to_string(),
        // Load/Store opcodes
        OPCODE_LOADW => "LOADW".to_string(),
        OPCODE_LOADBU => "LOADBU".to_string(),
        OPCODE_LOADHU => "LOADHU".to_string(),
        OPCODE_STOREW => "STOREW".to_string(),
        OPCODE_STOREH => "STOREH".to_string(),
        OPCODE_STOREB => "STOREB".to_string(),
        OPCODE_LOADB => "LOADB".to_string(),
        OPCODE_LOADH => "LOADH".to_string(),
        // Other opcodes
        OPCODE_BEQ => "BEQ".to_string(),
        OPCODE_BNE => "BNE".to_string(),
        OPCODE_BLT => "BLT".to_string(),
        OPCODE_BLTU => "BLTU".to_string(),
        OPCODE_BGE => "BGE".to_string(),
        OPCODE_BGEU => "BGEU".to_string(),
        OPCODE_JAL => "JAL".to_string(),
        OPCODE_LUI => "LUI".to_string(),
        OPCODE_JALR => "JALR".to_string(),
        OPCODE_AUIPC => "AUIPC".to_string(),
        OPCODE_MUL => "MUL".to_string(),
        OPCODE_MULH => "MULH".to_string(),
        OPCODE_MULHSU => "MULHSU".to_string(),
        OPCODE_MULHU => "MULHU".to_string(),
        OPCODE_DIV => "DIV".to_string(),
        OPCODE_DIVU => "DIVU".to_string(),
        OPCODE_REM => "REM".to_string(),
        OPCODE_REMU => "REMU".to_string(),
        OPCODE_HINT_STOREW => "HINT_STOREW".to_string(),
        OPCODE_HINT_BUFFER => "HINT_BUFFER".to_string(),
        // Bigint opcodes
        BIGINT_OPCODE_BEQ => "BIGINT_BEQ".to_string(),
        BIGINT_OPCODE_BNE => "BIGINT_BNE".to_string(),
        BIGINT_OPCODE_BLT => "BIGINT_BLT".to_string(),
        BIGINT_OPCODE_BLTU => "BIGINT_BLTU".to_string(),
        BIGINT_OPCODE_BGE => "BIGINT_BGE".to_string(),
        BIGINT_OPCODE_BGEU => "BIGINT_BGEU".to_string(),
        other => format!("<opcode {other}>"),
    }
}
