use crate::opcode::*;
use openvm_instructions::instruction::Instruction;
use openvm_stark_backend::p3_field::PrimeField32;

pub fn openvm_instruction_formatter<F: PrimeField32>(instruction: &Instruction<F>) -> String {
    let Instruction {
        opcode,
        a,
        b,
        c,
        d,
        e,
        f,
        g,
    } = instruction;

    // Opcodes taken from:
    // https://github.com/openvm-org/openvm/blob/v1.0.0/extensions/rv32im/transpiler/src/instructions.rs
    match opcode.as_usize() as u32 {
        // Alu instructions, see:
        // https://github.com/openvm-org/openvm/blob/v1.0.0/extensions/rv32im/circuit/src/adapters/alu.rs#L197-L201
        512..=521 => {
            assert_eq!(d, &F::ONE);
            assert_eq!(f, &F::ZERO);
            assert_eq!(g, &F::ZERO);
            let opcode = match opcode.as_usize() as u32 {
                // Rv32BaseAluChip
                OPCODE_ADD => "ADD",
                OPCODE_SUB => "SUB",
                OPCODE_XOR => "XOR",
                OPCODE_OR => "OR",
                OPCODE_AND => "AND",
                // Rv32ShiftChip
                OPCODE_SLL => "SLL",
                OPCODE_SRL => "SRL",
                OPCODE_SRA => "SRA",
                // Rv32LessThanChip
                OPCODE_SLT => "SLT",
                OPCODE_SLTU => "SLTU",
                _ => unreachable!(),
            };
            format!("{opcode} rd_ptr = {a}, rs1_ptr = {b}, rs2 = {c}, rs2_as = {e}")
        }

        // Load/Store instructions, see:
        // https://github.com/openvm-org/openvm/blob/v1.0.0/extensions/rv32im/circuit/src/adapters/loadstore.rs#L340-L346
        528..=535 => {
            assert_eq!(d, &F::ONE);
            let opcode = match opcode.as_usize() as u32 {
                OPCODE_LOADW => "LOADW",
                OPCODE_LOADBU => "LOADBU",
                OPCODE_LOADHU => "LOADHU",
                OPCODE_STOREW => "STOREW",
                OPCODE_STOREH => "STOREH",
                OPCODE_STOREB => "STOREB",
                OPCODE_LOADB => "LOADB",
                OPCODE_LOADH => "LOADH",
                _ => unreachable!(),
            };
            format!("{opcode} rd_rs2_ptr = {a}, rs1_ptr = {b}, imm = {c}, mem_as = {e}, needs_write = {f}, imm_sign = {g}")
        }

        OPCODE_BEQ => format!("BEQ {a} {b} {c} {d} {e}"),
        OPCODE_BNE => format!("BNE {a} {b} {c} {d} {e}"),

        OPCODE_BLT => format!("BLT {a} {b} {c} {d} {e}"),
        OPCODE_BLTU => format!("BLTU {a} {b} {c} {d} {e}"),
        OPCODE_BGE => format!("BGE {a} {b} {c} {d} {e}"),
        OPCODE_BGEU => format!("BGEU {a} {b} {c} {d} {e}"),

        OPCODE_JAL => format!("JAL {a} {b} {c} {d} {e}"),
        OPCODE_LUI => format!("LUI {a} {b} {c} {d} {e}"),

        OPCODE_JALR => format!("JALR {a} {b} {c} {d} {e}"),

        OPCODE_AUIPC => format!("AUIPC {a} {b} {c} {d} {e}"),

        OPCODE_MUL => format!("MUL {a} {b} {c} {d} {e}"),
        OPCODE_MULH => format!("MULH {a} {b} {c} {d} {e}"),
        OPCODE_MULHSU => format!("MULHSU {a} {b} {c} {d} {e}"),
        OPCODE_MULHU => format!("MULHU {a} {b} {c} {d} {e}"),

        OPCODE_DIV => format!("DIV {a} {b} {c} {d} {e}"),
        OPCODE_DIVU => format!("DIVU {a} {b} {c} {d} {e}"),
        OPCODE_REM => format!("REM {a} {b} {c} {d} {e}"),
        OPCODE_REMU => format!("REMU {a} {b} {c} {d} {e}"),

        OPCODE_HINT_STOREW => format!("HINT_STOREW {a} {b} {c} {d} {e}"),
        OPCODE_HINT_BUFFER => format!("HINT_BUFFER {a} {b} {c} {d} {e}"),
        _ => format!("<opcode {opcode}> {a} {b} {c} {d} {e} {f} {g}"),
    }
}
