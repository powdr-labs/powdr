//! Builds SymbolicInstructionStatement to create input program for testing powdr_autoprecompile::build
use crate::opcode::*;
use powdr_autoprecompiles::SymbolicInstructionStatement;
use powdr_number::FieldElement;

// Unified builder for all 5-argument instructions (padded to 7 args)
macro_rules! build_instr5 {
    ($(($name:ident, $code:expr)),+) => {
        $(
            pub fn $name<T: FieldElement>(
                a: u32,
                b: u32,
                c: u32,
                d: u32,
                e: u32,
            ) -> SymbolicInstructionStatement<T> {
                SymbolicInstructionStatement {
                    opcode: $code as usize,
                    args: vec![
                        T::from(a),
                        T::from(b),
                        T::from(c),
                        T::from(d),
                        T::from(e),
                        T::zero(),
                        T::zero(),
                    ],
                }
            }
        )+
    };
}

// ALU instructions (7 args, fixed d=1, f=0, g=0)
macro_rules! alu_ops {
    ($(($name:ident, $code:expr)),+) => {
        $(
            pub fn $name<T: FieldElement>(
                rd_ptr: u32,
                rs1_ptr: u32,
                rs2: u32,
                rs2_as: u32,
            ) -> SymbolicInstructionStatement<T> {
                SymbolicInstructionStatement {
                    opcode: $code as usize,
                    args: vec![
                        T::from(rd_ptr),
                        T::from(rs1_ptr),
                        T::from(rs2),
                        T::one(),
                        T::from(rs2_as),
                        T::zero(),
                        T::zero(),
                    ],
                }
            }
        )+
    };
}

// Load/Store (7 args, fixed d=1)
macro_rules! ls_ops {
    ($(($name:ident, $code:expr)),+) => {
        $(
            pub fn $name<T: FieldElement>(
                rd_rs2_ptr: u32,
                rs1_ptr: u32,
                imm: u32,
                mem_as: u32,
                needs_write: u32,
                imm_sign: u32,
            ) -> SymbolicInstructionStatement<T> {
                SymbolicInstructionStatement {
                    opcode: $code as usize,
                    args: vec![
                        T::from(rd_rs2_ptr),
                        T::from(rs1_ptr),
                        T::from(imm),
                        T::one(),
                        T::from(mem_as),
                        T::from(needs_write),
                        T::from(imm_sign),
                    ],
                }
            }
        )+
    };
}

// 5-arg instructions
build_instr5!(
    (beq, OPCODE_BEQ),
    (bne, OPCODE_BNE),
    (blt, OPCODE_BLT),
    (bltu, OPCODE_BLTU),
    (bge, OPCODE_BGE),
    (bgeu, OPCODE_BGEU),
    (jal, OPCODE_JAL),
    (lui, OPCODE_LUI),
    (jalr, OPCODE_JALR),
    (auipc, OPCODE_AUIPC),
    (mul, OPCODE_MUL),
    (mulh, OPCODE_MULH),
    (mulhsu, OPCODE_MULHSU),
    (mulhu, OPCODE_MULHU),
    (div, OPCODE_DIV),
    (divu, OPCODE_DIVU),
    (rem, OPCODE_REM),
    (remu, OPCODE_REMU),
    (hint_storew, OPCODE_HINT_STOREW),
    (hint_buffer, OPCODE_HINT_BUFFER)
);

// Use macros to define ALU and LS ops
alu_ops!(
    (add, OPCODE_ADD),
    (sub, OPCODE_SUB),
    (xor, OPCODE_XOR),
    (or, OPCODE_OR),
    (and, OPCODE_AND),
    (sll, OPCODE_SLL),
    (srl, OPCODE_SRL),
    (sra, OPCODE_SRA),
    (slt, OPCODE_SLT),
    (sltu, OPCODE_SLTU)
);

ls_ops!(
    (loadw, OPCODE_LOADW),
    (loadbu, OPCODE_LOADBU),
    (loadhu, OPCODE_LOADHU),
    (storew, OPCODE_STOREW),
    (storeh, OPCODE_STOREH),
    (storeb, OPCODE_STOREB),
    (loadb, OPCODE_LOADB),
    (loadh, OPCODE_LOADH)
);
