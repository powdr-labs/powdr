//! Builds SymbolicInstructionStatement to create input program for testing powdr_autoprecompile::build
use crate::opcode::*;
use powdr_autoprecompiles::SymbolicInstructionStatement;
use powdr_number::FieldElement;

// Unified builder for all 5-argument instructions (padded to 7 args)
macro_rules! build_instr5 {
    (
        $(
            $(#[$doc:meta])*
            ($name:ident, $code:expr)
        ),+ $(,)?
    ) => {
        $(
            $(#[$doc])*
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
    (
        $(
            $(#[$doc:meta])*
            ($name:ident, $code:expr)
        ),+ $(,)?
    ) => {
        $(
            $(#[$doc])*
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
    (
        $(
            $(#[$doc:meta])*
            ($name:ident, $code:expr)
        ),+ $(,)?
    ) => {
        $(
            $(#[$doc])*
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
    /// Branch equal (Branch adapter and Branch Eq core):
    /// - to_pc = pc + imm if load(REG, rs1_ptr) == load(REG, rs2_ptr) else pc + 4
    /// - arguments: `pc = pc + c` if `[a]_1 == [b]_1`, so `d` and `e` both must be `1`
    (beq, OPCODE_BEQ),
    /// Branch not equal (Branch adapter and Branch Eq core):
    /// - to_pc = pc + imm if load(REG, rs1_ptr) != load(REG, rs2_ptr) else pc + 4
    /// - arguments: `pc = pc + c` if `[a]_1 != [b]_1`, so `d` and `e` both must be `1`
    (bne, OPCODE_BNE),

    /// Branch less than signed (Branch adapter and Branch Lt core):
    /// - to_pc = pc + imm if load(REG, rs1_ptr) < load(REG, rs2_ptr) else pc + 4
    (blt, OPCODE_BLT),
    /// Branch less than unsigned (Branch adapter and Branch Lt core):
    /// - to_pc = pc + imm if load(REG, rs1_ptr) < load(REG, rs2_ptr) else pc + 4
    (bltu, OPCODE_BLTU),
    /// Branch greater than or equal signed (Branch adapter and Branch Lt core):
    /// - to_pc = pc + imm if load(REG, rs1_ptr) >= load(REG, rs2_ptr) else pc + 4
    (bge, OPCODE_BGE),
    /// Branch greater than or equal unsigned (Branch adapter and Branch Lt core):
    /// - to_pc = pc + imm if load(REG, rs1_ptr) >= load(REG, rs2_ptr) else pc + 4
    (bgeu, OPCODE_BGEU),

    /// Jump and link (Rdwrite adapter and JAL_LUI core):
    /// - to_pc = pc + imm
    /// - store(REG, rd_ptr, pc + 4)
    (jal, OPCODE_JAL),
    /// Load upper immediate (Rdwrite adapter and JAL_LUI core):
    /// - store(REG, rd_ptr, imm * 2^8)
    (lui, OPCODE_LUI),

    /// Jump and link register (JALR adapter and JALR core):
    /// - to_pc = load(REG, rs1_ptr) + imm
    /// - store(REG, rd_ptr, pc + 4)
    (jalr, OPCODE_JALR),

    /// Add upper immediate to PC (but does not change PC) (Rdwrite adapter and AUIPC core):
    /// - store(REG, rd_ptr, pc + imm * 2^8)
    (auipc, OPCODE_AUIPC),

    /// Multiplication (Mul adapter and Multiplication core):
    /// - store(REG, rd_ptr, load(REG, rs1_ptr) * load(REG, rs2_ptr) % 2^32)
    (mul, OPCODE_MUL),

    /// Signed * signed multiplication high (Mul adapter and MULH core):
    /// - store(REG, rd_ptr, load(REG, rs1_ptr) * load(REG, rs2_ptr) / 2^32), where `/` is integer division
    (mulh, OPCODE_MULH),
    /// Signed * unsigned multiplication high (Mul adapter and MULH core):
    /// - store(REG, rd_ptr, load(REG, rs1_ptr) * load(REG, rs2_ptr) / 2^32), where `/` is integer division
    (mulhsu, OPCODE_MULHSU),
    /// Unsigned * unsigned multiplication high (Mul adapter and MULH core):
    /// - store(REG, rd_ptr, load(REG, rs1_ptr) * load(REG, rs2_ptr) / 2^32), where `/` is integer division
    (mulhu, OPCODE_MULHU),

    /// Signed division (Mul adapter and Divrem core):
    /// - store(REG, rd_ptr, load(REG, rs1_ptr) / load(REG, rs2_ptr)), where `/` is integer division
    /// - Exception: store(REG, rd_ptr, -1) if `load(REG, rs2_ptr) == 0`
    (div, OPCODE_DIV),
    /// Unsigned division (Mul adapter and Divrem core):
    /// - store(REG, rd_ptr, load(REG, rs1_ptr) / load(REG, rs2_ptr)), where `/` is integer division
    /// - Exception: store(REG, rd_ptr, 2^32 - 1) if `load(REG, rs2_ptr) == 0`
    (divu, OPCODE_DIVU),
    /// Signed remainder (Mul adapter and Divrem core):
    /// - store(REG, rd_ptr, load(REG, rs1_ptr) % load(REG, rs2_ptr))
    (rem, OPCODE_REM),
    /// Unsigned remainder (Mul adapter and Divrem core):
    /// - store(REG, rd_ptr, load(REG, rs1_ptr) % load(REG, rs2_ptr))
    (remu, OPCODE_REMU),

    (hint_storew, OPCODE_HINT_STOREW),
    (hint_buffer, OPCODE_HINT_BUFFER)
);

// Use macros to define ALU and LS ops
alu_ops!(
    /// Addition (ALU adapter and ALU core):
    /// - store(REG, rd_ptr, load(REG, rs1_ptr) + load(rs2_as, rs2))
    (add, OPCODE_ADD),
    /// Subtraction (ALU adapter and ALU core):
    /// - store(REG, rd_ptr, load(REG, rs1_ptr) - load(rs2_as, rs2))
    (sub, OPCODE_SUB),
    /// XOR (ALU adapter and ALU core):
    /// - store(REG, rd_ptr, load(REG, rs1_ptr) XOR load(rs2_as, rs2))
    (xor, OPCODE_XOR),
    /// OR (ALU adapter and ALU core):
    /// - store(REG, rd_ptr, load(REG, rs1_ptr) OR load(rs2_as, rs2))
    (or, OPCODE_OR),
    /// AND (ALU adapter and ALU core):
    /// - store(REG, rd_ptr, load(REG, rs1_ptr) AND load(rs2_as, rs2))
    (and, OPCODE_AND),

    /// Shift left (ALU adapter and Shift core):
    /// - store(REG, rd_ptr, load(REG, rs1_ptr) << (load(rs2_as, rs2) % 32))
    (sll, OPCODE_SLL),
    /// Shift right (ALU adapter and Shift core):
    /// - store(REG, rd_ptr, load(REG, rs1_ptr) >> (load(rs2_as, rs2) % 32))
    (srl, OPCODE_SRL),
    /// Shift right arithmetic (signed) (ALU adapter and Shift core):
    /// - store(REG, rd_ptr, sign_extend(load(REG, rs1_ptr) >> (load(rs2_as, rs2) % 32)))
    (sra, OPCODE_SRA),

    /// Less than signed (ALU adapter and Less than core):
    /// - store(REG, rd_ptr, 1 if load(REG, rs1_ptr) < load(rs2_as, rs2) else 0)
    (slt, OPCODE_SLT),
    /// Less than unsigned (ALU adapter and Less than core):
    /// - store(REG, rd_ptr, 1 if load(REG, rs1_ptr) < load(rs2_as, rs2) else 0)
    (sltu, OPCODE_SLTU)
);

ls_ops!(
    /// Load word (Load/store adapter and Load sign extend core):
    /// - store(REG, rd_ptr, load(mem_as, val(rs1) + imm)), where val(rs1) = load(REG, rs1_ptr)
    (loadw, OPCODE_LOADW),
    /// Load byte unsigned (Load/store adapter and Load sign extend core):
    /// - store(REG, rd_ptr, load_byte_unsigned(mem_as, val(rs1) + imm)), where val(rs1) = load(REG, rs1_ptr)
    (loadbu, OPCODE_LOADBU),
    /// Load half-word unsigned (Load/store adapter and Load sign extend core):
    /// - store(REG, rd_ptr, load_half_word_unsigned(mem_as, val(rs1) + imm)), where val(rs1) = load(REG, rs1_ptr)
    (loadhu, OPCODE_LOADHU),

    /// Store word (Load/store adapter and Loadstore core):
    /// - store(mem_as, val(rs1) + imm, load(REG, rd_ptr)), where val(rs1) = load(REG, rs1_ptr)
    (storew, OPCODE_STOREW),
    /// Store half-word (Load/store adapter and Loadstore core):
    /// - store_half_word(mem_as, val(rs1) + imm, load(REG, rd_ptr)), where val(rs1) = load(REG, rs1_ptr)
    (storeh, OPCODE_STOREH),
    /// Store byte (Load/store adapter and Loadstore core):
    /// - store_byte(mem_as, val(rs1) + imm, load(REG, rd_ptr)), where val(rs1) = load(REG, rs1_ptr)
    (storeb, OPCODE_STOREB),

    /// Load byte signed (Load/store adapter and Load sign extend core):
    /// - store(REG, rd_ptr, load_byte_signed(mem_as, val(rs1) + imm)), where val(rs1) = load(REG, rs1_ptr)
    (loadb, OPCODE_LOADB),
    /// Load half-word signed (Load/store adapter and Load sign extend core):
    /// - store(REG, rd_ptr, load_half_word_signed(mem_as, val(rs1) + imm)), where val(rs1) = load(REG, rs1_ptr)
    (loadh, OPCODE_LOADH)
);
