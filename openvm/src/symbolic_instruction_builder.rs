//! Builds SymbolicInstructionStatement to create input program for testing powdr_autoprecompile::build
use crate::opcode::*;
use openvm_instructions::{instruction::Instruction, VmOpcode};
use openvm_stark_backend::p3_field::PrimeField32;

// Generic instructions (5 args, fixed f=0, g=0)
macro_rules! build_instr5 {
    (
        $(
            $(#[$doc:meta])*
            ($name:ident, $code:expr)
        ),+ $(,)?
    ) => {
        $(
            $(#[$doc])*
            pub fn $name<T: PrimeField32>(
                a: u32,
                b: u32,
                c: u32,
                d: u32,
                e: u32,
            ) -> Instruction<T> {
                Instruction {
                    opcode: VmOpcode::from_usize($code as usize),
                    a: T::from_canonical_u32(a),
                    b: T::from_canonical_u32(b),
                    c: T::from_canonical_u32(c),
                    d: T::from_canonical_u32(d),
                    e: T::from_canonical_u32(e),
                    f: T::ZERO,
                    g: T::ZERO,
                }
            }
        )+
    };
}

// ALU instructions (4 args, fixed d=1, f=0, g=0)
macro_rules! alu_ops {
    (
        $(
            $(#[$doc:meta])*
            ($name:ident, $code:expr)
        ),+ $(,)?
    ) => {
        $(
            $(#[$doc])*
            pub fn $name<T: PrimeField32>(
                rd_ptr: u32,
                rs1_ptr: u32,
                rs2: u32,
                rs2_as: u32,
            ) -> Instruction<T> {
                Instruction {
                    opcode: VmOpcode::from_usize($code as usize),
                    a: T::from_canonical_u32(rd_ptr),
                    b: T::from_canonical_u32(rs1_ptr),
                    c: T::from_canonical_u32(rs2),
                    d: T::ONE,
                    e: T::from_canonical_u32(rs2_as),
                    f: T::ZERO,
                    g: T::ZERO,
                }
            }
        )+
    };
}

// Load/Store and Load/Store Sign Extend instructions (6 args, fixed d=1)
macro_rules! ls_ops {
    (
        $(
            $(#[$doc:meta])*
            ($name:ident, $code:expr)
        ),+ $(,)?
    ) => {
        $(
            $(#[$doc])*
            pub fn $name<T: PrimeField32>(
                rd_rs2_ptr: u32,
                rs1_ptr: u32,
                imm: u32,
                mem_as: u32,
                needs_write: u32,
                imm_sign: u32,
            ) -> Instruction<T> {
                Instruction {
                    opcode: VmOpcode::from_usize($code as usize),
                    a: T::from_canonical_u32(rd_rs2_ptr),
                    b: T::from_canonical_u32(rs1_ptr),
                    c: T::from_canonical_u32(imm),
                    d: T::ONE,
                    e: T::from_canonical_u32(mem_as),
                    f: T::from_canonical_u32(needs_write),
                    g: T::from_canonical_u32(imm_sign),
                }
            }
        )+
    };
}

// Branch Lt and Branch Eq instructions (3 args, fixed d=1, e=1, f=0, g=0)
macro_rules! branch_ops {
    (
        $(
            $(#[$doc:meta])*
            ($name:ident, $code:expr)
        ),+ $(,)?
    ) => {
        $(
            $(#[$doc])*
            pub fn $name<T: PrimeField32>(
                rs1_ptr: u32,
                rs2_ptr: u32,
                imm: i32,
            ) -> Instruction<T> {
                Instruction {
                    opcode: VmOpcode::from_usize($code as usize),
                    a: T::from_canonical_u32(rs1_ptr),
                    b: T::from_canonical_u32(rs2_ptr),
                    c: T::from_canonical_u32(imm as u32),
                    d: T::ONE,
                    e: T::ONE,
                    f: T::ZERO,
                    g: T::ZERO,
                }
            }
        )+
    };
}

// Generic instructions
build_instr5!(
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

// ALU instructions
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

// Load/Store and Load/Store Sign Extend instructions
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

// Branch Eq and Branch Lt instructions
branch_ops!(
    /// Branch equal (Branch adapter and Branch Eq core):
    /// - to_pc = pc + imm if load(REG, rs1_ptr) == load(REG, rs2_ptr) else pc + 4
    (beq, OPCODE_BEQ),
    /// Branch not equal (Branch adapter and Branch Eq core):
    /// - to_pc = pc + imm if load(REG, rs1_ptr) != load(REG, rs2_ptr) else pc + 4
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
);
