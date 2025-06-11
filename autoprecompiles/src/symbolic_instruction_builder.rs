use crate::SymbolicInstructionStatement;
use powdr_number::FieldElement;

// Unified builder for all 5-argument instructions
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
    (beq, 544),
    (bne, 545),
    (blt, 549),
    (bltu, 550),
    (bge, 551),
    (bgeu, 552),
    (jal, 560),
    (lui, 561),
    (jalr, 565),
    (auipc, 576),
    (mul, 592),
    (mulh, 593),
    (mulhsu, 594),
    (mulhu, 595),
    (div_, 596),
    (divu, 597),
    (rem, 598),
    (remu, 599),
    (hint_storew, 608),
    (hint_buffer, 609)
);

// Use macros to define ALU and LS ops
alu_ops!(
    (add, 512),
    (sub, 513),
    (xor_, 514),
    (or_, 515),
    (and_, 516),
    (sll, 517),
    (srl, 518),
    (sra, 519),
    (slt, 520),
    (sltu, 521)
);

ls_ops!(
    (loadw, 528),
    (loadbu, 529),
    (loadhu, 530),
    (storew, 531),
    (storeh, 532),
    (storeb, 533),
    (loadb, 534),
    (loadh, 535)
);
