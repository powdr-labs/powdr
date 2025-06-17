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
    /// Branch adapter:
    /// - `rs1`, `rs2`, and `rd` are register addresses
    /// - `from_pc` is the current program address
    /// - `to_pc` is the destination program address
    ///
    /// Branch adapter proves the following:
    /// - A memory read from register `rs1` is performed
    /// - A memory read from register `rs2` is performed
    /// - The instruction is correctly fetched from the program ROM at address `from_pc` and the program counter is set to `to_pc`.
    ///
    /// Branch eq core:
    /// - `a` and `b` are decompositions of the operands, with their limbs assumed to be in the range `[0, 2^RV32_CELL_BITS)`
    /// - `opcode_beq_flag` and `opcode_bne_flag` indicate if the instruction is `beq` or `bne`
    /// - `imm` is the immediate value
    /// - `to_pc` is the destination program address
    ///
    /// Branch eq core proves that:
    /// - If `opcode_beq_flag` is true and `a` is equal to `b`, then `to_pc == pc + imm`, otherwise `to_pc == pc + 4`
    /// - If `opcode_bne_flag` is true and `a` is not equal to `b`, then `to_pc == pc + imm`, otherwise `to_pc == pc + 4`
    
    /// Branch equal: to_pc = pc + imm if load(REG, rs1_ptr) == load(REG, rs2_ptr) else pc + 4
    (beq, OPCODE_BEQ),
    /// Branch not equal: to_pc = pc + imm if load(REG, rs1_ptr) != load(REG, rs2_ptr) else pc + 4
    (bne, OPCODE_BNE),

    /// Branch adapter:
    /// - `rs1`, `rs2`, and `rd` are register addresses
    /// - `from_pc` is the current program address
    /// - `to_pc` is the destination program address
    ///
    /// Branch adapter proves the following:
    /// - A memory read from register `rs1` is performed
    /// - A memory read from register `rs2` is performed
    /// - The instruction is correctly fetched from the program ROM at address `from_pc` and the program counter is set to `to_pc`.
    ///
    /// Branch Lt core:
    /// - `a` and `b` are decompositions of the operands, with their limbs assumed to be in the range `[0, 2^RV32_CELL_BITS)`
    /// - Flags indicating if the instruction is one of `blt`, `bltu`, `bge`, `bgeu`
    /// - `imm` is the immediate value
    /// - `to_pc` is the destination program address
    ///
    /// Branch Lt core proves that:
    /// - If the instruction is `blt` and `compose(a) < compose(b)` (signed comparison), then `to_pc == pc + imm`, otherwise `to_pc == pc + 4`
    /// - If the instruction is `bltu` and `compose(a) < compose(b)` (unsigned comparison), then `to_pc == pc + imm`, otherwise `to_pc == pc + 4`
    /// - If the instruction is `bge` and `compose(a) >= compose(b)` (signed comparison), then `to_pc == pc + imm`, otherwise `to_pc == pc + 4`
    /// - If the instruction is `bgeu` and `compose(a) >= compose(b)` (unsigned comparison), then `to_pc == pc + imm`, otherwise `to_pc == pc + 4`
    
    /// Branch less than signed: to_pc = pc + imm if load(REG, rs1_ptr) < load(REG, rs2_ptr) else pc + 4
    (blt, OPCODE_BLT),
    /// Branch less than unsigned: to_pc = pc + imm if load(REG, rs1_ptr) < load(REG, rs2_ptr) else pc + 4
    (bltu, OPCODE_BLTU),
    /// Branch greater than or equal signed: to_pc = pc + imm if load(REG, rs1_ptr) >= load(REG, rs2_ptr) else pc + 4
    (bge, OPCODE_BGE),
    /// Branch greater than or equal unsigned: to_pc = pc + imm if load(REG, rs1_ptr) >= load(REG, rs2_ptr) else pc + 4
    (bgeu, OPCODE_BGEU),

    /// Rdwrite adapter:
    /// - `rd` is a register address
    /// - `from_pc` is the current program address
    /// - `to_pc` is the destination program address
    ///
    /// Rdwrite adapter proves the following:
    /// - A memory write to register `rd` is performed if `rd` is not `x0`
    /// - The instruction is correctly fetched from the program ROM at address `from_pc` and the program counter is set to `to_pc`
    ///
    /// JAL_LUI core:
    /// - `rd` is the decomposition of the result
    /// - `imm` is the immediate value
    /// - `to_pc` is the destination program address
    /// - `opcode` indicates the operation to be performed
    ///
    /// JAL_LUI core proves that:
    /// - Each limb of `rd` is in the range `[0, 2^RV32_CELL_BITS)`
    /// - If `opcode` is `jal`, then
    /// - `to_pc == pc + imm`
    /// - `compose(rd) == pc + 4`
    /// - The most significant limb of `rd` is in the range `[0, 2^(PC_BITS - RV32_CELL_BITS * (RV32_REGISTER_NUM_LIMBS - 1))`
    /// - If `opcode` is `lui`, then
    /// - `to_pc == pc + 4`
    /// - `compose(rd) == imm * 2^8`
    
    /// Jump and link: to_pc = pc + imm, store(REG, rd_ptr, pc + 4)
    (jal, OPCODE_JAL),
    /// Load upper immediate: store(REG, rd_ptr, imm * 2^8)
    (lui, OPCODE_LUI),

    /// JALR adapter:
    /// - `rd`, `rs1` are register addresses
    /// - `from_pc` is the current program address
    /// - `to_pc` is the destination program address
    ///
    /// JALR adapter proves the following:
    /// - A memory read from register `rs1` is performed
    /// - A memory write to register `rd` is performed if `rd` is not `x0`
    /// - The instruction is correctly fetched from the program ROM at address `from_pc` and the program counter is set to `to_pc`
    ///
    /// JALR core:
    /// - `rs1` is the decomposition of the operand, with its limbs assumed to be in the range `[0, 2^RV32_CELL_BITS)`
    /// - `rd` is the decomposition of the result
    /// - `imm` is the immediate value
    /// - `to_pc_limbs` is the decomposition into 16-bit limbs of the destination program address
    ///
    /// JALR core proves that:
    /// - `compose(to_pc_limbs) == compose(rs1) + imm`
    /// - `compose(rd) == pc + 4`
    /// - Each limb of `rd` is in the range `[0, 2^RV32_CELL_BITS)`
    /// - The most significant limb of `rd` is in the range `[0, 2^(PC_BITS - RV32_CELL_BITS * (RV32_REGISTER_NUM_LIMBS - 1))`
    /// - `to_pc_limbs[0]` is in the range `[0, 2^15)`
    /// - `to_pc_limbs[1]` is in the range `[0, 2^(PC_BITS - 16))`
    
    
    (jalr, OPCODE_JALR),

    /// Rdwrite adapter:
    /// - `rd` is a register address
    /// - `from_pc` is the current program address
    /// - `to_pc` is the destination program address
    ///
    /// Rdwrite adapter proves the following:
    /// - A memory write to register `rd` is performed if `rd` is not `x0`
    /// - The instruction is correctly fetched from the program ROM at address `from_pc` and the program counter is set to `to_pc`
    ///
    /// AUIPC core:
    /// - `rd` is the decomposition of the result
    /// - `imm_limbs` are the decomposition of the immediate value
    /// - `pc_limbs` are the decomposition of the program counter
    ///
    /// AUIPC core proves that:
    /// - `compose(rd) == compose(pc_limbs) + compose(imm_limbs) * 2^8`
    /// - `compose(pc_limbs) == pc`
    /// - Each limb of `rd`, `imm_limbs`, and `pc_limbs` is in the range `[0, 2^RV32_CELL_BITS)`
    /// - The most significant limb of `pc_limbs` is in the range `[0, 2^(PC_BITS - RV32_CELL_BITS * (RV32_REGISTER_NUM_LIMBS - 1))`
    (auipc, OPCODE_AUIPC),

    /// Mul adapter:
    /// - `rd`, `rs1`, `rs2` are register addresses
    /// - `from_pc` is the current program address
    ///
    /// Mul adapter proves the following:
    /// - A memory read from register `rs1` is performed
    /// - A memory read from register `rs2` is performed
    /// - A memory write to register `rd` is performed with the result of the multiplication
    /// - The instruction is correctly fetched from the program ROM at address `from_pc` and the program counter is set to `from_pc + 4`
    ///
    /// Multiplication core:
    /// - `b`, `c` are decompositions of the operands, with their limbs assumed to be in the range `[0, 2^RV32_CELL_BITS)`
    /// - `a` is the decomposition of the lower 32 bits of the result
    /// - `opcode` indicates the operation to be performed
    ///
    /// Multiplication core proves that:
    /// - `compose(a) == (compose(b) * compose(c)) % 2^32`
    /// - Each limb of `a` is in the range `[0, 2^RV32_CELL_BITS)`
    (mul, OPCODE_MUL),

    /// Mul adapter:
    /// - `rd`, `rs1`, `rs2` are register addresses
    /// - `from_pc` is the current program address
    ///
    /// Mul adapter proves the following:
    /// - A memory read from register `rs1` is performed
    /// - A memory read from register `rs2` is performed
    /// - A memory write to register `rd` is performed with the result of the multiplication
    /// - The instruction is correctly fetched from the program ROM at address `from_pc` and the program counter is set to `from_pc + 4`
    ///
    /// MULH core:
    /// - `b`, `c` are decompositions of the operands, with their limbs assumed to be in the range `[0, 2^RV32_CELL_BITS)`
    /// - `a` is the decomposition of the upper 32 bits of the result
    /// - `opcode` indicates the operation to be performed
    ///
    /// MULH core proves that:
    /// - `compose(a) == floor((compose(b) * compose(c)) / 2^32)`
    /// - Each limb of `a` is in the range `[0, 2^RV32_CELL_BITS)`
    (mulh, OPCODE_MULH),
    (mulhsu, OPCODE_MULHSU),
    (mulhu, OPCODE_MULHU),

    /// Mul adapter:
    /// - `rd`, `rs1`, `rs2` are register addresses
    /// - `from_pc` is the current program address
    ///
    /// Mul adapter proves the following:
    /// - A memory read from register `rs1` is performed
    /// - A memory read from register `rs2` is performed
    /// - A memory write to register `rd` is performed with the result of the multiplication
    /// - The instruction is correctly fetched from the program ROM at address `from_pc` and the program counter is set to `from_pc + 4`
    ///
    /// Divrem core:
    /// - `b` and `c` are decompositions of the operands, with their limbs assumed to be in the range `[0, 2^RV32_CELL_BITS)`
    /// - `q` is the decomposition of the quotient
    /// - `r` is the decomposition of the remainder
    /// - `a` is the decomposition of the result
    /// - Flags indicating if the instruction is `div`, `divu`, `rem`, `remu`
    ///
    /// Divrem core proves that:
    /// - `compose(b) = compose(c) * compose(q) + compose(r)`
    /// - `0 <= |compose(r)| < |compose(c)|`
    /// - If `compose(c) == 0`, then `compose(q) == -1` for signed operations and `compose(q) == 2^32 - 1` for unsigned operations
    /// - Each limb of `q` and `r` is in the range `[0, 2^RV32_CELL_BITS)`
    /// - `a = q` if the instruction is `div` or `divu`
    /// - `a = r` if the instruction is `rem` or `remu`
    (div, OPCODE_DIV),
    (divu, OPCODE_DIVU),
    (rem, OPCODE_REM),
    (remu, OPCODE_REMU),


    (hint_storew, OPCODE_HINT_STOREW),
    (hint_buffer, OPCODE_HINT_BUFFER)
);

// Use macros to define ALU and LS ops
alu_ops!(
    /// ALU adapter:
    /// - `rs1`, `rs2`, and `rd` are register addresses
    /// - `rs2_as` is a boolean indicating if `rs2` is an immediate value
    /// - `from_pc` is the current program address
    ///
    /// ALU adapter proves the following:
    /// - A memory read from register `rs1` is performe
    /// - If `rs2_as` is false, a memory read from register `rs2` is performed
    /// - A memory write to register `rd` is performed with the result of the operation
    /// - The instruction is correctly fetched from the program ROM at address `from_pc` and the program counter is set to `from_pc + 4`
    ///
    /// Base ALU core:
    /// - `b` and `c` are decompositions of the operands, with their limbs assumed to be in the range `[0, 2^RV32_CELL_BITS)`
    /// - `a` is the decomposition of the result
    /// - `opcode` indicates the operation to be performed
    ///
    /// Base ALU core proves that:
    /// - `compose(a) == compose(b) op compose(c)`
    /// - Each limb of `a` is within the range `[0, 2^RV32_CELL_BITS)`

    /// Addition: store(REG, rd_ptr, load(REG, rs1_ptr) + load(rs2_as, rs2))
    (add, OPCODE_ADD),
    /// Subtraction: store(REG, rd_ptr, load(REG, rs1_ptr) - load(rs2_as, rs2))
    (sub, OPCODE_SUB),
    /// XOR: store(REG, rd_ptr, load(REG, rs1_ptr) XOR load(rs2_as, rs2))
    (xor, OPCODE_XOR),
    /// OR: store(REG, rd_ptr, load(REG, rs1_ptr) OR load(rs2_as, rs2))
    (or, OPCODE_OR),
    /// AND: store(REG, rd_ptr, load(REG, rs1_ptr) AND load(rs2_as, rs2))
    (and, OPCODE_AND),

    /// ALU adapter:
    /// - `rs1`, `rs2`, and `rd` are register addresses
    /// - `rs2_as` is a boolean indicating if `rs2` is an immediate value
    /// - `from_pc` is the current program address
    ///
    /// ALU adapter proves the following:
    /// - A memory read from register `rs1` is performe
    /// - If `rs2_as` is false, a memory read from register `rs2` is performed
    /// - A memory write to register `rd` is performed with the result of the operation
    /// - The instruction is correctly fetched from the program ROM at address `from_pc` and the program counter is set to `from_pc + 4`
    ///
    /// Shift core:
    /// - `b`, `c` are decompositions of the operands, with their limbs assumed to be in the range `[0, 2^RV32_CELL_BITS)`
    /// - `a` is the decomposition of the result
    /// - `opcode` indicates the operation to be performed
    ///
    /// Shift core proves that:
    /// - If `opcode` is `sll`, then `compose(a) == compose(b) << (compose(c) % (RV32_CELL_BITS * RV32_REGISTER_NUM_LIMBS))`
    /// - If `opcode` is `srl`, then `compose(a) == compose(b) >> (compose(c) % (RV32_CELL_BITS * RV32_REGISTER_NUM_LIMBS))`
    /// - If `opcode` is `sra`, then `compose(a) == sign_extend(compose(b) >> (compose(c) % (RV32_CELL_BITS * RV32_REGISTER_NUM_LIMBS)))`
    /// - Each limb of `a` is in the range `[0, 2^RV32_CELL_BITS)`
    
    /// Shift left: store(REG, rd_ptr, load(REG, rs1_ptr) << (load(rs2_as, rs2) % 32))
    (sll, OPCODE_SLL),
    /// Shift right: store(REG, rd_ptr, load(REG, rs1_ptr) >> (load(rs2_as, rs2) % 32))
    (srl, OPCODE_SRL),
    /// Shift right arithmetic (signed): store(REG, rd_ptr, sign_extend(load(REG, rs1_ptr) >> (load(rs2_as, rs2) % 32)))
    (sra, OPCODE_SRA),

    /// ALU adapter:
    /// - `rs1`, `rs2`, and `rd` are register addresses
    /// - `rs2_as` is a boolean indicating if `rs2` is an immediate value
    /// - `from_pc` is the current program address
    ///
    /// ALU adapter proves the following:
    /// - A memory read from register `rs1` is performe
    /// - If `rs2_as` is false, a memory read from register `rs2` is performed
    /// - A memory write to register `rd` is performed with the result of the operation
    /// - The instruction is correctly fetched from the program ROM at address `from_pc` and the program counter is set to `from_pc + 4`
    ///
    /// Less than core:
    /// - `b`, `c` are decompositions of the operands, with their limbs assumed to be in the range `[0, 2^RV32_CELL_BITS)`
    /// - `a` is the result
    /// - `opcode` indicates the operation to be performed
    ///
    /// Less than core proves that:
    /// - If `opcode` is `slt` and `compose(b) < compose(c)` (signed comparison), then `a` is 1.
    /// - If `opcode` is `sltu` and `compose(b) < compose(c)` (unsigned comparison), then `a` is 1.
    /// - Otherwise, `a` is 0.

    /// Less than signed: store(REG, rd_ptr, 1 if load(REG, rs1_ptr) < load(rs2_as, rs2) else 0)
    (slt, OPCODE_SLT),
    /// Less than unsigned: store(REG, rd_ptr, 1 if load(REG, rs1_ptr) < load(rs2_as, rs2) else 0)
    (sltu, OPCODE_SLTU)
);

ls_ops!(
    /// Load/store adapter:
    /// - `rd`, `rs1` are register addresses
    /// - `imm` is an immediate value
    /// - `mem_as` is an address space
    /// - `is_load` is a boolean indicating if the instruction is a load
    /// - `from_pc` is the current program address
    ///
    /// Load/store adapter proves the following:
    /// - If `is_load` is true:
    ///     - `mem_as` is in `{0, 1, 2}`
    ///     - A memory read from register `rs1` is performed
    ///     - A memory read from `mem_as` is performed at address `val(rs1) + imm` where `val(rs1)` is the value read from register `rs1`
    ///     - A memory write to register `rd` is performed if `rd` is not `x0`
    ///     - The instruction is correctly fetched from the program ROM at address `from_pc` and the program counter is set to `from_pc + 4`
    /// - Otherwise:
    ///     - `mem_as` is in `{2, 3, 4}`
    ///     - A memory read from register `rs1` is performed
    ///     - A memory read from register `rd` is performed
    ///     - A memory write to `mem_as` is performed at address `val(rs1) + imm` where `val(rs1)` is the value read from register `rs1`
    ///     - The instruction is correctly fetched from the program ROM at address `from_pc` and the program counter is set to `from_pc + 4`
    ///
    /// Load sign extend core AND Loadstore core:
    /// - `read_data` is the data read from `mem_as[aligned(val(rs1) + imm)]` if the instruction is load, otherwise it is the data read from register `rd`
    /// - `write_data` is the data to be written to register `rd` if the instruction is load, otherwise it is the data to be written to `mem_as[aligned(val(rs1) + imm)]`
    /// - `opcode` indicates the operation to be performed
    ///
    /// Load sign extend core AND Loadstore core proves that `write_data` equals `shift(read_data)`, where the shift amount is adjusted according to the instruction.
    
    /// Load word: store(REG, rd_ptr, load(mem_as, val(rs1) + imm)), where val(rs1) = load(REG, rs1_ptr)
    (loadw, OPCODE_LOADW),
    /// Load byte unsigned: store(REG, rd_ptr, load_byte_unsigned(mem_as, val(rs1) + imm)), where val(rs1) = load(REG, rs1_ptr)
    (loadbu, OPCODE_LOADBU),
    /// Load half-word unsigned: store(REG, rd_ptr, load_half_word_unsigned(mem_as, val(rs1) + imm)), where val(rs1) = load(REG, rs1_ptr)
    (loadhu, OPCODE_LOADHU),
    /// Store word: store(mem_as, val(rs1) + imm, load(REG, rd_ptr)), where val(rs1) = load(REG, rs1_ptr)
    (storew, OPCODE_STOREW),
    /// Store half-word: store_half_word(mem_as, val(rs1) + imm, load(REG, rd_ptr)), where val(rs1) = load(REG, rs1_ptr)
    (storeh, OPCODE_STOREH),
    /// Store byte: store_byte(mem_as, val(rs1) + imm, load(REG, rd_ptr)), where val(rs1) = load(REG, rs1_ptr)
    (storeb, OPCODE_STOREB),
    /// Load byte signed: store(REG, rd_ptr, load_byte_signed(mem_as, val(rs1) + imm)), where val(rs1) = load(REG, rs1_ptr)
    (loadb, OPCODE_LOADB),
    /// Load half-word signed: store(REG, rd_ptr, load_half_word_signed(mem_as, val(rs1) + imm)), where val(rs1) = load(REG, rs1_ptr)
    (loadh, OPCODE_LOADH)
);
