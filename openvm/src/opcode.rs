use openvm_instructions::LocalOpcode;
use openvm_rv32im_transpiler::*;

/// Defines each opcode as a `pub const u32` and also generates
/// a `pub const ALL_OPCODES: &[u32]` containing all of them.
macro_rules! define_opcodes {
    (
        $( $name:ident = $ty:ident :: $variant:ident, )*
    ) => {
        $(
            // e.g. OPCODE_ADD = BaseAluOpcode::ADD as usize + BaseAluOpcode::CLASS_OFFSET
            pub const $name: u32 =
                ($ty::$variant as usize + <$ty as LocalOpcode>::CLASS_OFFSET) as u32;
        )*

        /// A slice containing *all* opcode constants.
        pub const ALL_OPCODES: &[u32] = &[
            $( $name ),*
        ];
    }
}

define_opcodes!(
    // Rv32BaseAluChip
    OPCODE_ADD = BaseAluOpcode::ADD,
    OPCODE_SUB = BaseAluOpcode::SUB,
    OPCODE_XOR = BaseAluOpcode::XOR,
    OPCODE_OR = BaseAluOpcode::OR,
    OPCODE_AND = BaseAluOpcode::AND,
    // Rv32ShiftChip opcodes
    OPCODE_SLL = ShiftOpcode::SLL,
    OPCODE_SRL = ShiftOpcode::SRL,
    OPCODE_SRA = ShiftOpcode::SRA,
    // Rv32LessThanChip opcodes
    OPCODE_SLT = LessThanOpcode::SLT,
    OPCODE_SLTU = LessThanOpcode::SLTU,
    // Load/Store opcodes
    OPCODE_LOADW = Rv32LoadStoreOpcode::LOADW,
    OPCODE_LOADBU = Rv32LoadStoreOpcode::LOADBU,
    OPCODE_LOADHU = Rv32LoadStoreOpcode::LOADHU,
    OPCODE_STOREW = Rv32LoadStoreOpcode::STOREW,
    OPCODE_STOREH = Rv32LoadStoreOpcode::STOREH,
    OPCODE_STOREB = Rv32LoadStoreOpcode::STOREB,
    OPCODE_LOADB = Rv32LoadStoreOpcode::LOADB,
    OPCODE_LOADH = Rv32LoadStoreOpcode::LOADH,
    // Other opcodes
    OPCODE_BEQ = BranchEqualOpcode::BEQ,
    OPCODE_BNE = BranchEqualOpcode::BNE,
    OPCODE_BLT = BranchLessThanOpcode::BLT,
    OPCODE_BLTU = BranchLessThanOpcode::BLTU,
    OPCODE_BGE = BranchLessThanOpcode::BGE,
    OPCODE_BGEU = BranchLessThanOpcode::BGEU,
    OPCODE_JAL = Rv32JalLuiOpcode::JAL,
    OPCODE_LUI = Rv32JalLuiOpcode::LUI,
    OPCODE_JALR = Rv32JalrOpcode::JALR,
    OPCODE_AUIPC = Rv32AuipcOpcode::AUIPC,
    OPCODE_MUL = MulOpcode::MUL,
    OPCODE_MULH = MulHOpcode::MULH,
    OPCODE_MULHSU = MulHOpcode::MULHSU,
    OPCODE_MULHU = MulHOpcode::MULHU,
    OPCODE_DIV = DivRemOpcode::DIV,
    OPCODE_DIVU = DivRemOpcode::DIVU,
    OPCODE_REM = DivRemOpcode::REM,
    OPCODE_REMU = DivRemOpcode::REMU,
    OPCODE_HINT_STOREW = Rv32HintStoreOpcode::HINT_STOREW,
    OPCODE_HINT_BUFFER = Rv32HintStoreOpcode::HINT_BUFFER,
);
