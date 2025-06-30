use std::collections::BTreeSet;

use openvm_bigint_transpiler::{Rv32BranchEqual256Opcode, Rv32BranchLessThan256Opcode};
use openvm_instructions::LocalOpcode;
use openvm_rv32im_transpiler::*;

/// Defines each opcode as a `pub const usize` and also generates
/// a `pub const ALL_OPCODES: &[usize]` containing all of them.
macro_rules! define_opcodes {
    (
        // Non-bigint opcodes
        // e.g. OPCODE_BEQ = BranchEqualOpcode::BEQ as usize + BranchEqualOpcode::CLASS_OFFSET
        $( $non_big_int_name:ident = $ty:ident :: $variant:ident, )*
        ; // Intentional pattern split delimiter
        // Bigint opcodes
        // e.g. BIGINT_OPCODE_BEQ = BranchEqualOpcode::BEQ as usize + Rv32BranchEqual256Opcode::CLASS_OFFSET
        $( $bigint_name:ident = $big_ty:ident ; $small_ty:ident :: $small_variant:ident, )*
    ) => {
        $(
            pub const $non_big_int_name: usize = (
                $ty::$variant as usize
                + < $ty as LocalOpcode >::CLASS_OFFSET
            ) as usize;
        )*

        $(
            pub const $bigint_name: usize = (
                $small_ty::$small_variant as usize
                + < $big_ty as LocalOpcode >::CLASS_OFFSET
            ) as usize;
        )*

        /// All opcodes in one slice
        pub const ALL_OPCODES: &[usize] = &[
            $( $non_big_int_name, )*
            $( $bigint_name, )*
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
    ; // Intentional pattern split delimiter
    // Bigint opcodes
    BIGINT_OPCODE_BEQ = Rv32BranchEqual256Opcode; BranchEqualOpcode::BEQ,
    BIGINT_OPCODE_BNE = Rv32BranchEqual256Opcode; BranchEqualOpcode::BNE,
    BIGINT_OPCODE_BLT = Rv32BranchLessThan256Opcode; BranchLessThanOpcode::BLT,
    BIGINT_OPCODE_BLTU = Rv32BranchLessThan256Opcode; BranchLessThanOpcode::BLTU,
    BIGINT_OPCODE_BGE = Rv32BranchLessThan256Opcode; BranchLessThanOpcode::BGE,
    BIGINT_OPCODE_BGEU = Rv32BranchLessThan256Opcode; BranchLessThanOpcode::BGEU,
);

pub const BRANCH_OPCODES_BIGINT: &[usize] = &[
    BIGINT_OPCODE_BEQ,
    BIGINT_OPCODE_BNE,
    BIGINT_OPCODE_BLT,
    BIGINT_OPCODE_BLTU,
    BIGINT_OPCODE_BGE,
    BIGINT_OPCODE_BGEU,
];

pub const BRANCH_OPCODES: &[usize] = &[
    OPCODE_BEQ,
    OPCODE_BNE,
    OPCODE_BLT,
    OPCODE_BLTU,
    OPCODE_BGE,
    OPCODE_BGEU,
    OPCODE_JAL,
    OPCODE_JALR,
];

// Allowed opcodes = ALL_OPCODES - HINT_STOREW - HINT_BUFFER
pub fn instruction_allowlist() -> BTreeSet<usize> {
    // Filter out HINT_STOREW and HINT_BUFFER, which contain next references that don't work with apc
    ALL_OPCODES
        .iter()
        .copied()
        .filter(|&op| op != OPCODE_HINT_BUFFER && op != OPCODE_HINT_STOREW)
        .collect()
}

pub fn branch_opcodes_bigint_set() -> BTreeSet<usize> {
    let mut set = BTreeSet::new();
    set.extend(BRANCH_OPCODES_BIGINT);
    set
}

pub fn branch_opcodes_set() -> BTreeSet<usize> {
    let mut set = branch_opcodes_bigint_set();
    set.extend(BRANCH_OPCODES);
    set
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_all_opcodes() {
        let expected = &[
            512, 513, 514, 515, 516, 517, 518, 519, 520, 521, 528, 529, 530, 531, 532, 533, 534,
            535, 544, 545, 549, 550, 551, 552, 560, 561, 565, 576, 592, 593, 594, 595, 596, 597,
            598, 599, 608, 609, 1056, 1057, 1061, 1062, 1063, 1064,
        ];
        assert_eq!(ALL_OPCODES.len(), 44); // 38 non-bigint + 6 bigint
        assert_eq!(ALL_OPCODES, expected);
    }

    #[test]
    fn test_instruction_allowlist() {
        let allowlist = instruction_allowlist();
        let expected = [
            512, 513, 514, 515, 516, 517, 518, 519, 520, 521, 528, 529, 530, 531, 532, 533, 534,
            535, 544, 545, 549, 550, 551, 552, 560, 561, 565, 576, 592, 593, 594, 595, 596, 597,
            598, 599, 1056, 1057, 1061, 1062, 1063, 1064,
        ]
        .into_iter()
        .collect();
        assert_eq!(allowlist.len(), ALL_OPCODES.len() - 2); // Excluding HINT_STOREW and HINT_BUFFER
        assert_eq!(allowlist, expected);
    }
}
