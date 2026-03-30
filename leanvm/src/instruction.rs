use std::fmt::Display;

use powdr_autoprecompiles::blocks::{Instruction, PcStep};
use powdr_number::FieldElement;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum LeanVmOpcode {
    Add,
    Mul,
    Deref,
    Jump,
}

impl Display for LeanVmOpcode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LeanVmOpcode::Add => write!(f, "ADD"),
            LeanVmOpcode::Mul => write!(f, "MUL"),
            LeanVmOpcode::Deref => write!(f, "DEREF"),
            LeanVmOpcode::Jump => write!(f, "JUMP"),
        }
    }
}

/// A LeanVM instruction with its decoded fields.
///
/// The 12 instruction columns from the bytecode are:
/// operand_A, operand_B, operand_C, flag_A, flag_B, flag_C, flag_C_fp, flag_AB_fp,
/// MUL, JUMP, AUX, PRECOMPILE_DATA
///
/// For this bootstrap, PRECOMPILE_DATA is always 0.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LeanVmInstruction<F> {
    pub opcode: LeanVmOpcode,
    pub operand_a: F,
    pub operand_b: F,
    pub operand_c: F,
    pub flag_a: F,
    pub flag_b: F,
    pub flag_c: F,
    pub flag_c_fp: F,
    pub flag_ab_fp: F,
}

impl<F: FieldElement> LeanVmInstruction<F> {
    /// Returns the 12 instruction column values: operand_A..C, flag_A..C, flag_C_fp,
    /// flag_AB_fp, MUL, JUMP, AUX, PRECOMPILE_DATA
    pub fn instruction_columns(&self) -> [F; 12] {
        let (mul, jump, aux) = match self.opcode {
            LeanVmOpcode::Add => (F::zero(), F::zero(), F::one()),
            LeanVmOpcode::Mul => (F::one(), F::zero(), F::zero()),
            LeanVmOpcode::Deref => (F::zero(), F::zero(), F::from(2)),
            LeanVmOpcode::Jump => (F::zero(), F::one(), F::zero()),
        };
        [
            self.operand_a,
            self.operand_b,
            self.operand_c,
            self.flag_a,
            self.flag_b,
            self.flag_c,
            self.flag_c_fp,
            self.flag_ab_fp,
            mul,
            jump,
            aux,
            F::zero(), // PRECOMPILE_DATA
        ]
    }
}

impl<F: FieldElement> PcStep for LeanVmInstruction<F> {
    fn pc_step() -> u32 {
        1
    }
}

impl<F: FieldElement> Display for LeanVmInstruction<F> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} a={}, b={}, c={}, fA={}, fB={}, fC={}, fCfp={}, fABfp={}",
            self.opcode,
            self.operand_a,
            self.operand_b,
            self.operand_c,
            self.flag_a,
            self.flag_b,
            self.flag_c,
            self.flag_c_fp,
            self.flag_ab_fp,
        )
    }
}

impl<F: FieldElement> Instruction<F> for LeanVmInstruction<F> {
    /// PC lookup row: [instruction_cols[0..12], pc]
    fn pc_lookup_row(&self, pc: u64) -> Vec<F> {
        let mut row: Vec<F> = self.instruction_columns().to_vec();
        row.push(F::from(pc));
        row
    }
}

// Builder functions for tests

/// ADD: m[fp + alpha] = m[fp + beta] + m[fp + gamma]
/// With flags controlling immediate vs memory-read for each operand.
pub fn add<F: FieldElement>(
    operand_a: u64,
    operand_b: u64,
    operand_c: u64,
    flag_a: bool,
    flag_b: bool,
    flag_c: bool,
) -> LeanVmInstruction<F> {
    LeanVmInstruction {
        opcode: LeanVmOpcode::Add,
        operand_a: F::from(operand_a),
        operand_b: F::from(operand_b),
        operand_c: F::from(operand_c),
        flag_a: if flag_a { F::one() } else { F::zero() },
        flag_b: if flag_b { F::one() } else { F::zero() },
        flag_c: if flag_c { F::one() } else { F::zero() },
        flag_c_fp: F::zero(),
        flag_ab_fp: F::zero(),
    }
}

/// MUL: m[fp + alpha] = m[fp + beta] * m[fp + gamma]
pub fn mul<F: FieldElement>(
    operand_a: u64,
    operand_b: u64,
    operand_c: u64,
    flag_a: bool,
    flag_b: bool,
    flag_c: bool,
) -> LeanVmInstruction<F> {
    LeanVmInstruction {
        opcode: LeanVmOpcode::Mul,
        operand_a: F::from(operand_a),
        operand_b: F::from(operand_b),
        operand_c: F::from(operand_c),
        flag_a: if flag_a { F::one() } else { F::zero() },
        flag_b: if flag_b { F::one() } else { F::zero() },
        flag_c: if flag_c { F::one() } else { F::zero() },
        flag_c_fp: F::zero(),
        flag_ab_fp: F::zero(),
    }
}

/// DEREF: m[m[fp + alpha] + beta] = nu_C
pub fn deref<F: FieldElement>(
    operand_a: u64,
    operand_b: u64,
    operand_c: u64,
    flag_c: bool,
    flag_c_fp: bool,
) -> LeanVmInstruction<F> {
    LeanVmInstruction {
        opcode: LeanVmOpcode::Deref,
        operand_a: F::from(operand_a),
        operand_b: F::from(operand_b),
        operand_c: F::from(operand_c),
        flag_a: F::zero(),
        flag_b: F::zero(),
        flag_c: if flag_c { F::one() } else { F::zero() },
        flag_c_fp: if flag_c_fp { F::one() } else { F::zero() },
        flag_ab_fp: F::zero(),
    }
}

/// JUMP: conditional jump based on nu_A
pub fn jump<F: FieldElement>(
    operand_a: u64,
    operand_b: u64,
    operand_c: u64,
    flag_a: bool,
    flag_b: bool,
    flag_c: bool,
    flag_c_fp: bool,
) -> LeanVmInstruction<F> {
    LeanVmInstruction {
        opcode: LeanVmOpcode::Jump,
        operand_a: F::from(operand_a),
        operand_b: F::from(operand_b),
        operand_c: F::from(operand_c),
        flag_a: if flag_a { F::one() } else { F::zero() },
        flag_b: if flag_b { F::one() } else { F::zero() },
        flag_c: if flag_c { F::one() } else { F::zero() },
        flag_c_fp: if flag_c_fp { F::one() } else { F::zero() },
        flag_ab_fp: F::zero(),
    }
}
