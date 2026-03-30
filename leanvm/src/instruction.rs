use std::fmt::Display;

use lean_vm::{
    Instruction as UpstreamInstruction, Label, MemOrConstant, MemOrFpOrConstant, Operation, F as KB,
};
use lean_vm_backend::{PrimeCharacteristicRing, PrimeField32};
use powdr_autoprecompiles::blocks::{Instruction, PcStep};
use powdr_number::KoalaBearField;
use serde::{Deserialize, Serialize};

/// A LeanVM instruction wrapping the upstream `lean_vm::Instruction` type.
///
/// The 12 instruction columns from the bytecode are:
/// operand_A, operand_B, operand_C, flag_A, flag_B, flag_C, flag_C_fp, flag_AB_fp,
/// MUL, JUMP, AUX, PRECOMPILE_DATA
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LeanVmInstruction(pub UpstreamInstruction);

impl LeanVmInstruction {
    /// Returns the 12 instruction column values matching the upstream encoding:
    /// [operand_A, operand_B, operand_C, flag_A, flag_B, flag_C, flag_C_fp, flag_AB_fp,
    ///  MUL, JUMP, AUX, PRECOMPILE_DATA]
    pub fn instruction_columns(&self) -> [KoalaBearField; 12] {
        let mut cols = [KoalaBearField::from(0u64); 12];

        match &self.0 {
            UpstreamInstruction::Computation {
                operation,
                arg_a,
                arg_c,
                res,
            } => {
                // nu_a ← arg_a (input 1), nu_b ← res (result), nu_c ← arg_c (input 2)
                set_nu_a(&mut cols, arg_a);
                set_nu_b(&mut cols, res);
                set_nu_c(&mut cols, arg_c);
                match operation {
                    Operation::Add => cols[10] = KoalaBearField::from(1u64), // AUX=1
                    Operation::Mul => cols[8] = KoalaBearField::from(1u64),  // MUL=1
                }
            }
            UpstreamInstruction::Deref {
                shift_0,
                shift_1,
                res,
            } => {
                cols[0] = KoalaBearField::from(*shift_0 as u64); // operand_A = shift_0
                cols[1] = KoalaBearField::from(*shift_1 as u64); // operand_B = shift_1
                cols[4] = KoalaBearField::from(1u64); // flag_B = 1 (shift_1 is immediate)
                set_nu_c(&mut cols, res);
                cols[10] = KoalaBearField::from(2u64); // AUX=2
            }
            UpstreamInstruction::Jump {
                condition,
                label: _,
                dest,
                updated_fp,
            } => {
                // nu_a ← condition, nu_b ← dest, nu_c ← updated_fp
                set_nu_a(&mut cols, condition);
                set_nu_b(&mut cols, dest);
                set_nu_c(&mut cols, updated_fp);
                cols[9] = KoalaBearField::from(1u64); // JUMP=1
            }
            UpstreamInstruction::Precompile { .. } => {
                unimplemented!("Precompile instruction encoding")
            }
        }

        cols
    }
}

/// Convert a lean_vm field element to a powdr field element.
fn lean_to_powdr(kb: KB) -> KoalaBearField {
    KoalaBearField::from(kb.as_canonical_u32() as u64)
}

fn set_nu_a(cols: &mut [KoalaBearField; 12], operand: &MemOrConstant) {
    match operand {
        MemOrConstant::Constant(c) => {
            cols[0] = lean_to_powdr(*c); // operand_A
            cols[3] = KoalaBearField::from(1u64); // flag_A = 1
        }
        MemOrConstant::MemoryAfterFp { offset } => {
            cols[0] = KoalaBearField::from(*offset as u64); // operand_A
        }
    }
}

fn set_nu_b(cols: &mut [KoalaBearField; 12], operand: &MemOrConstant) {
    match operand {
        MemOrConstant::Constant(c) => {
            cols[1] = lean_to_powdr(*c); // operand_B
            cols[4] = KoalaBearField::from(1u64); // flag_B = 1
        }
        MemOrConstant::MemoryAfterFp { offset } => {
            cols[1] = KoalaBearField::from(*offset as u64); // operand_B
        }
    }
}

fn set_nu_c(cols: &mut [KoalaBearField; 12], operand: &MemOrFpOrConstant) {
    match operand {
        MemOrFpOrConstant::Constant(c) => {
            cols[2] = lean_to_powdr(*c); // operand_C
            cols[5] = KoalaBearField::from(1u64); // flag_C = 1
        }
        MemOrFpOrConstant::MemoryAfterFp { offset } => {
            cols[2] = KoalaBearField::from(*offset as u64); // operand_C
        }
        MemOrFpOrConstant::FpRelative { offset } => {
            cols[2] = KoalaBearField::from(*offset as u64); // operand_C
            cols[6] = KoalaBearField::from(1u64); // flag_C_fp = 1
        }
    }
}

impl PcStep for LeanVmInstruction {
    fn pc_step() -> u32 {
        1
    }
}

impl Display for LeanVmInstruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.0, f)
    }
}

impl Instruction<KoalaBearField> for LeanVmInstruction {
    fn pc_lookup_row(&self, pc: u64) -> Vec<KoalaBearField> {
        let mut row: Vec<KoalaBearField> = self.instruction_columns().to_vec();
        row.push(KoalaBearField::from(pc));
        row
    }
}

// Serde: the upstream Instruction doesn't derive Serialize/Deserialize,
// so we implement them via the instruction column representation.
// TODO: Implement proper structured serialization once the upstream type supports it.
impl Serialize for LeanVmInstruction {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        use serde::ser::SerializeStruct;
        let cols = self.instruction_columns();
        let mut state = serializer.serialize_struct("LeanVmInstruction", 2)?;
        state.serialize_field("columns", &cols)?;
        state.end()
    }
}

impl<'de> Deserialize<'de> for LeanVmInstruction {
    fn deserialize<D: serde::Deserializer<'de>>(_deserializer: D) -> Result<Self, D::Error> {
        unimplemented!("LeanVmInstruction deserialization not yet implemented")
    }
}

// Builder functions for tests

/// Create an ADD instruction: res = arg_a + arg_c
///
/// Arguments are memory offsets from fp (or immediate values if the corresponding flag is true).
/// - `res`: result destination
/// - `arg_a`: first input
/// - `arg_c`: second input
pub fn add(
    res: usize,
    arg_a: usize,
    arg_c: usize,
    flag_res: bool,
    flag_a: bool,
    flag_c: bool,
) -> LeanVmInstruction {
    LeanVmInstruction(UpstreamInstruction::Computation {
        operation: Operation::Add,
        arg_a: make_mem_or_const(arg_a, flag_a),
        arg_c: make_mem_or_fp_or_const(arg_c, flag_c, false),
        res: make_mem_or_const(res, flag_res),
    })
}

/// Create a MUL instruction: res = arg_a * arg_c
pub fn mul(
    res: usize,
    arg_a: usize,
    arg_c: usize,
    flag_res: bool,
    flag_a: bool,
    flag_c: bool,
) -> LeanVmInstruction {
    LeanVmInstruction(UpstreamInstruction::Computation {
        operation: Operation::Mul,
        arg_a: make_mem_or_const(arg_a, flag_a),
        arg_c: make_mem_or_fp_or_const(arg_c, flag_c, false),
        res: make_mem_or_const(res, flag_res),
    })
}

/// Create a DEREF instruction: res = m[m[fp + shift_0] + shift_1]
pub fn deref(
    shift_0: usize,
    shift_1: usize,
    res: usize,
    flag_c: bool,
    flag_c_fp: bool,
) -> LeanVmInstruction {
    LeanVmInstruction(UpstreamInstruction::Deref {
        shift_0,
        shift_1,
        res: make_mem_or_fp_or_const(res, flag_c, flag_c_fp),
    })
}

/// Create a JUMP instruction: if condition != 0 then pc=dest, fp=updated_fp
pub fn jump(
    condition: usize,
    dest: usize,
    updated_fp: usize,
    flag_a: bool,
    flag_b: bool,
    flag_c: bool,
    flag_c_fp: bool,
) -> LeanVmInstruction {
    LeanVmInstruction(UpstreamInstruction::Jump {
        condition: make_mem_or_const(condition, flag_a),
        label: Label::custom("jump_target"),
        dest: make_mem_or_const(dest, flag_b),
        updated_fp: make_mem_or_fp_or_const(updated_fp, flag_c, flag_c_fp),
    })
}

fn make_mem_or_const(value: usize, is_const: bool) -> MemOrConstant {
    if is_const {
        MemOrConstant::Constant(KB::from_usize(value))
    } else {
        MemOrConstant::MemoryAfterFp { offset: value }
    }
}

fn make_mem_or_fp_or_const(value: usize, is_const: bool, is_fp: bool) -> MemOrFpOrConstant {
    if is_const {
        MemOrFpOrConstant::Constant(KB::from_usize(value))
    } else if is_fp {
        MemOrFpOrConstant::FpRelative { offset: value }
    } else {
        MemOrFpOrConstant::MemoryAfterFp { offset: value }
    }
}
