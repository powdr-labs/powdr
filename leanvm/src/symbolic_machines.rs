//! Builds the single SymbolicMachine for the LeanVM execution table AIR.
//!
//! LeanVM has ONE AIR with 20 committed columns. All 4 instructions (ADD, MUL, DEREF, JUMP)
//! share the same table — opcode selectors (MUL, JUMP, AUX) gate which constraints are active.
//!
//! Compared to the original LeanVM spec which constrains next_pc/next_fp via transition
//! constraints on down columns, we use an execution bus instead:
//!   - Receive (pc, fp) with mult=-1
//!   - Send (next_pc, next_fp) with mult=+1
//!
//! where next_pc/next_fp are algebraic expressions encoding the JUMP conditional logic.

use std::sync::Arc;

use powdr_autoprecompiles::expression::{AlgebraicExpression, AlgebraicReference};
use powdr_autoprecompiles::symbolic_machine::{
    SymbolicBusInteraction, SymbolicConstraint, SymbolicMachine,
};
use powdr_number::KoalaBearField;

pub const EXEC_BUS_ID: u64 = 0;
pub const MEMORY_BUS_ID: u64 = 1;
pub const PC_LOOKUP_BUS_ID: u64 = 2;

// Column indices (matching leanvm_air.rs)
pub const PC: u64 = 0;
pub const FP: u64 = 1;
pub const ADDR_A: u64 = 2;
pub const ADDR_B: u64 = 3;
pub const ADDR_C: u64 = 4;
pub const VALUE_A: u64 = 5;
pub const VALUE_B: u64 = 6;
pub const VALUE_C: u64 = 7;
pub const OPERAND_A: u64 = 8;
pub const OPERAND_B: u64 = 9;
pub const OPERAND_C: u64 = 10;
pub const FLAG_A: u64 = 11;
pub const FLAG_B: u64 = 12;
pub const FLAG_C: u64 = 13;
pub const FLAG_C_FP: u64 = 14;
pub const FLAG_AB_FP: u64 = 15;
pub const COL_MUL: u64 = 16;
pub const COL_JUMP: u64 = 17;
pub const COL_AUX: u64 = 18;
pub const PRECOMPILE_DATA: u64 = 19;

pub const NUM_COLUMNS: u64 = 20;

type Expr = AlgebraicExpression<KoalaBearField>;

fn col(id: u64, name: &str) -> Expr {
    AlgebraicExpression::Reference(AlgebraicReference {
        name: Arc::new(name.to_string()),
        id,
    })
}

fn num(v: u64) -> Expr {
    AlgebraicExpression::from(KoalaBearField::from(v))
}

fn one() -> Expr {
    num(1)
}

fn two() -> Expr {
    num(2)
}

/// Build the single unified SymbolicMachine for LeanVM's execution table AIR.
///
/// This encodes all 13 constraints from the spec (with JUMP's next_pc/next_fp constraints
/// replaced by execution bus interactions), plus memory and PC lookup bus interactions.
pub fn build_execution_machine() -> SymbolicMachine<KoalaBearField> {
    let pc = col(PC, "pc");
    let fp = col(FP, "fp");
    let addr_a = col(ADDR_A, "addr_A");
    let addr_b = col(ADDR_B, "addr_B");
    let addr_c = col(ADDR_C, "addr_C");
    let value_a = col(VALUE_A, "value_A");
    let value_b = col(VALUE_B, "value_B");
    let value_c = col(VALUE_C, "value_C");
    let operand_a = col(OPERAND_A, "operand_A");
    let operand_b = col(OPERAND_B, "operand_B");
    let operand_c = col(OPERAND_C, "operand_C");
    let flag_a = col(FLAG_A, "flag_A");
    let flag_b = col(FLAG_B, "flag_B");
    let flag_c = col(FLAG_C, "flag_C");
    let flag_c_fp = col(FLAG_C_FP, "flag_C_fp");
    let flag_ab_fp = col(FLAG_AB_FP, "flag_AB_fp");
    let mul = col(COL_MUL, "MUL");
    let jump = col(COL_JUMP, "JUMP");
    let aux = col(COL_AUX, "AUX");

    // Derived values
    // (1 - flag_A - flag_AB_fp)
    let one_minus_fa_fabfp = one() - flag_a.clone() - flag_ab_fp.clone();
    // (1 - flag_B - flag_AB_fp)
    let one_minus_fb_fabfp = one() - flag_b.clone() - flag_ab_fp.clone();
    // (1 - flag_C - flag_C_fp)
    let one_minus_fc_fcfp = one() - flag_c.clone() - flag_c_fp.clone();

    // nu_A = flag_A * operand_A + (1 - flag_A - flag_AB_fp) * value_A + flag_AB_fp * (fp + operand_A)
    let nu_a = flag_a.clone() * operand_a.clone()
        + one_minus_fa_fabfp.clone() * value_a.clone()
        + flag_ab_fp.clone() * (fp.clone() + operand_a.clone());

    // nu_B = flag_B * operand_B + (1 - flag_B - flag_AB_fp) * value_B + flag_AB_fp * (fp + operand_B)
    let nu_b = flag_b.clone() * operand_b.clone()
        + one_minus_fb_fabfp.clone() * value_b.clone()
        + flag_ab_fp.clone() * (fp.clone() + operand_b.clone());

    // nu_C = flag_C * operand_C + (1 - flag_C - flag_C_fp) * value_C + flag_C_fp * (fp + operand_C)
    let nu_c = flag_c.clone() * operand_c.clone()
        + one_minus_fc_fcfp.clone() * value_c.clone()
        + flag_c_fp.clone() * (fp.clone() + operand_c.clone());

    // ADD = P1(AUX) = AUX * (2 - AUX)
    let add = aux.clone() * (two() - aux.clone());

    // DEREF = P2(AUX) = AUX * (AUX - 1) / 2
    // Note: In a prime field, /2 is multiplication by the inverse of 2.
    // KoalaBear: inv(2) = (p+1)/2
    let inv2: Expr =
        AlgebraicExpression::from(KoalaBearField::from(1u64) / KoalaBearField::from(2u64));
    let deref = aux.clone() * (aux - one()) * inv2;

    // J = JUMP * nu_A (the "jump-and-condition" selector)
    let j = jump.clone() * nu_a.clone();

    // --- Constraints ---
    let constraints: Vec<SymbolicConstraint<KoalaBearField>> = vec![
        // 1-3: Address constraints
        (one_minus_fa_fabfp * (addr_a - (fp.clone() + operand_a))).into(),
        (one_minus_fb_fabfp * (addr_b.clone() - (fp.clone() + operand_b.clone()))).into(),
        (one_minus_fc_fcfp * (addr_c - (fp.clone() + operand_c))).into(),
        // 4: ADD * (nu_B - (nu_A + nu_C)) = 0
        (add * (nu_b.clone() - (nu_a.clone() + nu_c.clone()))).into(),
        // 5: MUL * (nu_B - nu_A * nu_C) = 0
        (mul * (nu_b - nu_a.clone() * nu_c.clone())).into(),
        // 6-7: DEREF constraints
        (deref.clone() * (addr_b - (value_a + operand_b))).into(),
        (deref * (value_b - nu_c.clone())).into(),
        // 8: J * (nu_A - 1) = 0 (enforces nu_A in {0,1})
        (j.clone() * (nu_a.clone() - one())).into(),
    ];

    // Constraints 9-12 from the original spec constrain next_pc and next_fp.
    // Instead, we encode the next state via the execution bus.
    //
    // next_pc = J * nu_B + (1 - J) * (pc + 1)
    // next_fp = J * nu_C + (1 - J) * fp
    //
    // For non-jump instructions (J=0): next_pc = pc+1, next_fp = fp
    // For jump with condition=1 (J=1): next_pc = nu_B, next_fp = nu_C
    // For jump with condition=0 (J=0): next_pc = pc+1, next_fp = fp (since JUMP*0=0)
    let next_pc = j.clone() * nu_b_for_jump() + (one() - j.clone()) * (pc.clone() + one());
    let next_fp = j.clone() * nu_c_for_jump() + (one() - j) * fp.clone();

    // --- Bus interactions ---
    let minus_one: Expr = AlgebraicExpression::from(KoalaBearField::from(-1i64));

    let bus_interactions = vec![
        // Execution bus: receive current state (pc, fp)
        SymbolicBusInteraction {
            id: EXEC_BUS_ID,
            mult: minus_one,
            args: vec![pc.clone(), fp.clone()],
        },
        // Execution bus: send next state
        SymbolicBusInteraction {
            id: EXEC_BUS_ID,
            mult: one(),
            args: vec![next_pc, next_fp],
        },
        // Memory bus: 3 lookups per row
        SymbolicBusInteraction {
            id: MEMORY_BUS_ID,
            mult: one(),
            args: vec![col(ADDR_A, "addr_A"), col(VALUE_A, "value_A")],
        },
        SymbolicBusInteraction {
            id: MEMORY_BUS_ID,
            mult: one(),
            args: vec![col(ADDR_B, "addr_B"), col(VALUE_B, "value_B")],
        },
        SymbolicBusInteraction {
            id: MEMORY_BUS_ID,
            mult: one(),
            args: vec![col(ADDR_C, "addr_C"), col(VALUE_C, "value_C")],
        },
        // PC lookup
        SymbolicBusInteraction {
            id: PC_LOOKUP_BUS_ID,
            mult: one(),
            args: vec![
                col(OPERAND_A, "operand_A"),
                col(OPERAND_B, "operand_B"),
                col(OPERAND_C, "operand_C"),
                col(FLAG_A, "flag_A"),
                col(FLAG_B, "flag_B"),
                col(FLAG_C, "flag_C"),
                col(FLAG_C_FP, "flag_C_fp"),
                col(FLAG_AB_FP, "flag_AB_fp"),
                col(COL_MUL, "MUL"),
                col(COL_JUMP, "JUMP"),
                col(COL_AUX, "AUX"),
                col(PRECOMPILE_DATA, "PRECOMPILE_DATA"),
                col(PC, "pc"),
            ],
        },
    ];

    SymbolicMachine {
        constraints,
        bus_interactions,
        derived_columns: vec![],
    }
}

/// Rebuild nu_B from scratch (needed for the execution bus send to avoid expression cloning issues).
/// nu_B = flag_B * operand_B + (1 - flag_B - flag_AB_fp) * value_B + flag_AB_fp * (fp + operand_B)
fn nu_b_for_jump() -> Expr {
    let fp = col(FP, "fp");
    let value_b = col(VALUE_B, "value_B");
    let operand_b = col(OPERAND_B, "operand_B");
    let flag_b = col(FLAG_B, "flag_B");
    let flag_ab_fp = col(FLAG_AB_FP, "flag_AB_fp");

    flag_b.clone() * operand_b.clone()
        + (one() - flag_b - flag_ab_fp.clone()) * value_b
        + flag_ab_fp * (fp + operand_b)
}

/// Rebuild nu_C from scratch for the execution bus send.
/// nu_C = flag_C * operand_C + (1 - flag_C - flag_C_fp) * value_C + flag_C_fp * (fp + operand_C)
fn nu_c_for_jump() -> Expr {
    let fp = col(FP, "fp");
    let value_c = col(VALUE_C, "value_C");
    let operand_c = col(OPERAND_C, "operand_C");
    let flag_c = col(FLAG_C, "flag_C");
    let flag_c_fp = col(FLAG_C_FP, "flag_C_fp");

    flag_c.clone() * operand_c.clone()
        + (one() - flag_c - flag_c_fp.clone()) * value_c
        + flag_c_fp * (fp + operand_c)
}
