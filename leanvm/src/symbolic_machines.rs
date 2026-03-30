//! Builds SymbolicMachine representations for each LeanVM instruction type.
//!
//! Each machine encodes the AIR constraints and bus interactions for a single instruction.
//! Columns are numbered 0..19 (20 committed columns from the spec).

use std::sync::Arc;

use powdr_autoprecompiles::expression::{AlgebraicExpression, AlgebraicReference};
use powdr_autoprecompiles::symbolic_machine::{
    SymbolicBusInteraction, SymbolicConstraint, SymbolicMachine,
};
use powdr_number::BabyBearField;

use crate::instruction::LeanVmOpcode;

pub const EXEC_BUS_ID: u64 = 0;
pub const MEMORY_BUS_ID: u64 = 1;
pub const PC_LOOKUP_BUS_ID: u64 = 2;

// Column indices
const PC: u64 = 0;
const FP: u64 = 1;
const ADDR_A: u64 = 2;
const ADDR_B: u64 = 3;
const ADDR_C: u64 = 4;
const VALUE_A: u64 = 5;
const VALUE_B: u64 = 6;
const VALUE_C: u64 = 7;
const OPERAND_A: u64 = 8;
const OPERAND_B: u64 = 9;
const OPERAND_C: u64 = 10;
const FLAG_A: u64 = 11;
const FLAG_B: u64 = 12;
const FLAG_C: u64 = 13;
const FLAG_C_FP: u64 = 14;
const FLAG_AB_FP: u64 = 15;
// Opcode selector columns
const COL_MUL: u64 = 16;
const COL_JUMP: u64 = 17;
const COL_AUX: u64 = 18;
const PRECOMPILE_DATA: u64 = 19;

type Expr = AlgebraicExpression<BabyBearField>;

fn col(id: u64, name: &str) -> Expr {
    AlgebraicExpression::Reference(AlgebraicReference {
        name: Arc::new(name.to_string()),
        id,
    })
}

fn num(v: u64) -> Expr {
    AlgebraicExpression::from(BabyBearField::from(v))
}

fn one() -> Expr {
    num(1)
}

/// Build the resolved operand expressions nu_A, nu_B, nu_C.
fn build_nus() -> (Expr, Expr, Expr) {
    let fp = col(FP, "fp");
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

    // nu_A = flag_A * operand_A + (1 - flag_A - flag_AB_fp) * value_A + flag_AB_fp * (fp + operand_A)
    let nu_a = flag_a.clone() * operand_a.clone()
        + (one() - flag_a - flag_ab_fp.clone()) * value_a
        + flag_ab_fp.clone() * (fp.clone() + operand_a);

    // nu_B = flag_B * operand_B + (1 - flag_B - flag_AB_fp) * value_B + flag_AB_fp * (fp + operand_B)
    let nu_b = flag_b.clone() * operand_b.clone()
        + (one() - flag_b - flag_ab_fp.clone()) * value_b
        + flag_ab_fp * (fp.clone() + operand_b);

    // nu_C = flag_C * operand_C + (1 - flag_C - flag_C_fp) * value_C + flag_C_fp * (fp + operand_C)
    let nu_c = flag_c.clone() * operand_c.clone()
        + (one() - flag_c - flag_c_fp.clone()) * value_c
        + flag_c_fp * (fp + operand_c);

    (nu_a, nu_b, nu_c)
}

/// Address constraints: when a memory read is needed, addr must equal fp + operand.
fn address_constraints() -> Vec<SymbolicConstraint<BabyBearField>> {
    let fp = col(FP, "fp");
    let flag_a = col(FLAG_A, "flag_A");
    let flag_b = col(FLAG_B, "flag_B");
    let flag_c = col(FLAG_C, "flag_C");
    let flag_c_fp = col(FLAG_C_FP, "flag_C_fp");
    let flag_ab_fp = col(FLAG_AB_FP, "flag_AB_fp");

    vec![
        // (1 - flag_A - flag_AB_fp) * (addr_A - (fp + operand_A)) = 0
        ((one() - flag_a - flag_ab_fp.clone())
            * (col(ADDR_A, "addr_A") - (fp.clone() + col(OPERAND_A, "operand_A"))))
        .into(),
        // (1 - flag_B - flag_AB_fp) * (addr_B - (fp + operand_B)) = 0
        ((one() - flag_b - flag_ab_fp)
            * (col(ADDR_B, "addr_B") - (fp.clone() + col(OPERAND_B, "operand_B"))))
        .into(),
        // (1 - flag_C - flag_C_fp) * (addr_C - (fp + operand_C)) = 0
        ((one() - flag_c - flag_c_fp)
            * (col(ADDR_C, "addr_C") - (fp + col(OPERAND_C, "operand_C"))))
        .into(),
    ]
}

/// Memory bus interactions: 3 lookups per row.
fn memory_bus_interactions() -> Vec<SymbolicBusInteraction<BabyBearField>> {
    vec![
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
    ]
}

/// PC lookup bus interaction.
fn pc_lookup_interaction() -> SymbolicBusInteraction<BabyBearField> {
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
    }
}

/// Execution bus interactions for non-branching instructions (ADD, MUL, DEREF):
/// receive (pc, fp), send (pc+1, fp)
fn exec_bus_sequential() -> Vec<SymbolicBusInteraction<BabyBearField>> {
    let pc = col(PC, "pc");
    let fp = col(FP, "fp");
    let minus_one: Expr = AlgebraicExpression::from(BabyBearField::from(-1i64));

    vec![
        // Receive current state
        SymbolicBusInteraction {
            id: EXEC_BUS_ID,
            mult: minus_one,
            args: vec![pc.clone(), fp.clone()],
        },
        // Send next state: pc+1, fp unchanged
        SymbolicBusInteraction {
            id: EXEC_BUS_ID,
            mult: one(),
            args: vec![pc + one(), fp],
        },
    ]
}

pub fn build_machine(opcode: LeanVmOpcode) -> SymbolicMachine<BabyBearField> {
    match opcode {
        LeanVmOpcode::Add => build_add_machine(),
        LeanVmOpcode::Mul => build_mul_machine(),
        LeanVmOpcode::Deref => build_deref_machine(),
        LeanVmOpcode::Jump => build_jump_machine(),
    }
}

fn build_add_machine() -> SymbolicMachine<BabyBearField> {
    let (nu_a, nu_b, nu_c) = build_nus();

    let mut constraints = address_constraints();
    // ADD: nu_B = nu_A + nu_C => nu_B - nu_A - nu_C = 0
    constraints.push((nu_b - (nu_a + nu_c)).into());

    let mut bus_interactions = exec_bus_sequential();
    bus_interactions.extend(memory_bus_interactions());
    bus_interactions.push(pc_lookup_interaction());

    SymbolicMachine {
        constraints,
        bus_interactions,
        derived_columns: vec![],
    }
}

fn build_mul_machine() -> SymbolicMachine<BabyBearField> {
    let (nu_a, nu_b, nu_c) = build_nus();

    let mut constraints = address_constraints();
    // MUL: nu_B = nu_A * nu_C => nu_B - nu_A * nu_C = 0
    constraints.push((nu_b - nu_a * nu_c).into());

    let mut bus_interactions = exec_bus_sequential();
    bus_interactions.extend(memory_bus_interactions());
    bus_interactions.push(pc_lookup_interaction());

    SymbolicMachine {
        constraints,
        bus_interactions,
        derived_columns: vec![],
    }
}

fn build_deref_machine() -> SymbolicMachine<BabyBearField> {
    let (_nu_a, _nu_b, nu_c) = build_nus();

    let mut constraints = address_constraints();
    // DEREF: addr_B = value_A + operand_B
    constraints.push(
        (col(ADDR_B, "addr_B") - (col(VALUE_A, "value_A") + col(OPERAND_B, "operand_B"))).into(),
    );
    // DEREF: value_B = nu_C
    constraints.push((col(VALUE_B, "value_B") - nu_c).into());

    let mut bus_interactions = exec_bus_sequential();
    bus_interactions.extend(memory_bus_interactions());
    bus_interactions.push(pc_lookup_interaction());

    SymbolicMachine {
        constraints,
        bus_interactions,
        derived_columns: vec![],
    }
}

fn build_jump_machine() -> SymbolicMachine<BabyBearField> {
    let (nu_a, nu_b, nu_c) = build_nus();

    let pc = col(PC, "pc");
    let fp = col(FP, "fp");

    let mut constraints = address_constraints();
    // nu_A must be boolean: nu_A * (nu_A - 1) = 0
    constraints.push((nu_a.clone() * (nu_a.clone() - one())).into());

    // Execution bus for JUMP: conditional branching
    // next_pc = nu_A * nu_B + (1 - nu_A) * (pc + 1)
    // next_fp = nu_A * nu_C + (1 - nu_A) * fp
    let next_pc = nu_a.clone() * nu_b + (one() - nu_a.clone()) * (pc.clone() + one());
    let next_fp = nu_a.clone() * nu_c + (one() - nu_a) * fp.clone();

    let minus_one: Expr = AlgebraicExpression::from(BabyBearField::from(-1i64));
    let bus_interactions = vec![
        // Receive current state
        SymbolicBusInteraction {
            id: EXEC_BUS_ID,
            mult: minus_one,
            args: vec![pc, fp],
        },
        // Send next state
        SymbolicBusInteraction {
            id: EXEC_BUS_ID,
            mult: one(),
            args: vec![next_pc, next_fp],
        },
    ];

    let mut all_bus = bus_interactions;
    all_bus.extend(memory_bus_interactions());
    all_bus.push(pc_lookup_interaction());

    SymbolicMachine {
        constraints,
        bus_interactions: all_bus,
        derived_columns: vec![],
    }
}
