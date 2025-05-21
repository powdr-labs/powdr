use std::collections::{BTreeMap, HashMap};
use std::fmt;
use std::hash::Hash;
use std::{collections::HashSet, fmt::Display};

use itertools::Itertools;
use powdr_ast::analyzed::{
    algebraic_expression_conversion, AlgebraicExpression, AlgebraicReference, Challenge, PolyID,
    PolynomialType,
};
use powdr_autoprecompiles::memory_optimizer::optimize_memory;
use powdr_autoprecompiles::{MemoryBusInteraction, MemoryType, SymbolicMachine};
use powdr_constraint_solver::quadratic_symbolic_expression::RangeConstraintProvider;
use powdr_constraint_solver::range_constraint::RangeConstraint;
use powdr_constraint_solver::{
    quadratic_symbolic_expression::QuadraticSymbolicExpression,
    symbolic_expression::SymbolicExpression,
};
use powdr_number::{BabyBearField, FieldElement, LargeInt};

#[test]
fn load_machine_cbor() {
    let file = std::fs::File::open("tests/keccak_apc_post_opt.cbor").unwrap();
    let reader = std::io::BufReader::new(file);
    let machine: SymbolicMachine<BabyBearField> = serde_cbor::from_reader(reader).unwrap();
    assert_eq!(machine.constraints.len(), 506);
    assert_eq!(machine.bus_interactions.len(), 6485);
}

#[test]
fn analyze_for_memory() {
    let file = std::fs::File::open("tests/keccak_apc_post_opt.cbor").unwrap();
    let reader = std::io::BufReader::new(file);
    let machine: SymbolicMachine<BabyBearField> = serde_cbor::from_reader(reader).unwrap();

    let bus_interactions_before = machine.bus_interactions.len();

    let machine = optimize_memory(machine);

    println!(
        "columns: {}, constraints: {}, bus interactions: {}",
        machine.constraint_columns().len(),
        machine.constraints.len(),
        machine.bus_interactions.len()
    );
    assert_eq!(
        bus_interactions_before - machine.bus_interactions.len(),
        450
    );
}
