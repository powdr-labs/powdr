use std::fmt::Display;
use std::hash::Hash;

use crate::{
    algebraic_constraint,
    constraint_system::{BusInteraction, BusInteractionHandler, DefaultBusInteractionHandler},
    grouped_expression::{GroupedExpression, NoRangeConstraints},
    indexed_constraint_system::IndexedConstraintSystem,
    range_constraint::RangeConstraint,
    rule_based_optimizer::driver::rule_based_optimization,
    solver::Solver,
};
use expect_test::expect;
use num_traits::Zero;
use powdr_number::{BabyBearField, FieldElement, LargeInt};

fn assert_zero<T: FieldElement, V: Hash + Eq + Ord + Clone + Display>(
    expr: GroupedExpression<T, V>,
) -> algebraic_constraint::AlgebraicConstraint<GroupedExpression<T, V>> {
    algebraic_constraint::AlgebraicConstraint::assert_zero(expr)
}

fn v(name: &str) -> GroupedExpression<BabyBearField, String> {
    GroupedExpression::from_unknown_variable(name.to_string())
}

fn c(value: i64) -> GroupedExpression<BabyBearField, String> {
    GroupedExpression::from_number(BabyBearField::from(value))
}

fn new_var() -> impl FnMut(&str) -> String {
    let mut counter = 0;
    move |prefix: &str| {
        let name = format!("{prefix}_{counter}");
        counter += 1;
        name
    }
}

fn handle_variable_range_checker<T: FieldElement>(
    payload: &[RangeConstraint<T>],
) -> Vec<RangeConstraint<T>> {
    const MAX_BITS: u64 = 25;
    // See: https://github.com/openvm-org/openvm/blob/v1.0.0/crates/circuits/primitives/src/var_range/bus.rs
    // Expects (x, bits), where `x` is in the range [0, 2^bits - 1]
    let [_x, bits] = payload else {
        panic!("Expected arguments (x, bits)");
    };
    match bits.try_to_single_value() {
        Some(bits_value) if bits_value.to_degree() <= MAX_BITS => {
            let bits_value = bits_value.to_integer().try_into_u64().unwrap();
            let mask = (1u64 << bits_value) - 1;
            vec![RangeConstraint::from_mask(mask), *bits]
        }
        _ => {
            vec![
                RangeConstraint::from_mask((1u64 << MAX_BITS) - 1),
                RangeConstraint::from_range(T::from(0), T::from(MAX_BITS)),
            ]
        }
    }
}

fn try_handle_bus_interaction<T: FieldElement>(
    bus_interaction: &BusInteraction<RangeConstraint<T>>,
) -> Option<BusInteraction<RangeConstraint<T>>> {
    let mult = bus_interaction.multiplicity.try_to_single_value()?;
    if mult == Zero::zero() {
        return None;
    }
    let bus_id = bus_interaction
        .bus_id
        .try_to_single_value()?
        .to_integer()
        .try_into_u64()?;
    let payload_constraints = match bus_id {
        3 => handle_variable_range_checker(&bus_interaction.payload),
        _ => return None,
    };
    Some(BusInteraction {
        payload: payload_constraints,
        ..bus_interaction.clone()
    })
}

#[derive(Clone)]
#[allow(dead_code)]
struct TestBusInteractionHandler;

impl<T: FieldElement> BusInteractionHandler<T> for TestBusInteractionHandler {
    fn handle_bus_interaction(
        &self,
        bus_interaction: BusInteraction<RangeConstraint<T>>,
    ) -> BusInteraction<RangeConstraint<T>> {
        try_handle_bus_interaction(&bus_interaction).unwrap_or(bus_interaction)
    }
}

#[allow(dead_code)]
fn bit_constraint(
    variable: &str,
    bits: u32,
) -> BusInteraction<GroupedExpression<BabyBearField, String>> {
    BusInteraction {
        bus_id: c(3),
        payload: vec![v(variable), c(bits as i64)],
        multiplicity: c(1),
    }
}

#[test]
fn test_rule_based_optimization_empty() {
    let system: IndexedConstraintSystem<BabyBearField, String> = IndexedConstraintSystem::default();
    let optimized_system = rule_based_optimization(
        system,
        NoRangeConstraints,
        DefaultBusInteractionHandler::default(),
        &mut new_var(),
        None,
    );
    assert_eq!(optimized_system.0.system().algebraic_constraints.len(), 0);
}

#[test]
fn test_rule_based_optimization_simple_assignment() {
    let mut system = IndexedConstraintSystem::default();
    let x = v("x");
    system.add_algebraic_constraints([
        assert_zero(x * BabyBearField::from(7) - c(21)),
        assert_zero(v("y") * (v("y") - c(1)) - v("x")),
    ]);
    let optimized_system = rule_based_optimization(
        system,
        NoRangeConstraints,
        DefaultBusInteractionHandler::default(),
        &mut new_var(),
        None,
    );
    expect!["(y) * (y - 1) - 3 = 0"].assert_eq(&optimized_system.0.to_string());
}

#[test]
fn test_rule_split_constraints_based_on_minimal_range() {
    let mut system = IndexedConstraintSystem::default();
    //opcode_sub_flag_21 + 2 * opcode_xor_flag_21 + 3 * opcode_or_flag_21 + 4 * opcode_and_flag_21 = 0
    system.add_algebraic_constraints([assert_zero(
        (v("opcode_sub_flag_21")
            + c(2) * v("opcode_xor_flag_21")
            + c(3) * v("opcode_or_flag_21")
            + c(4) * v("opcode_and_flag_21")),
    )]);
    system.add_bus_interactions([
        bit_constraint("opcode_sub_flag_21", 1),
        bit_constraint("opcode_xor_flag_21", 1),
        bit_constraint("opcode_or_flag_21", 1),
        bit_constraint("opcode_and_flag_21", 1),
    ]);

    let range_constraints = std::collections::HashMap::from([
        ("opcode_sub_flag_21", RangeConstraint::from_mask(0x1u32)),
        ("opcode_xor_flag_21", RangeConstraint::from_mask(0x1u32)),
        ("opcode_or_flag_21", RangeConstraint::from_mask(0x1u32)),
        ("opcode_and_flag_21", RangeConstraint::from_mask(0x1u32)),
    ]);

    let mut solver = crate::solver::new_solver(
        system.clone().into(),
        DefaultBusInteractionHandler::default(),
    );
    for (var, constraint) in range_constraints {
        solver.add_range_constraint(&var.to_string(), constraint);
    }

    let optimized_system = rule_based_optimization(
        system,
        solver,
        DefaultBusInteractionHandler::default(),
        &mut new_var(),
        None,
    );
    assert_eq!(optimized_system.0.system().algebraic_constraints.len(), 0);
}
