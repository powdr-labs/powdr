use std::collections::BTreeMap;

use itertools::Itertools;
use num_traits::identities::{One, Zero};
use powdr_constraint_solver::{
    constraint_system::{
        BusInteraction, BusInteractionHandler, ConstraintSystem, DefaultBusInteractionHandler,
    },
    grouped_expression::GroupedExpression,
    indexed_constraint_system::apply_substitutions,
    range_constraint::RangeConstraint,
    solver::{solve_system, Error},
    symbolic_expression::SymbolicExpression,
    test_utils::{constant, var, Qse},
};
use powdr_number::{FieldElement, GoldilocksField, LargeInt};
use test_log::test;

use pretty_assertions::assert_eq;

pub type Var = &'static str;

pub type QuadraticSymbolicExpression<T, V> = GroupedExpression<SymbolicExpression<T, V>, V>;

pub fn assert_solve_result<B: BusInteractionHandler<GoldilocksField>>(
    system: ConstraintSystem<SymbolicExpression<GoldilocksField, Var>, Var>,
    bus_interaction_handler: B,
    expected_assignments: Vec<(Var, GoldilocksField)>,
) {
    let final_state = solve_system(system, bus_interaction_handler).unwrap();
    let expected_final_state = expected_assignments.into_iter().collect();
    assert_expected_state(final_state.assignments, expected_final_state);
}

fn assert_expected_state(
    final_state: impl IntoIterator<Item = (Var, QuadraticSymbolicExpression<GoldilocksField, Var>)>,
    expected_final_state: BTreeMap<Var, GoldilocksField>,
) {
    let final_state = final_state.into_iter().collect::<BTreeMap<_, _>>();
    assert_eq!(
        final_state.keys().collect::<Vec<_>>(),
        expected_final_state.keys().collect::<Vec<_>>(),
        "Different set of variables"
    );

    let mut error = false;
    for (variable, value) in expected_final_state {
        // Compare string representation, so that range constraints are ignored.
        if final_state[variable].to_string() != value.to_string() {
            log::error!("Mismatch for variable {variable}:");
            log::error!("  Expected: {value}");
            log::error!("  Actual:   {}", final_state[variable]);
            error = true;
        }
    }
    assert!(!error, "Final state does not match expected state");
}

#[test]
fn single_variable() {
    assert_solve_result(
        ConstraintSystem {
            algebraic_constraints: vec![var("x") - constant(5)],
            bus_interactions: vec![],
        },
        DefaultBusInteractionHandler::default(),
        vec![("x", 5.into())],
    );
}

#[test]
fn concretely_solvable() {
    let constraint_system = ConstraintSystem {
        algebraic_constraints: vec![
            var("a") - constant(2),
            var("b") - constant(3),
            // c = a * b = 6
            var("c") - var("a") * var("b"),
            // d = c * 4 - a = 22
            var("d") - (var("c") * constant(4) - var("a")),
        ],
        bus_interactions: vec![],
    };
    assert_solve_result(
        constraint_system,
        DefaultBusInteractionHandler::default(),
        vec![
            ("a", 2.into()),
            ("b", 3.into()),
            ("c", 6.into()),
            ("d", 22.into()),
        ],
    );
}

#[test]
fn bit_decomposition() {
    let constraint_system = ConstraintSystem {
        algebraic_constraints: vec![
            // 4 bit-constrained variables:
            var("b0") * (var("b0") - constant(1)),
            var("b1") * (var("b1") - constant(1)),
            var("b2") * (var("b2") - constant(1)),
            var("b3") * (var("b3") - constant(1)),
            // Bit-decomposition of a concrete value:
            var("b0") + var("b1") * constant(2) + var("b2") * constant(4) + var("b3") * constant(8)
                - constant(0b1110),
        ],
        bus_interactions: vec![],
    };

    assert_solve_result(
        constraint_system,
        DefaultBusInteractionHandler::default(),
        vec![
            ("b0", 0.into()),
            ("b1", 1.into()),
            ("b2", 1.into()),
            ("b3", 1.into()),
        ],
    );
}

const BYTE_BUS_ID: u64 = 42;
const XOR_BUS_ID: u64 = 43;

struct TestBusInteractionHandler;
impl BusInteractionHandler<GoldilocksField> for TestBusInteractionHandler {
    fn handle_bus_interaction(
        &self,
        bus_interaction: BusInteraction<RangeConstraint<GoldilocksField>>,
    ) -> BusInteraction<RangeConstraint<GoldilocksField>> {
        let (Some(bus_id), Some(multiplicity)) = (
            bus_interaction.bus_id.try_to_single_value(),
            bus_interaction.multiplicity.try_to_single_value(),
        ) else {
            return bus_interaction;
        };

        if multiplicity.is_zero() {
            return bus_interaction;
        }

        assert!(multiplicity.is_one(), "Only expected send interactions");
        let byte_constraint = RangeConstraint::from_mask(0xffu32);
        let payload_constraints = match bus_id.to_integer().try_into_u64().unwrap() {
            BYTE_BUS_ID => {
                assert_eq!(bus_interaction.payload.len(), 1);
                vec![byte_constraint]
            }
            XOR_BUS_ID => {
                assert_eq!(bus_interaction.payload.len(), 3);
                if let (Some(a), Some(b)) = (
                    bus_interaction.payload[0].try_to_single_value(),
                    bus_interaction.payload[1].try_to_single_value(),
                ) {
                    // Both inputs are known, can compute result concretely
                    let result = GoldilocksField::from(
                        a.to_integer().try_into_u64().unwrap()
                            ^ b.to_integer().try_into_u64().unwrap(),
                    );
                    vec![
                        bus_interaction.payload[0].clone(),
                        bus_interaction.payload[1].clone(),
                        RangeConstraint::from_value(result),
                    ]
                } else {
                    vec![byte_constraint; 3]
                }
            }
            _ => {
                panic!("Unexpected bus ID: {bus_id}");
            }
        };
        BusInteraction {
            payload: payload_constraints,
            ..bus_interaction
        }
    }
}

fn send(
    bus_id: u64,
    payload: Vec<QuadraticSymbolicExpression<GoldilocksField, Var>>,
) -> BusInteraction<QuadraticSymbolicExpression<GoldilocksField, Var>> {
    BusInteraction {
        multiplicity: constant(1),
        bus_id: constant(bus_id),
        payload,
    }
}

#[test]
fn byte_decomposition() {
    let constraint_system = ConstraintSystem {
        algebraic_constraints: vec![
            // Byte-decomposition of a concrete value:
            var("b0")
                + var("b1") * constant(1 << 8)
                + var("b2") * constant(1 << 16)
                + var("b3") * constant(1 << 24)
                - constant(0xabcdef12),
        ],
        // Byte range constraints on b0..3
        bus_interactions: (0..4)
            .map(|i| send(BYTE_BUS_ID, vec![var(format!("b{i}").leak())]))
            .collect(),
    };

    assert_solve_result(
        constraint_system,
        TestBusInteractionHandler,
        vec![
            ("b0", 0x12.into()),
            ("b1", 0xef.into()),
            ("b2", 0xcd.into()),
            ("b3", 0xab.into()),
        ],
    );
}

#[test]
fn xor() {
    let constraint_system = ConstraintSystem {
        algebraic_constraints: vec![
            // a and b are the byte decomposition of 0xa00b
            // Note that solving this requires range constraints on a and b
            constant(1 << 8) * var("a") + var("b") - constant(0xa00b),
        ],
        // Send (a, b, c) to the XOR table.
        // Initially, this should return the required range constraints for a and b.
        // Once a and b are known concretely, c can be computed concretely as well.
        bus_interactions: vec![send(XOR_BUS_ID, vec![var("a"), var("b"), var("c")])],
    };

    assert_solve_result(
        constraint_system,
        TestBusInteractionHandler,
        vec![("a", 0xa0.into()), ("b", 0x0b.into()), ("c", 0xab.into())],
    );
}

#[test]
fn xor_invalid() {
    let constraint_system = ConstraintSystem {
        algebraic_constraints: vec![
            var("a") - constant(0xa0),
            var("b") - constant(0x0b),
            var("c") - constant(0xff),
        ],
        // Send (a, b, c) to the XOR table.
        // Note that this violates the bus rules, because 0xa0 ^ 0x0b != 0xff.
        bus_interactions: vec![send(XOR_BUS_ID, vec![var("a"), var("b"), var("c")])],
    };

    match solve_system(constraint_system, TestBusInteractionHandler) {
        Err(e) => assert_eq!(e, Error::BusInteractionError),
        _ => panic!("Expected error!"),
    }
}

#[test]
fn add_with_carry() {
    // This tests a case of equivalent constraints that appear in the
    // way "add with carry" is performed in openvm.
    // X and Y end up being equivalent because they are both either
    // A or A - 256, depending on whether the value of A is between
    // 0 and 255 or not.
    // A is the result of an addition with carry.
    let constraint_system = ConstraintSystem {
        algebraic_constraints: vec![
            (var("X") * constant(7) - var("A") * constant(7) + constant(256) * constant(7))
                * (var("X") * constant(7) - var("A") * constant(7)),
            (var("Y") - var("A") + constant(256)) * (var("Y") - var("A")),
        ],
        // Byte range constraints on X and Y
        bus_interactions: vec![
            send(BYTE_BUS_ID, vec![var("X")]),
            send(BYTE_BUS_ID, vec![var("Y")]),
        ],
    };

    let final_state = solve_system(constraint_system.clone(), TestBusInteractionHandler).unwrap();
    let final_state = apply_substitutions(constraint_system, final_state.assignments)
        .algebraic_constraints
        .iter()
        .format("\n")
        .to_string();
    assert_eq!(
        final_state,
        "(7 * A - 7 * X - 1792) * (7 * A - 7 * X)
(A - X - 256) * (A - X)"
    );
}

#[test]
fn one_hot_flags() {
    let constraint_system = ConstraintSystem {
        algebraic_constraints: vec![
            // Boolean flags
            var("flag0") * (var("flag0") - constant(1)),
            var("flag1") * (var("flag1") - constant(1)),
            var("flag2") * (var("flag2") - constant(1)),
            var("flag3") * (var("flag3") - constant(1)),
            // Exactly one flag is active
            var("flag0") + var("flag1") + var("flag2") + var("flag3") - constant(1),
            // Flag 2 is active
            var("flag0") * constant(0)
                + var("flag1") * constant(1)
                + var("flag2") * constant(2)
                + var("flag3") * constant(3)
                - constant(2),
        ],
        bus_interactions: vec![],
    };

    // This can be solved via backtracking: There are 16 possible assignments
    // for the 4 flags, but only 1 of them satisfies all the constraints.
    assert_solve_result(
        constraint_system,
        DefaultBusInteractionHandler::default(),
        vec![
            ("flag0", 0.into()),
            ("flag1", 0.into()),
            ("flag2", 1.into()),
            ("flag3", 0.into()),
        ],
    );
}

#[test]
fn binary_flags() {
    let bit_to_expression = |bit, var| match bit {
        true => var,
        false => constant(1) - var,
    };
    let index_to_expression = |i: usize| -> Qse {
        (0..3)
            .map(move |j| bit_to_expression(i & (1 << j) != 0, var(format!("flag{j}").leak())))
            .fold(constant(1), |acc, x| acc * x)
    };
    let constraint_system = ConstraintSystem {
        algebraic_constraints: vec![
            // Boolean flags
            var("flag0") * (var("flag0") - constant(1)),
            var("flag1") * (var("flag1") - constant(1)),
            var("flag2") * (var("flag2") - constant(1)),
            index_to_expression(0b000) * constant(101)
                + index_to_expression(0b001) * constant(102)
                + index_to_expression(0b010) * constant(103)
                + index_to_expression(0b011) * constant(104)
                + index_to_expression(0b100) * constant(105)
                + index_to_expression(0b101) * constant(106)
                + index_to_expression(0b110) * constant(107)
                + index_to_expression(0b111) * constant(108)
                - constant(104),
        ],
        bus_interactions: vec![],
    };

    assert_solve_result(
        constraint_system,
        DefaultBusInteractionHandler::default(),
        vec![
            ("flag0", 1.into()),
            ("flag1", 1.into()),
            ("flag2", 0.into()),
        ],
    );
}

#[test]
fn ternary_flags() {
    // Implementing this logic in the OpenVM load/store chip:
    // https://github.com/openvm-org/openvm/blob/v1.2.0/extensions/rv32im/circuit/src/loadstore/core.rs#L110-L139
    let two_inv = Qse::from_number(GoldilocksField::one() / GoldilocksField::from(2));
    let neg_one = Qse::from_number(-GoldilocksField::one());
    let sum = var("flag0") + var("flag1") + var("flag2") + var("flag3");
    // The flags must be 0, 1, or 2, and their sum must be 1 or 2.
    // Given these constraints, there are 14 possible assignments. The following
    // expressions evaluate to 1 for exactly one of them, and otherwise to 0:
    let cases = vec![
        // (2, 0, 0, 0), (0, 2, 0, 0), (0, 0, 2, 0), (0, 0, 0, 2)
        var("flag0") * (var("flag0") - constant(1)) * two_inv.clone(),
        var("flag1") * (var("flag1") - constant(1)) * two_inv.clone(),
        var("flag2") * (var("flag2") - constant(1)) * two_inv.clone(),
        var("flag3") * (var("flag3") - constant(1)) * two_inv.clone(),
        // (1, 0, 0, 0), (0, 1, 0, 0), (0, 0, 1, 0), (0, 0, 0, 1)
        var("flag0") * (sum.clone() - constant(2)) * neg_one.clone(),
        var("flag1") * (sum.clone() - constant(2)) * neg_one.clone(),
        var("flag2") * (sum.clone() - constant(2)) * neg_one.clone(),
        var("flag3") * (sum.clone() - constant(2)) * neg_one.clone(),
        // (1, 1, 0, 0), (1, 0, 1, 0), (1, 0, 0, 1), (0, 1, 1, 0), (0, 1, 0, 1), (0, 0, 1, 1)
        var("flag0") * var("flag1"),
        var("flag0") * var("flag2"),
        var("flag0") * var("flag3"),
        var("flag1") * var("flag2"),
        var("flag1") * var("flag3"),
        var("flag2") * var("flag3"),
    ];
    let constraint_system = ConstraintSystem {
        algebraic_constraints: vec![
            // All flags are either 0, 1, or 2.
            var("flag0") * (var("flag0") - constant(1)) * (var("flag0") - constant(2)),
            var("flag1") * (var("flag1") - constant(1)) * (var("flag1") - constant(2)),
            var("flag2") * (var("flag2") - constant(1)) * (var("flag2") - constant(2)),
            var("flag3") * (var("flag3") - constant(1)) * (var("flag3") - constant(2)),
            // The sum of flags is either 1 or 2.
            (sum.clone() - constant(1)) * (sum.clone() - constant(2)),
            // Of the expressions in `cases`, exactly one must evaluate to 1.
            // From this constraint, it can be derived that it must be one of case 3, 4, 5, or 6.
            cases[0].clone() * constant(1)
                + (cases[1].clone() + cases[2].clone()) * constant(2)
                + (cases[3].clone() + cases[4].clone() + cases[5].clone() + cases[6].clone())
                    * constant(3)
                + cases[7].clone() * constant(4)
                + (cases[8].clone() + cases[9].clone()) * constant(5)
                + (cases[10].clone() + cases[11].clone() + cases[12].clone() + cases[13].clone())
                    * constant(6)
                - constant(3),
            // We don't know which case is active, but for any of the cases that it could be,
            // is_load would be 1, so we should be able to solve for it.
            var("is_load")
                - (cases[0].clone()
                    + cases[1].clone()
                    + cases[2].clone()
                    + cases[3].clone()
                    + cases[4].clone()
                    + cases[5].clone()
                    + cases[6].clone()),
        ],
        bus_interactions: vec![],
    };

    assert_solve_result(
        constraint_system,
        DefaultBusInteractionHandler::default(),
        vec![("is_load", 1.into())],
    );
}
