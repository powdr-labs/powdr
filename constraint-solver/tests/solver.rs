use std::collections::BTreeMap;

use num_traits::identities::One;
use powdr_constraint_solver::{
    bus_interaction::{BusInteraction, BusInteractionHandler},
    constraint_system::ConstraintSystem,
    quadratic_symbolic_expression::QuadraticSymbolicExpression,
    range_constraint::RangeConstraint,
    solver::Solver,
};
use powdr_number::{FieldElement, GoldilocksField};
use test_log::test;

pub type Var = &'static str;
pub type Qse = QuadraticSymbolicExpression<GoldilocksField, Var>;

pub fn var(name: Var) -> Qse {
    Qse::from_unknown_variable(name)
}

pub fn constant(value: u64) -> Qse {
    GoldilocksField::from(value).into()
}

pub fn assert_solve_result(
    solver: Solver<GoldilocksField, Var>,
    expected_assignments: Vec<(Var, GoldilocksField)>,
) {
    let final_state = solver.solve().unwrap();
    let expected_final_state = expected_assignments.into_iter().collect();
    assert_expected_state(final_state.assignments, expected_final_state);
}

fn assert_expected_state(
    final_state: BTreeMap<Var, GoldilocksField>,
    expected_final_state: BTreeMap<Var, GoldilocksField>,
) {
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
        Solver::new(ConstraintSystem {
            algebraic_constraints: vec![var("x") - constant(5)],
            bus_interactions: vec![],
        }),
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
        Solver::new(constraint_system),
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
        Solver::new(constraint_system),
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

struct TestBusInteractionHandler {}
impl BusInteractionHandler for TestBusInteractionHandler {
    type T = GoldilocksField;
    type V = Var;

    fn handle_bus_interaction(
        &self,
        bus_id: GoldilocksField,
        payload: Vec<RangeConstraint<GoldilocksField>>,
        multiplicity: GoldilocksField,
    ) -> Vec<RangeConstraint<GoldilocksField>> {
        assert!(multiplicity.is_one(), "Only expected send interactions");
        let byte_constraint = RangeConstraint::from_mask(0xffu32);
        match bus_id.to_degree() {
            BYTE_BUS_ID => {
                assert_eq!(payload.len(), 1);
                vec![byte_constraint]
            }
            XOR_BUS_ID => {
                assert_eq!(payload.len(), 3);
                if let (Some(a), Some(b)) = (
                    payload[0].try_to_single_value(),
                    payload[1].try_to_single_value(),
                ) {
                    // Both inputs are known, can compute result concretely
                    let result = GoldilocksField::from(a.to_degree() ^ b.to_degree());
                    vec![
                        payload[0].clone(),
                        payload[1].clone(),
                        RangeConstraint::from_value(result),
                    ]
                } else {
                    vec![byte_constraint; 3]
                }
            }
            _ => {
                panic!("Unexpected bus ID: {bus_id}");
            }
        }
    }
}

fn send(
    bus_id: u64,
    payload: Vec<QuadraticSymbolicExpression<GoldilocksField, Var>>,
) -> BusInteraction<GoldilocksField, Var> {
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

    let solver = Solver::new(constraint_system)
        .with_bus_interaction_handler(Box::new(TestBusInteractionHandler {}));

    assert_solve_result(
        solver,
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

    let solver = Solver::new(constraint_system)
        .with_bus_interaction_handler(Box::new(TestBusInteractionHandler {}));

    assert_solve_result(
        solver,
        vec![("a", 0xa0.into()), ("b", 0x0b.into()), ("c", 0xab.into())],
    );
}
