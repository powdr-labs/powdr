use std::collections::BTreeMap;

use powdr_constraint_solver::{
    quadratic_symbolic_expression::QuadraticSymbolicExpression, solver::Solver,
};
use powdr_number::GoldilocksField;
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
    mut constraints: Vec<Qse>,
    expected_assignments: Vec<(Var, GoldilocksField)>,
) {
    // Reverse to make sure several passes are necessary
    constraints.reverse();

    let final_state = Solver::new(constraints).solve().unwrap();
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
    assert_solve_result(vec![var("x") - constant(5)], vec![("x", 5.into())]);
}

#[test]
fn concretely_solvable() {
    assert_solve_result(
        vec![
            var("a") - constant(2),
            var("b") - constant(3),
            // c = a * b = 6
            var("c") - var("a") * var("b"),
            // d = c * 4 - a = 22
            var("d") - (var("c") * constant(4) - var("a")),
        ],
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
    assert_solve_result(
        vec![
            // 4 bit-constrained variables:
            var("b0") * (var("b0") - constant(1)),
            var("b1") * (var("b1") - constant(1)),
            var("b2") * (var("b2") - constant(1)),
            var("b3") * (var("b3") - constant(1)),
            // Bit-decomposition of a concrete value:
            var("b0") + var("b1") * constant(2) + var("b2") * constant(4) + var("b3") * constant(8)
                - constant(0b1110),
        ],
        vec![
            ("b0", 0.into()),
            ("b1", 1.into()),
            ("b2", 1.into()),
            ("b3", 1.into()),
        ],
    );
}
