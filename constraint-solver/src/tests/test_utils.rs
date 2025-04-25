use std::collections::BTreeMap;

use powdr_number::GoldilocksField;

use crate::{quadratic_symbolic_expression::QuadraticSymbolicExpression, solver::Solver};

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
    init_logging();

    // Reverse to make sure several passes are necessary
    constraints.reverse();

    let final_state = Solver::new(constraints).solve().unwrap();
    let expected_final_state = expected_assignments.into_iter().collect();
    assert_expected_state(final_state.assignments, expected_final_state);
}

fn init_logging() {
    static INIT: std::sync::Once = std::sync::Once::new();
    INIT.call_once(|| {
        env_logger::Builder::from_env(env_logger::Env::default().default_filter_or("trace"))
            .is_test(true)
            .init();
    });
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
