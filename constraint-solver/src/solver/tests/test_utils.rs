use std::collections::BTreeMap;

use powdr_number::GoldilocksField;

use crate::{
    quadratic_symbolic_expression::QuadraticSymbolicExpression, range_constraint::RangeConstraint,
    solver::Solver, symbolic_expression::SymbolicExpression,
};

pub type Var = &'static str;
pub type Qse = QuadraticSymbolicExpression<GoldilocksField, Var>;
pub type Expr = SymbolicExpression<GoldilocksField, Var>;

pub fn var(name: Var) -> Qse {
    Qse::from_unknown_variable(name)
}

pub fn known(name: Var) -> Qse {
    Qse::from_known_symbol(name, RangeConstraint::default())
}

pub fn constant(value: u64) -> Qse {
    GoldilocksField::from(value).into()
}

pub fn constant_expr(value: u64) -> SymbolicExpression<GoldilocksField, Var> {
    GoldilocksField::from(value).into()
}

pub fn var_expr(name: Var) -> SymbolicExpression<GoldilocksField, Var> {
    SymbolicExpression::Symbol(name, RangeConstraint::default())
}

pub fn assert_solve_result(mut constraints: Vec<Qse>, expected_final_state: Vec<(Var, Expr)>) {
    init_logging();

    // Reverse to make sure several passes are necessary
    constraints.reverse();

    let final_state = Solver::new(constraints).solve().unwrap();
    let expected_final_state = expected_final_state.into_iter().collect();
    assert_expected_state(final_state, expected_final_state);
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
    final_state: BTreeMap<Var, SymbolicExpression<GoldilocksField, Var>>,
    expected_final_state: BTreeMap<Var, SymbolicExpression<GoldilocksField, Var>>,
) {
    assert_eq!(
        final_state.keys().collect::<Vec<_>>(),
        expected_final_state.keys().collect::<Vec<_>>(),
        "Different set of variables"
    );

    let mut error = false;
    for (variable, expression) in expected_final_state {
        // Compare string representation, so that range constraints are ignored.
        if final_state[variable].to_string() != expression.to_string() {
            log::error!("Mismatch for variable {variable}:");
            log::error!("  Expected: {expression}");
            log::error!("  Actual:   {}", final_state[variable]);
            error = true;
        }
    }
    assert!(!error, "Final state does not match expected state");
}
