use std::collections::BTreeMap;

use powdr_number::GoldilocksField;

use super::{QuadraticSymbolicExpression, RangeConstraint, SymbolicExpression};

pub type Qse = QuadraticSymbolicExpression<GoldilocksField, &'static str>;

pub fn var(name: &'static str) -> Qse {
    Qse::from_unknown_variable(name)
}

pub fn known(name: &'static str) -> Qse {
    Qse::from_known_symbol(name, RangeConstraint::default())
}

pub fn constant(value: u64) -> Qse {
    GoldilocksField::from(value).into()
}

pub fn constant_expr(value: u64) -> SymbolicExpression<GoldilocksField, &'static str> {
    GoldilocksField::from(value).into()
}

pub fn var_expr(name: &'static str) -> SymbolicExpression<GoldilocksField, &'static str> {
    SymbolicExpression::Symbol(name, RangeConstraint::default())
}

pub fn assert_expected_state(
    final_state: BTreeMap<&'static str, SymbolicExpression<GoldilocksField, &'static str>>,
    expected_final_state: BTreeMap<&'static str, SymbolicExpression<GoldilocksField, &'static str>>,
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

pub fn init_logging() {
    static INIT: std::sync::Once = std::sync::Once::new();
    INIT.call_once(|| {
        env_logger::Builder::from_env(env_logger::Env::default().default_filter_or("trace"))
            .is_test(true)
            .init();
    });
}
