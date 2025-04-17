mod test_utils;

use super::*;
use test_utils::*;

#[test]
fn single_variable() {
    init_logging();
    let constraints = vec![var("x") - constant(5)];
    let final_state = Solver::new(constraints).solve().unwrap();

    let expected_final_state = BTreeMap::from([("x", constant_expr(5))]);
    assert_expected_state(final_state, expected_final_state);
}

#[test]
fn concretely_solvable() {
    init_logging();
    let constraints = [
        var("a") - constant(2),
        var("b") - constant(3),
        // c = a * b = 6
        var("c") - var("a") * var("b"),
        // d = c * 4 - a = 22
        var("d") - (var("c") * constant(4) - var("a")),
    ]
    .into_iter()
    // Reverse to make sure several passes are necessary
    .rev()
    .collect();

    let final_state = Solver::new(constraints).solve().unwrap();
    let expected_final_state = BTreeMap::from([
        ("a", constant_expr(2)),
        ("b", constant_expr(3)),
        ("c", constant_expr(6)),
        ("d", constant_expr(22)),
    ]);
    assert_expected_state(final_state, expected_final_state);
}

#[test]
fn symbolically_solvable() {
    init_logging();
    let constraints = [
        // Like above, but this time `a` is only known at runtime
        var("b") - constant(3),
        var("c") - known("a") * var("b"),
        var("d") - (var("c") * constant(4) - known("a")),
    ]
    .into_iter()
    // Reverse to make sure several passes are necessary
    .rev()
    .collect();

    let final_state = Solver::new(constraints).solve().unwrap();
    let expected_final_state = BTreeMap::from([
        ("b", constant_expr(3)),
        // TODO: This should simplify to `3 * a`
        ("c", -constant_expr(3) * (-var_expr("a"))),
        // TODO: This should simplify to `11 * a`
        ("d", -var_expr("a") + var_expr("c") * constant_expr(4)),
    ]);
    assert_expected_state(final_state, expected_final_state);
}
