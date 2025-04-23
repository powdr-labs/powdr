mod test_utils;

use test_utils::*;

#[test]
fn single_variable() {
    assert_solve_result(vec![var("x") - constant(5)], vec![("x", constant_expr(5))]);
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
            ("a", constant_expr(2)),
            ("b", constant_expr(3)),
            ("c", constant_expr(6)),
            ("d", constant_expr(22)),
        ],
    );
}

#[test]
fn symbolically_solvable() {
    assert_solve_result(
        vec![
            // Like above, but this time `a` is only known at runtime
            var("b") - constant(3),
            var("c") - known("a") * var("b"),
            var("d") - (var("c") * constant(4) - known("a")),
        ],
        vec![
            ("b", constant_expr(3)),
            // TODO: This should simplify to `3 * a`
            ("c", -constant_expr(3) * (-var_expr("a"))),
            // TODO: This should simplify to `11 * a`
            ("d", -var_expr("a") + var_expr("c") * constant_expr(4)),
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
            ("b0", constant_expr(0)),
            ("b1", constant_expr(1)),
            ("b2", constant_expr(1)),
            ("b3", constant_expr(1)),
        ],
    );
}
