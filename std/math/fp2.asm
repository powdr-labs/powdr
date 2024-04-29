use std::math::ff::inverse;
use std::convert::fe;
use std::convert::int;
use std::convert::expr;
use std::field::modulus;
use std::prover::eval;


enum Fp2Expr {
    Fp2(expr, expr)
}

enum Fp2Value {
    Fp2(fe, fe)
}

let zero_expr = Fp2Expr::Fp2(0, 0);
let one_expr = Fp2Expr::Fp2(1, 0);

let zero_value = Fp2Value::Fp2(0, 0);
let one_value = Fp2Value::Fp2(1, 0);

let add_ext = |a, b| match (a, b) {
    (Fp2Expr::Fp2(a0, a1), Fp2Expr::Fp2(b0, b1)) => Fp2Expr::Fp2(
        a0 + b0,
        a1 + b1
    )
};

let sub_ext = |a, b| match (a, b) {
    (Fp2Expr::Fp2(a0, a1), Fp2Expr::Fp2(b0, b1)) => Fp2Expr::Fp2(
        a0 - b0,
        a1 - b1
    )
};

let mul_ext = |a, b| match (a, b) {
    (Fp2Expr::Fp2(a0, a1), Fp2Expr::Fp2(b0, b1)) => Fp2Expr::Fp2(
        // Multiplication modulo the polynomial x^2 - 7 => x^2 = 7 (mod x^2 - 7)
        // x^2 - 7 is an irreducible polynomial in both the Goldilocks and BN254 field
        // (because 7 is not a square residue in both)
        a0 * b0 + 7 * a1 * b1,
        a1 * b0 + a0 * b1
    )
};


let eval_ext = query |a| match a {
    Fp2Expr::Fp2(a0, a1) => Fp2Value::Fp2(eval(a0), eval(a1))
};

let expr_ext = |a| match a {
    Fp2Value::Fp2(a0, a1) => Fp2Expr::Fp2(expr(a0), expr(a1))
};

let eq_ext = |a, b| match (a, b) {
    (Fp2Value::Fp2(a0, a1), Fp2Value::Fp2(b0, b1)) => (a0 == b0) && (a1 == b1)
};

// Extension field inversion (defined on fe instead of expression)
let inv_field: fe -> fe = |x| fe(inverse(int(x), modulus()));
let inv_ext: Fp2Value -> Fp2Value = |a| match a {
    // The inverse of (a0, a1) is a point (b0, b1) such that:
    // (a0 + a1 * x) (b0 + b1 * x) = 1 (mod x^2 - 7)
    // Multiplying out and plugging in x^2 = 7 yields the following system of linear equations:
    // a0 * b0 + 7 * a1 * b1 = 1
    // a1 * b1 + a0 * b1 = 0
    // Solving for (b0, b1) yields:
    Fp2Value::Fp2(a0, a1) => Fp2Value::Fp2(
        -a0 * inv_field(7 * a1 * a1 - a0 * a0),
        a1 * inv_field(7 * a1 * a1 - a0 * a0)
    )
};

mod test {
    use std::math::fp2::Fp2Expr;
    use std::math::fp2::Fp2Value;
    use std::math::fp2::add_ext;
    use std::math::fp2::sub_ext;
    use std::math::fp2::mul_ext;
    use std::math::fp2::inv_ext;
    use std::math::fp2::eval_ext;
    use std::math::fp2::expr_ext;
    use std::math::fp2::eq_ext;
    use std::math::fp2::zero_expr;
    use std::math::fp2::zero_value;
    use std::math::fp2::one_expr;
    use std::math::fp2::one_value;
    use std::prover::eval;
    use std::check::assert;
    use std::array::map;

    
    let add = query || {
        let test_add = |a, b, c| assert(eq_ext(eval_ext(add_ext(a, b)), c), || "Wrong addition result");

        // Test adding 0
        let _ = test_add(zero_expr, zero_expr, zero_value);
        let _ = test_add(Fp2Expr::Fp2(123, 1234), zero_expr, Fp2Value::Fp2(123, 1234));
        let _ = test_add(zero_expr, Fp2Expr::Fp2(123, 1234), Fp2Value::Fp2(123, 1234));

        // Add arbitrary elements
        let _ = test_add(Fp2Expr::Fp2(123, 1234), Fp2Expr::Fp2(567, 5678), Fp2Value::Fp2(690, 6912));
        test_add(Fp2Expr::Fp2(0xffffffff00000000, 0xffffffff00000000), Fp2Expr::Fp2(3, 4), Fp2Value::Fp2(2, 3))
    };

    let sub = query || {
        let test_sub = |a, b, c| assert(eq_ext(eval_ext(sub_ext(a, b)), c), || "Wrong subtraction result");

        // Test subtracting 0
        let _ = test_sub(zero_expr, zero_expr, zero_value);
        let _ = test_sub(Fp2Expr::Fp2(123, 1234), zero_expr, Fp2Value::Fp2(123, 1234));

        // Subtract arbitrary elements
        let _ = test_sub(Fp2Expr::Fp2(123, 1234), Fp2Expr::Fp2(567, 5678), Fp2Value::Fp2(18446744069414583877, 18446744069414579877));
        test_sub(Fp2Expr::Fp2(0xffffffff00000000, 0xffffffff00000000), Fp2Expr::Fp2(0x100000000, 1), Fp2Value::Fp2(0xfffffffe00000000, 0xfffffffeffffffff))
    };

    let mul = query || {
        let test_mul = |a, b, c| assert(eq_ext(eval_ext(mul_ext(a, b)), c), || "Wrong multiplication result");

        // Test multiplication by 1
        let _ = test_mul(one_expr, one_expr, one_value);
        let _ = test_mul(Fp2Expr::Fp2(123, 1234), one_expr, Fp2Value::Fp2(123, 1234));
        let _ = test_mul(one_expr, Fp2Expr::Fp2(123, 1234), Fp2Value::Fp2(123, 1234));

        // Test multiplication by 0
        let _ = test_mul(Fp2Expr::Fp2(123, 1234), zero_expr, zero_value);
        let _ = test_mul(zero_expr, Fp2Expr::Fp2(123, 1234), zero_value);

        // Multiply arbitrary elements
        test_mul(Fp2Expr::Fp2(123, 1234), Fp2Expr::Fp2(567, 5678), Fp2Value::Fp2(49116305, 1398072))
    };

    let inverse = query || {
        let test_elements = [
            one_expr,
            Fp2Expr::Fp2(123, 1234),
            Fp2Expr::Fp2(0xffffffff00000000, 0xffffffff00000000)
        ];

        map(test_elements, |x| {
            let inv_x = expr_ext(inv_ext(eval_ext(x)));
            let mul_with_inverse = eval_ext(mul_ext(x, inv_x));

            assert(eq_ext(mul_with_inverse, one_value), || "Should be 1")
        })
    };
}