use std::convert::fe;
use std::convert::int;
use std::convert::expr;
use std::field::modulus;
use std::prover::eval;

/// An extension field element, represented as a tuple (a0, a1) of expressions (which evaluate to field elements).
/// An element of F_{p^2} is a polynomial a0 + a1 * x (mod x^2 - 7).
/// The polynomial x^2 - 7 is irreducible in both the Goldilocks and BN254 field
/// (which follows from 7 not having a square root in both fields).
enum Fp2<T> {
    Fp2(T, T)
}

let<T: FromLiteral> zero_ext: -> Fp2<T> = || Fp2::Fp2(0, 0);
let<T: FromLiteral> one_ext: -> Fp2<T> = || Fp2::Fp2(1, 0);

/// Extension field addition
let<T: Add> add_ext: Fp2<T>, Fp2<T> -> Fp2<T> = |a, b| match (a, b) {
    (Fp2::Fp2(a0, a1), Fp2::Fp2(b0, b1)) => Fp2::Fp2(
        a0 + b0,
        a1 + b1
    )
};

/// Extension field subtraction
let<T: Sub> sub_ext: Fp2<T>, Fp2<T> -> Fp2<T> = |a, b| match (a, b) {
    (Fp2::Fp2(a0, a1), Fp2::Fp2(b0, b1)) => Fp2::Fp2(
        a0 - b0,
        a1 - b1
    )
};

/// Extension field multiplication
let<T: Add + FromLiteral + Mul> mul_ext: Fp2<T>, Fp2<T> -> Fp2<T> = |a, b| match (a, b) {
    (Fp2::Fp2(a0, a1), Fp2::Fp2(b0, b1)) => Fp2::Fp2(
        // Multiplication modulo the polynomial x^2 - 7. We'll use the fact
        // that x^2 == 7 (mod x^2 - 7), so:
        // (a0 + a1 * x) * (b0 + b1 * x) = a0 * b0 + 7 * a1 * b1 + (a1 * b0 + a0 * b1) * x (mod x^2 - 7)
        a0 * b0 + 7 * a1 * b1,
        a1 * b0 + a0 * b1
    )
};

/// Converts and Fp2<expr> into an Fp2<fe>
let eval_ext: Fp2<expr> -> Fp2<fe> = query |a| match a {
    Fp2::Fp2(a0, a1) => Fp2::Fp2(eval(a0), eval(a1))
};

/// Converts and Fp2<fe> into an Fp2<expr>
let expr_ext: Fp2<fe> -> Fp2<expr> = |a| match a {
    Fp2::Fp2(a0, a1) => Fp2::Fp2(expr(a0), expr(a1))
};

/// Extension field equality
let<T: Eq> eq_ext: Fp2<T>, Fp2<T> -> bool = |a, b| match (a, b) {
    (Fp2::Fp2(a0, a1), Fp2::Fp2(b0, b1)) => (a0 == b0) && (a1 == b1)
};

/// Field inversion (defined on fe instead of int)
let inv_field: fe -> fe = |x| fe(std::math::ff::inverse(int(x), modulus()));

/// Extension field inversion
let inv_ext: Fp2<fe> -> Fp2<fe> = |a| match a {
    // The inverse of (a0, a1) is a point (b0, b1) such that:
    // (a0 + a1 * x) (b0 + b1 * x) = 1 (mod x^2 - 7)
    // Multiplying out and plugging in x^2 = 7 yields the following system of linear equations:
    // a0 * b0 + 7 * a1 * b1 = 1
    // a1 * b0 + a0 * b1 = 0
    // Solving for (b0, b1) yields:
    Fp2::Fp2(a0, a1) => {
        let factor = inv_field(7 * a1 * a1 - a0 * a0);
        Fp2::Fp2(-a0 * factor, a1 * factor)
    }
};

mod test {
    use super::Fp2;
    use super::one_ext;
    use super::zero_ext;
    use super::add_ext;
    use super::sub_ext;
    use super::mul_ext;
    use super::inv_ext;
    use super::eval_ext;
    use super::expr_ext;
    use super::eq_ext;
    use std::check::assert;
    use std::array::map;


    let add = query || {
        let test_add = |a, b, c| assert(eq_ext(eval_ext(add_ext(a, b)), c), || "Wrong addition result");

        // Test adding 0
        let _ = test_add(zero_ext(), zero_ext(), zero_ext());
        let _ = test_add(Fp2::Fp2(123, 1234), zero_ext(), Fp2::Fp2(123, 1234));
        let _ = test_add(zero_ext(), Fp2::Fp2(123, 1234), Fp2::Fp2(123, 1234));

        // Add arbitrary elements
        let _ = test_add(Fp2::Fp2(123, 1234), Fp2::Fp2(567, 5678), Fp2::Fp2(690, 6912));
        test_add(Fp2::Fp2(0xffffffff00000000, 0xffffffff00000000), Fp2::Fp2(3, 4), Fp2::Fp2(2, 3))
    };

    let sub = query || {
        let test_sub = |a, b, c| assert(eq_ext(eval_ext(sub_ext(a, b)), c), || "Wrong subtraction result");

        // Test subtracting 0
        let _ = test_sub(zero_ext(), zero_ext(), zero_ext());
        let _ = test_sub(Fp2::Fp2(123, 1234), zero_ext(), Fp2::Fp2(123, 1234));

        // Subtract arbitrary elements
        let _ = test_sub(Fp2::Fp2(123, 1234), Fp2::Fp2(567, 5678), Fp2::Fp2(18446744069414583877, 18446744069414579877));
        test_sub(Fp2::Fp2(0xffffffff00000000, 0xffffffff00000000), Fp2::Fp2(0x100000000, 1), Fp2::Fp2(0xfffffffe00000000, 0xfffffffeffffffff))
    };

    let mul = query || {
        let test_mul = |a, b, c| assert(eq_ext(eval_ext(mul_ext(a, b)), c), || "Wrong multiplication result");

        // Test multiplication by 1
        let _ = test_mul(one_ext(), one_ext(), one_ext());
        let _ = test_mul(Fp2::Fp2(123, 1234), one_ext(), Fp2::Fp2(123, 1234));
        let _ = test_mul(one_ext(), Fp2::Fp2(123, 1234), Fp2::Fp2(123, 1234));

        // Test multiplication by 0
        let _ = test_mul(Fp2::Fp2(123, 1234), zero_ext(), zero_ext());
        let _ = test_mul(zero_ext(), Fp2::Fp2(123, 1234), zero_ext());

        // Multiply arbitrary elements
        test_mul(Fp2::Fp2(123, 1234), Fp2::Fp2(567, 5678), Fp2::Fp2(49116305, 1398072))
    };

    let inverse = query || {
        let test_elements = [
            one_ext(),
            Fp2::Fp2(123, 1234),
            Fp2::Fp2(0xffffffff00000000, 0xffffffff00000000)
        ];

        map(test_elements, |x| {
            let inv_x = expr_ext(inv_ext(eval_ext(x)));
            let mul_with_inverse = eval_ext(mul_ext(x, inv_x));

            assert(eq_ext(mul_with_inverse, one_ext()), || "Should be 1")
        })
    };
}