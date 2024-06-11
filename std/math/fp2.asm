use std::convert::fe;
use std::convert::int;
use std::convert::expr;
use std::field::modulus;
use std::prover::eval;

/// An element of the extension field over the implied base field (which has to be either
/// the Goldilocks or the BN254 field) relative to the irreducible polynomial X^2 - 7,
/// where Fp2(a0, a1) is interpreted as a0 + a1 * X.
/// T is assumed to either be fe, expr or any other object whose algebraic operations
/// are compatible with fe.
enum Fp2<T> {
    Fp2(T, T)
}

/// Converts a base field element to an extension field element
let<T: FromLiteral> from_base: T -> Fp2<T> = |x| Fp2::Fp2(x, 0);

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

/// Converts an Fp2<expr> into an Fp2<fe>
let eval_ext: Fp2<expr> -> Fp2<fe> = query |a| match a {
    Fp2::Fp2(a0, a1) => Fp2::Fp2(eval(a0), eval(a1))
};

/// Converts an Fp2<fe> into an Fp2<expr>
let expr_ext: Fp2<fe> -> Fp2<expr> = |a| match a {
    Fp2::Fp2(a0, a1) => Fp2::Fp2(expr(a0), expr(a1))
};

/// Extension field equality
let eq_ext: Fp2<fe>, Fp2<fe> -> bool = |a, b| match (a, b) {
    (Fp2::Fp2(a0, a1), Fp2::Fp2(b0, b1)) => (a0 == b0) && (a1 == b1)
};

/// Returns constraints that two extension field elements are equal
let constrain_eq_ext: Fp2<expr>, Fp2<expr> -> Constr[] = |a, b| match (a, b) {
    (Fp2::Fp2(a0, a1), Fp2::Fp2(b0, b1)) => [a0 = b0, a1 = b1]
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

/// Applies the next operator to both components of the extension field element
let next_ext: Fp2<expr> -> Fp2<expr> = |a| match a {
    Fp2::Fp2(a0, a1) => Fp2::Fp2(a0', a1')
};

/// Returns the two components of the extension field element
let<T> unpack_ext: Fp2<T> -> (T, T) = |a| match a {
    Fp2::Fp2(a0, a1) => (a0, a1)
};

mod test {
    use super::Fp2;
    use super::from_base;
    use super::add_ext;
    use super::sub_ext;
    use super::mul_ext;
    use super::inv_ext;
    use super::eq_ext;
    use std::check::assert;
    use std::array::map;

    let add = || {
        let test_add = |a, b, c| assert(eq_ext(add_ext(a, b), c), || "Wrong addition result");

        // Test adding 0
        let _ = test_add(from_base(0), from_base(0), from_base(0));
        let _ = test_add(Fp2::Fp2(123, 1234), from_base(0), Fp2::Fp2(123, 1234));
        let _ = test_add(from_base(0), Fp2::Fp2(123, 1234), Fp2::Fp2(123, 1234));

        // Add arbitrary elements
        let _ = test_add(Fp2::Fp2(123, 1234), Fp2::Fp2(567, 5678), Fp2::Fp2(690, 6912));
        test_add(Fp2::Fp2(-1, -1), Fp2::Fp2(3, 4), Fp2::Fp2(2, 3))
    };

    let sub = || {
        let test_sub = |a, b, c| assert(eq_ext(sub_ext(a, b), c), || "Wrong subtraction result");

        // Test subtracting 0
        let _ = test_sub(from_base(0), from_base(0), from_base(0));
        let _ = test_sub(Fp2::Fp2(123, 1234), from_base(0), Fp2::Fp2(123, 1234));

        // Subtract arbitrary elements
        let _ = test_sub(Fp2::Fp2(123, 1234), Fp2::Fp2(567, 5678), Fp2::Fp2(123 - 567, 1234 - 5678));
        test_sub(Fp2::Fp2(-1, -1), Fp2::Fp2(0x100000000, 1), Fp2::Fp2(-0x100000000 - 1, -2))
    };

    let mul = || {
        let test_mul = |a, b, c| assert(eq_ext(mul_ext(a, b), c), || "Wrong multiplication result");

        // Test multiplication by 1
        let _ = test_mul(from_base(1), from_base(1), from_base(1));
        let _ = test_mul(Fp2::Fp2(123, 1234), from_base(1), Fp2::Fp2(123, 1234));
        let _ = test_mul(from_base(1), Fp2::Fp2(123, 1234), Fp2::Fp2(123, 1234));

        // Test multiplication by 0
        let _ = test_mul(Fp2::Fp2(123, 1234), from_base(0), from_base(0));
        let _ = test_mul(from_base(0), Fp2::Fp2(123, 1234), from_base(0));

        // Multiply arbitrary elements
        let _ = test_mul(Fp2::Fp2(123, 1234), Fp2::Fp2(567, 5678), Fp2::Fp2(49116305, 1398072));

        // Multiplication with field overflow
        test_mul(Fp2::Fp2(-1, -2), Fp2::Fp2(-3, 4), Fp2::Fp2(3 - 7 * 8, 6 - 4))
    };

    let inverse = || {
        let test_elements = [
            from_base(1),
            Fp2::Fp2(123, 1234),
            Fp2::Fp2(-1, 500)
        ];

        map(test_elements, |x| {
            let mul_with_inverse = mul_ext(x, inv_ext(x));

            assert(eq_ext(mul_with_inverse, from_base(1)), || "Should be 1")
        })
    };
}
