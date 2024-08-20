use std::check::assert;
use std::check::panic;
use std::convert::fe;
use std::convert::int;
use std::convert::expr;
use std::math::ff::inv_field;
use std::prover::eval;


/// BETA = 11
/// NBETA = 2013265910

/// An element of the extension field over the BabyBear Field
/// relative to the irreducible polynomial X^4 + 11,
/// where Fp4(a0, a1, a2, a3) is interpreted as a0 + a1 * X + a2 * X^2 + a3 * X^3
/// T is assumed to either be fe, expr or any other object whose algebraic operations
/// are compatible with fe.
enum Fp4<T> {
    Fp4(T, T, T, T)
}

/// Converts a base field element to the extension field
let<T: FromLiteral> from_base: T -> Fp4<T> = |x| Fp4::Fp4(x, 0, 0, 0);

/// Addition for extension field
let<T: Add> add_ext: Fp4<T>, Fp4<T> -> Fp4<T> = |a, b| match (a, b) {
    (Fp4::Fp4(a0, a1, a2, a3), Fp4::Fp4(b0, b1, b2, b3)) => Fp4::Fp4(
        a0 + b0,
        a1 + b1,
        a2 + b2,
        a3 + b3
    )
};

/// Subtraction for extension field
let<T: Sub> sub_ext: Fp4<T>, Fp4<T> -> Fp4<T> = |a, b| match (a, b) {
    (Fp4::Fp4(a0, a1, a2, a3), Fp4::Fp4(b0, b1, b2, b3)) => Fp4::Fp4(
        a0 - b0,
        a1 - b1,
        a2 - b2,
        a3 - b3
    )
};

/// Multiplication for the extension field:
/// Multiply out the polynomial representations, and then reduce modulo
/// `x^4 - B`, which means powers >= 4 get shifted back 4 and
/// multiplied by `-beta`.
///
/// Multiplication modulo the polynomial x^4 + 11. We'll use the fact
/// that x^4 == -11 (mod x^4 + 11), so:
/// (a0 + a1 * x + a2 * x^2 + a3 * x^3) * (b0 + b1 * x + b2 * x^2 + b3 * x^3) = 
/// a0 * b0 + NBETA * (a1 * b3 + a2 * b2 + a3 * b1)
/// + (a0 * b1 + a1 * b0 + NBETA * (a2 * b3 + a3 * b2)) * X
/// + (a0 * b2 + a1 * b1 + a2 * b0 + NBETA * (a3 * b3)) * X^2
/// + (a0 * b3 + a1 * b2 + a2 * b1 + a3 * b0) * X^3
let<T: Add + FromLiteral + Mul> mul_ext: Fp4<T>, Fp4<T> -> Fp4<T> = |a, b| match (a, b) {
    (Fp4::Fp4(a0, a1, a2, a3), Fp4::Fp4(b0, b1, b2, b3)) => Fp4::Fp4(
        a0 * b0 + 2013265910 * (a1 * b3 + a2 * b2 + a3 * b1),
        a0 * b1 + a1 * b0 + 2013265910 * (a2 * b3 + a3 * b2),
        a0 * b2 + a1 * b1 + a2 * b0 + 2013265910 * (a3 * b3),
        a0 * b3 + a1 * b2 + a2 * b1 + a3 * b0
    )
};

/// Inversion for an Fp4 element
// The inverse of (a0, a1, a2, a3) is a point (b0, b1, b2, b3) such that:
/// (a0 + a1 * x + a2 * x^2 + a3 * x^3) (b0 + b1 * x + b2 * x^2 + b3 * x^3) = 1 (mod x^4 + 11)
/// Multiplying out and plugging in x^4 = -11 yields the following result
/// a[0] * b0 + BETA * a[2] * b2
/// + (-a[1] * b0 + NBETA * a[3] * b2) * X
/// + (-a[0] * b2 + a[2] * b0) * X^2
/// + (a[1] * b2 - a[3] * b0) * X^3
let inv_ext: Fp4<fe> -> Fp4<fe> = |a| match a {
    Fp4::Fp4(a0, a1, a2, a3) => {
        let b0 = a0 * a0 + 11 * (a1 * (a3 + a3) - a2 * a2);
        let b2 = a0 * (a2 + a2) - a1 * a1 + 11 * (a3 * a3);
        let c = b0 * b0 + 11 * b2 * b2;
        let ic = inv_field(c);
        let b_0 = b0 * ic;
        let b_2 = b2 * ic;
        Fp4::Fp4(
            a0 * b_0 + 11 * a2 * b_2,
            -1 * a1 * b_0 + 2013265910 * a3 * b_2,
            -1 * a0 * b_2 + a2 * b_0,
            a1 * b_2 - a3 * b_0
        )
    }
};

/// Converts an Fp4<expr> into an Fp4<fe>
let eval_ext: Fp4<expr> -> Fp4<fe> = query |a| match a {
    Fp4::Fp4(a0, a1, a2, a3) => Fp4::Fp4(eval(a0), eval(a1), eval(a2), eval(a3))
};

/// Converts an Fp4<fe> into an Fp4<expr>
let expr_ext: Fp4<fe> -> Fp4<expr> = |a| match a {
    Fp4::Fp4(a0, a1, a2, a3) => Fp4::Fp4(expr(a0), expr(a1), expr(a2), expr(a3))
};

/// Checks the equality of two Fp4 elements
let eq_ext: Fp4<fe>, Fp4<fe> -> bool = |a, b| match (a, b) {
    (Fp4::Fp4(a0, a1, a2, a3), Fp4::Fp4(b0, b1, b2, b3)) => (a0 == b0) && (a1 == b1) && (a2 == b2) && (a3 == b3)
};

/// Returns constraints that two Fp4 elements are equal
let constrain_eq_ext: Fp4<expr>, Fp4<expr> -> Constr[] = |a, b| match (a, b) {
    (Fp4::Fp4(a0, a1, a2, a3), Fp4::Fp4(b0, b1, b2, b3)) => [a0 = b0, a1 = b1, a2 = b2, a3 = b3]
};

/// Applies the next operator to both components of the extension field element
let next_ext: Fp4<expr> -> Fp4<expr> = |a| match a {
    Fp4::Fp4(a0, a1, a2, a3) => Fp4::Fp4(a0', a1', a2', a3')
};

/// Returns the two components of the extension field element as a tuple
let<T> unpack_ext: Fp4<T> -> (T, T, T, T) = |a| match a {
    Fp4::Fp4(a0, a1, a2, a3) => (a0, a1, a2, a3)
};

/// Returns the two components of the extension field element as an array
let<T> unpack_ext_array: Fp4<T> -> T[] = |a| match a {
    Fp4::Fp4(a0, a1, a2, a3) => [a0, a1, a2, a3]
};

mod test {
    use super::Fp4;
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
        let _ = test_add(Fp4::Fp4(123, 1234, 1, 2), from_base(0), Fp4::Fp4(123, 1234, 1, 2));
        let _ = test_add(from_base(0), Fp4::Fp4(123, 1234, 123, 1334), Fp4::Fp4(123, 1234, 123, 1334));

        // Add arbitrary elements
        let _ = test_add(Fp4::Fp4(123, 1234, 123, 122), Fp4::Fp4(567, 5678, 250, 678), Fp4::Fp4(690, 6912, 373, 800));
        test_add(Fp4::Fp4(-1, -1, -1, -1), Fp4::Fp4(3, 4, 5, 6), Fp4::Fp4(2, 3, 4, 5));

        // Add to the modulo
        test_add(Fp4::Fp4(-11, -11, -11, -11), Fp4::Fp4(0, 0, 0, 0), Fp4::Fp4(2013265910, 2013265910, 2013265910, 2013265910));

        // p - 1 + 1 = 0
        test_add(Fp4::Fp4(0x78000000, 0, 0, 0), Fp4::Fp4(1, 0, 0, 0), from_base(0));
    };

    let sub = || {
        let test_sub = |a, b, c| assert(eq_ext(sub_ext(a, b), c), || "Wrong subtraction result");

        // Test subtracting 0
        let _ = test_sub(from_base(0), from_base(0), from_base(0));
        let _ = test_sub(Fp4::Fp4(123, 1234, 124, 1235), from_base(0), Fp4::Fp4(123, 1234, 124, 1235));

        // Subtract arbitrary elements
        let _ = test_sub(Fp4::Fp4(123, 1234, 248, 5000), Fp4::Fp4(567, 5678, 300, 2380), Fp4::Fp4(123 - 567, 1234 - 5678, 248 - 300, 5000 - 2380));
        test_sub(Fp4::Fp4(-1, -1, 0, 0), Fp4::Fp4(0x78000000, 1, 0, 0), Fp4::Fp4(-0x78000000 - 1, -2, 0, 0))
    };

    let mul = || {
        let test_mul = |a, b, c| assert(eq_ext(mul_ext(a, b), c), || "Wrong multiplication result");

        // Test multiplication by 1
        let _ = test_mul(from_base(1), from_base(1), from_base(1));
        let _ = test_mul(Fp4::Fp4(123, 1234, 280, 400), from_base(1), Fp4::Fp4(123, 1234, 280, 400));
        let _ = test_mul(from_base(1), Fp4::Fp4(123, 1234, 12, 15), Fp4::Fp4(123, 1234, 12, 15));

        // Test multiplication by 0
        let _ = test_mul(Fp4::Fp4(123, 1234, 234, 500), from_base(0), from_base(0));
        let _ = test_mul(from_base(0), Fp4::Fp4(123, 1234, 33, 200), from_base(0));

        // Multiply arbitrary elements
        test_mul(Fp4::Fp4(1, 2, 3, 4), Fp4::Fp4(5, 6, 7, 8), Fp4::Fp4(2013265255, 2013265365, 2013265603, 60));

        // Multiplication with field overflow
        test_mul(Fp4::Fp4(-1, -2, -3, -4), Fp4::Fp4(-3, 4, 4, 5), Fp4::Fp4(421, 343, 217, -13));
    };

    let inverse = || {
        let test_elements = [
            from_base(1),
            Fp4::Fp4(123, 1234, 1, 2),
            Fp4::Fp4(-1, 500, 3, 5)
        ];

        map(test_elements, |x| {
            let mul_with_inverse = mul_ext(x, inv_ext(x));

            assert(eq_ext(mul_with_inverse, from_base(1)), || "Should be 1")
        })
    };
}