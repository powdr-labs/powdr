use std::array;
use std::array::len;
use std::math::fp2::Fp2;
use std::math::fp2::add_ext;
use std::math::fp2::mul_ext;
use std::math::fp2::pow_ext;
use std::math::fp2::from_base;
use std::math::fp2::eval_ext;
use std::check::assert;
use std::utils::fold;
use std::prover::eval;

/// Maps [x_1, x_2, ..., x_n] to its Read-Solomon fingerprint, using a challenge alpha: $\sum_{i=1}^n alpha**{(n - i)} * x_i$
/// To generate an expression that computes the fingerprint, use `fingerprint_inter` instead.
/// Note that alpha is passed as an expressions, so that it is only evaluated if needed (i.e., if len(expr_array) > 1).
let fingerprint: fe[], Fp2<expr> -> Fp2<fe> = query |expr_array, alpha| {
    fingerprint_impl(expr_array, eval_ext(alpha), len(expr_array))
};

let fingerprint_impl: fe[], Fp2<fe>, int -> Fp2<fe> = query |expr_array, alpha, l| if l == 1 {
    // Base case
    from_base(expr_array[0])
} else {

    // Recursively compute the fingerprint as fingerprint(expr_array[:-1], alpha) * alpha + expr_array[-1]
    let intermediate_fingerprint = fingerprint_impl(expr_array, alpha, l - 1);
    add_ext(mul_ext(alpha, intermediate_fingerprint), from_base(expr_array[l - 1]))
};

let fingerprint2: fe[], Fp2<expr> -> Fp2<fe> = query |expr_array, alpha| {
    let n = len(expr_array);
    fold(
        n,
        |i| if expr_array[i] == 0 {from_base(0)} else {mul_ext(pow_ext(eval_ext(alpha), n - i - 1), from_base(expr_array[i]))},
        from_base(0),
        |sum_acc, el| add_ext(sum_acc, el)
    )
};

/// Like `fingerprint`, but "materializes" the intermediate results as intermediate columns.
/// Inlining them would lead to an exponentially-sized expression.
let fingerprint_inter: expr[], Fp2<expr> -> Fp2<expr> = |expr_array, alpha| if len(expr_array) == 1 {
    // Base case
    from_base(expr_array[0])
} else {
    assert(len(expr_array) > 1, || "fingerprint requires at least one element");

    // Recursively compute the fingerprint as fingerprint(expr_array[:-1], alpha) * alpha + expr_array[-1]
    let intermediate_fingerprint = match fingerprint_inter(array::sub_array(expr_array, 0, len(expr_array) - 1), alpha) {
        Fp2::Fp2(a0, a1) => {
            let intermediate_fingerprint_0: inter = a0;
            let intermediate_fingerprint_1: inter = a1;
            Fp2::Fp2(intermediate_fingerprint_0, intermediate_fingerprint_1)
        }
    };
    add_ext(mul_ext(alpha, intermediate_fingerprint), from_base(expr_array[len(expr_array) - 1]))
};

/// Maps [id, x_1, x_2, ..., x_n] to its Read-Solomon fingerprint, using a challenge alpha: $\sum_{i=1}^n alpha**{(n - i)} * x_i$
let fingerprint_with_id: fe, fe[], Fp2<expr> -> Fp2<fe> = query |id, expr_array, alpha| fingerprint([id] + expr_array, alpha);
let fingerprint_with_id2: fe, fe[], Fp2<expr> -> Fp2<fe> = query |id, expr_array, alpha| fingerprint2([id] + expr_array, alpha);

/// Maps [id, x_1, x_2, ..., x_n] to its Read-Solomon fingerprint, using a challenge alpha: $\sum_{i=1}^n alpha**{(n - i)} * x_i$
let fingerprint_with_id_inter: expr, expr[], Fp2<expr> -> Fp2<expr> = |id, expr_array, alpha| fingerprint_inter([id] + expr_array, alpha);

mod test {
    use super::fingerprint;
    use std::check::assert;
    use std::math::fp2::Fp2;
    use std::math::fp2::from_base;

    /// Helper function to assert that the fingerprint of a tuple is equal to the expected value.
    let assert_fingerprint_equal: fe[], expr, fe -> () = query |tuple, challenge, expected| {
        let result = fingerprint(tuple, from_base(challenge));
        match result {
            Fp2::Fp2(actual, should_be_zero) => {
                assert(should_be_zero == 0, || "Returned an extension field element");
                assert(expected == actual, || "expected != actual");
            }
        }
    };

    let test_fingerprint = query || {
        // A tuple t of size n with challenge x should be mapped to:
        // t[0] * x**(n-1) + t[1] * x**(n-2) + ... + t[n-2] * x + t[n-1]

        // For lists of length one, the fingerprint is the element itself
        assert_fingerprint_equal([123], 0, 123);
        assert_fingerprint_equal([123], 1234, 123);
        assert_fingerprint_equal([123], -4, 123);

        // For a list [a, b] of length two, the fingerprint is a * x + b
        assert_fingerprint_equal([123, 456], 0, 456);
        assert_fingerprint_equal([123, 456], 1, 123 + 456);
        assert_fingerprint_equal([123, 456], 2, 123 * 2 + 456);
        assert_fingerprint_equal([123, 456], -1, -123 + 456);

        // For a list [a, b, c] of length three, the fingerprint is a * x**2 + b * x + c
        assert_fingerprint_equal([123, 456, 789], 0, 789);
        assert_fingerprint_equal([123, 456, 789], 1, 123 + 456 + 789);
        assert_fingerprint_equal([123, 456, 789], 2, 123 * 4 + 456 * 2 + 789);
    };
}