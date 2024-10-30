use std::array::fold;
use std::array::len;
use std::math::fp2::Fp2;
use std::math::fp2::add_ext;
use std::math::fp2::mul_ext;
use std::math::fp2::from_base;

/// Maps [x_1, x_2, ..., x_n] to its Read-Solomon fingerprint, using a challenge alpha: $\sum_{i=1}^n alpha**{(n - i)} * x_i$
let<T: Add + Mul + FromLiteral> fingerprint: T[], Fp2<T> -> Fp2<T> = |expr_array, alpha| if len(expr_array) == 1 {
    // The else branch below would generate `0 * alpha + expr_array[0]`, which is equivalent.
    // This expression does not use alpha though, which would be removed by the optimizer.
    from_base(expr_array[0])
} else{
    fold(
        expr_array,
        from_base(0),
        |sum_acc, el| add_ext(mul_ext(alpha, sum_acc), from_base(el))
    )
};

/// Maps [id, x_1, x_2, ..., x_n] to its Read-Solomon fingerprint, using a challenge alpha: $\sum_{i=1}^n alpha**{(n - i)} * x_i$
let<T: Add + Mul + FromLiteral> fingerprint_with_id: T, T[], Fp2<T> -> Fp2<T> = |id, expr_array, alpha| fingerprint([id] + expr_array, alpha);

mod test {
    use super::fingerprint;
    use std::check::assert;
    use std::math::fp2::Fp2;
    use std::math::fp2::from_base;

    // Helper function to assert that the fingerprint of a tuple is equal to the expected value
    // We are working on integers here, wrapping them as Fp2 elements.
    let assert_fingerprint_equal: int[], int, int -> () = |tuple, challenge, expected| {
        let result = fingerprint(tuple, from_base(challenge));
        match result {
            Fp2::Fp2(actual, should_be_zero) => {
                assert(should_be_zero == 0, || "Returned an extension field element");
                assert(expected == actual, || "expected != actual");
            }
        }
    };

    let test_fingerprint = || {
        // A tuple t of size n with challenge x should be mapped to:
        // t[0] * x**(n-1) + t[1] * x**(n-2) + ... + t[n-2] * x + t[n-1]

        // For lists of length one, the fingerprint is the element itself
        assert_fingerprint_equal([123], 0, 123);
        assert_fingerprint_equal([123], 1234, 123);
        assert_fingerprint_equal([123], -4, 123);

        // For a list [a, b] of length two, the fingerprint is a * x + b
        assert_fingerprint_equal([123, 456], 0, 456);
        assert_fingerprint_equal([123, 456], 1, 579);
        assert_fingerprint_equal([123, 456], 2, 702);
        assert_fingerprint_equal([123, 456], -1, 333);

        // For a list [a, b, c] of length three, the fingerprint is a * x**2 + b * x + c
        assert_fingerprint_equal([123, 456, 789], 0, 789);
        assert_fingerprint_equal([123, 456, 789], 1, 1368);
        assert_fingerprint_equal([123, 456, 789], 2, 2193);
    };
}