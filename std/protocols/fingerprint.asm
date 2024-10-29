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