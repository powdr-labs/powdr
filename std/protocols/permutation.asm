use std::prover::challenge;
use std::array::fold;
use std::array::map;
use std::utils::unwrap_or_else;
use std::array::len;
use std::check::assert;
use std::check::panic;
use std::field::known_field;
use std::field::KnownField;
use std::math::fp2::Fp2;
use std::math::fp2::add_ext;
use std::math::fp2::sub_ext;
use std::math::fp2::mul_ext;
use std::math::fp2::unpack_ext;
use std::math::fp2::next_ext;
use std::math::fp2::inv_ext;
use std::math::fp2::eval_ext;
use std::math::fp2::from_base;
use std::math::fp2::constrain_eq_ext;

let is_first: col = |i| if i == 0 { 1 } else { 0 };

/// Get two phase-2 challenges to use in all permutation arguments.
/// Note that this assumes that globally no other challenge of these IDs is used,
/// and that challenges for multiple permutation arguments are re-used.
/// We declare two components for each challenge here, in case we need to operate
/// on the extension field. If we don't, we won't end up needing it and the optimizer
/// will remove it.
let alpha1: expr = challenge(0, 1);
let alpha2: expr = challenge(0, 2);

let beta1: expr = challenge(0, 3);
let beta2: expr = challenge(0, 4);

let unpack_permutation_constraint: Constr -> (expr, expr[], expr, expr[]) = |permutation_constraint| match permutation_constraint {
    Constr::Permutation((lhs_selector, rhs_selector), values) => (
        unwrap_or_else(lhs_selector, || 1),
        map(values, |(lhs, _)| lhs),
        unwrap_or_else(rhs_selector, || 1),
        map(values, |(_, rhs)| rhs)
    ),
    _ => panic("Expected permutation constraint")
};

/// Whether we need to operate on the F_{p^2} extension field (because the current field is too small).
let needs_extension: -> bool = || match known_field() {
    Option::Some(KnownField::Goldilocks) => true,
    Option::Some(KnownField::BN254) => false,
    None => panic("The permutation argument is not implemented for the current field!")
};

/// Maps [x_1, x_2, ..., x_n] to its Read-Solomon fingerprint, using challenge alpha: $\sum_{i=1}^n alpha**{(n - i)} * x_i$
let<T: Add + Mul + FromLiteral> compress_expression_array: T[], Fp2<T> -> Fp2<T> = |expr_array, alpha| fold(
    expr_array,
    from_base(0),
    |sum_acc, el| add_ext(mul_ext(alpha, sum_acc), from_base(el))
);

/// Takes a boolean selector (0/1) and a value, returns equivalent of `if selector { value } else { 1 }`
/// Implemented as: selector * (value - 1) + 1
let<T: Add + Mul + Sub + FromLiteral> selected_or_one: T, Fp2<T> -> Fp2<T> = |selector, value| add_ext(mul_ext(from_base(selector), sub_ext(value, from_base(1))), from_base(1));

/// Compute acc' = acc * selected_or_one(sel_a, beta - a) / selected_or_one(sel_b, beta - b),
/// using extension field arithmetic (where expressions for sel_a, a, sel_b, b are derived from
/// the provided permutation constraint).
/// This is intended to be used as a hint in the extension field case; for the base case
/// automatic witgen is smart enough to figure out the value of the accumulator.
let compute_next_z: Fp2<expr>, Constr -> fe[] = query |acc, permutation_constraint| {

    let (lhs_selector, lhs, rhs_selector, rhs) = unpack_permutation_constraint(permutation_constraint);

    let alpha = if len(lhs) > 1 {
        Fp2::Fp2(alpha1, alpha2)
    } else {
        // The optimizer will have removed alpha, but the compression function
        // still accesses it (to multiply by 0 in this case)
        from_base(0)
    };
    let beta = Fp2::Fp2(beta1, beta2);
    
    let lhs_folded = selected_or_one(lhs_selector, sub_ext(beta, compress_expression_array(lhs, alpha)));
    let rhs_folded = selected_or_one(rhs_selector, sub_ext(beta, compress_expression_array(rhs, alpha)));
    
    // acc' = acc * lhs_folded / rhs_folded
    let res = mul_ext(
        eval_ext(mul_ext(acc, lhs_folded)),
        inv_ext(eval_ext(rhs_folded))
    );

    match res {
        Fp2::Fp2(a0_fe, a1_fe) => [a0_fe, a1_fe]
    }
};

/// Returns constraints that enforce that lhs is a permutation of rhs
///
/// # Arguments:
/// - acc: A phase-2 witness column to be used as the accumulator. If 2 are provided, computations
///        are done on the F_{p^2} extension field.
/// - permutation_constraint: The permutation constraint
///
/// # Returns:
/// - Constraints to be added to enforce the permutation
///
/// # Implementation:
/// This function implements a permutation argument described e.g. in
/// https://people.cs.georgetown.edu/jthaler/ProofsArgsAndZK.pdf, section 6.6.2,
/// page 99, paragraph "Multiset equality checking (a.k.a. permutation checking)
/// via fingerprinting". In short:
/// 1. The LHS and RHS are Reed-Solomon fingerprinted using challenge $\alpha$
///    (see `compress_expression_array`).
/// 2. If the selector is one, the accumulator is updated as:
///    `acc' = acc * (beta - lhs) / (beta - rhs)`.
/// This iteratively evaluates the fraction of polynomials $\prod_i (X - lhs_i)$
/// and $\prod_i (X - rhs_i)$, evaluated at $X = \beta$. With high probability, this fraction
/// is equal to 1 if and only if the permutation holds. This is enforced by exploiting
/// the wrapping behavior: The first accumulator is constrained to be 1, and the last
/// accumulator is the same as the first one, because of wrapping.
/// For small fields, this computation should happen in the extension field.
let permutation: expr[], Constr -> Constr[] = |acc, permutation_constraint| {

    let (lhs_selector, lhs, rhs_selector, rhs) = unpack_permutation_constraint(permutation_constraint);

    let _ = assert(len(lhs) == len(rhs), || "LHS and RHS should have equal length");
    let with_extension = match len(acc) {
        1 => false,
        2 => true,
        _ => panic("Expected 1 or 2 accumulator columns!")
    };

    let _ = if !with_extension {
        assert(!needs_extension(), || "The Goldilocks field is too small and needs to move to the extension field. Pass two accumulators instead!")
    } else { () };

    // On the extension field, we'll need two field elements to represent the challenge.
    // If we don't need an extension field, we can simply set the second component to 0,
    // in which case the operations below effectively only operate on the first component.
    let fp2_from_array = |arr| if with_extension { Fp2::Fp2(arr[0], arr[1]) } else { from_base(arr[0]) };
    let acc_ext = fp2_from_array(acc);
    let alpha = fp2_from_array([alpha1, alpha2]);
    let beta = fp2_from_array([beta1, beta2]);

    // If the selector is 1, contribute a factor of `beta - compress_expression_array(lhs)` to accumulator.
    // If the selector is 0, contribute a factor of 1 to the accumulator.
    // Implemented as: folded = selector * (beta - compress_expression_array(values) - 1) + 1;
    let lhs_folded = selected_or_one(lhs_selector, sub_ext(beta, compress_expression_array(lhs, alpha)));
    let rhs_folded = selected_or_one(rhs_selector, sub_ext(beta, compress_expression_array(rhs, alpha)));

    let next_acc = if with_extension {
        next_ext(acc_ext)
    } else {
        // The second component is 0, but the next operator is not defined on it...
        from_base(acc[0]')
    };

    // Update rule:
    // acc' = acc * lhs_folded / rhs_folded
    // => rhs_folded * acc' - lhs_folded * acc = 0
    let diff_from_expected = sub_ext(
        mul_ext(rhs_folded, next_acc),
        mul_ext(lhs_folded, acc_ext)
    );

    let (acc_1, acc_2) = unpack_ext(acc_ext);

    [
        // First and last z needs to be 1
        // (because of wrapping, the z[0] and z[N] are the same)
        is_first * (acc_1 - 1) = 0,

        // Note that if with_extension is false, this generates 0 = 0 and is removed
        // by the optimizer.
        is_first * acc_2 = 0
    ] + constrain_eq_ext(diff_from_expected, from_base(0))
};