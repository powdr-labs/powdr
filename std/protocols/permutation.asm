use std::array::map;
use std::array::len;
use std::check::assert;
use std::check::panic;
use std::math::fp2::Fp2;
use std::math::fp2::add_ext;
use std::math::fp2::sub_ext;
use std::math::fp2::mul_ext;
use std::math::fp2::unpack_ext;
use std::math::fp2::next_ext;
use std::math::fp2::inv_ext;
use std::math::fp2::eval_ext;
use std::math::fp2::from_base;
use std::math::fp2::needs_extension;
use std::math::fp2::is_extension;
use std::math::fp2::fp2_from_array;
use std::math::fp2::constrain_eq_ext;
use std::protocols::fingerprint::fingerprint;
use std::utils::unwrap_or_else;

let unpack_permutation_constraint: Constr -> (expr, expr[], expr, expr[]) = |permutation_constraint| match permutation_constraint {
    Constr::Permutation((lhs_selector, rhs_selector), values) => (
        unwrap_or_else(lhs_selector, || 1),
        map(values, |(lhs, _)| lhs),
        unwrap_or_else(rhs_selector, || 1),
        map(values, |(_, rhs)| rhs)
    ),
    _ => panic("Expected permutation constraint")
};

/// Takes a boolean selector (0/1) and a value, returns equivalent of `if selector { value } else { 1 }`
/// Implemented as: selector * (value - 1) + 1
let<T: Add + Mul + Sub + FromLiteral> selected_or_one: T, Fp2<T> -> Fp2<T> = |selector, value| add_ext(mul_ext(from_base(selector), sub_ext(value, from_base(1))), from_base(1));

/// Compute acc' = acc * selected_or_one(sel_a, beta - a) / selected_or_one(sel_b, beta - b),
/// using extension field arithmetic (where expressions for sel_a, a, sel_b, b are derived from
/// the provided permutation constraint).
/// This is intended to be used as a hint in the extension field case; for the base case
/// automatic witgen is smart enough to figure out the value of the accumulator.
let compute_next_z: Fp2<expr>, Fp2<expr>, Fp2<expr>, Constr -> fe[] = query |acc, alpha, beta, permutation_constraint| {

    let (lhs_selector, lhs, rhs_selector, rhs) = unpack_permutation_constraint(permutation_constraint);
    
    let lhs_folded = selected_or_one(lhs_selector, sub_ext(beta, fingerprint(lhs, alpha)));
    let rhs_folded = selected_or_one(rhs_selector, sub_ext(beta, fingerprint(rhs, alpha)));
    
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
///    (see `std::fingerprint::fingerprint`).
/// 2. If the selector is one, the accumulator is updated as:
///    `acc' = acc * (beta - lhs) / (beta - rhs)`.
/// This iteratively evaluates the fraction of polynomials $\prod_i (X - lhs_i)$
/// and $\prod_i (X - rhs_i)$, evaluated at $X = \beta$. With high probability, this fraction
/// is equal to 1 if and only if the permutation holds. This is enforced by exploiting
/// the wrapping behavior: The first accumulator is constrained to be 1, and the last
/// accumulator is the same as the first one, because of wrapping.
/// For small fields, this computation should happen in the extension field.
let permutation: expr, expr[], Fp2<expr>, Fp2<expr>, Constr -> Constr[] = |is_first, acc, alpha, beta, permutation_constraint| {

    let (lhs_selector, lhs, rhs_selector, rhs) = unpack_permutation_constraint(permutation_constraint);

    let _ = assert(len(lhs) == len(rhs), || "LHS and RHS should have equal length");
    let _ = if !is_extension(acc) {
        assert(!needs_extension(), || "The Goldilocks field is too small and needs to move to the extension field. Pass two accumulators instead!")
    } else { };

    // On the extension field, we'll need two field elements to represent the challenge.
    // If we don't need an extension field, we can simply set the second component to 0,
    // in which case the operations below effectively only operate on the first component.
    let fp2_from_array = |arr| if is_extension(acc) { Fp2::Fp2(arr[0], arr[1]) } else { from_base(arr[0]) };
    let acc_ext = fp2_from_array(acc);

    // If the selector is 1, contribute a factor of `beta - fingerprint(lhs)` to accumulator.
    // If the selector is 0, contribute a factor of 1 to the accumulator.
    // Implemented as: folded = selector * (beta - fingerprint(values) - 1) + 1;
    let lhs_folded = selected_or_one(lhs_selector, sub_ext(beta, fingerprint(lhs, alpha)));
    let rhs_folded = selected_or_one(rhs_selector, sub_ext(beta, fingerprint(rhs, alpha)));

    let next_acc = if is_extension(acc) {
        next_ext(acc_ext)
    } else {
        // The second component is 0, but the next operator is not defined on it...
        from_base(acc[0]')
    };

    // Update rule:
    // acc' = acc * lhs_folded / rhs_folded
    // => rhs_folded * acc' - lhs_folded * acc = 0
    let update_expr = sub_ext(
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
    ] + constrain_eq_ext(update_expr, from_base(0))
};