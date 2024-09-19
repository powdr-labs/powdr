use std::array::map;
use std::array::len;
use std::check::assert;
use std::check::panic;
use std::math::fp2::Fp2;
use std::math::fp2::add_ext;
use std::math::fp2::sub_ext;
use std::math::fp2::mul_ext;
use std::math::fp2::unpack_ext;
use std::math::fp2::unpack_ext_array;
use std::math::fp2::next_ext;
use std::math::fp2::inv_ext;
use std::math::fp2::eval_ext;
use std::math::fp2::from_base;
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

    unpack_ext_array(res)
};

/// Returns constraints that enforce that lhs is a permutation of rhs
///
/// # Arguments:
/// - acc: A phase-2 witness column to be used as the accumulator. If 2 are provided, computations
///        are done on the F_{p^2} extension field.
/// - alpha: A challenge used to compress the LHS and RHS values
/// - beta: A challenge used to update the accumulator
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
let permutation: expr[], Fp2<expr>, Fp2<expr>, Constr -> () = constr |acc, alpha, beta, permutation_constraint| {

    let (lhs_selector, lhs, rhs_selector, rhs) = unpack_permutation_constraint(permutation_constraint);

    // If the selector is 1, contribute a factor of `beta - fingerprint(lhs)` to accumulator.
    // If the selector is 0, contribute a factor of 1 to the accumulator.
    // Implemented as: folded = selector * (beta - fingerprint(values) - 1) + 1;
    let lhs_folded = selected_or_one(lhs_selector, sub_ext(beta, fingerprint(lhs, alpha)));
    let rhs_folded = selected_or_one(rhs_selector, sub_ext(beta, fingerprint(rhs, alpha)));
    let acc_ext = fp2_from_array(acc);
    let next_acc = next_ext(acc_ext);

    // Update rule:
    // acc' = acc * lhs_folded / rhs_folded
    // => rhs_folded * acc' - lhs_folded * acc = 0
    let update_expr = sub_ext(
        mul_ext(rhs_folded, next_acc),
        mul_ext(lhs_folded, acc_ext)
    );

    let is_first: col = std::well_known::is_first;

    let (acc_1, acc_2) = unpack_ext(acc_ext);

    // First and last acc needs to be 1
    // (because of wrapping, the acc[0] and acc[N] are the same)
    is_first * (acc_1 - 1) = 0;
    is_first * acc_2 = 0;
    constrain_eq_ext(update_expr, from_base(0));
};