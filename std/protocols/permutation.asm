use std::array::map;
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
use std::math::fp2::required_extension_size;
use std::math::fp2::needs_extension;
use std::protocols::fingerprint::fingerprint;
use std::protocols::fingerprint::fingerprint_inter;
use std::prover::eval;
use std::array;
use std::utils::unwrap_or_else;
use std::constraints::to_phantom_permutation;

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
    
    let lhs_folded = selected_or_one(eval(lhs_selector), sub_ext(eval_ext(beta), fingerprint(array::eval(lhs), alpha)));
    let rhs_folded = selected_or_one(eval(rhs_selector), sub_ext(eval_ext(beta), fingerprint(array::eval(rhs), alpha)));
    
    // acc' = acc * lhs_folded / rhs_folded
    let res = mul_ext(
        mul_ext(eval_ext(acc), lhs_folded),
        inv_ext(rhs_folded)
    );

    unpack_ext_array(res)
};

/// Returns constraints that enforce that lhs is a permutation of rhs
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
let permutation: Constr -> () = constr |permutation_constraint| {
    std::check::assert(required_extension_size() <= 2, || "Invalid extension size");
    // Alpha is used to compress the LHS and RHS arrays
    let alpha = fp2_from_array(std::array::new(required_extension_size(), |i| challenge(0, i + 1)));
    // Beta is used to update the accumulator
    let beta = fp2_from_array(std::array::new(required_extension_size(), |i| challenge(0, i + 3)));

    let (lhs_selector, lhs, rhs_selector, rhs) = unpack_permutation_constraint(permutation_constraint);

    // If the selector is 1, contribute a factor of `beta - fingerprint(lhs)` to accumulator.
    // If the selector is 0, contribute a factor of 1 to the accumulator.
    // Implemented as: folded = selector * (beta - fingerprint(values) - 1) + 1;
    let lhs_folded = selected_or_one(lhs_selector, sub_ext(beta, fingerprint_inter(lhs, alpha)));
    let rhs_folded = selected_or_one(rhs_selector, sub_ext(beta, fingerprint_inter(rhs, alpha)));

    let acc = std::array::new(required_extension_size(), |i| std::prover::new_witness_col_at_stage("acc", 1));
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

    // Add an annotation for witness generation
    to_phantom_permutation(permutation_constraint);

    // In the extension field, we need a prover function for the accumulator.
    if needs_extension() {
        // TODO: Helper columns, because we can't access the previous row in hints
        let acc_next_col = std::array::map(acc, |_| std::prover::new_witness_col_at_stage("acc_next", 1));
        query |i| {
            let _ = std::array::zip(
                acc_next_col,
                compute_next_z(acc_ext, alpha, beta, permutation_constraint),
                |acc_next, hint_val| std::prover::provide_value(acc_next, i, hint_val)
            );
        };
        std::array::zip(acc, acc_next_col, |acc_col, acc_next| {
            acc_col' = acc_next
        });
    } else {
    }
};