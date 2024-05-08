use std::prover::challenge;
use std::array::fold;
use std::array::len;
use std::convert::int;
use std::check::assert;
use std::check::panic;
use std::field::known_field;
use std::field::KnownField;
use std::math::fp2::Fp2Value;
use std::math::fp2::Fp2Expr;
use std::math::fp2::add_ext;
use std::math::fp2::sub_ext;
use std::math::fp2::mul_ext;
use std::math::fp2::unpack_ext;
use std::math::fp2::next_ext;
use std::math::fp2::eval_ext;
use std::math::fp2::expr_ext;
use std::math::fp2::inv_ext;

let is_first: col = |i| if i == 0 { 1 } else { 0 };

// Get two phase-2 challenges to use in all permutation arguments.
// Note that this assumes that globally no other challenge of these IDs is used,
// and that challenges for multiple permutation arguments are re-used.
// We declare two components for each challenge here, in case we need to operate
// on the extension field. If we don't, we won't end up needing it and the optimizer
// will remove it.
let alpha1: expr = challenge(0, 1);
let alpha2: expr = challenge(0, 2);

let beta1: expr = challenge(0, 3);
let beta2: expr = challenge(0, 4);

let unpack_permutation_constraint: Constr -> (expr, expr[], expr, expr[]) = |permutation_constraint| match permutation_constraint {
    Constr::Permutation(Option::Some(lhs_selector), lhs, Option::Some(rhs_selector), rhs) => (lhs_selector, lhs, rhs_selector, rhs),
    _ => panic("Expected permutation constraint")
};

// Whether we need to operate on the F_{p^2} extension field (because the current field is too small).
let _needs_extension: -> bool = || match known_field() {
    Option::Some(KnownField::Goldilocks) => true,
    Option::Some(KnownField::BN254) => false,
    None => panic("The permutation argument is not implemented for the current field!")
};
let needs_extension: -> bool = || true;

// Maps [x_1, x_2, ..., x_n] to alpha**(n - 1) * x_1 + alpha ** (n - 2) * x_2 + ... + x_n
let compress_expression_array = |expr_array, alpha| fold(
    expr_array,
    Fp2Expr::Fp2(0, 0),
    |sum_acc, el| add_ext(mul_ext(alpha, sum_acc), Fp2Expr::Fp2(el, 0))
);

// Compute z' = z * (beta - a) / (beta - b), using extension field arithmetic
let compute_next_z: Fp2Expr, Constr -> fe[] = query |acc, permutation_constraint| {

    let (lhs_selector, lhs, rhs_selector, rhs) = unpack_permutation_constraint(permutation_constraint);

    let alpha = if len(lhs) > 1 {
        Fp2Expr::Fp2(alpha1, alpha2)
    } else {
        // The optimizer will have removed alpha, but the compression function
        // still accesses it (to multiply by 0 in this case)
        Fp2Expr::Fp2(0, 0)
    };
    let beta = Fp2Expr::Fp2(beta1, beta2);
    
    let lhs_folded = mul_ext(Fp2Expr::Fp2(lhs_selector, 0), compress_expression_array(lhs, alpha));
    let rhs_folded = mul_ext(Fp2Expr::Fp2(rhs_selector, 0), compress_expression_array(rhs, alpha));
    
    let res = eval_ext(mul_ext(mul_ext(acc, sub_ext(beta, lhs_folded)), expr_ext(inv_ext(eval_ext(sub_ext(beta, rhs_folded))))));

    match res {
        Fp2Value::Fp2(a0_fe, a1_fe) => [a0_fe, a1_fe]
    }
};

// Adds constraints that enforce that lhs is a permutation of rhs
// Arguments:
// - acc: A phase-2 witness column to be used as the accumulator
// - lhs_selector: (assumed to be) binary selector to check which elements from the LHS to include
// - lhs: An array of expressions
// - rhs_selector: (assumed to be) binary selector to check which elements from the RHS to include
// - rhs: An array of expressions
let permutation: expr[], Constr -> Constr[] = |acc, permutation_constraint| {

    let (lhs_selector, lhs, rhs_selector, rhs) = unpack_permutation_constraint(permutation_constraint);

    let _ = assert(len(lhs) == len(rhs), || "LHS and RHS should have equal length");

    // On the extension field, we'll need two field elements to represent the challenge.
    // If we don't need an extension field, we can simply set the second component to 0,
    // in which case the operations below effectively only operate on the first component.
    let acc_ext = if needs_extension() {
        let _ = assert(len(acc) == 2, || "Expected 2 accumulators");
        Fp2Expr::Fp2(acc[0], acc[1])
    } else {
        let _ = assert(len(acc) == 1, || "Expected 1 accumulators");
        Fp2Expr::Fp2(acc[0], 0)
    };
    let alpha = if needs_extension() {Fp2Expr::Fp2(alpha1, alpha2)} else {Fp2Expr::Fp2(alpha1, 0)};
    let beta = if needs_extension() {Fp2Expr::Fp2(beta1, beta2)} else {Fp2Expr::Fp2(beta1, 0)};

    let lhs_folded = mul_ext(Fp2Expr::Fp2(lhs_selector, 0), compress_expression_array(lhs, alpha));
    let rhs_folded = mul_ext(Fp2Expr::Fp2(rhs_selector, 0), compress_expression_array(rhs, alpha));

    // Update rule:
    // acc' = acc * (beta - lhs_folded) / (beta - rhs_folded)
    // => (beta - rhs_folded) * acc' - (beta - lhs_folded) * acc = 0
    let update_expr = unpack_ext(sub_ext(
        mul_ext(sub_ext(beta, rhs_folded), next_ext(acc_ext)),
        mul_ext(sub_ext(beta, lhs_folded), acc_ext)
    ));

    [
        // First and last z needs to be 1
        // (because of wrapping, the z[0] and z[N] are the same)
        is_first * (acc[0] - 1) = 0,
        is_first * acc[1] = 0,

        // Update rule:
        // acc' = acc * (beta - lhs_folded) / (beta - rhs_folded)
        update_expr[0] = 0,
        update_expr[1] = 0
    ]
};