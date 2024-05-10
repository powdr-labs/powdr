use std::prover::challenge;
use std::array::fold;
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
let needs_extension: -> bool = || match known_field() {
    Option::Some(KnownField::Goldilocks) => true,
    Option::Some(KnownField::BN254) => false,
    None => panic("The permutation argument is not implemented for the current field!")
};

// Maps [x_1, x_2, ..., x_n] to alpha**(n - 1) * x_1 + alpha ** (n - 2) * x_2 + ... + x_n
let<T: Add + Mul + FromLiteral> compress_expression_array: T[], Fp2<T> -> Fp2<T> = |expr_array, alpha| fold(
    expr_array,
    Fp2::Fp2(0, 0),
    |sum_acc, el| add_ext(mul_ext(alpha, sum_acc), Fp2::Fp2(el, 0))
);

// Compute z' = z * (beta - a) / (beta - b), using extension field arithmetic
// This is intended to be used as a hint in the extension field case; for the base case
// automatic witgen is smart enough to figure out the value if the accumulator.
let compute_next_z: Fp2<expr>, Constr -> fe[] = query |acc, permutation_constraint| {

    let (lhs_selector, lhs, rhs_selector, rhs) = unpack_permutation_constraint(permutation_constraint);

    let alpha = if len(lhs) > 1 {
        Fp2::Fp2(alpha1, alpha2)
    } else {
        // The optimizer will have removed alpha, but the compression function
        // still accesses it (to multiply by 0 in this case)
        Fp2::Fp2(0, 0)
    };
    let beta = Fp2::Fp2(beta1, beta2);
    
    let lhs_folded = mul_ext(from_base(lhs_selector), compress_expression_array(lhs, alpha));
    let rhs_folded = mul_ext(from_base(rhs_selector), compress_expression_array(rhs, alpha));
    
    // acc' = acc * (beta - lhs_folded) / (beta - rhs_folded)
    let res = mul_ext(
        eval_ext(mul_ext(acc, sub_ext(beta, lhs_folded))),
        inv_ext(eval_ext(sub_ext(beta, rhs_folded)))
    );

    match res {
        Fp2::Fp2(a0_fe, a1_fe) => [a0_fe, a1_fe]
    }
};

// Adds constraints that enforce that lhs is a permutation of rhs
// Arguments:
// - acc: A phase-2 witness column to be used as the accumulator. If 2 are provided, computations
//        are done on the F_{p^2} extension field.
// - lhs_selector: (assumed to be) binary selector to check which elements from the LHS to include
// - lhs: An array of expressions
// - rhs_selector: (assumed to be) binary selector to check which elements from the RHS to include
// - rhs: An array of expressions
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
    } else { [] };

    // On the extension field, we'll need two field elements to represent the challenge.
    // If we don't need an extension field, we can simply set the second component to 0,
    // in which case the operations below effectively only operate on the first component.
    let fp2_from_array = |arr| if with_extension { Fp2::Fp2(arr[0], arr[1]) } else { Fp2::Fp2(arr[0], 0) };
    let acc_ext = fp2_from_array(acc);
    let alpha = fp2_from_array([alpha1, alpha2]);
    let beta = fp2_from_array([beta1, beta2]);

    let lhs_folded = mul_ext(Fp2::Fp2(lhs_selector, 0), compress_expression_array(lhs, alpha));
    let rhs_folded = mul_ext(Fp2::Fp2(rhs_selector, 0), compress_expression_array(rhs, alpha));

    let next_acc = if with_extension {
        next_ext(acc_ext)
    } else {
        // The second component is 0, but the next operator is not defined on it...
        Fp2::Fp2(acc[0]', 0)
    };

    // Update rule:
    // acc' = acc * (beta - lhs_folded) / (beta - rhs_folded)
    // => (beta - rhs_folded) * acc' - (beta - lhs_folded) * acc = 0
    let (update_expr_1, update_expr_2) = unpack_ext(sub_ext(
        mul_ext(sub_ext(beta, rhs_folded), next_acc),
        mul_ext(sub_ext(beta, lhs_folded), acc_ext)
    ));

    let (acc_1, acc_2) = unpack_ext(acc_ext);

    [
        // First and last z needs to be 1
        // (because of wrapping, the z[0] and z[N] are the same)
        is_first * (acc_1 - 1) = 0,

        // Note that if with_extension is false, this generates 0 = 0 and is removed
        // by the optimizer.
        is_first * acc_2 = 0,

        // Assert that the update rule has been obeyed
        update_expr_1 = 0,

        // Again, update_expr_2 will be equal to 0 in the non-extension case.
        update_expr_2 = 0
    ]
};