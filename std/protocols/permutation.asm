use std::prover::challenge;
use std::array::fold;
use std::array::len;
use std::convert::int;
use std::check::assert;
use std::field::modulus;
use std::math::fp2::Fp2Expr;
use std::math::fp2::add_ext;
use std::math::fp2::sub_ext;
use std::math::fp2::mul_ext;

let next_ext = |a| match a {
    Fp2Expr::Fp2(a0, a1) => Fp2Expr::Fp2(a0', a1')
};
let unpack_ext = |a| match a {
    Fp2Expr::Fp2(a0, a1) => [a0, a1]
};

let is_first: col = |i| if i == 0 { 1 } else { 0 };

// Get two phase-2 challenges to use in all permutation arguments.
// Note that this assumes that globally no other challenge of these IDs is used.
let alpha1: expr = challenge(0, 1);
let alpha2: expr = challenge(0, 2);

let beta1: expr = challenge(0, 3);
let beta2: expr = challenge(0, 4);

// Adds constraints that enforce that lhs is a permutation of rhs
// Arguments:
// - acc: A phase-2 witness column to be used as the accumulator
// - lhs_selector: (assumed to be) binary selector to check which elements from the LHS to include
// - lhs: An array of expressions
// - rhs_selector: (assumed to be) binary selector to check which elements from the RHS to include
// - rhs: An array of expressions
let permutation = |acc, lhs_selector, lhs, rhs_selector, rhs| {

    let GOLDILOCKS_PRIME = 2**64 - 2**32 + 1;
    let _ = assert(len(lhs) == len(rhs), || "LHS and RHS should have equal length");
    let _ = assert(modulus() > 2**100 || modulus() == GOLDILOCKS_PRIME, || "No implementation on this small field");
    let _ = assert(len(acc) == 2, || "Expected 2 accumulators");

    let acc_ext = Fp2Expr::Fp2(acc[0], acc[1]);

    // On the Goldilocks field, we evaluate the polynomial on the F_{p^2} extension field
    // modulo the irreducible polynomial x^2 - 7.
    // TODO: This is always true, to test that phase-2 witgen works
    let needs_extension = modulus() != 42;
    // let needs_extension = modulus() == GOLDILOCKS_PRIME;

    // On the extension field, we'll need two field elements to represent the challenge.
    // If we don't need an extension field, we can simply set the second component to 0,
    // in which case the operations below effectively only operate on the first component.
    let alpha = if needs_extension {Fp2Expr::Fp2(alpha1, alpha2)} else {Fp2Expr::Fp2(alpha1, 0)};
    let beta = if needs_extension {Fp2Expr::Fp2(beta1, beta2)} else {Fp2Expr::Fp2(beta1, 0)};

    // Maps [x_1, x_2, ..., x_n] to alpha**(n - 1) * x_1 + alpha ** (n - 2) * x_2 + ... + x_n
    let compress_expression_array = |expr_array| fold(expr_array, Fp2Expr::Fp2(0, 0), |sum_acc, el| add_ext(mul_ext(alpha, sum_acc), Fp2Expr::Fp2(el, 0)));

    let lhs_folded = mul_ext(Fp2Expr::Fp2(lhs_selector, 0), compress_expression_array(lhs));
    let rhs_folded = mul_ext(Fp2Expr::Fp2(rhs_selector, 0), compress_expression_array(rhs));

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