use std::prover::challenge;
use std::array::fold;
use std::array::len;
use std::convert::int;
use std::check::assert;
use std::field::modulus;

let is_first: col = |i| if i == 0 { 1 } else { 0 };

// Get two phase-2 challenges to use in all permutation arguments.
// Note that this assumes that globally no other challenge of these IDs is used.
let alpha: expr = challenge(0, 1);
let beta: expr = challenge(0, 2);

// Maps [x_1, x_2, ..., x_n] to alpha**(n - 1) * x_1 + alpha ** (n - 2) * x_2 + ... + x_n
let compress_expression_array = |expr_array| fold(expr_array, 0, |acc, el| alpha * acc + el);

// Adds constraints that enforce that lhs is a permutation of rhs
// Arguments:
// - acc: A phase-2 witness column to be used as the accumulator
// - lhs_selector: (assumed to be) binary selector to check which elements from the LHS to include
// - lhs: An array of expressions
// - rhs_selector: (assumed to be) binary selector to check which elements from the RHS to include
// - rhs: An array of expressions
let permutation = |acc, lhs_selector, lhs, rhs_selector, rhs| {

    let _ = assert(len(lhs) == len(rhs), || "LHS and RHS should have equal length");
    let _ = assert(modulus() > 2**100, || "This implementation assumes a large field");

    let lhs_folded = lhs_selector * compress_expression_array(lhs);
    let rhs_folded = rhs_selector * compress_expression_array(rhs);

    [
        // First and last z needs to be 1
        // (because of wrapping, the z[0] and z[N] are the same)
        is_first * (acc - 1) = 0,

        // Update rule:
        // acc' = acc * (beta - lhs_folded) / (beta - rhs_folded)
        (beta - rhs_folded) * acc' = acc * (beta - lhs_folded)
    ]
};