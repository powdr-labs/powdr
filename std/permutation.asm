use std::prover::challenge;
use std::array::fold;
use std::array::len;
use std::convert::int;
use std::check::assert;

let is_first: col = |i| if i == 0 { 1 } else { 0 };

let alpha: expr = challenge(0, 1);
let beta: expr = challenge(0, 2);

// Maps [x_1, x_2, ..., x_n] to alpha**(n - 1) * x_1 + alpha ** (n - 2) * x_2 + ... + x_n
let compress_expressions = |tuple| fold(tuple, 0, |acc, el| alpha * acc + el);

// Adds constraints that lhs is a permutation of rhs
// Arguments:
// - acc: A phase-2 witness column to be used as the accumulator
// - lhs_selector: (assumed to be) binary selector to check which elements from the LHS to include
// - lhs: An array of expressions
// - rhs_selector: (assumed to be) binary selector to check which elements from the RHS to include
// - rhs: An array of expressions
let permutation = |acc, lhs_selector, lhs, rhs_selector, rhs| {

    // TODO: This doesn't work: Tried to add a constraint in a pure context
    // assert(len(lhs) == len(rhs), || "LHS and RHS should have equal length");

    let lhs_folded = lhs_selector * compress_expressions(lhs);
    let rhs_folded = rhs_selector * compress_expressions(rhs);

    [
        // First and last z needs to be 1
        // (because of wrapping, the z[0] and z[N] are the same)
        is_first * (acc - 1) = 0,

        // Update rule:
        // acc' = acc * (beta - lhs_folded) / (beta - rhs_folded)
        (beta - rhs_folded) * acc' = acc * (beta - lhs_folded)
    ]
};