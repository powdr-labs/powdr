let N: int = 8;

use std::array;
use std::check;
use std::convert::fe_wrapping;

// TODO I think we do not have away to dynamically create a fixed column...

let permutation = constr |first, left, right| {
    std::check::assert_eq(array::len(left), array::len(right));

    let alpha = std::prover::challenge(0, 1);
    let beta = std::prover::challenge(0, 2);

    let left_compressed = compress_selected_exprs(alpha, beta, 1, left);
    let right_compressed = compress_selected_exprs(alpha, beta, 1, right);

    let z = std::prover::new_witness_in_stage("permutation_accumulator", 1);

    first * (z - 1) = 0;

    // Update rule:
    // z' = z * (beta - a) / (beta - b)
    (beta - left_compressed) * z' = z * (beta - right_compressed);
    // we need to return something...
    []
};

let compress_selected_exprs: expr, expr, expr, expr[] -> expr = |alpha, beta, sel, exprs|
    sel * (compress_columns(alpha, exprs) + beta - 1) + 1;

let compress_columns: expr, expr[] -> expr = |alpha, cols|
    std::array::fold(cols, 0, |acc, c| (acc + c) * alpha);

machine Main {

    col witness a(i) query std::prover::Query::Hint(fe_wrapping(i));
    col witness b(i) query std::prover::Query::Hint(fe_wrapping(i + 3));
    col witness c(i) query std::prover::Query::Hint(fe_wrapping(2048 - i));
    col witness d(i) query std::prover::Query::Hint(fe_wrapping(2048 - (i + 3)));

    let first: col = |i| if i == 0 { 1 } else { 0 };

    permutation(first, [a, b], [c, d]);
}