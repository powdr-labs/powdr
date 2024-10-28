use std::prelude::Query;
use std::convert::fe;
use std::protocols::permutation_via_bus::permutation;
use std::protocols::permutation_via_bus::compute_next_z_send_permutation;
use std::protocols::permutation_via_bus::compute_next_z_receive_permutation;
use std::math::fp2::Fp2;
use std::prover::challenge;

machine Main with degree: 8 {

    col fixed first_four = [1, 1, 1, 1, 0, 0, 0, 0];

    // Two pairs of witness columns, claimed to be permutations of one another
    // (when selected by first_four and (1 - first_four), respectively)
    col witness a1, a2, b1, b2;
    query |i| {
        std::prover::provide_value(a1, i, fe(i));
        std::prover::provide_value(a2, i, fe(i + 42));
        std::prover::provide_value(b1, i, fe(7 - i));
        std::prover::provide_value(b2, i, fe(7 - i + 42));
    };

    let permutation_constraint = first_four $ [a1, a2] is (1 - first_four) $ [b1, b2];

    permutation(1, permutation_constraint);

    let is_first: col = std::well_known::is_first;
    
    // TODO
    // is_first' * (z1 + u1) = 0;
    // is_first' * (z2 + u2) = 0;
}
