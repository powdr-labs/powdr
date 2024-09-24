use std::prelude::Query;
use std::convert::fe;
use std::protocols::lookup::lookup;
use std::protocols::lookup::compute_next_z;
use std::math::fp2::Fp2;
use std::prover::challenge;

machine Main with degree: 8 {
    col fixed a_sel = [0, 1, 1, 1, 0, 1, 0, 0];
    col fixed b_sel = [1, 1, 0, 1, 1, 1, 1, 0];

    col fixed a1 = [16, 20, 22, 17, 16, 20, 4, 1];
    col fixed a2 = [12, 5, 7, 2, 5, 5, 4, 1];
    col fixed a3 = [20, 36, 38, 33, 36, 36, 4, 3];
    col witness b1, b2, b3;
    query |i| {
        std::prover::provide_value(b1, i, fe(i + 16));
        std::prover::provide_value(b2, i, fe(i + 1));
        std::prover::provide_value(b3, i, fe(i + 32));
    };
    col fixed m = [0, 1, 0, 0, 2, 0, 1, 0];

    let lookup_constraint = a_sel $ [a1, a2, a3] in b_sel $ [b1, b2, b3];

    lookup(lookup_constraint, m);
}