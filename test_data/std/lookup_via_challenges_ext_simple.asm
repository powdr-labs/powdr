use std::prelude::Query;
use std::convert::fe;
use std::protocols::lookup::lookup;
use std::protocols::lookup::compute_next_z;
use std::math::fp2::Fp2;
use std::prover::challenge;

machine Main with degree: 8 {
    col fixed a = [1, 1, 4, 1, 1, 2, 1, 1];
    let b;
    query |i| {
        std::prover::provide_value(b, i, fe(i + 1));
    };
    col fixed m = [6, 1, 0, 1, 0, 0, 0, 0];

    let lookup_constraint = [a] in [b];

    lookup(lookup_constraint, m);
}