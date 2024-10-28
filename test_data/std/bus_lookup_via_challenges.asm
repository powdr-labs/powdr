use std::prelude::Query;
use std::convert::fe;
use std::protocols::lookup_via_bus::lookup;
use std::math::fp2::from_base;
use std::prover::challenge;

machine Main with degree: 8 {

    col fixed random_six = [1, 1, 1, 0, 1, 1, 1, 0];
    col fixed first_seven = [1, 1, 1, 1, 1, 1, 1, 0];

    col fixed a1 = [1, 2, 4, 3, 1, 1, 4, 1];
    col fixed a2 = [1, 2, 4, 1, 1, 1, 4, 1];
    col fixed a3 = [1, 2, 4, 1, 1, 1, 4, 3];
    col witness b1, b2, b3;
    query |i| {
        std::prover::provide_value(b1, i, fe(i + 1));
        std::prover::provide_value(b2, i, fe(i + 1));
        std::prover::provide_value(b3, i, fe(i + 1));
    };
    col fixed m = [3, 1, 0, 2, 0, 0, 0, 0];

    let lookup_constraint = random_six $ [a1, a2, a3] in first_seven $ [b1, b2, b3];

    lookup(1, lookup_constraint, m);

    /*
    let is_first: col = std::well_known::is_first;
    is_first' * (z + u) = 0;
    */
}
