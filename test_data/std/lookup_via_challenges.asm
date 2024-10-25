use std::prelude::Query;
use std::convert::fe;
use std::protocols::lookup::lookup;
use std::math::fp2::from_base;
use std::prover::challenge;

machine Main with degree: 8 {

    // Pre-compute f(X) = X + 1 for X = 1, 2, 3, 4, 5, 6, 7, 8
    col fixed X_PRECOMPUTED = [1, 2, 3, 4, 5, 6, 7, 8];
    col fixed Y_PRECOMPUTED = [2, 3, 4, 5, 6, 7, 8, 9];

    // Runtime values, masking some elements
    col fixed x = [2, 2, 5, 4, 7, 8, 3, 3];
    col fixed MASK = [1, 1, 0, 1, 1, 1, 0, 1];
    col witness y;

    col fixed m = [0, 2, 1, 1, 0, 0, 1, 1];

    let lookup_constraint = MASK $ [x, y] in [X_PRECOMPUTED, Y_PRECOMPUTED];

    lookup(lookup_constraint, m);
}
