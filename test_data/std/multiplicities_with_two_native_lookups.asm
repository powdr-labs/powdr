use std::prover::Query;
use std::convert::fe;
use std::protocols::lookup_via_bus::lookup;
use std::math::fp2::from_base;
use std::prover::challenge;

machine Main with degree: 8 {

    // Some input values
    col fixed x = [1, 5, 2, 6, 4, 2, 6, 3];
    // The computed result
    col witness y;

    // Pre-compute f(x) = x + 1 for all x in [1, 8]
    col fixed INC_X = [1, 2, 3, 4, 5, 6, 7, 8];
    col fixed INC_Y = [2, 3, 4, 5, 6, 7, 8, 9];

    // Native lookup to implement y = f(x). From this, witgen should
    // figure out the values in column y via FixedLookup.
    [x, y] in [INC_X, INC_Y];

    // Some input values
    col fixed x2 = [1, 5, 2, 6, 4, 2, 6, 3];
    // The computed result
    col witness y2;

    // Pre-compute f(x) = x + 1 for all x in [1, 8]
    col fixed INC_X2 = [1, 2, 3, 4, 5, 6, 7, 8];
    col fixed INC_Y2 = [2, 3, 4, 5, 6, 7, 8, 9];

    [x2, y2] in [INC_X2, INC_Y2];

    // The PIL implementation of LogUp
    let alpha = from_base(challenge(0, 1));
    let beta = from_base(challenge(0, 2));

    // This is the multiplicities witness column. Currently, witgen just sets it to
    // 0, causing the constraints to fail.
    //col witness m;

    // This would be the correct multiplicity values that would satisfy the constraints:
    col fixed m = [1, 2, 1, 1, 1, 2, 0, 0];

    let lookup_constraint = Constr::Lookup(
        (Option::None, Option::None),
        [(x, INC_X), (y, INC_Y)]
    );

    // TODO: Functions currently cannot add witness columns at later stages,
    // so we have to manually create it here and pass it to lookup(). 
    col witness stage(1) z;
    col witness stage(1) u;

    let is_first: col = std::well_known::is_first;
    lookup(is_first, 1, [z], [u], alpha, beta, lookup_constraint, m);

    is_first' * (z + u) = 0;
}