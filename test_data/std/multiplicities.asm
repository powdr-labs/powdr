use std::convert::fe;
use std::protocols::lookup::lookup;
use std::math::fp2::from_base;
use std::prover::challenge;

machine Main with degree: 8 {

    col fixed x = [1, 5, 2, 6, 4, 2, 6, 3];
    col witness y;

    // Pre-compute f(x) = x + 1 for all x in [1, 8]
    col fixed INC_X = [1, 2, 3, 4, 5, 6, 7, 8];
    col fixed INC_Y = [2, 3, 4, 5, 6, 7, 8, 9];

    // Native lookup to implement y = f(x). From this, witgen should
    // figure out the values in column y via FixedLookup.
    [x, y] in [INC_X, INC_Y];

    // Machine extractor currently accepts the multiplicity column with exact the name of "m_logup_multiplicity"
    col witness m_logup_multiplicity;

    // This would be the correct multiplicity values that would satisfy the constraints:
    //col fixed m_logup_multiplicity = [1, 2, 1, 1, 1, 2, 0, 0];

    let lookup_constraint = [x, y] in [INC_X, INC_Y];

    lookup(lookup_constraint, m_logup_multiplicity);
}