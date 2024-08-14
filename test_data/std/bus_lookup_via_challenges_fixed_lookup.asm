use std::prover::Query;
use std::convert::fe;
use std::protocols::lookup_via_bus::lookup;
use std::math::fp2::from_base;
use std::prover::challenge;

machine Main with degree: 8 {
    
    let alpha = from_base(challenge(0, 1));
    let beta = from_base(challenge(0, 2));

    col fixed a_sel = [1, 1, 0, 1, 0, 0, 0, 0];
    col fixed b_sel = [1, 1, 1, 0, 0, 0, 0, 0];

    col witness a1(i) query Query::Hint(fe(i+1));
    col witness a2(i) query Query::Hint(fe(i+1));
    col witness a3(i) query Query::Hint(fe(i+1));
    col fixed b1 = [1, 2, 4, 3, 2, 8, 4, 1];
    col fixed b2 = [1, 2, 4, 1, 1, 7, 5, 1];
    col fixed b3 = [1, 2, 4, 1, 1, 1, 4, 3];
    col fixed m = [1, 1, 1, 0, 0, 0, 0, 0];

    [a1, a2, a3] in [b1, b2, b3];

    let lookup_constraint = Constr::Lookup(
        (Option::Some(a_sel), Option::Some(b_sel)),
        [(a1, b1), (a2, b2), (a3, b3)]
    );

    // TODO: Functions currently cannot add witness columns at later stages,
    // so we have to manually create it here and pass it to lookup(). 
    col witness stage(1) z;
    col witness stage(1) u;

    let is_first: col = std::well_known::is_first;
    lookup(is_first, 1, [z], [u], alpha, beta, lookup_constraint, m);

    is_first' * (z + u) = 0;
}
