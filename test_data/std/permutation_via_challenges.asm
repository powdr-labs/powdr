use std::prover::Query;
use std::convert::fe;
use std::protocols::permutation::permutation;
use std::math::fp2::from_base;
use std::prover::challenge;

machine Main with degree: 8 {

    let alpha = from_base(challenge(0, 1));
    let beta = from_base(challenge(0, 2));

    col fixed first_four = [1, 1, 1, 1, 0, 0, 0, 0];

    // Two pairs of witness columns, claimed to be permutations of one another
    // (when selected by first_four and (1 - first_four), respectively)
    col witness a1(i) query Query::Hint(fe(i));
    col witness a2(i) query Query::Hint(fe(i + 42));
    col witness b1(i) query Query::Hint(fe(7 - i));
    col witness b2(i) query Query::Hint(fe(7 - i + 42));

    let permutation_constraint = Constr::Permutation(
        (Option::Some(first_four), Option::Some(1 - first_four)),
        [(a1, b1), (a2, b2)]
    );

    // TODO: Functions currently cannot add witness columns at later stages,
    // so we have to manually create it here and pass it to permutation(). 
    col witness stage(1) z;
    let is_first: col = std::well_known::is_first;
    permutation(is_first, [z], alpha, beta, permutation_constraint);

}
