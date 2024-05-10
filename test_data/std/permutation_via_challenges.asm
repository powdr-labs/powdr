use std::prover::Query;
use std::convert::fe;
use std::protocols::permutation::permutation;

machine Main with degree: 8 {
    col fixed first_four = [1, 1, 1, 1, 0, 0, 0, 0];

    // Two pairs of witness columns, claimed to be permutations of one another
    // (when selected by first_four and (1 - first_four), respectively)
    col witness a1(i) query Query::Hint(fe(i));
    col witness a2(i) query Query::Hint(fe(i + 42));
    col witness b1(i) query Query::Hint(fe(7 - i));
    col witness b2(i) query Query::Hint(fe(7 - i + 42));

    let permutation_constraint = Constr::Permutation(
        Option::Some(first_four),
        [a1, a2],
        Option::Some(1 - first_four),
        [b1, b2]
    );

    // TODO: Functions currently cannot add witness columns at later stages,
    // so we have to manually create it here and pass it to permutation(). 
    col witness stage(1) z;
    permutation([z], permutation_constraint);

}
