use std::prover::Query;
use std::convert::fe;
use std::prover::challenge;
use std::prover::eval;
use std::math::ff::inverse;
use std::convert::int;
use std::field::modulus;
use std::protocols::permutation::permutation;
use std::protocols::permutation::beta1;
use std::protocols::permutation::beta2;
use std::protocols::permutation::compute_next_z;
use std::math::fp2::Fp2Expr;
use std::math::fp2::Fp2Value;
use std::math::fp2::add_ext;
use std::math::fp2::sub_ext;
use std::math::fp2::mul_ext;
use std::math::fp2::inv_ext;
use std::math::fp2::eval_ext;
use std::math::fp2::expr_ext;

machine Main with degree: 8 {
    col fixed first_four = [1, 1, 1, 1, 0, 0, 0, 0];

    // Two pairs of witness columns, claimed to be permutations of one another
    // (when selected by first_four and (1 - first_four), respectively)
    col witness a1(i) query Query::Hint(fe(i));
    col witness a2(i) query Query::Hint(fe(i + 42));
    col witness b1(i) query Query::Hint(fe(7 - i));
    col witness b2(i) query Query::Hint(fe(7 - i + 42));

    // TODO: Functions currently cannot add witness columns at later stages,
    // so we have to manually create it here and pass it to permutation(). 
    col witness stage(1) z1;
    col witness stage(1) z2;

    // TODO: Helper columns, because we can't access the previous row in hints
    col witness stage(1) z1_next(i) query Query::Hint(compute_next_z(Fp2Expr::Fp2(z1, z2), first_four, [a1, a2], 1 - first_four, [b1, b2])[0]);
    col witness stage(1) z2_next(i) query Query::Hint(compute_next_z(Fp2Expr::Fp2(z1, z2), first_four, [a1, a2], 1 - first_four, [b1, b2])[1]);

    z1' = z1_next;
    z2' = z2_next;

    permutation([z1, z2], first_four, [a1, a2], 1 - first_four, [b1, b2]);
}
