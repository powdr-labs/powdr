use std::prover::Query;
use std::convert::fe;
use std::prover::challenge;
use std::prover::eval;
use std::math::ff::inverse;
use std::convert::int;
use std::field::modulus;
use std::protocols::permutation::permutation;
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

    // TODO: Copied from permutation.asm
    let beta1: expr = challenge(0, 3);
    let beta2: expr = challenge(0, 4);

    // Compute z' = z * (beta - a) / (beta - b), using extension field arithmetic
    let compute_next_z = query || {
        let z = Fp2Expr::Fp2(z1, z2);
        let a = Fp2Expr::Fp2(a1, 0);
        let b = Fp2Expr::Fp2(b1, 0);
        // TODO: Witgen doesn't filter out queries with challenges of a later phase, use a hard-coded challenge for now.
        let beta = Fp2Expr::Fp2(1, 2);
        //let beta = [beta1, beta2];
        
        let res = eval_ext(mul_ext(mul_ext(z, sub_ext(beta, a)), expr_ext(inv_ext(eval_ext(sub_ext(beta, b))))));

        match res {
            Fp2Value::Fp2(a0, a1) => [a0, a1]
        }
    };

    // TODO: Functions currently cannot add witness columns at later stages,
    // so we have to manually create it here and pass it to permutation(). 
    col witness stage(1) z1;
    col witness stage(1) z2;

    // TODO: Helper columns, because we can't access the previous row in hints
    col witness stage(1) z1_next(i) query Query::Hint(compute_next_z()[0]);
    col witness stage(1) z2_next(i) query Query::Hint(compute_next_z()[1]);

    z1' = z1_next;
    z2' = z2_next;

    permutation([z1, z2], 1, [a1], 1, [b1]);
    //permutation([z1, z2], first_four, [a1, a2], 1 - first_four, [b1, b2]);
}
