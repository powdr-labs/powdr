use std::prover::Query;
use std::convert::fe;
use std::prover::challenge;
use std::prover::eval;
use std::math::ff::inverse;
use std::convert::int;
use std::field::modulus;
use std::protocols::permutation::permutation;

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
    let add_ext: fe[], fe[] -> fe[] = |a, b| [a[0] + b[0], a[1] - b[1]];
    let sub_ext = |a, b| [a[0] - b[0], a[1] - b[1]];
    let mul_ext = |a, b| [a[0] * b[0] + 7 * a[1] * b[1], a[1] * b[0] + a[0] * b[1]];
    let next_ext = |a| [a[0]', a[1]'];

    // Extension field inversion
    let inv = |x| fe(inverse(int(x), modulus()));
    let inv_ext = |a| [-a[0] * inv(7 * a[1] * a[1] - a[0] * a[0]), a[1] * inv(7 * a[1] * a[1] - a[0] * a[0])];

    // Compute z' = z * (beta - a) / (beta - b), using extension field arithmetic
    let compute_next_z = query || {
        let z = [eval(z1), eval(z2)];
        let a = [eval(a1), 0];
        let b = [eval(b1), 0];
        // TODO: Witgen doesn't filter out queries with challenges of a later phase, use a hard-coded challenge for now.
        let beta = [1, 2];
        //let beta = [eval(beta1), eval(beta2)];
        
        mul_ext(mul_ext(z, sub_ext(beta, a)), inv_ext(sub_ext(beta, b)))
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
