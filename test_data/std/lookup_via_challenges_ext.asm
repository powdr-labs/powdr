use std::prelude::Query;
use std::convert::fe;
use std::protocols::lookup::lookup;
use std::protocols::lookup::compute_next_z;
use std::math::fp2::Fp2;
use std::prover::challenge;

machine Main with degree: 8 {

    let alpha1: expr = challenge(0, 1);
    let alpha2: expr = challenge(0, 2);
    let beta1: expr = challenge(0, 3);
    let beta2: expr = challenge(0, 4);
    let alpha = Fp2::Fp2(alpha1, alpha2);
    let beta = Fp2::Fp2(beta1, beta2);

    col fixed a_sel = [0, 1, 1, 1, 0, 1, 0, 0];
    col fixed b_sel = [1, 1, 0, 1, 1, 1, 1, 0];

    col fixed a1 = [16, 20, 22, 17, 16, 20, 4, 1];
    col fixed a2 = [12, 5, 7, 2, 5, 5, 4, 1];
    col fixed a3 = [20, 36, 38, 33, 36, 36, 4, 3];
    col witness b1(i) query Query::Hint(fe(i+16));
    col witness b2(i) query Query::Hint(fe(i+1));
    col witness b3(i) query Query::Hint(fe(i+32));
    col fixed m = [0, 1, 0, 0, 2, 0, 1, 0];

    let lookup_constraint = Constr::Lookup(
        (Option::Some(a_sel), Option::Some(b_sel)),
        [(a1, b1), (a2, b2), (a3, b3)]
    );

    // TODO: Functions currently cannot add witness columns at later stages,
    // so we have to manually create it here and pass it to lookup(). 
    col witness stage(1) z1;
    col witness stage(1) z2;
    let z = Fp2::Fp2(z1, z2);

    let is_first: col = std::well_known::is_first;
    lookup(is_first, [z1, z2], alpha, beta, lookup_constraint, m);

    // TODO: Helper columns, because we can't access the previous row in hints
    let hint = query |i| Query::Hint(compute_next_z(z, alpha, beta, lookup_constraint, m)[i]); 
    col witness stage(1) z1_next(i) query hint(0);
    col witness stage(1) z2_next(i) query hint(1);

    z1' = z1_next;
    z2' = z2_next;
}