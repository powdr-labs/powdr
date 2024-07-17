use std::prover::Query;
use std::convert::fe;
use std::protocols::permutation_via_bus::permutation;
use std::protocols::permutation_via_bus::compute_next_z_send_permutation;
use std::protocols::permutation_via_bus::compute_next_z_receive_permutation;
use std::math::fp2::Fp2;
use std::prover::challenge;

machine Main with degree: 8 {

    let alpha1: expr = challenge(0, 1);
    let alpha2: expr = challenge(0, 2);
    let beta1: expr = challenge(0, 3);
    let beta2: expr = challenge(0, 4);
    let alpha = Fp2::Fp2(alpha1, alpha2);
    let beta = Fp2::Fp2(beta1, beta2);

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
    col witness stage(1) z1;
    col witness stage(1) z2;
    let z = Fp2::Fp2(z1, z2);

    col witness stage(1) u1;
    col witness stage(1) u2;
    let u = Fp2::Fp2(u1, u2);

    let is_first: col = std::well_known::is_first;
    permutation(is_first, 1, [z1,z2], [u1, u2], alpha, beta, permutation_constraint);

    let hint_send = query |i| Query::Hint(compute_next_z_send_permutation(is_first, 1, z, alpha, beta, permutation_constraint)[i]);
    col witness stage(1) z1_next(i) query hint_send(0);
    col witness stage(1) z2_next(i) query hint_send(1);

    z1' = z1_next;
    z2' = z2_next;

    let hint_receive = query |i| Query::Hint(compute_next_z_receive_permutation(is_first, 1, u, alpha, beta, permutation_constraint)[i]);
    col witness stage(1) u1_next(i) query hint_receive(0);
    col witness stage(1) u2_next(i) query hint_receive(1);

    u1' = u1_next;
    u2' = u2_next;

    is_first' * (z1 + u1) = 0;
    is_first' * (z2 + u2) = 0;
}
