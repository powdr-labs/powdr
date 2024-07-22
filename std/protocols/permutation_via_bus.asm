use std::array::len;
use std::check::assert;
use std::protocols::bus::bus_send;
use std::protocols::bus::bus_receive;
use std::protocols::bus::compute_next_z_send;
use std::protocols::bus::compute_next_z_receive;
use std::protocols::permutation::unpack_permutation_constraint;
use std::math::fp2::Fp2;

// Example usage of the bus: Implement a permutation constraint
// To make this sound, the last values of `acc_lhs` and `acc_rhs` need to be
// exposed as publics, and the verifier needs to assert that they sum to 0.
let permutation: expr, expr, expr[], expr[], Fp2<expr>, Fp2<expr>, Constr -> Constr[] = |is_first, id, acc_lhs, acc_rhs, alpha, beta, permutation_constraint| {
    let (lhs_selector, lhs, rhs_selector, rhs) = unpack_permutation_constraint(permutation_constraint);
    bus_send(is_first, id, lhs, lhs_selector, acc_lhs, alpha, beta) + bus_receive(is_first, id, rhs, rhs_selector, acc_rhs, alpha, beta)
};

let compute_next_z_send_permutation: expr, expr, Fp2<expr>, Fp2<expr>, Fp2<expr>, Constr -> fe[] = query |is_first, id, acc, alpha, beta, permutation_constraint| {
    let (lhs_selector, lhs, rhs_selector, rhs) = unpack_permutation_constraint(permutation_constraint);  
    compute_next_z_send(is_first, id, lhs, lhs_selector, acc, alpha, beta)
};

let compute_next_z_receive_permutation: expr, expr, Fp2<expr>, Fp2<expr>, Fp2<expr>, Constr -> fe[] = query |is_first, id, acc, alpha, beta, permutation_constraint| {
    let (lhs_selector, lhs, rhs_selector, rhs) = unpack_permutation_constraint(permutation_constraint);  
    compute_next_z_receive(is_first, id, rhs, rhs_selector, acc, alpha, beta)
};