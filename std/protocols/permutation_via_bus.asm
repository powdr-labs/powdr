use std::protocols::bus::bus_send;
use std::protocols::bus::bus_receive;
use std::protocols::permutation::unpack_permutation_constraint;
use std::math::fp2::Fp2;

// Example usage of the bus: Implement a permutation constraint
// To make this sound, the last values of `acc_lhs` and `acc_rhs` need to be
// exposed as publics, and the verifier needs to assert that they sum to 0.
let permutation: expr, Constr -> () = constr |id, permutation_constraint| {
    let (lhs_selector, lhs, rhs_selector, rhs) = unpack_permutation_constraint(permutation_constraint);
    bus_send(id, lhs, lhs_selector);
    bus_receive(id, rhs, rhs_selector);
};