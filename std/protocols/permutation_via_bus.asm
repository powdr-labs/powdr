use std::array::len;
use std::check::assert;
use std::protocols::bus::bus_send;
use std::protocols::bus::bus_receive;
use std::protocols::permutation::unpack_permutation_constraint;
use std::constraints::to_phantom_permutation;
use std::math::fp2::Fp2;

let permutation_send: expr, Constr -> () = constr |id, permutation_constraint| {
    let (lhs_selector, lhs, rhs_selector, rhs) = unpack_permutation_constraint(permutation_constraint);
    bus_send(id, lhs, lhs_selector);

    // Add an annotation for witness generation
    to_phantom_permutation(permutation_constraint);
};

let permutation_receive: expr, Constr -> () = constr |id, permutation_constraint| {
    let (lhs_selector, lhs, rhs_selector, rhs) = unpack_permutation_constraint(permutation_constraint);
    bus_receive(id, rhs, rhs_selector);
};