use std::array::len;
use std::check::assert;
use std::protocols::bus::bus_send;
use std::protocols::bus::bus_receive;
use std::protocols::bus::compute_next_z_send;
use std::protocols::bus::compute_next_z_receive;
use std::protocols::lookup::unpack_lookup_constraint;
use std::math::fp2::Fp2;

// Example usage of the bus: Implement a lookup constraint
// To make this sound, the last values of `acc_lhs` and `acc_rhs` need to be
// exposed as publics, and the verifier needs to assert that they sum to 0.
let lookup: expr, Constr, expr -> () = constr |id, lookup_constraint, multiplicities| {
    let (lhs_selector, lhs, rhs_selector, rhs) = unpack_lookup_constraint(lookup_constraint);
    bus_send(id, lhs, lhs_selector);
    bus_receive(id, rhs, rhs_selector * multiplicities);
};

// Sends looked up values to the bus
let compute_next_z_send_lookup: expr, expr, Fp2<expr>, Fp2<expr>, Fp2<expr>, Constr -> fe[] = query |is_first, id, acc, alpha, beta, lookup_constraint| {
    let (lhs_selector, lhs, rhs_selector, rhs) = unpack_lookup_constraint(lookup_constraint);  
    compute_next_z_send(is_first, id, lhs, lhs_selector, acc, alpha, beta)
};

// Receives the lookup table with multiplicity
let compute_next_z_receive_lookup: expr, expr, Fp2<expr>, Fp2<expr>, Fp2<expr>, Constr, expr -> fe[] = query |is_first, id, acc, alpha, beta, lookup_constraint, multiplicities| {
    let (lhs_selector, lhs, rhs_selector, rhs) = unpack_lookup_constraint(lookup_constraint);  
    compute_next_z_receive(is_first, id, rhs, rhs_selector * multiplicities, acc, alpha, beta)
};