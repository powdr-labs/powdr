use std::protocols::bus::bus_send;
use std::protocols::bus::bus_receive;
use std::protocols::permutation::unpack_permutation_constraint;
use std::constraints::to_phantom_permutation;

/// Given an ID and permutation constraints, receives the (ID, permutation_constraint.rhs...) tuple from the bus
/// with a prover-provided multiplicity if permutation_constraint.rhs_selector is 1.
/// Also adds an annotation for witness generation.
let permutation_receive: expr, Constr, expr -> () = constr |id, permutation_constraint, latch| {
    let (lhs_selector, lhs, rhs_selector, rhs) = unpack_permutation_constraint(permutation_constraint);
    
    bus_receive(id, rhs, rhs_selector, latch);
};