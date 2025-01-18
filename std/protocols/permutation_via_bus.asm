use std::protocols::bus::bus_send;
use std::protocols::bus::bus_receive;
use std::protocols::permutation::unpack_permutation_constraint;
use std::constraints::to_phantom_permutation;

/// Given an ID, selector, tuple and latch, receives (ID, ...tuple) tuple from the bus
/// with multiplicity 1 if the selector is 1.
let permutation_receive: expr, expr, expr[], expr -> () = constr |id, selector, tuple, latch| {
    bus_receive(id, tuple, selector, latch);
};