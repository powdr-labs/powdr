use std::protocols::bus::bus_send;
use std::protocols::bus::bus_receive;
use std::protocols::lookup::unpack_lookup_constraint;
use std::constraints::to_phantom_lookup;

/// Given an ID and lookup constraints, sends the (ID, lookup_constraint.lhs...) tuple to the bus
/// if lookup_constraint.lhs_selector is 1.
let lookup_send: expr, Constr -> () = constr |id, lookup_constraint| {
    let (lhs_selector, lhs, rhs_selector, rhs) = unpack_lookup_constraint(lookup_constraint);

    bus_send(id, lhs, lhs_selector);
};

/// Given an ID and lookup constraints, receives the (ID, lookup_constraint.rhs...) tuple from the bus
/// with a prover-provided multiplicity if lookup_constraint.rhs_selector is 1.
/// Also adds an annotation for witness generation.
let lookup_receive: expr, Constr -> () = constr |id, lookup_constraint| {
    let (lhs_selector, lhs, rhs_selector, rhs) = unpack_lookup_constraint(lookup_constraint);

    let multiplicities;
    (1 - rhs_selector) * multiplicities = 0;
    
    bus_receive(id, rhs, multiplicities);
    
    // Add an annotation for witness generation
    to_phantom_lookup(lookup_constraint, multiplicities);
};