use std::protocols::bus::bus_receive;

/// Given an ID, selector, tuple and latch, receives (ID, ...tuple) tuple from the bus
/// with a prover-provided multiplicity if the selector is 1.
let lookup_receive: expr, expr, expr[] -> () = constr |id, selector, tuple| {
    let multiplicities;
    (1 - selector) * multiplicities = 0;
    
    bus_receive(id, tuple, multiplicities, selector);
};