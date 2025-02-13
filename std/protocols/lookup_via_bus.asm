use std::protocols::bus::bus_receive;
use std::protocols::bus::BusInteraction;
use std::protocols::bus::bus_multi_receive;
use std::array;

/// Given an ID, selector, and tuple, receives (ID, ...tuple) tuple from the bus
/// with a prover-provided multiplicity if the selector is 1.
let lookup_receive: expr, expr, expr[] -> () = constr |id, selector, tuple| {
    let multiplicities;
    (1 - selector) * multiplicities = 0;
    
    bus_receive(BusInteraction::Receive(id, tuple, multiplicities, selector));
};

/// Batched version of `lookup_receive` that uses the more column-saving `bus_multi_receive`.
/// Ideally, should use `bus_multi_receive` to batch both lookup and permutation receives.
/// Note that we cannot input BusInteraction::Receive, which is defined differently.
let lookup_multi_receive: (expr, expr, expr[])[] -> () = constr |inputs| {
    let inputs_inner = array::fold(inputs, [], constr |acc, input| {
        let (id, selector, tuple) = input;
        let multiplicity;
        (1 - selector) * multiplicity = 0;
        acc + [BusInteraction::Receive(id, tuple, multiplicity, selector)]
    });
    bus_multi_receive(inputs_inner);
};
