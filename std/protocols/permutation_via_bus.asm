use std::protocols::bus::bus;
use std::protocols::bus::bus_multi;
use std::protocols::bus::BusInteraction;
use std::array;

/// Given an ID, selector, and tuple, receives (ID, ...tuple) tuple from the bus
/// with multiplicity 1 if the selector is 1.
let permutation_receive: expr, expr, expr[] -> () = constr |id, selector, tuple| {
    bus(BusInteraction::Receive(id, tuple, selector, selector));
};

/// Batched version of `permutation_receive` that uses the more column-saving `bus_multi`.
/// Ideally, should use `bus_multi` to batch both lookup and permutation receives.
/// Note that we cannot input BusInteraction::Receive, which is defined differently.
let permutation_multi_receive: (expr, expr, expr[])[] -> () = constr |inputs| {
    let inputs_inner = array::fold(inputs, [], |acc, input| {
        let (id, selector, tuple) = input;
        acc + [BusInteraction::Receive(id, tuple, selector, selector)]
    });
    bus_multi(inputs_inner);
};
