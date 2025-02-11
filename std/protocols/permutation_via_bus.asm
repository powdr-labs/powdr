use std::protocols::bus::bus_receive;
use std::protocols::bus::bus_multi_receive;
use std::array;

/// Given an ID, selector, and tuple, receives (ID, ...tuple) tuple from the bus
/// with multiplicity 1 if the selector is 1.
let permutation_receive: expr, expr, expr[] -> () = constr |id, selector, tuple| {
    bus_receive(id, tuple, selector, selector);
};

let permutation_multi_receive: (expr, expr, expr[])[] -> () = constr |inputs| {
    let inputs_inner: (expr, expr[], expr, expr)[] = array::fold(inputs, [], |acc, input| {
        let (id, selector, tuple) = input;
        acc + [(id, tuple, selector, selector)]
    });
    bus_multi_receive(inputs_inner);
};
