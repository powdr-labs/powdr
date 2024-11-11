use std::array::len;
use std::check::assert;
use std::protocols::bus::bus_send;
use std::protocols::bus::bus_receive;
use std::protocols::lookup::unpack_lookup_constraint;
use std::math::fp2::Fp2;
use std::constraints::to_phantom_lookup;

let lookup_send: expr, Constr -> () = constr |id, lookup_constraint| {
    let (lhs_selector, lhs, rhs_selector, rhs) = unpack_lookup_constraint(lookup_constraint);

    bus_send(id, lhs, lhs_selector);
};

let lookup_receive: expr, Constr -> () = constr |id, lookup_constraint| {
    let (lhs_selector, lhs, rhs_selector, rhs) = unpack_lookup_constraint(lookup_constraint);

    let multiplicities;
    (1 - rhs_selector) * multiplicities = 0;
    
    bus_receive(id, rhs, multiplicities);
    
    // Add an annotation for witness generation
    to_phantom_lookup(lookup_constraint, multiplicities);
};