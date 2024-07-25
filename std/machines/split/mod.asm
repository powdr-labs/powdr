mod split_bn254;
mod split_gl;

use std::utils::cross_product;

// Byte comparison block machine
machine ByteCompare with latch: latch, operation_id: operation_id {
    let inputs = cross_product([256, 256]);
    let a: int -> int = inputs[0];
    let b: int -> int = inputs[1];
    let P_A: col = a;
    let P_B: col = b;
    col fixed P_LT(i) { if a(i) < b(i) { 1 } else { 0 } };
    col fixed P_GT(i) { if a(i) > b(i) { 1 } else { 0 } };

    operation run<0> P_A, P_B -> P_LT, P_GT;

    col fixed latch = [1]*;
    col fixed operation_id = [0]*;
}