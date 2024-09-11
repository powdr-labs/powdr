mod split_bn254;
mod split_gl;

use std::utils::cross_product;

// Byte comparison block machine
machine ByteCompare with
        latch: latch,
        operation_id: operation_id,
        degree: 65536
    {
    let inputs = cross_product([256, 256]);
    let a: int -> int = inputs[0];
    let b: int -> int = inputs[1];
    let P_A: col = a;
    let P_B: col = b;
    let P_LT: col = |i| { if a(i) < b(i) { 1 } else { 0 } };
    let P_GT: col = |i| { if a(i) > b(i) { 1 } else { 0 } };

    operation run<0> P_A, P_B -> P_LT, P_GT;

    let latch: col = |i| 1;
    let operation_id: col = |i| 0;
}