mod split_bn254;
mod split_gl;
mod split_gl_vec;
mod split_bb;

use std::utils::cross_product;

// Byte comparison block machine
machine ByteCompare with
        latch: latch,
        degree: 65536
    {
    let inputs = cross_product([256, 256]);
    let a: int -> int = inputs[0];
    let b: int -> int = inputs[1];
    let P_A: col = a;
    let P_B: col = b;
    col fixed P_LT(i) { if a(i) < b(i) { 1 } else { 0 } };
    col fixed P_GT(i) { if a(i) > b(i) { 1 } else { 0 } };

    operation run P_A, P_B -> P_LT, P_GT;

    col fixed latch = [1]*;
}
