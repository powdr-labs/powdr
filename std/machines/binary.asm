use std::convert::int;
use std::utils::cross_product;

// Binary for single bytes using an exhaustive table
machine ByteBinary with
    latch: latch,
    degree: 262144
{
    operation run P_operation, P_A, P_B -> P_C;

    col fixed latch = [1]*;

    let bit_counts = [256, 256, 3];
    let min_degree = std::array::product(bit_counts);
    std::check::assert(std::prover::min_degree() >= std::array::product(bit_counts), || "The binary machine needs at least 196608 rows to work.");
    // TODO would be nice with destructuring assignment for arrays.
    let inputs: (int -> int)[] = cross_product(bit_counts);
    let a = inputs[0];
    let b = inputs[1];
    let op = inputs[2];
    let P_A: col = a;
    let P_B: col = b;
    let P_operation: col = op;
    let P_C: col = |i| {
        match op(i) {
            0 => a(i) & b(i),
            1 => a(i) | b(i),
            2 => a(i) ^ b(i),
        }
    };
}
