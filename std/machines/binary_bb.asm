use std::convert::int;
use std::utils::cross_product;
use std::utils::unchanged_until;

// Binary for single bytes using an exhaustive table
machine ByteBinary with
    latch: latch,
    operation_id: operation_id
{
    operation run<0> P_operation, P_A, P_B -> P_C;

    col fixed latch = [1]*;
    col fixed operation_id = [0]*;

    let bit_counts = [256, 256, 3];
    let min_degree = std::array::product(bit_counts);
    std::check::assert(std::prover::degree() >= std::array::product(bit_counts), || "The binary machine needs at least 196608 rows to work.");
    // TODO would be nice with destructuring assignment for arrays.
    let inputs: (int -> int)[] = cross_product(bit_counts);
    let a = inputs[0];
    let b = inputs[1];
    let op = inputs[2];
    let P_A: col = a;
    let P_B: col = b;
    let P_operation: col = op;
    col fixed P_C(i) {
        match op(i) {
            0 => a(i) & b(i),
            1 => a(i) | b(i),
            2 => a(i) ^ b(i),
        }
    };
}

machine Binary8(byte_binary: ByteBinary) with
    latch: latch,
    operation_id: operation_id,
    // Allow this machine to be connected via a permutation
    call_selectors: sel,
{
    operation and<0> A1, A2, A3, A4, B1, B2, B3, B4 -> C1, C2, C3, C4;

    operation or<1> A1, A2, A3, A4, B1, B2, B3, B4 -> C1, C2, C3, C4;

    operation xor<2> A1, A2, A3, A4, B1, B2, B3, B4 -> C1, C2, C3, C4;

    col witness operation_id;

    col fixed latch(i) { 1 };

    col witness A1, A2, A3, A4;
    col witness B1, B2, B3, B4;
    col witness C1, C2, C3, C4;

    link => C1 = byte_binary.run(operation_id, A1, B1);
    link => C2 = byte_binary.run(operation_id, A2, B2);
    link => C3 = byte_binary.run(operation_id, A3, B3);
    link => C4 = byte_binary.run(operation_id, A4, B4);
}

machine Binary16(byte_binary: ByteBinary) with
    latch: latch,
    operation_id: operation_id,
    // Allow this machine to be connected via a permutation
    call_selectors: sel,
{
    operation and<0> I1, I2, I3, I4 -> O1, O2;
    operation or<1> I1, I2, I3, I4 -> O1, O2;
    operation xor<2> I1, I2, I3, I4 -> O1, O2;

    col witness operation_id;

    col fixed latch(i) { 1 };

    let I1: inter = A1 + 256 * A2;
    let I2: inter = A3 + 256 * A4;
    let I3: inter = B1 + 256 * B2;
    let I4: inter = B3 + 256 * B4;
    let O1: inter = C1 + 256 * C2;
    let O2: inter = C3 + 256 * C4;

    col witness A1, A2, A3, A4;
    col witness B1, B2, B3, B4;
    col witness C1, C2, C3, C4;

    link => C1 = byte_binary.run(operation_id, A1, B1);
    link => C2 = byte_binary.run(operation_id, A2, B2);
    link => C3 = byte_binary.run(operation_id, A3, B3);
    link => C4 = byte_binary.run(operation_id, A4, B4);
}
