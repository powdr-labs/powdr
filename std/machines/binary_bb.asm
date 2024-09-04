use std::convert::int;
use std::utils::cross_product;
use std::utils::unchanged_until;
use std::machines::binary::ByteBinary;

// Computes bitwise operations on two 32-bit numbers
// decomposed into 4 bytes each.
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

// Computes bitwise operations on two 32-bit numbers
// decomposed into two 16-bit limbs each.
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
