use std::utils::unchanged_until;
use std::utils::cross_product;
use std::convert::int;

// Rotate for single bytes using an exhaustive table, returning two 16-bit values as result.
// TODO this way, we cannot prove anything that rotates by more than 31 bits.
machine ByteRotate with
    latch: latch,
    operation_id: operation_id,
    degree: 65536
{
    // P_C0 and P_C1 are both 16 bit limbs of P_C, where P_C0 is the less significant limb.
    operation run<0> P_operation, P_A, P_B, P_ROW -> P_C0, P_C1;

    col fixed latch = [1]*;
    col fixed operation_id = [0]*;

    let bit_counts = [256, 32, 4, 2];
    let min_degree = std::array::product(bit_counts);
    std::check::assert(std::prover::min_degree() >= std::array::product(bit_counts), || "The rotate machine needs at least 65536 rows to work.");
    std::check::assert(std::field::modulus() >= 65535, || "The field modulo should be at least 2^16 - 1 to work in the rotate machine.");
    let inputs = cross_product(bit_counts);
    let a: int -> int = inputs[0];
    let b: int -> int = inputs[1];
    let row: int -> int = inputs[2];
    let op: int -> int = inputs[3];
    let P_A: col = a;
    let P_B: col = b;
    let P_ROW: col = row;
    let P_operation: col = op;
    let c: int -> int = |i| match op(i) {
        0 => a(i) << (b(i) + (row(i) * 8)) | (a(i) << (row(i) * 8)) >> (32 - b(i)),
        1 => ((a(i) << (row(i) * 8)) >> b(i)) | (a(i) << (32 - b(i) + (row(i) * 8))),
    };
    col fixed P_C0(i) { c(i) & 0xffff };
    col fixed P_C1(i) { (c(i) >> 16) & 0xffff };
}

machine Rotate(byte_rotate: ByteRotate) with
    latch: latch,
    operation_id: operation_id,
    // Allow this machine to be connected via a permutation
    call_selectors: sel,
{
    operation rotl<0> A0, A1, B -> C0, C1;

    operation rotr<1> A0, A1, B -> C0, C1;

    col witness operation_id;
    unchanged_until(operation_id, latch);

    col fixed latch(i) { if (i % 4) == 3 { 1 } else { 0 } };
    col fixed FACTOR_ROW(i) { (i + 1) % 4 };
    col fixed FACTOR_A0 = [256, 0, 0, 1]*;
    col fixed FACTOR_A1 = [0, 1, 256, 0]*;

    col witness A_byte;
    col witness C0_part, C1_part;

    col witness A0, A1;
    col witness B;
    col witness C0, C1;

    A0' = A0 * (1 - latch) + A_byte * FACTOR_A0;
    A1' = A1 * (1 - latch) + A_byte * FACTOR_A1;
    unchanged_until(B, latch);
    C0' = C0 * (1 - latch) + C0_part;
    C1' = C1 * (1 - latch) + C1_part;

    link => (C0_part, C1_part) = byte_rotate.run(operation_id', A_byte, B', FACTOR_ROW);
}
