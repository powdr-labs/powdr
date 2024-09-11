use std::utils::unchanged_until;
use std::utils::cross_product;
use std::convert::int;

// Shift for single bytes using an exhaustive table
// TODO this way, we cannot prove anything that shifts by more than 31 bits.
machine ByteShift with
    latch: latch,
    operation_id: operation_id,
    degree: 65536
{
    operation run<0> P_operation, P_A, P_B, P_ROW -> P_C;

    let latch: col = |i| 1;
    let operation_id: col = |i| 0;

    let bit_counts = [256, 32, 4, 2];
    let min_degree = std::array::product(bit_counts);
    std::check::assert(std::prover::min_degree() >= std::array::product(bit_counts), || "The shift machine needs at least 65536 rows to work.");
    let inputs = cross_product(bit_counts);
    let a: int -> int = inputs[0];
    let b: int -> int = inputs[1];
    let row: int -> int = inputs[2];
    let op: int -> int = inputs[3];
    let P_A: col = a;
    let P_B: col = b;
    let P_ROW: col = row;
    let P_operation: col = op;
    let P_C: col = |i| {
        match op(i) {
            0 => a(i) << (b(i) + (row(i) * 8)),
            1 => (a(i) << (row(i) * 8)) >> b(i),
        } & 0xffffffff
    };
}

machine Shift(byte_shift: ByteShift) with
    latch: latch,
    operation_id: operation_id,
    // Allow this machine to be connected via a permutation
    call_selectors: sel,
{
    operation shl<0> A, B -> C;

    operation shr<1> A, B -> C;

    let operation_id;
    unchanged_until(operation_id, latch);

    let latch: col = |i| { if (i % 4) == 3 { 1 } else { 0 } };
    let FACTOR_ROW: col = |i| { (i + 1) % 4 };
    let FACTOR: col = |i| { 1 << (((i + 1) % 4) * 8) };

    let A_byte;
    let C_part;

    let A;
    let B;
    let C;

    A' = A * (1 - latch) + A_byte * FACTOR;
    unchanged_until(B, latch);
    C' = C * (1 - latch) + C_part;

    link => C_part = byte_shift.run(operation_id', A_byte, B', FACTOR_ROW);
}
