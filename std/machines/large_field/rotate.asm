use std::utils::unchanged_until;
use std::utils::cross_product;
use std::convert::int;
use std::check::require_field_bits;

/// Rotate on single byte input using an exhaustive table, returning 32-bit value as result.
/// We can rotate by at most 31 bits
machine ByteRotate with
    latch: latch,
    degree: 65536
{
    operation run P_operation, P_A, P_B, P_ROW -> P_C;

    col fixed latch = [1]*;
    let bit_counts = [256, 32, 4, 2];
    let min_degree = std::array::product(bit_counts);
    std::check::assert(std::prover::min_degree() >= std::array::product(bit_counts), || "The rotate machine needs at least 65536 rows to work.");
    require_field_bits(32, || "Rotate requires a field that fits any 32-Bit value.");
    
    let out_byte_offset = cross_product(bit_counts);
    let a: int -> int = out_byte_offset[0];
    let b: int -> int = out_byte_offset[1];
    let row: int -> int = out_byte_offset[2];
    let op: int -> int = out_byte_offset[3];

    let P_A: col = a;
    let P_B: col = b;
    let P_ROW: col = row;
    let P_operation: col = op;
    let P_C: col = |i| {
        match op(i) {
            0 => a(i) << (b(i) + (row(i) * 8)) | (a(i) << (row(i) * 8)) >> (32 - b(i)),
            1 => ((a(i) << (row(i) * 8)) >> b(i)) | (a(i) << (32 - b(i) + (row(i) * 8))),
        } & 0xffffffff
    };
}

machine Rotate(byte_rotate: ByteRotate) with
    latch: latch,
    operation_id: operation_id,
    // Allow this machine to be connected via a permutation
    call_selectors: sel,
{
    require_field_bits(32, || "Rotate requires a field that fits any 32-Bit value.");

    operation rotl<0> A, B -> C;

    operation rotr<1> A, B -> C;

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

    link => C_part = byte_rotate.run(operation_id', A_byte, B', FACTOR_ROW);
}
