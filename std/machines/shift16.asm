use std::utils::unchanged_until;
use std::utils::cross_product;
use std::convert::int;

// Shift for single bytes using an exhaustive table, returning two 16-bit values as result.
// TODO this way, we cannot prove anything that shifts by more than 31 bits.
machine ByteShift16 with
    latch: latch,
    operation_id: operation_id,
    degree: 65536
{
    // P_C1 and P_C2 are both 16 bit limbs of P_C, where P_C1 is the less significant limb.
    operation run<0> P_operation, P_A, P_B, P_ROW -> P_C1, P_C2;

    col fixed latch = [1]*;
    col fixed operation_id = [0]*;

    let bit_counts = [256, 32, 4, 2];
    let min_degree = std::array::product(bit_counts);
    std::check::assert(std::prover::min_degree() >= std::array::product(bit_counts), || "The shift16 machine needs at least 65536 rows to work.");
    std::check::assert(std::field::modulus() >= 65535, || "The field modulo should be at least 2^16 - 1 to work in the shift16 machine.");
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
        0 => a(i) << (b(i) + (row(i) * 8)),
        1 => (a(i) << (row(i) * 8)) >> b(i)
    };
    col fixed P_C1(i) { c(i) & 0xffff };
    col fixed P_C2(i) { (c(i) >> 16) & 0xffff };
}

machine Shift16(byte_shift_16: ByteShift16) with
    latch: latch,
    operation_id: operation_id,
    // Allow this machine to be connected via a permutation
    call_selectors: sel,
{
    operation shl<0> ALow, AHi, B -> C1, C2;

    operation shr<1> ALow, AHi, B -> C1, C2;

    col witness operation_id;
    unchanged_until(operation_id, latch);

    col fixed latch(i) { if (i % 4) == 3 { 1 } else { 0 } };
    col fixed FACTOR_ROW(i) { (i + 1) % 4 };
    col fixed FACTOR_ALow = [256, 0, 0, 1];
    col fixed FACTOR_AHi = [0, 1, 256, 0];

    col witness A_byte;
    col witness C_part1, C_part2;

    col witness ALow, AHi;
    col witness B;
    col witness C1, C2;

    ALow' = ALow * (1 - latch) + A_byte * FACTOR_ALow;
    AHi' = AHi * (1 - latch) + A_byte * FACTOR_AHi;
    unchanged_until(B, latch);
    C1' = C1 * (1 - latch) + C_part1;
    C2' = C2 * (1 - latch) + C_part2;

    link => (C_part1, C_part2) = byte_shift_16.run(operation_id', A_byte, B', FACTOR_ROW);
}
