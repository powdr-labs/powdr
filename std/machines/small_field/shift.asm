use std::utils::unchanged_until;
use std::utils::cross_product;
use std::convert::int;
use std::field::modulus;
use std::check::assert;
use std::check::require_field_bits;

// Shift for single bytes using an exhaustive table, returning two 16-bit values as result.
// TODO this way, we cannot prove anything that shifts by more than 31 bits.
machine ByteShift with
    latch: latch,
    operation_id: operation_id,
    degree: 65536
{
    // P_CLow and P_CHi are both 16 bit limbs of P_C, where P_CLow is the less significant limb.
    operation run<0> P_operation, P_A, P_B, P_ROW -> P_CLow, P_CHi;

    col fixed latch = [1]*;
    col fixed operation_id = [0]*;

    let bit_counts = [256, 32, 4, 2];
    let min_degree = std::array::product(bit_counts);
    std::check::assert(std::prover::min_degree() >= std::array::product(bit_counts), || "The shift16 machine needs at least 65536 rows to work.");
    require_field_bits(16, || "The field modulus should be at least 2^16 - 1 to work in the shift16 machine.");
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
    col fixed P_CLow(i) { c(i) & 0xffff };
    col fixed P_CHi(i) { (c(i) >> 16) & 0xffff };
}

machine Shift(byte_shift: ByteShift) with
    latch: latch,
    operation_id: operation_id,
    // Allow this machine to be connected via a permutation
    call_selectors: sel,
{
    require_field_bits(16, || "Shift requires a field that fits any 17-Bit value.");

    operation shl<0> ALow, AHi, B -> CLow, CHi;

    operation shr<1> ALow, AHi, B -> CLow, CHi;

    col witness operation_id;
    unchanged_until(operation_id, latch);

    col fixed latch(i) { if (i % 4) == 3 { 1 } else { 0 } };
    col fixed FACTOR_ROW(i) { (i + 1) % 4 };
    col fixed FACTOR_ALow = [256, 0, 0, 1]*;
    col fixed FACTOR_AHi = [0, 1, 256, 0]*;

    col witness A_byte;
    col witness C_part_low, C_part_hi;

    col witness ALow, AHi;
    col witness B;
    col witness CLow, CHi;

    ALow' = ALow * (1 - latch) + A_byte * FACTOR_ALow;
    AHi' = AHi * (1 - latch) + A_byte * FACTOR_AHi;
    unchanged_until(B, latch);
    CLow' = CLow * (1 - latch) + C_part_low;
    CHi' = CHi * (1 - latch) + C_part_hi;

    link => (C_part_low, C_part_hi) = byte_shift.run(operation_id', A_byte, B', FACTOR_ROW);
}
