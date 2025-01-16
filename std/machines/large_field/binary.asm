use std::utils::unchanged_until;
use std::field::modulus;
use std::check::require_field_bits;
use std::machines::binary::ByteBinary;

machine Binary(byte_binary: ByteBinary) with
    latch: latch,
    operation_id: operation_id,
    // Allow this machine to be connected via a permutation
    call_selectors: sel,
{
    require_field_bits(32, || "Binary requires a field that fits any 32-Bit value.");

    operation and<0> A, B -> C;

    operation or<1> A, B -> C;

    operation xor<2> A, B -> C;

    let operation_id;
    unchanged_until(operation_id, latch);

    let latch: col = |i| { if (i % 4) == 3 { 1 } else { 0 } };
    let FACTOR: col = |i| { 1 << (((i + 1) % 4) * 8) };

    let A_byte;
    let B_byte;
    let C_byte;

    let A;
    let B;
    let C;

    A' = A * (1 - latch) + A_byte * FACTOR;
    B' = B * (1 - latch) + B_byte * FACTOR;
    C' = C * (1 - latch) + C_byte * FACTOR;

    // TODO: Currently, the bus linker does not support next references in operations and links.
    //       We add an extra witness column to make the Goldilocks RISC-V machine work for now.
    //       This will be fixed with #2140.
    col witness operation_id_next;
    operation_id' = operation_id_next;
    link => C_byte = byte_binary.run(operation_id_next, A_byte, B_byte);
}
