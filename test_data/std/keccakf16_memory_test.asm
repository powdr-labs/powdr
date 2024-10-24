use std::machines::hash::keccakf16_memory::Keccakf16Memory;
use std::machines::small_field::memory::Memory;
use std::machines::small_field::add_sub::AddSub;
use std::machines::range::Byte2;
use std::machines::range::Bit12;

machine Main with degree: 65536 {
    reg pc[@pc];

    reg X1[<=];
    reg X2[<=];

    reg Y1[<=];
    reg Y2[<=];

    Byte2 byte2;
    Bit12 bit12;
    AddSub add_sub(byte2);
    Memory memory(bit12, byte2);

    Keccakf16Memory keccakf16_memory(memory, add_sub);

    col fixed STEP(i) { i };

    // Big endian.
    // Usage: mstore addr_h, addr_l, val_h, val_l
    instr mstore X1, X2, Y1, Y2 -> link ~> memory.mstore(X1, X2, STEP, Y1, Y2);
    // Usage: keccakf16_memory input_addr_h, input_addr_l, output_addr_h, output_addr_l;
    instr keccakf16_memory X1, X2, Y1, Y2 -> link ~> keccakf16_memory.keccakf16_memory(X1, X2, Y1, Y2, STEP);

    col witness val_h, val_l;
    // Usage: assert_eq addr_h, addr_l, val_h, val_l
    instr assert_eq X1, X2, Y1, Y2 ->
        link ~> (val_h, val_l) = memory.mload(X1, X2, STEP)
    {
        val_h = Y1,
        val_l = Y2
    }

    function main {
        // 0 for all 25 64-bit inputs except setting the second 64-bit input to 1. All 64-bit inputs in chunks of 4 16-bit little endian limbs.
        mstore 0, 0, 0, 0;
        mstore 0, 4, 0, 0;
        mstore 0, 8, 0, 1;
        mstore 0, 12, 0, 0;
        mstore 0, 16, 0, 0;
        mstore 0, 20, 0, 0;
        mstore 0, 24, 0, 0;
        mstore 0, 28, 0, 0;
        mstore 0, 32, 0, 0;
        mstore 0, 36, 0, 0;
        mstore 0, 40, 0, 0;
        mstore 0, 44, 0, 0;
        mstore 0, 48, 0, 0;
        mstore 0, 52, 0, 0;
        mstore 0, 56, 0, 0;
        mstore 0, 60, 0, 0;
        mstore 0, 64, 0, 0;
        mstore 0, 68, 0, 0;
        mstore 0, 72, 0, 0;
        mstore 0, 76, 0, 0;
        mstore 0, 80, 0, 0;
        mstore 0, 84, 0, 0;
        mstore 0, 88, 0, 0;
        mstore 0, 92, 0, 0;
        mstore 0, 96, 0, 0;
        mstore 0, 100, 0, 0;
        mstore 0, 104, 0, 0;
        mstore 0, 108, 0, 0;
        mstore 0, 112, 0, 0;
        mstore 0, 116, 0, 0;
        mstore 0, 120, 0, 0;
        mstore 0, 124, 0, 0;
        mstore 0, 128, 0, 0;
        mstore 0, 132, 0, 0;
        mstore 0, 136, 0, 0;
        mstore 0, 140, 0, 0;
        mstore 0, 144, 0, 0;
        mstore 0, 148, 0, 0;
        mstore 0, 152, 0, 0;
        mstore 0, 156, 0, 0;
        mstore 0, 160, 0, 0;
        mstore 0, 164, 0, 0;
        mstore 0, 168, 0, 0;
        mstore 0, 172, 0, 0;
        mstore 0, 176, 0, 0;
        mstore 0, 180, 0, 0;
        mstore 0, 184, 0, 0;
        mstore 0, 188, 0, 0;
        mstore 0, 192, 0, 0;
        mstore 0, 196, 0, 0;

        keccakf16_memory 0, 0, 0, 200;

        // Selectively checking a few registers only.
        assert_eq 0, 200, 0, 1234;

        return;
    }
}
