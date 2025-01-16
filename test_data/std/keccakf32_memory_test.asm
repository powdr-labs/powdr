use std::machines::hash::keccakf32_memory::Keccakf32Memory;
use std::machines::large_field::memory::Memory;
use std::machines::range::Byte2;

let MIN: int = 2**5;
let MAX: int = 2**8;
machine Main with min_degree: MIN, max_degree: MAX {
    reg pc[@pc];

    reg X[<=];

    reg Y[<=];

    Byte2 byte2;
    Memory memory(byte2, MIN, MAX);

    Keccakf32Memory keccakf32_memory(memory, MIN, MAX);

    col fixed STEP(i) { i * 2 };

    // Big endian.
    // Usage: mstore addr, val;
    instr mstore X, Y -> link ~> memory.mstore(X, STEP, Y);
    // Usage: keccakf32_memory input_addr, output_addr;
    instr keccakf32_memory X, Y -> link ~> keccakf32_memory.keccakf32_memory(X, Y, STEP);

    col witness val;
    // Usage: assert_eq addr, val;
    instr assert_eq X, Y ->
        link ~> val = memory.mload(X, STEP)
    {
        val = Y
    }

    function main {
        // Test 1: 0 for all 25 64-bit inputs except setting the second 64-bit input to 1. All 64-bit inputs in chunks of 2 32-bit big endian limbs.
        mstore 0, 0;
        mstore 4, 0;
        mstore 8, 1;
        mstore 12, 0;
        mstore 16, 0;
        mstore 20, 0;
        mstore 24, 0;
        mstore 28, 0;
        mstore 32, 0;
        mstore 36, 0;
        mstore 40, 0;
        mstore 44, 0;
        mstore 48, 0;
        mstore 52, 0;
        mstore 56, 0;
        mstore 60, 0;
        mstore 64, 0;
        mstore 68, 0;
        mstore 72, 0;
        mstore 76, 0;
        mstore 80, 0;
        mstore 84, 0;
        mstore 88, 0;
        mstore 92, 0;
        mstore 96, 0;
        // Input address 0. Output address 200.
        keccakf32_memory 0, 200;
        // Selectively checking a few registers only.
        // Test vector generated from Tiny Keccak.
        assert_eq 200, 0x9001405f;
        assert_eq 204, 0xfdbbbbdf;
        assert_eq 392, 0x664deb35;
        assert_eq 396, 0xeac9f006;

        // Test 2: Same as Test 1 but sets input and output addresses to be the same.
        // No need to rerun the mstores because input values from Test 1 should be intact.
        keccakf32_memory 0, 0;
        // Selectively checking a few registers only.
        // Test vector generated from Tiny Keccak.
        assert_eq 0, 0x9001405f;
        assert_eq 4, 0xfdbbbbdf;
        assert_eq 192, 0x664deb35;
        assert_eq 196, 0xeac9f006;

        return;
    }
}
