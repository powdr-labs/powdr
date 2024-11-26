use std::machines::hash::poseidon_gl_memory::PoseidonGLMemory;
use std::machines::range::Byte2;
use std::machines::large_field::memory::Memory;
use std::machines::split::ByteCompare;
use std::machines::split::split_gl::SplitGL;

let main_degree: int = 2**16;
let split_degree: int = 2**16;
let memory_degree: int = 2**16;
let poseidon_degree: int = 2**16;

machine Main with degree: main_degree {
    reg pc[@pc];
    reg X1[<=];
    reg X2[<=];
    reg ADDR1[<=];
    reg ADDR2[<=];

    ByteCompare byte_compare;
    SplitGL split(byte_compare, split_degree, split_degree);

    // Increase the time step by 2 in each row, so that the poseidon machine
    // can read in the given time step and write in the next time step.
    col fixed STEP(i) { 2 * i };
    Byte2 byte2;
    Memory memory(byte2, memory_degree, memory_degree);
    instr mstore_le ADDR1, X1, X2 ->
        link ~> memory.mstore(ADDR1, STEP, X2)
        link ~> memory.mstore(ADDR1 + 4, STEP, X1);

    PoseidonGLMemory poseidon(memory, split, poseidon_degree, poseidon_degree);
    instr poseidon ADDR1, ADDR2 -> link ~> poseidon.poseidon_permutation(ADDR1, ADDR2, STEP);

    col witness val_low, val_high;
    instr assert_eq ADDR1, X1 ->
        link ~> val_low = memory.mload(ADDR1, STEP)
        link ~> val_high = memory.mload(ADDR1 + 4, STEP)
    {
        val_low + 2**32 * val_high = X1
    }

    function main {

        // See test vectors at:
        // https://github.com/0xPolygonHermez/zkevm-testvectors/blob/main/poseidon/raw-hash.json

        mstore_le 0, 0, 0;
        mstore_le 8, 0, 0;
        mstore_le 16, 0, 0;
        mstore_le 24, 0, 0;
        mstore_le 32, 0, 0;
        mstore_le 40, 0, 0;
        mstore_le 48, 0, 0;
        mstore_le 56, 0, 0;
        mstore_le 64, 0, 0;
        mstore_le 72, 0, 0;
        mstore_le 80, 0, 0;
        mstore_le 88, 0, 0;
 
        poseidon 0, 0;

        assert_eq 0, 4330397376401421145;
        assert_eq 8, 14124799381142128323;
        assert_eq 16, 8742572140681234676;
        assert_eq 24, 14345658006221440202;

        mstore_le 0, 0, 1;
        mstore_le 8, 0, 1;
        mstore_le 16, 0, 1;
        mstore_le 24, 0, 1;
        mstore_le 32, 0, 1;
        mstore_le 40, 0, 1;
        mstore_le 48, 0, 1;
        mstore_le 56, 0, 1;
        mstore_le 64, 0, 1;
        mstore_le 72, 0, 1;
        mstore_le 80, 0, 1;
        mstore_le 88, 0, 1;

        poseidon 0, 0;

        assert_eq 0, 16428316519797902711;
        assert_eq 8, 13351830238340666928;
        assert_eq 16, 682362844289978626;
        assert_eq 24, 12150588177266359240;

        // Pass test vector (-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1)
        mstore_le 0, 0xffffffff, 0;
        mstore_le 8, 0xffffffff, 0;
        mstore_le 16, 0xffffffff, 0;
        mstore_le 24, 0xffffffff, 0;
        mstore_le 32, 0xffffffff, 0;
        mstore_le 40, 0xffffffff, 0;
        mstore_le 48, 0xffffffff, 0;
        mstore_le 56, 0xffffffff, 0;
        mstore_le 64, 0xffffffff, 0;
        mstore_le 72, 0xffffffff, 0;
        mstore_le 80, 0xffffffff, 0;
        mstore_le 88, 0xffffffff, 0;

        poseidon 0, 0;

        assert_eq 0, 13691089994624172887;
        assert_eq 8, 15662102337790434313;
        assert_eq 16, 14940024623104903507;
        assert_eq 24, 10772674582659927682;

        // Pass test vector (923978, 235763497586, 9827635653498, 112870, 289273673480943876, 230295874986745876, 6254867324987, 2087, 0, 0, 0, 0)
        // = (0xe194a, 0x36_e4997a72, 0x8f0_2cbb6b7a, 0x1b8e6, 0x403b4df_96b43904, 0x3322ce0_cdd2a014, 0x5b0_5325203b, 0x827, 0x0, 0x0, 0x0, 0x0)
        mstore_le 0, 0, 0xe194a;
        mstore_le 8, 0x36, 0xe4997a72;
        mstore_le 16, 0x8f0, 0x2cbb6b7a;
        mstore_le 24, 0, 0x1b8e6;
        mstore_le 32, 0x403b4df, 0x96b43904;
        mstore_le 40, 0x3322ce0, 0xcdd2a014;
        mstore_le 48, 0x5b0, 0x5325203b;
        mstore_le 56, 0, 0x827;
        mstore_le 64, 0, 0;
        mstore_le 72, 0, 0;
        mstore_le 80, 0, 0;
        mstore_le 88, 0, 0;

        poseidon 0, 0;

        assert_eq 0, 1892171027578617759;
        assert_eq 8, 984732815927439256;
        assert_eq 16, 7866041765487844082;
        assert_eq 24, 8161503938059336191;

        // Repeat the first test, but be fancy with the memory pointers being passed:

        mstore_le 100, 0, 0;
        mstore_le 108, 0, 0;
        mstore_le 116, 0, 0;
        mstore_le 124, 0, 0;
        mstore_le 132, 0, 0;
        mstore_le 140, 0, 0;
        mstore_le 148, 0, 0;
        mstore_le 156, 0, 0;
        mstore_le 164, 0, 0;
        mstore_le 172, 0, 0;
        mstore_le 180, 0, 0;
        mstore_le 188, 0, 0;
 
        // This will read bytes [100, 195] and write the result to bytes [104, 131]
        poseidon 100, 104;

        assert_eq 104, 4330397376401421145;
        assert_eq 112, 14124799381142128323;
        assert_eq 120, 8742572140681234676;
        assert_eq 128, 14345658006221440202;

        return;
    }
}
