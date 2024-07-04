use std::machines::hash::poseidon_gl_memory::PoseidonGLMemory;
use std::machines::memory::Memory;
use std::machines::split::split_gl::SplitGL;

machine Main with degree: 65536 {
    reg pc[@pc];
    reg X1[<=];
    reg X2[<=];
    reg X3[<=];
    reg X4[<=];
    reg X5[<=];
    reg X6[<=];
    reg X7[<=];
    reg X8[<=];
    reg X9[<=];
    reg X10[<=];
    reg X11[<=];
    reg X12[<=];
    reg ADDR1[<=];
    reg ADDR2[<=];

    SplitGL split;

    col fixed STEP(i) { i };
    Memory memory;
    instr mstore_le ADDR1, X1, X2 ->
        link ~> memory.mstore(ADDR1, STEP, X2)
        link ~> memory.mstore(ADDR1 + 4, STEP, X1);

    PoseidonGLMemory poseidon(memory, split);
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
 
        poseidon 0, 96;

        assert_eq 96, 4330397376401421145;
        assert_eq 104, 14124799381142128323;
        assert_eq 112, 8742572140681234676;
        assert_eq 120, 14345658006221440202;


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

        poseidon 0, 96;

        assert_eq 96, 16428316519797902711;
        assert_eq 104, 13351830238340666928;
        assert_eq 112, 682362844289978626;
        assert_eq 120, 12150588177266359240;

        /*

        mstore_state 0, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1;
        poseidon 0, 48;
        assert_eq 48, 13691089994624172887, 15662102337790434313, 14940024623104903507, 10772674582659927682;

        mstore_state 0, 923978, 235763497586, 9827635653498, 112870, 289273673480943876, 230295874986745876, 6254867324987, 2087, 0, 0, 0, 0;
        poseidon 0, 48;
        assert_eq 48, 1892171027578617759, 984732815927439256, 7866041765487844082, 8161503938059336191;
        */

        return;
    }
}
