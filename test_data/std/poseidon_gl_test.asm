use std::machines::hash::poseidon_gl::PoseidonGL;
use std::machines::memory::Memory;

machine Main with degree: 65536 {
    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg A;

    col fixed STEP(i) { i };
    Memory memory;
    instr mload X -> Y ~ memory.mload X, STEP -> Y;
    instr mstore X, Y -> ~ memory.mstore X, STEP, Y ->;

    PoseidonGL poseidon(memory);

    instr poseidon X, Y -> ~ poseidon.poseidon_permutation X, Y, STEP ->;

    instr assert_eq X, Y {
        X = Y
    }

    function main {

        // See test vectors at:
        // https://github.com/0xPolygonHermez/zkevm-testvectors/blob/main/poseidon/raw-hash.json
        mstore 0, 0;
        mstore 4, 0;
        mstore 8, 0;
        mstore 12, 0;
        mstore 16, 0;
        mstore 20, 0;
        mstore 24, 0;
        mstore 28, 0;
        mstore 32, 0;
        mstore 36, 0;
        mstore 40, 0;
        mstore 44, 0;
        poseidon 0, 48;
        A <== mload(48);
        assert_eq A, 4330397376401421145;
        A <== mload(52);
        assert_eq A, 14124799381142128323;
        A <== mload(56);
        assert_eq A, 8742572140681234676;
        A <== mload(60);
        assert_eq A, 14345658006221440202;


        mstore 0, 1;
        mstore 4, 1;
        mstore 8, 1;
        mstore 12, 1;
        mstore 16, 1;
        mstore 20, 1;
        mstore 24, 1;
        mstore 28, 1;
        mstore 32, 1;
        mstore 36, 1;
        mstore 40, 1;
        mstore 44, 1;
        poseidon 0, 48;
        A <== mload(48);
        assert_eq A, 16428316519797902711;
        A <== mload(52);
        assert_eq A, 13351830238340666928;
        A <== mload(56);
        assert_eq A, 682362844289978626;
        A <== mload(60);
        assert_eq A, 12150588177266359240;

        /*

        A, B, C, D <== poseidon(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1);
        assert_eq A, 13691089994624172887;
        assert_eq B, 15662102337790434313;
        assert_eq C, 14940024623104903507;
        assert_eq D, 10772674582659927682;

        A, B, C, D <== poseidon(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
        assert_eq A, 4330397376401421145;
        assert_eq B, 14124799381142128323;
        assert_eq C, 8742572140681234676;
        assert_eq D, 14345658006221440202;

        A, B, C, D <== poseidon(923978, 235763497586, 9827635653498, 112870, 289273673480943876, 230295874986745876, 6254867324987, 2087, 0, 0, 0, 0);
        assert_eq A, 1892171027578617759;
        assert_eq B, 984732815927439256;
        assert_eq C, 7866041765487844082;
        assert_eq D, 8161503938059336191;
        */

        return;
    }
}
