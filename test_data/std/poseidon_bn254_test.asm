use std::machines::hash::poseidon_bn254::PoseidonBN254;
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

    PoseidonBN254 poseidon(memory);

    instr poseidon X, Y -> ~ poseidon.poseidon_permutation X, Y, STEP ->;

    instr assert_eq X, Y {
        X = Y
    }

    function main {

        // Test vector for poseidonperm_x5_254_3 from:
        // https://extgit.iaik.tugraz.at/krypto/hadeshash/-/blob/master/code/test_vectors.txt
        mstore 0, 0;
        mstore 4, 1;
        mstore 8, 2;
        poseidon 0, 12;
        A <== mload(12);
        assert_eq A, 0x115cc0f5e7d690413df64c6b9662e9cf2a3617f2743245519e19607a4417189a;

        // Validated by modifying and running `code/poseidonperm_x5_254_3.sage` from the same repository.
        mstore 0, 0;
        mstore 4, 0;
        mstore 8, 0;
        poseidon 0, 12;
        A <== mload(12);
        assert_eq A, 0x2098f5fb9e239eab3ceac3f27b81e481dc3124d55ffed523a839ee8446b64864;

        mstore 0, -1;
        mstore 4, -2;
        mstore 8, -3;
        poseidon 0, 12;
        A <== mload(12);
        assert_eq A, 0x15492e60e5ae9f3d254f2d44650795c4cac1c924981fb7ca8645a7790971b70c;

        mstore 0, A + 1;
        mstore 4, A + 2;
        mstore 8, A + 3;
        poseidon 0, 12;
        A <== mload(12);
        assert_eq A, 0x188ada144ed909426b0396e967a82e26d739652cff288d13306279d91f29010c;

        return;
    }
}
