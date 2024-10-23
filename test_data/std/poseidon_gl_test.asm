use std::machines::hash::poseidon_gl::PoseidonGL;

machine Main with min_degree: 32, max_degree: 256 {
    reg pc[@pc];
    reg X0[<=];
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
    reg X13[<=];
    reg X14[<=];
    reg X15[<=]; 
    reg A;
    reg B;
    reg C;
    reg D;

    PoseidonGL poseidon;

    instr poseidon X0, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11 -> X12, X13, X14, X15
        link ~> (X12, X13, X14, X15) = poseidon.poseidon_permutation(X0, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11);

    instr assert_eq X0, X1 {
        X0 = X1
    }

    function main {

        // See test vectors at:
        // https://github.com/0xPolygonHermez/zkevm-testvectors/blob/main/poseidon/raw-hash.json
        A, B, C, D <== poseidon(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
        assert_eq A, 4330397376401421145;
        assert_eq B, 14124799381142128323;
        assert_eq C, 8742572140681234676;
        assert_eq D, 14345658006221440202;

        A, B, C, D <== poseidon(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1);
        assert_eq A, 16428316519797902711;
        assert_eq B, 13351830238340666928;
        assert_eq C, 682362844289978626;
        assert_eq D, 12150588177266359240;

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

        return;
    }
}
