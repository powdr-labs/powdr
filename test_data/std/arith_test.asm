use std::arith::Arith;

machine Main{
    degree 65536;

    reg pc[@pc];
    reg A0[<=];
    reg A1[<=];
    reg A2[<=];
    reg A3[<=];
    reg A4[<=];
    reg A5[<=];
    reg A6[<=];
    reg A7[<=];
    reg B0[<=];
    reg B1[<=];
    reg B2[<=];
    reg B3[<=];
    reg B4[<=];
    reg B5[<=];
    reg B6[<=];
    reg B7[<=];
    reg C0[<=];
    reg C1[<=];
    reg C2[<=];
    reg C3[<=];
    reg C4[<=];
    reg C5[<=];
    reg C6[<=];
    reg C7[<=];
    reg D0[<=];
    reg D1[<=];
    reg D2[<=];
    reg D3[<=];
    reg D4[<=];
    reg D5[<=];
    reg D6[<=];
    reg D7[<=];
    reg E0[<=];
    reg E1[<=];
    reg E2[<=];
    reg E3[<=];
    reg E4[<=];
    reg E5[<=];
    reg E6[<=];
    reg E7[<=];
    reg F0[<=];
    reg F1[<=];
    reg F2[<=];
    reg F3[<=];
    reg F4[<=];
    reg F5[<=];
    reg F6[<=];
    reg F7[<=];
    
    reg t_0_0;
    reg t_0_1;
    reg t_0_2;
    reg t_0_3;
    reg t_0_4;
    reg t_0_5;
    reg t_0_6;
    reg t_0_7;
    reg t_1_0;
    reg t_1_1;
    reg t_1_2;
    reg t_1_3;
    reg t_1_4;
    reg t_1_5;
    reg t_1_6;
    reg t_1_7;

    Arith arith;

    instr affine_256 A0, A1, A2, A3, A4, A5, A6, A7, B0, B1, B2, B3, B4, B5, B6, B7, C0, C1, C2, C3, C4, C5, C6, C7 -> D0, D1, D2, D3, D4, D5, D6, D7, E0, E1, E2, E3, E4, E5, E6, E7 = arith.affine_256;
    instr ec_add A0, A1, A2, A3, A4, A5, A6, A7, B0, B1, B2, B3, B4, B5, B6, B7, C0, C1, C2, C3, C4, C5, C6, C7, D0, D1, D2, D3, D4, D5, D6, D7 -> E0, E1, E2, E3, E4, E5, E6, E7, F0, F1, F2, F3, F4, F5, F6, F7 = arith.ec_add;
    instr ec_double A0, A1, A2, A3, A4, A5, A6, A7, B0, B1, B2, B3, B4, B5, B6, B7 -> E0, E1, E2, E3, E4, E5, E6, E7, F0, F1, F2, F3, F4, F5, F6, F7 = arith.ec_double;

    instr assert_eq A0, A1, A2, A3, A4, A5, A6, A7, B0, B1, B2, B3, B4, B5, B6, B7 {
        A0 = B0,
        A1 = B1,
        A2 = B2,
        A3 = B3,
        A4 = B4,
        A5 = B5,
        A6 = B6,
        A7 = B7
    }


    function main {
        // 0x0000000011111111222222223333333344444444555555556666666677777777
        // * 0x8888888899999999aaaaaaaabbbbbbbbccccccccddddddddeeeeeeeeffffffff
        // + 0xaaaaaaaabbbbbbbbbbbbbbbbaaaaaaaaaaaaaaaabbbbbbbbbbbbbbbbaaaaaaaa
        // == 0x91a2b3c579be024740da740e6f8091a38e38e38f258bf259be024691fdb97530da740da60b60b60907f6e5d369d0369ca8641fda1907f6e33333333
        // == 0x00000000_091a2b3c_579be024_740da740_e6f8091a_38e38e38_f258bf25_9be02469 * 2**256 + 0x1fdb9753_0da740da_60b60b60_907f6e5d_369d0369_ca8641fd_a1907f6e_33333333

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== affine_256(
            0x77777777, 0x66666666, 0x55555555, 0x44444444, 0x33333333, 0x22222222, 0x11111111, 0x00000000,
            0xffffffff, 0xeeeeeeee, 0xdddddddd, 0xcccccccc, 0xbbbbbbbb, 0xaaaaaaaa, 0x99999999, 0x88888888,
            0xaaaaaaaa, 0xbbbbbbbb, 0xbbbbbbbb, 0xaaaaaaaa, 0xaaaaaaaa, 0xbbbbbbbb, 0xbbbbbbbb, 0xaaaaaaaa);
        
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0x9be02469, 0xf258bf25, 0x38e38e38, 0xe6f8091a, 0x740da740, 0x579be024, 0x091a2b3c, 0x00000000;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0x33333333, 0xa1907f6e, 0xca8641fd, 0x369d0369, 0x907f6e5d, 0x60b60b60, 0x0da740da, 0x1fdb9753;

        // Test vectors from: https://github.com/0xPolygonHermez/zkevm-proverjs/blob/main/test/sm/sm_arith.js

        // 2 * 3 + 5 = 11
        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== affine_256(
            2, 0, 0, 0, 0, 0, 0, 0,
            3, 0, 0, 0, 0, 0, 0, 0,
            5, 0, 0, 0, 0, 0, 0, 0);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0, 0, 0, 0, 0, 0, 0, 0;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 11, 0, 0, 0, 0, 0, 0, 0;

        // 256 * 256 + 1 = 65537
        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== affine_256(
            256, 0, 0, 0, 0, 0, 0, 0,
            256, 0, 0, 0, 0, 0, 0, 0,
            1, 0, 0, 0, 0, 0, 0, 0);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0, 0, 0, 0, 0, 0, 0, 0;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 65537, 0, 0, 0, 0, 0, 0, 0;

        // 3000 * 2000 + 5000 = 6005000
        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== affine_256(
            3000, 0, 0, 0, 0, 0, 0, 0,
            2000, 0, 0, 0, 0, 0, 0, 0,
            5000, 0, 0, 0, 0, 0, 0, 0);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0, 0, 0, 0, 0, 0, 0, 0;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 6005000, 0, 0, 0, 0, 0, 0, 0;

        // 3000000 * 2000000 + 5000000 = 6000005000000
        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== affine_256(
            3000000, 0, 0, 0, 0, 0, 0, 0,
            2000000, 0, 0, 0, 0, 0, 0, 0,
            5000000, 0, 0, 0, 0, 0, 0, 0);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0, 0, 0, 0, 0, 0, 0, 0;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0xfc2aab40, 0x574, 0, 0, 0, 0, 0, 0;

        // 3000 * 0 + 5000 = 5000
        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== affine_256(
            3000, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0,
            5000, 0, 0, 0, 0, 0, 0, 0);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0, 0, 0, 0, 0, 0, 0, 0;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 5000, 0, 0, 0, 0, 0, 0, 0;

        // 2**255 * 2 + 0 = 2 ** 256
        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== affine_256(
            0, 0, 0, 0, 0, 0, 0, 0x80000000,
            2, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 1, 0, 0, 0, 0, 0, 0, 0;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0, 0, 0, 0, 0, 0, 0, 0;

        // (2**256 - 1) * (2**256 - 1) + (2**256 - 1) = 2 ** 256 * 115792089237316195423570985008687907853269984665640564039457584007913129639935
        // = 2 ** 256 * 0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== affine_256(
            0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
            0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
            0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0, 0, 0, 0, 0, 0, 0, 0;

        // (2**256 - 1) * 1 + (2**256 - 1) = 2 ** 256 + 115792089237316195423570985008687907853269984665640564039457584007913129639934
        // = 2 ** 256 + 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffe
        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== affine_256(
            0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
            1, 0, 0, 0, 0, 0, 0, 0,
            0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 1, 0, 0, 0, 0, 0, 0, 0;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0xfffffffe, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff;

        // EC Addition:
        // x1: 55066263022277343669578718895168534326250603453777594175500187360389116729240
        //     = 0x79be667e f9dcbbac 55a06295 ce870b07 029bfcdb 2dce28d9 59f2815b 16f81798
        // y1: 32670510020758816978083085130507043184471273380659243275938904335757337482424
        //     = 0x483ada77 26a3c465 5da4fbfc 0e1108a8 fd17b448 a6855419 9c47d08f fb10d4b8
        // x2: 89565891926547004231252920425935692360644145829622209833684329913297188986597
        //     = 0xc6047f94 41ed7d6d 3045406e 95c07cd8 5c778e4b 8cef3ca7 abac09b9 5c709ee5
        // y2: 12158399299693830322967808612713398636155367887041628176798871954788371653930
        //     = 0x1ae168fe a63dc339 a3c58419 466ceaee f7f63265 3266d0e1 236431a9 50cfe52a
        // x3: 112711660439710606056748659173929673102114977341539408544630613555209775888121
        //     = 0xf9308a01 9258c310 49344f85 f89d5229 b531c845 836f99b0 8601f113 bce036f9
        // y3: 25583027980570883691656905877401976406448868254816295069919888960541586679410
        //     = 0x388f7b0f 632de814 0fe337e6 2a37f356 6500a999 34c2231b 6cb9fd75 84b8e672
        // Commented out for now, because Witgen for ec_add is not working yet.
        // t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_add(
        //     0x16f81798, 0x59f2815b, 0x2dce28d9, 0x029bfcdb, 0xce870b07, 0x55a06295, 0xf9dcbbac, 0x79be667e,
        //     0xfb10d4b8, 0x9c47d08f, 0xa6855419, 0xfd17b448, 0x0e1108a8, 0x5da4fbfc, 0x26a3c465, 0x483ada77,
        //     0x5c709ee5, 0xabac09b9, 0x8cef3ca7, 0x5c778e4b, 0x95c07cd8, 0x3045406e, 0x41ed7d6d, 0xc6047f94,
        //     0x50cfe52a, 0x236431a9, 0x3266d0e1, 0xf7f63265, 0x466ceaee, 0xa3c58419, 0xa63dc339, 0x1ae168fe);
        // assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0xbce036f9, 0x8601f113, 0x836f99b0, 0xb531c845, 0xf89d5229, 0x49344f85, 0x9258c310, 0xf9308a01;
        // assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0x84b8e672, 0x6cb9fd75, 0x34c2231b, 0x6500a999, 0x2a37f356, 0x0fe337e6, 0x632de814, 0x388f7b0f;
    }
}