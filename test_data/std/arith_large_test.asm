use std::machines::large_field::arith::Arith;

let main_degree: int = 2**9;
let arith_degree: int = 2**12;

machine Main with degree: main_degree {
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

    Arith arith(arith_degree, arith_degree);

    instr affine_256 A0, A1, A2, A3, A4, A5, A6, A7, B0, B1, B2, B3, B4, B5, B6, B7, C0, C1, C2, C3, C4, C5, C6, C7 -> D0, D1, D2, D3, D4, D5, D6, D7, E0, E1, E2, E3, E4, E5, E6, E7
        link ~> (D0, D1, D2, D3, D4, D5, D6, D7, E0, E1, E2, E3, E4, E5, E6, E7) = arith.affine_256(A0, A1, A2, A3, A4, A5, A6, A7, B0, B1, B2, B3, B4, B5, B6, B7, C0, C1, C2, C3, C4, C5, C6, C7);

    instr ec_add A0, A1, A2, A3, A4, A5, A6, A7, B0, B1, B2, B3, B4, B5, B6, B7, C0, C1, C2, C3, C4, C5, C6, C7, D0, D1, D2, D3, D4, D5, D6, D7 -> E0, E1, E2, E3, E4, E5, E6, E7, F0, F1, F2, F3, F4, F5, F6, F7
        link ~> (E0, E1, E2, E3, E4, E5, E6, E7, F0, F1, F2, F3, F4, F5, F6, F7) = arith.ec_add(A0, A1, A2, A3, A4, A5, A6, A7, B0, B1, B2, B3, B4, B5, B6, B7, C0, C1, C2, C3, C4, C5, C6, C7, D0, D1, D2, D3, D4, D5, D6, D7);

    instr ec_double A0, A1, A2, A3, A4, A5, A6, A7, B0, B1, B2, B3, B4, B5, B6, B7 -> E0, E1, E2, E3, E4, E5, E6, E7, F0, F1, F2, F3, F4, F5, F6, F7
        link ~> (E0, E1, E2, E3, E4, E5, E6, E7, F0, F1, F2, F3, F4, F5, F6, F7) = arith.ec_double(A0, A1, A2, A3, A4, A5, A6, A7, B0, B1, B2, B3, B4, B5, B6, B7);

    instr mod_256 D0, D1, D2, D3, D4, D5, D6, D7, E0, E1, E2, E3, E4, E5, E6, E7, A0, A1, A2, A3, A4, A5, A6, A7 -> C0, C1, C2, C3, C4, C5, C6, C7
        link ~> (C0, C1, C2, C3, C4, C5, C6, C7) = arith.mod_256(D0, D1, D2, D3, D4, D5, D6, D7, E0, E1, E2, E3, E4, E5, E6, E7, A0, A1, A2, A3, A4, A5, A6, A7);

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

        // Test vectors from: https://github.com/0xPolygonHermez/zkevm-proverjs/blob/a4006af3d7fe4a57a85500c01dc791fb5013cef0/test/sm/sm_arith.js

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

        // Mod 256:
        // 6 % 5 = 1
        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7 <== mod_256(
            0, 0, 0, 0, 0, 0, 0, 0,
            6, 0, 0, 0, 0, 0, 0, 0,
            5, 0, 0, 0, 0, 0, 0, 0);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 1, 0, 0, 0, 0, 0, 0, 0;

        // 3000 % 5000 = 3000
        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7 <== mod_256(
            0, 0, 0, 0, 0, 0, 0, 0,
            3000, 0, 0, 0, 0, 0, 0, 0,
            5000, 0, 0, 0, 0, 0, 0, 0);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 3000, 0, 0, 0, 0, 0, 0, 0;
        
        // (2 ** 508) % (2 ** 255) = 0
        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7 <== mod_256(
            0, 0, 0, 0, 0, 0, 0, 0x10000000,
            0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0x80000000);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0, 0, 0, 0, 0, 0, 0, 0;

        // (2 ** 508 + 1) % (2 ** 255) = 1
        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7 <== mod_256(
            0, 0, 0, 0, 0, 0, 0, 0x10000000,
            1, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0x80000000);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 1, 0, 0, 0, 0, 0, 0, 0;

        // 0xaaaaaaaabbbbbbbbcccccccc00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000011111111
        // % 0xddddddddeeeeeeeeffffffff0000000000000000000000000000000022222222
        // = 0x05973e6b48bd15d35f92aff26cad25d5b54f806e5ce298cda76b91baf89af7cf
        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7 <== mod_256(
            0, 0, 0, 0, 0, 0xcccccccc, 0xbbbbbbbb, 0xaaaaaaaa,
            0x11111111, 0, 0, 0, 0, 0, 0, 0,
            0x22222222, 0, 0, 0, 0, 0xffffffff, 0xeeeeeeee, 0xdddddddd);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0xF89AF7CF, 0xA76B91BA, 0x5CE298CD, 0xB54F806E, 0x6CAD25D5, 0x5F92AFF2, 0x48BD15D3, 0x05973E6B;

        // 0x11111111222222223333333344444444555555556666666677777777888888889999999900000000aaaaaaaabbbbbbbbccccccccddddddddeeeeeeeeffffffff
        // % 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2f (secp_modulus)
        // = 0xddddde1e 777777b9 55555596 999999da ddddde1f 22222263 7777783a 333428f9
        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7 <== mod_256(
            0x88888888, 0x77777777, 0x66666666, 0x55555555, 0x44444444, 0x33333333, 0x22222222, 0x11111111,
            0xffffffff, 0xeeeeeeee, 0xdddddddd, 0xcccccccc, 0xbbbbbbbb, 0xaaaaaaaa, 0x00000000, 0x99999999,
            0xfffffc2f, 0xfffffffe, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0x333428f9, 0x7777783a, 0x22222263, 0xddddde1f, 0x999999da, 0x55555596, 0x777777b9, 0xddddde1e;

        // ((2**256 - 1) * (2**256 - 1) + (2**256 - 1)) % (2**256 - 1)
        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== affine_256(
            0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
            0, 0, 0, 0, 0, 0, 0, 0,
            0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0, 0, 0, 0, 0, 0, 0, 0;

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
        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_add(
            0x16f81798, 0x59f2815b, 0x2dce28d9, 0x029bfcdb, 0xce870b07, 0x55a06295, 0xf9dcbbac, 0x79be667e,
            0xfb10d4b8, 0x9c47d08f, 0xa6855419, 0xfd17b448, 0x0e1108a8, 0x5da4fbfc, 0x26a3c465, 0x483ada77,
            0x5c709ee5, 0xabac09b9, 0x8cef3ca7, 0x5c778e4b, 0x95c07cd8, 0x3045406e, 0x41ed7d6d, 0xc6047f94,
            0x50cfe52a, 0x236431a9, 0x3266d0e1, 0xf7f63265, 0x466ceaee, 0xa3c58419, 0xa63dc339, 0x1ae168fe);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0xbce036f9, 0x8601f113, 0x836f99b0, 0xb531c845, 0xf89d5229, 0x49344f85, 0x9258c310, 0xf9308a01;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0x84b8e672, 0x6cb9fd75, 0x34c2231b, 0x6500a999, 0x2a37f356, 0x0fe337e6, 0x632de814, 0x388f7b0f;

        // EC Double:
        // x1: 115780575977492633039504758427830329241728645270042306223540962614150928364886
        //     = 0xfff97bd5 755eeea4 20453a14 355235d3 82f6472f 8568a18b 2f057a14 60297556
        // y1: 78735063515800386211891312544505775871260717697865196436804966483607426560663
        //     = 0xae12777a acfbb620 f3be9601 7f45c560 de80f0f6 518fe4a0 3c870c36 b075f297
        // x3: 94111259592240215275188773285036844871058226277992966241101117022315524122714
        //     = 0xd01115d5 48e7561b 15c38f00 4d734633 687cf441 9620095b c5b0f470 70afe85a
        // y3: 76870767327212528811304566602812752860184934880685532702451763239157141742375
        //     = 0xa9f34ffd c815e0d7 a8b64537 e17bd815 79238c5d d9a86d52 6b051b13 f4062327
        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_double(
            0x60297556, 0x2f057a14, 0x8568a18b, 0x82f6472f, 0x355235d3, 0x20453a14, 0x755eeea4, 0xfff97bd5,
            0xb075f297, 0x3c870c36, 0x518fe4a0, 0xde80f0f6, 0x7f45c560, 0xf3be9601, 0xacfbb620, 0xae12777a);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0x70afe85a, 0xc5b0f470, 0x9620095b, 0x687cf441, 0x4d734633, 0x15c38f00, 0x48e7561b, 0xd01115d5;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0xf4062327, 0x6b051b13, 0xd9a86d52, 0x79238c5d, 0xe17bd815, 0xa8b64537, 0xc815e0d7, 0xa9f34ffd;

        // Auto-generated rest of the test cases,
        // see: https://gist.github.com/georgwiese/a66425ae95c839b548edffffe6a692f9
        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_double(
            0x70afe85a, 0xc5b0f470, 0x9620095b, 0x687cf441, 0x4d734633, 0x15c38f00, 0x48e7561b, 0xd01115d5,
            0xf4062327, 0x6b051b13, 0xd9a86d52, 0x79238c5d, 0xe17bd815, 0xa8b64537, 0xc815e0d7, 0xa9f34ffd);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0xb202e6ce, 0x502bda8, 0x9d62b794, 0x68321543, 0x61ba8b09, 0x8ac09c91, 0x413d33d4, 0xfe72c435;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0xcf58c5bf, 0x978ed2fb, 0x6b4a9d22, 0x1dc88e3, 0x9d729981, 0xd3ab47e0, 0x7ff24a68, 0x6851de06;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_double(
            0xb202e6ce, 0x502bda8, 0x9d62b794, 0x68321543, 0x61ba8b09, 0x8ac09c91, 0x413d33d4, 0xfe72c435,
            0xcf58c5bf, 0x978ed2fb, 0x6b4a9d22, 0x1dc88e3, 0x9d729981, 0xd3ab47e0, 0x7ff24a68, 0x6851de06);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0x1118e5c3, 0x9bd870aa, 0x452bebc1, 0xfc579b27, 0xf4e65b4b, 0xb441656e, 0x9645307d, 0x6eca335d;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0x5a08668, 0x498a2f78, 0x3bf8ec34, 0x3a496a3a, 0x74b875a0, 0x592f5790, 0x7a7a0710, 0xd50123b5;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_double(
            0x1118e5c3, 0x9bd870aa, 0x452bebc1, 0xfc579b27, 0xf4e65b4b, 0xb441656e, 0x9645307d, 0x6eca335d,
            0x5a08668, 0x498a2f78, 0x3bf8ec34, 0x3a496a3a, 0x74b875a0, 0x592f5790, 0x7a7a0710, 0xd50123b5);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0x7f8cb0e3, 0x43933aca, 0xe1efe3a4, 0xa22eb53f, 0x4b2eb72e, 0x8fa64e04, 0x74456d8f, 0x3f0e80e5;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0xea5f404f, 0xcb0289e2, 0xa65b53a4, 0x9501253a, 0x485d01b3, 0xe90b9c08, 0x296cbc91, 0xcb66d7d7;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_double(
            0x7f8cb0e3, 0x43933aca, 0xe1efe3a4, 0xa22eb53f, 0x4b2eb72e, 0x8fa64e04, 0x74456d8f, 0x3f0e80e5,
            0xea5f404f, 0xcb0289e2, 0xa65b53a4, 0x9501253a, 0x485d01b3, 0xe90b9c08, 0x296cbc91, 0xcb66d7d7);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0x33ce1752, 0xc7b750f7, 0xd7cd204e, 0xe783c797, 0xd99c9aea, 0x812ddf64, 0xd01dc635, 0xd7a0da58;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0x762cef4, 0xbbc02738, 0xc062b742, 0xbe040a8, 0x40e28465, 0xf6f29283, 0x68008032, 0x912770e0;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_double(
            0x33ce1752, 0xc7b750f7, 0xd7cd204e, 0xe783c797, 0xd99c9aea, 0x812ddf64, 0xd01dc635, 0xd7a0da58,
            0x762cef4, 0xbbc02738, 0xc062b742, 0xbe040a8, 0x40e28465, 0xf6f29283, 0x68008032, 0x912770e0);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0xb5476085, 0xa908b701, 0x96eb9f84, 0xb5714e77, 0xa78ed1af, 0x10d3aad6, 0x7a08cd3e, 0x3443a706;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0x8b8f52d8, 0x6d3484bd, 0xd0c2b67f, 0x18a4b27, 0x8c7e1da9, 0x4f6e8c4b, 0x829b6f85, 0x661a7a5f;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_double(
            0xb5476085, 0xa908b701, 0x96eb9f84, 0xb5714e77, 0xa78ed1af, 0x10d3aad6, 0x7a08cd3e, 0x3443a706,
            0x8b8f52d8, 0x6d3484bd, 0xd0c2b67f, 0x18a4b27, 0x8c7e1da9, 0x4f6e8c4b, 0x829b6f85, 0x661a7a5f);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0xe57e8dfa, 0xfcfc0cb9, 0xa3c7e184, 0x9809191, 0xaca98ca0, 0xd9a30f8, 0xf0799c4c, 0x8262cf2f;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0xfbac376a, 0x35cff8d8, 0x2b14c478, 0x57b6ed33, 0xc5b34f34, 0x66fee22e, 0x9109e4e, 0x83fd95e2;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_double(
            0xe57e8dfa, 0xfcfc0cb9, 0xa3c7e184, 0x9809191, 0xaca98ca0, 0xd9a30f8, 0xf0799c4c, 0x8262cf2f,
            0xfbac376a, 0x35cff8d8, 0x2b14c478, 0x57b6ed33, 0xc5b34f34, 0x66fee22e, 0x9109e4e, 0x83fd95e2);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0x7c70620c, 0xd17cc1f2, 0xabc288d9, 0x4998c4be, 0x2b671780, 0xc60dd31a, 0x8d2c236d, 0x1653a8a4;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0x315b32cd, 0x6ca2e81d, 0xdfd3dc52, 0x12af748, 0x4efa701c, 0xeafa9947, 0x35af7f7a, 0x3382909;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_double(
            0x7c70620c, 0xd17cc1f2, 0xabc288d9, 0x4998c4be, 0x2b671780, 0xc60dd31a, 0x8d2c236d, 0x1653a8a4,
            0x315b32cd, 0x6ca2e81d, 0xdfd3dc52, 0x12af748, 0x4efa701c, 0xeafa9947, 0x35af7f7a, 0x3382909);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0xe71dabcd, 0x47d42ba6, 0x89e5cb4f, 0x54d3fe49, 0x60b5373f, 0x6098ae32, 0x6b63f43c, 0xd49ee4fb;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0x16603c2, 0xe66a90cf, 0x12ff7031, 0x129c5093, 0xa61bf356, 0xd7c87ea7, 0x9a5490d, 0x531e392;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_double(
            0xe71dabcd, 0x47d42ba6, 0x89e5cb4f, 0x54d3fe49, 0x60b5373f, 0x6098ae32, 0x6b63f43c, 0xd49ee4fb,
            0x16603c2, 0xe66a90cf, 0x12ff7031, 0x129c5093, 0xa61bf356, 0xd7c87ea7, 0x9a5490d, 0x531e392);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0xc8c828a, 0x53f30ab9, 0xc96ae41f, 0x132eb242, 0x17e81c75, 0xe44a0d8, 0xa4149e75, 0x5f94851c;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0x37344d80, 0xbfeb0a3f, 0x4fc68b04, 0x8c66df75, 0x8882f35e, 0xe5f0797d, 0xafa1fee8, 0x26b8c3b8;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_double(
            0xc8c828a, 0x53f30ab9, 0xc96ae41f, 0x132eb242, 0x17e81c75, 0xe44a0d8, 0xa4149e75, 0x5f94851c,
            0x37344d80, 0xbfeb0a3f, 0x4fc68b04, 0x8c66df75, 0x8882f35e, 0xe5f0797d, 0xafa1fee8, 0x26b8c3b8);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0xc5041216, 0x65b7f8f1, 0x842b836a, 0x3f7335f6, 0xdc2fed52, 0x128b59ef, 0x21f7acf4, 0xda75317b;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0x6e708572, 0xdaed3298, 0xe77aceda, 0xe9aac07a, 0x342d7fc6, 0xdf19e21b, 0xbf72d5f0, 0x73f8a046;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_double(
            0xc5041216, 0x65b7f8f1, 0x842b836a, 0x3f7335f6, 0xdc2fed52, 0x128b59ef, 0x21f7acf4, 0xda75317b,
            0x6e708572, 0xdaed3298, 0xe77aceda, 0xe9aac07a, 0x342d7fc6, 0xdf19e21b, 0xbf72d5f0, 0x73f8a046);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0x3c62bac0, 0x9505324f, 0x51f0ab06, 0x19150ddf, 0xc3e8b70e, 0x1364b7d2, 0x23f469c, 0x9530f0f9;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0x7618e309, 0x478abda9, 0x2f1fdc68, 0xe25b3285, 0x59b333e0, 0x34dd2f7f, 0x8f9f21e2, 0x8f3c305a;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_double(
            0x3c62bac0, 0x9505324f, 0x51f0ab06, 0x19150ddf, 0xc3e8b70e, 0x1364b7d2, 0x23f469c, 0x9530f0f9,
            0x7618e309, 0x478abda9, 0x2f1fdc68, 0xe25b3285, 0x59b333e0, 0x34dd2f7f, 0x8f9f21e2, 0x8f3c305a);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0xdc3c9c8f, 0x6704385, 0x3e4367b2, 0xf2816fee, 0xaaa332b0, 0x6f09ff43, 0xbe4298fd, 0x67be02dc;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0x593652d9, 0x55384998, 0xb88c2be, 0xcd993bf6, 0x8291693, 0xa2c945b6, 0x3e4def84, 0x7a9b55a7;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_double(
            0xdc3c9c8f, 0x6704385, 0x3e4367b2, 0xf2816fee, 0xaaa332b0, 0x6f09ff43, 0xbe4298fd, 0x67be02dc,
            0x593652d9, 0x55384998, 0xb88c2be, 0xcd993bf6, 0x8291693, 0xa2c945b6, 0x3e4def84, 0x7a9b55a7);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0x10aaa33a, 0x11f9bcbe, 0xc17b9ca5, 0x8c92dd29, 0xbc571836, 0xdf569013, 0xf4ef876a, 0x893b2492;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0xd1af3445, 0x67b80b8a, 0x13ceeb42, 0xa439e8a2, 0x66507f32, 0xf413a007, 0x72d1c89e, 0xcdb152b6;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_double(
            0x10aaa33a, 0x11f9bcbe, 0xc17b9ca5, 0x8c92dd29, 0xbc571836, 0xdf569013, 0xf4ef876a, 0x893b2492,
            0xd1af3445, 0x67b80b8a, 0x13ceeb42, 0xa439e8a2, 0x66507f32, 0xf413a007, 0x72d1c89e, 0xcdb152b6);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0xf6e55dc8, 0x4b891216, 0xeaca0439, 0x6ff95ab6, 0xc0509442, 0xba84a440, 0x90c5ffb2, 0x44314047;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0xdbe323b3, 0x31d944ae, 0x9eaa2e50, 0xa66a29b7, 0x5642fed7, 0xfe99837f, 0xe65366f8, 0x96b0c142;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_double(
            0xf6e55dc8, 0x4b891216, 0xeaca0439, 0x6ff95ab6, 0xc0509442, 0xba84a440, 0x90c5ffb2, 0x44314047,
            0xdbe323b3, 0x31d944ae, 0x9eaa2e50, 0xa66a29b7, 0x5642fed7, 0xfe99837f, 0xe65366f8, 0x96b0c142);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0x33f0e9aa, 0x3eb5e196, 0xb11bd34b, 0x68112776, 0xd58138d2, 0xb7924ae0, 0x575f26ad, 0xe5380fe8;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0x4082720f, 0xc4ba4136, 0xf468318e, 0x6fb94e5d, 0x924c8e01, 0x5b691363, 0x9087b41d, 0xb97fd873;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_double(
            0x33f0e9aa, 0x3eb5e196, 0xb11bd34b, 0x68112776, 0xd58138d2, 0xb7924ae0, 0x575f26ad, 0xe5380fe8,
            0x4082720f, 0xc4ba4136, 0xf468318e, 0x6fb94e5d, 0x924c8e01, 0x5b691363, 0x9087b41d, 0xb97fd873);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0xeebc61d6, 0x1aed361b, 0xd9ff42de, 0x8a8fd3a7, 0x5d6b1f51, 0xc395f0d1, 0xa3ed9af0, 0x939ff3e4;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0xa3f5cb70, 0xe75ea466, 0xb78c7f82, 0x980bf26e, 0xef016c04, 0x9d46fc4e, 0x8b7a90e, 0xdeab3bcf;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_double(
            0xeebc61d6, 0x1aed361b, 0xd9ff42de, 0x8a8fd3a7, 0x5d6b1f51, 0xc395f0d1, 0xa3ed9af0, 0x939ff3e4,
            0xa3f5cb70, 0xe75ea466, 0xb78c7f82, 0x980bf26e, 0xef016c04, 0x9d46fc4e, 0x8b7a90e, 0xdeab3bcf);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0xc497e0df, 0x16e134d, 0xecf76f53, 0x4c3bb436, 0xfe6029a0, 0x7858785, 0xae383293, 0xfdc63e52;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0xdb9eb19f, 0xf0604449, 0xbf35d9d5, 0x7bbeb22f, 0x8ae2e8b8, 0xe3df7142, 0xacebbb52, 0x292dad67;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_double(
            0xc497e0df, 0x16e134d, 0xecf76f53, 0x4c3bb436, 0xfe6029a0, 0x7858785, 0xae383293, 0xfdc63e52,
            0xdb9eb19f, 0xf0604449, 0xbf35d9d5, 0x7bbeb22f, 0x8ae2e8b8, 0xe3df7142, 0xacebbb52, 0x292dad67);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0xf55812dd, 0xa0a2a582, 0x552d30e2, 0x3d446723, 0xc058f78e, 0xb6abed6, 0x92ff352f, 0x7029bd7a;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0x1a2d2927, 0x721cc66b, 0x43b2c73c, 0x47dae842, 0xe30683ac, 0x7dd6544a, 0xfde8b3d2, 0xb0eefada;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_double(
            0xf55812dd, 0xa0a2a582, 0x552d30e2, 0x3d446723, 0xc058f78e, 0xb6abed6, 0x92ff352f, 0x7029bd7a,
            0x1a2d2927, 0x721cc66b, 0x43b2c73c, 0x47dae842, 0xe30683ac, 0x7dd6544a, 0xfde8b3d2, 0xb0eefada);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0xb181fdc2, 0xdcdabff9, 0x5cc62364, 0xdd2f62bb, 0x18a34e7e, 0x4aa264b8, 0xf47e6e47, 0xf42c102a;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0xa485d7fd, 0x81f00093, 0x9a2acf26, 0x4c15502d, 0xb86fe22a, 0x78fad05c, 0x6cfe806c, 0x57503ab4;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_double(
            0xb181fdc2, 0xdcdabff9, 0x5cc62364, 0xdd2f62bb, 0x18a34e7e, 0x4aa264b8, 0xf47e6e47, 0xf42c102a,
            0xa485d7fd, 0x81f00093, 0x9a2acf26, 0x4c15502d, 0xb86fe22a, 0x78fad05c, 0x6cfe806c, 0x57503ab4);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0xeedd7dd6, 0x3866d47d, 0x65e1968c, 0x49376fe2, 0xee7cfdec, 0xca5a7840, 0x24c7524b, 0x32cfcf6a;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0xfe08e330, 0x25fd44ae, 0x349a08b, 0x7a0d8cd2, 0x409f561e, 0x6208096a, 0x976a7748, 0x21846a34;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_double(
            0xeedd7dd6, 0x3866d47d, 0x65e1968c, 0x49376fe2, 0xee7cfdec, 0xca5a7840, 0x24c7524b, 0x32cfcf6a,
            0xfe08e330, 0x25fd44ae, 0x349a08b, 0x7a0d8cd2, 0x409f561e, 0x6208096a, 0x976a7748, 0x21846a34);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0x21231d11, 0xce674831, 0x3c2aaad7, 0x22ab36c6, 0xc777c398, 0x33d1155c, 0x8b9388e4, 0x3514d41e;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0xe3855df5, 0x53d6fb40, 0xaf79ebe, 0x9384f31d, 0x56839eff, 0xef44d11e, 0x16017eb8, 0x89a83250;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_double(
            0x21231d11, 0xce674831, 0x3c2aaad7, 0x22ab36c6, 0xc777c398, 0x33d1155c, 0x8b9388e4, 0x3514d41e,
            0xe3855df5, 0x53d6fb40, 0xaf79ebe, 0x9384f31d, 0x56839eff, 0xef44d11e, 0x16017eb8, 0x89a83250);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0x80633cb1, 0x2567e09e, 0x69d02113, 0x575a224b, 0x12181fcb, 0xc62732, 0x17aacad4, 0x6dde9cf3;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0x67ce6b34, 0x57dd49aa, 0xcf859ef3, 0x80b27fda, 0xa1ba66a8, 0x5c99ef86, 0xa707e41d, 0x9188fbe7;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_double(
            0x80633cb1, 0x2567e09e, 0x69d02113, 0x575a224b, 0x12181fcb, 0xc62732, 0x17aacad4, 0x6dde9cf3,
            0x67ce6b34, 0x57dd49aa, 0xcf859ef3, 0x80b27fda, 0xa1ba66a8, 0x5c99ef86, 0xa707e41d, 0x9188fbe7);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0x44e5467d, 0x4d0bd76a, 0x19bbface, 0x40908ab8, 0xec970e9, 0x2c21f62e, 0xfc69a122, 0x97d064f0;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0x1e9cb3fa, 0x797300fd, 0x54f17ccd, 0xda5fb3b8, 0xa850861f, 0x3f7c66f, 0xd33402cc, 0x89974f2e;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_double(
            0x44e5467d, 0x4d0bd76a, 0x19bbface, 0x40908ab8, 0xec970e9, 0x2c21f62e, 0xfc69a122, 0x97d064f0,
            0x1e9cb3fa, 0x797300fd, 0x54f17ccd, 0xda5fb3b8, 0xa850861f, 0x3f7c66f, 0xd33402cc, 0x89974f2e);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0x13613bec, 0xcca81cb9, 0x101cfe67, 0x8bb5fc9d, 0xc74f972a, 0xedf1b33d, 0xc93937bd, 0x2dcfcab8;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0x9a039215, 0x3e730924, 0xd33f5f38, 0x3732cfba, 0xd6f6c6f4, 0x65f088b7, 0x9474a412, 0x46dbc4dd;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_double(
            0x13613bec, 0xcca81cb9, 0x101cfe67, 0x8bb5fc9d, 0xc74f972a, 0xedf1b33d, 0xc93937bd, 0x2dcfcab8,
            0x9a039215, 0x3e730924, 0xd33f5f38, 0x3732cfba, 0xd6f6c6f4, 0x65f088b7, 0x9474a412, 0x46dbc4dd);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0x47fb9e1a, 0x17cd1708, 0xde2a3296, 0x7fe74b74, 0xbbab0e76, 0xf1a02bc9, 0xa48ec5a8, 0x1bec414a;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0x749c0443, 0x57f6e117, 0xe8c9796e, 0x681385da, 0x30c54b0f, 0x8a79bc57, 0x70126667, 0xe3586704;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_double(
            0x47fb9e1a, 0x17cd1708, 0xde2a3296, 0x7fe74b74, 0xbbab0e76, 0xf1a02bc9, 0xa48ec5a8, 0x1bec414a,
            0x749c0443, 0x57f6e117, 0xe8c9796e, 0x681385da, 0x30c54b0f, 0x8a79bc57, 0x70126667, 0xe3586704);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0xbb7ceceb, 0xf3f678ff, 0x8897faf0, 0x73a59f93, 0x6f6e6814, 0x36ffb812, 0x4276d450, 0x437a8620;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0x56c181e1, 0x7363bcc3, 0xdc8f9782, 0x87220fcf, 0x99d297ff, 0x69b8feb6, 0x3eeac32f, 0xb916ba1;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_double(
            0xbb7ceceb, 0xf3f678ff, 0x8897faf0, 0x73a59f93, 0x6f6e6814, 0x36ffb812, 0x4276d450, 0x437a8620,
            0x56c181e1, 0x7363bcc3, 0xdc8f9782, 0x87220fcf, 0x99d297ff, 0x69b8feb6, 0x3eeac32f, 0xb916ba1);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0xdcbf00eb, 0x4c9d9d87, 0xc18d0227, 0x41b4e98b, 0xa1a30bc2, 0x49be16f6, 0x96ead4dc, 0xb89070ae;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0x1b0e664e, 0x1b7f1bcd, 0xb6b96a67, 0xcb0d8b06, 0xc1c4a766, 0x472294e4, 0xc8a2d88f, 0x6f24c8c2;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_double(
            0xdcbf00eb, 0x4c9d9d87, 0xc18d0227, 0x41b4e98b, 0xa1a30bc2, 0x49be16f6, 0x96ead4dc, 0xb89070ae,
            0x1b0e664e, 0x1b7f1bcd, 0xb6b96a67, 0xcb0d8b06, 0xc1c4a766, 0x472294e4, 0xc8a2d88f, 0x6f24c8c2);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0xb6fbe7b2, 0xb9d6ff9a, 0x458d65a3, 0x5eadedc1, 0xb2a88460, 0xf336bbb1, 0x9cb441f8, 0x26488766;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0x21bc2a34, 0x932a78bc, 0x6a0eb603, 0x5638d981, 0xd02ddf18, 0x8f2f2dca, 0xb2014498, 0x9e15dab4;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_double(
            0xb6fbe7b2, 0xb9d6ff9a, 0x458d65a3, 0x5eadedc1, 0xb2a88460, 0xf336bbb1, 0x9cb441f8, 0x26488766,
            0x21bc2a34, 0x932a78bc, 0x6a0eb603, 0x5638d981, 0xd02ddf18, 0x8f2f2dca, 0xb2014498, 0x9e15dab4);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0x2b038315, 0x9690d306, 0x69310e6f, 0x9cacc433, 0x9794b862, 0x1e4680e3, 0x56771222, 0xaba55687;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0xae25fc0a, 0xf9a003f9, 0xd8b63338, 0x3fbfb532, 0x25130d6f, 0x63d570f6, 0xaa365edb, 0xa0e75d87;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_double(
            0x2b038315, 0x9690d306, 0x69310e6f, 0x9cacc433, 0x9794b862, 0x1e4680e3, 0x56771222, 0xaba55687,
            0xae25fc0a, 0xf9a003f9, 0xd8b63338, 0x3fbfb532, 0x25130d6f, 0x63d570f6, 0xaa365edb, 0xa0e75d87);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0x95bc15b4, 0x9cb9a134, 0x465a2ee6, 0x9275028e, 0xced7ca8d, 0xed858ee9, 0x51eeadc9, 0x10e90e2e;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0x58aa258d, 0x34ebe609, 0x2bb6a88, 0x4ca58963, 0x16ad1f75, 0x4d57a8c6, 0x80d5e042, 0xc68a3703;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_double(
            0x95bc15b4, 0x9cb9a134, 0x465a2ee6, 0x9275028e, 0xced7ca8d, 0xed858ee9, 0x51eeadc9, 0x10e90e2e,
            0x58aa258d, 0x34ebe609, 0x2bb6a88, 0x4ca58963, 0x16ad1f75, 0x4d57a8c6, 0x80d5e042, 0xc68a3703);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0x7a1c0a80, 0xf62abc8, 0xc65a9c74, 0x4d625158, 0x2ff9c3, 0xb17c9be7, 0xa614cca5, 0xb6b15a68;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0x41ce0a03, 0xb6cd0110, 0x82e16ee, 0x9c9a12b3, 0xef6536d4, 0xa54e223e, 0xd6cdb61e, 0xfae62e14;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_double(
            0x7a1c0a80, 0xf62abc8, 0xc65a9c74, 0x4d625158, 0x2ff9c3, 0xb17c9be7, 0xa614cca5, 0xb6b15a68,
            0x41ce0a03, 0xb6cd0110, 0x82e16ee, 0x9c9a12b3, 0xef6536d4, 0xa54e223e, 0xd6cdb61e, 0xfae62e14);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0x92b062d4, 0xa7caa50a, 0x9bb6a141, 0x7a5ce7e5, 0x83ea227a, 0x6fb1712, 0x3256eaca, 0x35963ea4;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0xbbb25302, 0xa10aa4d1, 0x64de59b1, 0xd04082b9, 0xf9c08a96, 0xbfcce196, 0x4951e5c9, 0xf65be145;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_double(
            0x92b062d4, 0xa7caa50a, 0x9bb6a141, 0x7a5ce7e5, 0x83ea227a, 0x6fb1712, 0x3256eaca, 0x35963ea4,
            0xbbb25302, 0xa10aa4d1, 0x64de59b1, 0xd04082b9, 0xf9c08a96, 0xbfcce196, 0x4951e5c9, 0xf65be145);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0x1d33fd27, 0xfa0bf5c5, 0xb646cc62, 0x445f573d, 0xda82361b, 0xd022388e, 0x2263e84c, 0x9ed73f09;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0x2716c458, 0x5972b2de, 0xb2e44934, 0x94a823e5, 0x42467254, 0xee75b4f3, 0xebb1eeea, 0xb6318967;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_double(
            0x1d33fd27, 0xfa0bf5c5, 0xb646cc62, 0x445f573d, 0xda82361b, 0xd022388e, 0x2263e84c, 0x9ed73f09,
            0x2716c458, 0x5972b2de, 0xb2e44934, 0x94a823e5, 0x42467254, 0xee75b4f3, 0xebb1eeea, 0xb6318967);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0xef028d83, 0x579623ae, 0xba743961, 0x6195926d, 0x15de69db, 0x6a5abe5a, 0xe3c785ec, 0xa7ebf7c4;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0x99d0bed1, 0x9640392b, 0x4b053919, 0x47a38927, 0x7044804b, 0xcfd9c737, 0xbfe362d5, 0x6205152f;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_double(
            0xef028d83, 0x579623ae, 0xba743961, 0x6195926d, 0x15de69db, 0x6a5abe5a, 0xe3c785ec, 0xa7ebf7c4,
            0x99d0bed1, 0x9640392b, 0x4b053919, 0x47a38927, 0x7044804b, 0xcfd9c737, 0xbfe362d5, 0x6205152f);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0x7bb61ee5, 0xf2884413, 0xfb1f0c13, 0xda4f04e2, 0x8974ae6e, 0x662638cd, 0xcc8721b8, 0xd4933230;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0xe5d694a8, 0x662da4d0, 0x5a438ddc, 0x1ad12c8c, 0x1ecafb5e, 0xedcc5e9d, 0xf51a9d23, 0x21c09ab;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_double(
            0x7bb61ee5, 0xf2884413, 0xfb1f0c13, 0xda4f04e2, 0x8974ae6e, 0x662638cd, 0xcc8721b8, 0xd4933230,
            0xe5d694a8, 0x662da4d0, 0x5a438ddc, 0x1ad12c8c, 0x1ecafb5e, 0xedcc5e9d, 0xf51a9d23, 0x21c09ab);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0xec04554a, 0x530ddcbc, 0x4688cffe, 0xaadcffbb, 0x7a10a2ec, 0x474652c2, 0x9873d1a0, 0x896f37c8;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0x929138df, 0xd68f9fe1, 0xacc417dc, 0xe6085b61, 0x4e811bf1, 0xda622bb0, 0x224ac4ac, 0x380423e7;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_double(
            0xec04554a, 0x530ddcbc, 0x4688cffe, 0xaadcffbb, 0x7a10a2ec, 0x474652c2, 0x9873d1a0, 0x896f37c8,
            0x929138df, 0xd68f9fe1, 0xacc417dc, 0xe6085b61, 0x4e811bf1, 0xda622bb0, 0x224ac4ac, 0x380423e7);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0xb43fca26, 0x84077d1a, 0xa3bc2367, 0x7dfb841d, 0xbf3578a2, 0xca6c209d, 0x774b6d6c, 0x11b3b97f;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0x5b679d58, 0xd3b27eaf, 0x4b9f9d42, 0x3bae231c, 0x2f36d3bb, 0x8cd5650c, 0xae600c50, 0x65331f9f;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_double(
            0xb43fca26, 0x84077d1a, 0xa3bc2367, 0x7dfb841d, 0xbf3578a2, 0xca6c209d, 0x774b6d6c, 0x11b3b97f,
            0x5b679d58, 0xd3b27eaf, 0x4b9f9d42, 0x3bae231c, 0x2f36d3bb, 0x8cd5650c, 0xae600c50, 0x65331f9f);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0x48dfd587, 0x79361bb, 0xc9b02656, 0x5ec4ba38, 0x2cf5a12d, 0x34867aaa, 0xacf4508b, 0x5084b41b;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0x91470e89, 0x6e79e97f, 0x6891f560, 0x5db6f560, 0x55292747, 0x619aa6c8, 0x1d980d31, 0x34a9631a;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_double(
            0x48dfd587, 0x79361bb, 0xc9b02656, 0x5ec4ba38, 0x2cf5a12d, 0x34867aaa, 0xacf4508b, 0x5084b41b,
            0x91470e89, 0x6e79e97f, 0x6891f560, 0x5db6f560, 0x55292747, 0x619aa6c8, 0x1d980d31, 0x34a9631a);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0x6c953fa9, 0x4d05956d, 0xf0b8c3db, 0x28ab2629, 0x4bd18c06, 0x3a5f485d, 0xaaab9323, 0xa49ed10e;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0x46fb4c72, 0x67b2bd22, 0x968e181b, 0x5ae87534, 0xa0dfddfb, 0xe03476c0, 0x660f5398, 0xcc72b894;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_add(
            0x60297556, 0x2f057a14, 0x8568a18b, 0x82f6472f, 0x355235d3, 0x20453a14, 0x755eeea4, 0xfff97bd5,
            0xb075f297, 0x3c870c36, 0x518fe4a0, 0xde80f0f6, 0x7f45c560, 0xf3be9601, 0xacfbb620, 0xae12777a,
            0x6c953fa9, 0x4d05956d, 0xf0b8c3db, 0x28ab2629, 0x4bd18c06, 0x3a5f485d, 0xaaab9323, 0xa49ed10e,
            0x46fb4c72, 0x67b2bd22, 0x968e181b, 0x5ae87534, 0xa0dfddfb, 0xe03476c0, 0x660f5398, 0xcc72b894);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0x23510ef9, 0xbb3af611, 0xf22c19c7, 0x87d4c3cb, 0x53cecf40, 0xd11823c1, 0xdc6f9ec5, 0xe12026ef;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0xb058f360, 0x96451fbb, 0x20e5efc6, 0xdcda7f72, 0xe03ad137, 0xf367ef20, 0x5a51eadc, 0x30a930ea;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_add(
            0x70afe85a, 0xc5b0f470, 0x9620095b, 0x687cf441, 0x4d734633, 0x15c38f00, 0x48e7561b, 0xd01115d5,
            0xf4062327, 0x6b051b13, 0xd9a86d52, 0x79238c5d, 0xe17bd815, 0xa8b64537, 0xc815e0d7, 0xa9f34ffd,
            0x23510ef9, 0xbb3af611, 0xf22c19c7, 0x87d4c3cb, 0x53cecf40, 0xd11823c1, 0xdc6f9ec5, 0xe12026ef,
            0xb058f360, 0x96451fbb, 0x20e5efc6, 0xdcda7f72, 0xe03ad137, 0xf367ef20, 0x5a51eadc, 0x30a930ea);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0xaee8b7f4, 0xbfd2ecb4, 0xe21c31ef, 0x22c7841e, 0x2da82592, 0x7d356af0, 0x64eab6f5, 0xd5ae772d;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0x454d87bc, 0x18cf6cf2, 0xe498d098, 0x8d035085, 0x121f031d, 0x43b132ac, 0x313a838a, 0x10231a1d;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_add(
            0xb202e6ce, 0x502bda8, 0x9d62b794, 0x68321543, 0x61ba8b09, 0x8ac09c91, 0x413d33d4, 0xfe72c435,
            0xcf58c5bf, 0x978ed2fb, 0x6b4a9d22, 0x1dc88e3, 0x9d729981, 0xd3ab47e0, 0x7ff24a68, 0x6851de06,
            0xaee8b7f4, 0xbfd2ecb4, 0xe21c31ef, 0x22c7841e, 0x2da82592, 0x7d356af0, 0x64eab6f5, 0xd5ae772d,
            0x454d87bc, 0x18cf6cf2, 0xe498d098, 0x8d035085, 0x121f031d, 0x43b132ac, 0x313a838a, 0x10231a1d);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0x29f01588, 0x47a519de, 0x80b7983e, 0xe3a17625, 0xc7a542b9, 0x2c03e1b0, 0xd5ba06de, 0xf952de32;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0x96452adf, 0xcc4f6b5a, 0xa0593002, 0x1ebbb2bb, 0x49fb7433, 0xee209edb, 0x9e3a6edb, 0x23c02d3;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_add(
            0x1118e5c3, 0x9bd870aa, 0x452bebc1, 0xfc579b27, 0xf4e65b4b, 0xb441656e, 0x9645307d, 0x6eca335d,
            0x5a08668, 0x498a2f78, 0x3bf8ec34, 0x3a496a3a, 0x74b875a0, 0x592f5790, 0x7a7a0710, 0xd50123b5,
            0x29f01588, 0x47a519de, 0x80b7983e, 0xe3a17625, 0xc7a542b9, 0x2c03e1b0, 0xd5ba06de, 0xf952de32,
            0x96452adf, 0xcc4f6b5a, 0xa0593002, 0x1ebbb2bb, 0x49fb7433, 0xee209edb, 0x9e3a6edb, 0x23c02d3);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0x170508d7, 0x5137b67a, 0xfec94132, 0xacb18631, 0xca0ddf6b, 0xa9f0b9c1, 0x59982afe, 0x19d6a989;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0x311ed142, 0xacf14bf8, 0x9cdeddfc, 0xa372ada, 0x47016a3d, 0xe73aacf, 0xa468f4db, 0x221a2b52;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_add(
            0x7f8cb0e3, 0x43933aca, 0xe1efe3a4, 0xa22eb53f, 0x4b2eb72e, 0x8fa64e04, 0x74456d8f, 0x3f0e80e5,
            0xea5f404f, 0xcb0289e2, 0xa65b53a4, 0x9501253a, 0x485d01b3, 0xe90b9c08, 0x296cbc91, 0xcb66d7d7,
            0x170508d7, 0x5137b67a, 0xfec94132, 0xacb18631, 0xca0ddf6b, 0xa9f0b9c1, 0x59982afe, 0x19d6a989,
            0x311ed142, 0xacf14bf8, 0x9cdeddfc, 0xa372ada, 0x47016a3d, 0xe73aacf, 0xa468f4db, 0x221a2b52);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0xb4774055, 0x3beba8b, 0x1e5354e0, 0x390e80f2, 0x84fe6b81, 0x7da29ade, 0x873a0892, 0x9e60108;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0xe91c37b4, 0x1f4b0a9e, 0xa0f5c9df, 0xf8b46cf3, 0xa2e33e67, 0xdd6c49ac, 0x8cc23a00, 0xb16a1638;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_add(
            0x33ce1752, 0xc7b750f7, 0xd7cd204e, 0xe783c797, 0xd99c9aea, 0x812ddf64, 0xd01dc635, 0xd7a0da58,
            0x762cef4, 0xbbc02738, 0xc062b742, 0xbe040a8, 0x40e28465, 0xf6f29283, 0x68008032, 0x912770e0,
            0xb4774055, 0x3beba8b, 0x1e5354e0, 0x390e80f2, 0x84fe6b81, 0x7da29ade, 0x873a0892, 0x9e60108,
            0xe91c37b4, 0x1f4b0a9e, 0xa0f5c9df, 0xf8b46cf3, 0xa2e33e67, 0xdd6c49ac, 0x8cc23a00, 0xb16a1638);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0xb2993603, 0x84fd71bd, 0x35e25ff7, 0x1077370c, 0x5867e485, 0x9edfb935, 0xd0b48228, 0x2de4cdc;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0xf3c18392, 0xd7b138d4, 0xa44f7f11, 0x1bd09b1d, 0x87477348, 0x53396a92, 0xc1c97199, 0x18b5caf6;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_add(
            0xb5476085, 0xa908b701, 0x96eb9f84, 0xb5714e77, 0xa78ed1af, 0x10d3aad6, 0x7a08cd3e, 0x3443a706,
            0x8b8f52d8, 0x6d3484bd, 0xd0c2b67f, 0x18a4b27, 0x8c7e1da9, 0x4f6e8c4b, 0x829b6f85, 0x661a7a5f,
            0xb2993603, 0x84fd71bd, 0x35e25ff7, 0x1077370c, 0x5867e485, 0x9edfb935, 0xd0b48228, 0x2de4cdc,
            0xf3c18392, 0xd7b138d4, 0xa44f7f11, 0x1bd09b1d, 0x87477348, 0x53396a92, 0xc1c97199, 0x18b5caf6);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0x694478f1, 0xb4fb31d0, 0x639a1309, 0xe3af2921, 0x8069c19c, 0xcea090c4, 0xdb4efc49, 0x2ce2bd56;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0xc94a43b7, 0xdb966e56, 0xa8a289c1, 0x166b72, 0xe9560522, 0x78291fe1, 0x3d95a2e2, 0xb56a711;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_add(
            0xe57e8dfa, 0xfcfc0cb9, 0xa3c7e184, 0x9809191, 0xaca98ca0, 0xd9a30f8, 0xf0799c4c, 0x8262cf2f,
            0xfbac376a, 0x35cff8d8, 0x2b14c478, 0x57b6ed33, 0xc5b34f34, 0x66fee22e, 0x9109e4e, 0x83fd95e2,
            0x694478f1, 0xb4fb31d0, 0x639a1309, 0xe3af2921, 0x8069c19c, 0xcea090c4, 0xdb4efc49, 0x2ce2bd56,
            0xc94a43b7, 0xdb966e56, 0xa8a289c1, 0x166b72, 0xe9560522, 0x78291fe1, 0x3d95a2e2, 0xb56a711);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0x44ad3367, 0x9aeb9669, 0x637f77d7, 0xc3a1a0e7, 0x6964096c, 0xdf790607, 0x3e8509c2, 0xab7527e3;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0xba33e3e9, 0xc844b48d, 0xda415aa3, 0xc572928e, 0xa95cf18e, 0x4778ec33, 0xfa8b39, 0xfac5ff0c;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_add(
            0x7c70620c, 0xd17cc1f2, 0xabc288d9, 0x4998c4be, 0x2b671780, 0xc60dd31a, 0x8d2c236d, 0x1653a8a4,
            0x315b32cd, 0x6ca2e81d, 0xdfd3dc52, 0x12af748, 0x4efa701c, 0xeafa9947, 0x35af7f7a, 0x3382909,
            0x44ad3367, 0x9aeb9669, 0x637f77d7, 0xc3a1a0e7, 0x6964096c, 0xdf790607, 0x3e8509c2, 0xab7527e3,
            0xba33e3e9, 0xc844b48d, 0xda415aa3, 0xc572928e, 0xa95cf18e, 0x4778ec33, 0xfa8b39, 0xfac5ff0c);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0x344d1571, 0xfbc3a3ed, 0x6d037843, 0xd86e1c94, 0xb24f4644, 0x3c6685fd, 0xb14dbaa6, 0xd4632e2b;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0xb26ac915, 0x57c30d45, 0xa60c041f, 0xaff15cc2, 0x76a49ec0, 0x2e672992, 0xed49e170, 0x5b7067;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_add(
            0xe71dabcd, 0x47d42ba6, 0x89e5cb4f, 0x54d3fe49, 0x60b5373f, 0x6098ae32, 0x6b63f43c, 0xd49ee4fb,
            0x16603c2, 0xe66a90cf, 0x12ff7031, 0x129c5093, 0xa61bf356, 0xd7c87ea7, 0x9a5490d, 0x531e392,
            0x344d1571, 0xfbc3a3ed, 0x6d037843, 0xd86e1c94, 0xb24f4644, 0x3c6685fd, 0xb14dbaa6, 0xd4632e2b,
            0xb26ac915, 0x57c30d45, 0xa60c041f, 0xaff15cc2, 0x76a49ec0, 0x2e672992, 0xed49e170, 0x5b7067);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0xdbe47240, 0xf2ee698, 0xb9575b37, 0xe2d2cfe2, 0x4a09b9d5, 0xbfe560fa, 0xaf3c4f5c, 0xb311519d;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0x66c734da, 0x1147627c, 0xa4e7d38c, 0x41bf546d, 0xa86dd053, 0xbd7ee596, 0x65390183, 0xe8608078;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_add(
            0xc8c828a, 0x53f30ab9, 0xc96ae41f, 0x132eb242, 0x17e81c75, 0xe44a0d8, 0xa4149e75, 0x5f94851c,
            0x37344d80, 0xbfeb0a3f, 0x4fc68b04, 0x8c66df75, 0x8882f35e, 0xe5f0797d, 0xafa1fee8, 0x26b8c3b8,
            0xdbe47240, 0xf2ee698, 0xb9575b37, 0xe2d2cfe2, 0x4a09b9d5, 0xbfe560fa, 0xaf3c4f5c, 0xb311519d,
            0x66c734da, 0x1147627c, 0xa4e7d38c, 0x41bf546d, 0xa86dd053, 0xbd7ee596, 0x65390183, 0xe8608078);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0x7950604f, 0x3904da0a, 0x8d3a8c08, 0x7f74a2e8, 0x5270445, 0x70f70734, 0x3ca5ebf9, 0xa4da524a;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0xe01e4087, 0x6f2bfded, 0x85e258c9, 0xb7498e0b, 0x51ab50dd, 0x1cb690a9, 0xae9bf4bc, 0x3bc500fb;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_add(
            0xc5041216, 0x65b7f8f1, 0x842b836a, 0x3f7335f6, 0xdc2fed52, 0x128b59ef, 0x21f7acf4, 0xda75317b,
            0x6e708572, 0xdaed3298, 0xe77aceda, 0xe9aac07a, 0x342d7fc6, 0xdf19e21b, 0xbf72d5f0, 0x73f8a046,
            0x7950604f, 0x3904da0a, 0x8d3a8c08, 0x7f74a2e8, 0x5270445, 0x70f70734, 0x3ca5ebf9, 0xa4da524a,
            0xe01e4087, 0x6f2bfded, 0x85e258c9, 0xb7498e0b, 0x51ab50dd, 0x1cb690a9, 0xae9bf4bc, 0x3bc500fb);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0x84b81f7f, 0xf0507d87, 0xa12969d7, 0xee9f7214, 0x1d049f95, 0xddb232b2, 0x640741b1, 0x448316e7;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0x5e2540f7, 0x3630d948, 0xb351e0f, 0x43ce853a, 0x803089f2, 0x692ec20e, 0x29d5008f, 0xf4bf3660;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_add(
            0x3c62bac0, 0x9505324f, 0x51f0ab06, 0x19150ddf, 0xc3e8b70e, 0x1364b7d2, 0x23f469c, 0x9530f0f9,
            0x7618e309, 0x478abda9, 0x2f1fdc68, 0xe25b3285, 0x59b333e0, 0x34dd2f7f, 0x8f9f21e2, 0x8f3c305a,
            0x84b81f7f, 0xf0507d87, 0xa12969d7, 0xee9f7214, 0x1d049f95, 0xddb232b2, 0x640741b1, 0x448316e7,
            0x5e2540f7, 0x3630d948, 0xb351e0f, 0x43ce853a, 0x803089f2, 0x692ec20e, 0x29d5008f, 0xf4bf3660);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0xc37466d3, 0x1258ebbc, 0x79fac61e, 0x5911c69b, 0x594aefa8, 0x6a450d16, 0xb5da30ac, 0xebee8e11;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0xaf04f45f, 0x99500512, 0xde6da3c0, 0xd8a47486, 0x8facdfd6, 0xab96ea50, 0xcc61f51d, 0x5c523787;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_add(
            0xdc3c9c8f, 0x6704385, 0x3e4367b2, 0xf2816fee, 0xaaa332b0, 0x6f09ff43, 0xbe4298fd, 0x67be02dc,
            0x593652d9, 0x55384998, 0xb88c2be, 0xcd993bf6, 0x8291693, 0xa2c945b6, 0x3e4def84, 0x7a9b55a7,
            0xc37466d3, 0x1258ebbc, 0x79fac61e, 0x5911c69b, 0x594aefa8, 0x6a450d16, 0xb5da30ac, 0xebee8e11,
            0xaf04f45f, 0x99500512, 0xde6da3c0, 0xd8a47486, 0x8facdfd6, 0xab96ea50, 0xcc61f51d, 0x5c523787);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0xa0aae439, 0x4f4c07cf, 0x89c4c360, 0x6f99cc22, 0x11576d96, 0x536674ab, 0xaa9cf13d, 0x9d9c34cc;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0xb57534d9, 0xab01d669, 0xa1e9a5a7, 0xcd33ec53, 0x421d0474, 0x399ae585, 0x40a53d7d, 0xd9c7c978;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_add(
            0x10aaa33a, 0x11f9bcbe, 0xc17b9ca5, 0x8c92dd29, 0xbc571836, 0xdf569013, 0xf4ef876a, 0x893b2492,
            0xd1af3445, 0x67b80b8a, 0x13ceeb42, 0xa439e8a2, 0x66507f32, 0xf413a007, 0x72d1c89e, 0xcdb152b6,
            0xa0aae439, 0x4f4c07cf, 0x89c4c360, 0x6f99cc22, 0x11576d96, 0x536674ab, 0xaa9cf13d, 0x9d9c34cc,
            0xb57534d9, 0xab01d669, 0xa1e9a5a7, 0xcd33ec53, 0x421d0474, 0x399ae585, 0x40a53d7d, 0xd9c7c978);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0x61dec56, 0xc3d986b6, 0x3fa64819, 0xa346dbb9, 0x5106c8a, 0xcf195055, 0x89c4b6c0, 0x4d83a5c;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0xde60d2c4, 0x28c617d, 0xbc06b39, 0x3ed8a31f, 0x85df1f33, 0xb8f6d515, 0x5a0b2233, 0xcf911e0f;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_add(
            0xf6e55dc8, 0x4b891216, 0xeaca0439, 0x6ff95ab6, 0xc0509442, 0xba84a440, 0x90c5ffb2, 0x44314047,
            0xdbe323b3, 0x31d944ae, 0x9eaa2e50, 0xa66a29b7, 0x5642fed7, 0xfe99837f, 0xe65366f8, 0x96b0c142,
            0x61dec56, 0xc3d986b6, 0x3fa64819, 0xa346dbb9, 0x5106c8a, 0xcf195055, 0x89c4b6c0, 0x4d83a5c,
            0xde60d2c4, 0x28c617d, 0xbc06b39, 0x3ed8a31f, 0x85df1f33, 0xb8f6d515, 0x5a0b2233, 0xcf911e0f);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0x11b05bca, 0x2a7f5d14, 0x3949a197, 0xb9db55f0, 0xb966688, 0x13962410, 0xbbc05a67, 0x6ed24224;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0x8e241ae8, 0x38555bc9, 0xdc0cc527, 0x55bb4406, 0xd4c00fb5, 0x447a0f71, 0xf36480ac, 0x5160ba0;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_add(
            0x33f0e9aa, 0x3eb5e196, 0xb11bd34b, 0x68112776, 0xd58138d2, 0xb7924ae0, 0x575f26ad, 0xe5380fe8,
            0x4082720f, 0xc4ba4136, 0xf468318e, 0x6fb94e5d, 0x924c8e01, 0x5b691363, 0x9087b41d, 0xb97fd873,
            0x11b05bca, 0x2a7f5d14, 0x3949a197, 0xb9db55f0, 0xb966688, 0x13962410, 0xbbc05a67, 0x6ed24224,
            0x8e241ae8, 0x38555bc9, 0xdc0cc527, 0x55bb4406, 0xd4c00fb5, 0x447a0f71, 0xf36480ac, 0x5160ba0);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0x6522cef6, 0xf3e9afc9, 0x3f587dda, 0xac84eab3, 0xa8c2e75d, 0x14466cd4, 0x17b57b58, 0x6d663b77;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0x4564c5a4, 0x827c89b6, 0xe66d0671, 0xff99027c, 0xa69fd33b, 0x4325889a, 0xa57f3c05, 0xd69cf941;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_add(
            0xeebc61d6, 0x1aed361b, 0xd9ff42de, 0x8a8fd3a7, 0x5d6b1f51, 0xc395f0d1, 0xa3ed9af0, 0x939ff3e4,
            0xa3f5cb70, 0xe75ea466, 0xb78c7f82, 0x980bf26e, 0xef016c04, 0x9d46fc4e, 0x8b7a90e, 0xdeab3bcf,
            0x6522cef6, 0xf3e9afc9, 0x3f587dda, 0xac84eab3, 0xa8c2e75d, 0x14466cd4, 0x17b57b58, 0x6d663b77,
            0x4564c5a4, 0x827c89b6, 0xe66d0671, 0xff99027c, 0xa69fd33b, 0x4325889a, 0xa57f3c05, 0xd69cf941);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0xc75826ca, 0x64c7ca6, 0xc829e086, 0xdc2329c, 0xff69f2ec, 0xa840f259, 0x40689eac, 0xf80118d;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0x49b394f1, 0x49420a27, 0xaecd9f53, 0xc5f848b0, 0x8658a660, 0x1e5fa185, 0x9fd2f732, 0xb518b863;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_add(
            0xc497e0df, 0x16e134d, 0xecf76f53, 0x4c3bb436, 0xfe6029a0, 0x7858785, 0xae383293, 0xfdc63e52,
            0xdb9eb19f, 0xf0604449, 0xbf35d9d5, 0x7bbeb22f, 0x8ae2e8b8, 0xe3df7142, 0xacebbb52, 0x292dad67,
            0xc75826ca, 0x64c7ca6, 0xc829e086, 0xdc2329c, 0xff69f2ec, 0xa840f259, 0x40689eac, 0xf80118d,
            0x49b394f1, 0x49420a27, 0xaecd9f53, 0xc5f848b0, 0x8658a660, 0x1e5fa185, 0x9fd2f732, 0xb518b863);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0x5d52bd3b, 0xd260a04e, 0xe527dc75, 0x41a7866d, 0xba1eb327, 0x1cc02fa9, 0xf290ba01, 0x1d931895;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0x54a028f0, 0x8a9692c7, 0xdd90d86a, 0xd007d5eb, 0x8ef4581a, 0xcb5c9f55, 0xfd528566, 0x17e3d9dd;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_add(
            0xf55812dd, 0xa0a2a582, 0x552d30e2, 0x3d446723, 0xc058f78e, 0xb6abed6, 0x92ff352f, 0x7029bd7a,
            0x1a2d2927, 0x721cc66b, 0x43b2c73c, 0x47dae842, 0xe30683ac, 0x7dd6544a, 0xfde8b3d2, 0xb0eefada,
            0x5d52bd3b, 0xd260a04e, 0xe527dc75, 0x41a7866d, 0xba1eb327, 0x1cc02fa9, 0xf290ba01, 0x1d931895,
            0x54a028f0, 0x8a9692c7, 0xdd90d86a, 0xd007d5eb, 0x8ef4581a, 0xcb5c9f55, 0xfd528566, 0x17e3d9dd);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0xb054c95a, 0xbcd8f87c, 0xb25e5d80, 0x459c9fcc, 0x1674b8f, 0x516609ce, 0x7cc748c, 0x2f6c48fd;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0x9bef73d9, 0x89dc0133, 0x19da7c6, 0xd2d4e81d, 0xada47bd1, 0xc91bfe10, 0x4a947582, 0x6633d51b;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_add(
            0xb181fdc2, 0xdcdabff9, 0x5cc62364, 0xdd2f62bb, 0x18a34e7e, 0x4aa264b8, 0xf47e6e47, 0xf42c102a,
            0xa485d7fd, 0x81f00093, 0x9a2acf26, 0x4c15502d, 0xb86fe22a, 0x78fad05c, 0x6cfe806c, 0x57503ab4,
            0xb054c95a, 0xbcd8f87c, 0xb25e5d80, 0x459c9fcc, 0x1674b8f, 0x516609ce, 0x7cc748c, 0x2f6c48fd,
            0x9bef73d9, 0x89dc0133, 0x19da7c6, 0xd2d4e81d, 0xada47bd1, 0xc91bfe10, 0x4a947582, 0x6633d51b);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0x7cf01352, 0x3fca8944, 0xc3b91a98, 0x47e08381, 0xc5409be0, 0x48662b2e, 0x51b3b0dd, 0x331795e5;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0x87069ca1, 0xb6fba74, 0xa10b410a, 0x866f98f, 0x5f2a5ed2, 0xa0afcbbe, 0x3f7674bd, 0x515daa7f;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_add(
            0xeedd7dd6, 0x3866d47d, 0x65e1968c, 0x49376fe2, 0xee7cfdec, 0xca5a7840, 0x24c7524b, 0x32cfcf6a,
            0xfe08e330, 0x25fd44ae, 0x349a08b, 0x7a0d8cd2, 0x409f561e, 0x6208096a, 0x976a7748, 0x21846a34,
            0x7cf01352, 0x3fca8944, 0xc3b91a98, 0x47e08381, 0xc5409be0, 0x48662b2e, 0x51b3b0dd, 0x331795e5,
            0x87069ca1, 0xb6fba74, 0xa10b410a, 0x866f98f, 0x5f2a5ed2, 0xa0afcbbe, 0x3f7674bd, 0x515daa7f);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0x472a1fd3, 0x3020c90, 0x8e1aca3, 0xc31a79e1, 0x633ce07, 0x73ea1256, 0xef361199, 0x63f74113;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0x48050037, 0x1b081bdc, 0x8371f934, 0xd3405d6b, 0x3b8c2882, 0xdf0fd90a, 0x730bdfce, 0x83b14db6;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_add(
            0x21231d11, 0xce674831, 0x3c2aaad7, 0x22ab36c6, 0xc777c398, 0x33d1155c, 0x8b9388e4, 0x3514d41e,
            0xe3855df5, 0x53d6fb40, 0xaf79ebe, 0x9384f31d, 0x56839eff, 0xef44d11e, 0x16017eb8, 0x89a83250,
            0x472a1fd3, 0x3020c90, 0x8e1aca3, 0xc31a79e1, 0x633ce07, 0x73ea1256, 0xef361199, 0x63f74113,
            0x48050037, 0x1b081bdc, 0x8371f934, 0xd3405d6b, 0x3b8c2882, 0xdf0fd90a, 0x730bdfce, 0x83b14db6);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0xa38fb89e, 0x72d6446a, 0xe51dd73e, 0x15410f1, 0xb5a38c2f, 0x99eade1f, 0x3f79e108, 0xfb202e0f;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0xcde0a70a, 0x5999a198, 0xb48c34b6, 0x343ea4ea, 0xcdb51927, 0x89aafe43, 0x4580257c, 0x5f924734;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_add(
            0x80633cb1, 0x2567e09e, 0x69d02113, 0x575a224b, 0x12181fcb, 0xc62732, 0x17aacad4, 0x6dde9cf3,
            0x67ce6b34, 0x57dd49aa, 0xcf859ef3, 0x80b27fda, 0xa1ba66a8, 0x5c99ef86, 0xa707e41d, 0x9188fbe7,
            0xa38fb89e, 0x72d6446a, 0xe51dd73e, 0x15410f1, 0xb5a38c2f, 0x99eade1f, 0x3f79e108, 0xfb202e0f,
            0xcde0a70a, 0x5999a198, 0xb48c34b6, 0x343ea4ea, 0xcdb51927, 0x89aafe43, 0x4580257c, 0x5f924734);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0x4e1fdadb, 0x30998607, 0x50dd64ce, 0xfdf652c1, 0xbc4a599, 0x122bc1a, 0xf1769ea5, 0x51a4c0ac;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0x521044ef, 0xadff4507, 0xfb522d06, 0x6f255923, 0x8cba892a, 0xe3517e53, 0x8c01d153, 0x77c8d346;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_add(
            0x44e5467d, 0x4d0bd76a, 0x19bbface, 0x40908ab8, 0xec970e9, 0x2c21f62e, 0xfc69a122, 0x97d064f0,
            0x1e9cb3fa, 0x797300fd, 0x54f17ccd, 0xda5fb3b8, 0xa850861f, 0x3f7c66f, 0xd33402cc, 0x89974f2e,
            0x4e1fdadb, 0x30998607, 0x50dd64ce, 0xfdf652c1, 0xbc4a599, 0x122bc1a, 0xf1769ea5, 0x51a4c0ac,
            0x521044ef, 0xadff4507, 0xfb522d06, 0x6f255923, 0x8cba892a, 0xe3517e53, 0x8c01d153, 0x77c8d346);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0x926341d9, 0xffcca36d, 0x20156334, 0x47b40a76, 0xe290d534, 0x19612e4a, 0xbb0b867, 0xbca7069b;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0xbb0b77b, 0xc0d93775, 0x6a1fc9b3, 0x6204875d, 0x70b64966, 0x982754ac, 0x1566660, 0x5a7df87d;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_add(
            0x13613bec, 0xcca81cb9, 0x101cfe67, 0x8bb5fc9d, 0xc74f972a, 0xedf1b33d, 0xc93937bd, 0x2dcfcab8,
            0x9a039215, 0x3e730924, 0xd33f5f38, 0x3732cfba, 0xd6f6c6f4, 0x65f088b7, 0x9474a412, 0x46dbc4dd,
            0x926341d9, 0xffcca36d, 0x20156334, 0x47b40a76, 0xe290d534, 0x19612e4a, 0xbb0b867, 0xbca7069b,
            0xbb0b77b, 0xc0d93775, 0x6a1fc9b3, 0x6204875d, 0x70b64966, 0x982754ac, 0x1566660, 0x5a7df87d);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0x2bbfca49, 0x712b0408, 0xd867f353, 0x62f95f3d, 0x41a92b20, 0xd1ea0c49, 0x7ac6f3aa, 0xdeb21645;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0xe16061a8, 0xcc4c43b3, 0xf6aa30a4, 0xb75e791b, 0x870bf702, 0x2ff3ef86, 0x5c58abfc, 0xe07e6c7e;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_add(
            0x47fb9e1a, 0x17cd1708, 0xde2a3296, 0x7fe74b74, 0xbbab0e76, 0xf1a02bc9, 0xa48ec5a8, 0x1bec414a,
            0x749c0443, 0x57f6e117, 0xe8c9796e, 0x681385da, 0x30c54b0f, 0x8a79bc57, 0x70126667, 0xe3586704,
            0x2bbfca49, 0x712b0408, 0xd867f353, 0x62f95f3d, 0x41a92b20, 0xd1ea0c49, 0x7ac6f3aa, 0xdeb21645,
            0xe16061a8, 0xcc4c43b3, 0xf6aa30a4, 0xb75e791b, 0x870bf702, 0x2ff3ef86, 0x5c58abfc, 0xe07e6c7e);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0x58a42d36, 0x8d906bf7, 0xb8d381b8, 0xd367e918, 0x4606faae, 0x17469bda, 0xdc7a255b, 0xeb6e3277;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0x993cc451, 0x56a4320a, 0x892949a2, 0x7abe2059, 0xa0c8d1ff, 0xe30a6394, 0x80e9e03d, 0x615fa4f7;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_add(
            0xbb7ceceb, 0xf3f678ff, 0x8897faf0, 0x73a59f93, 0x6f6e6814, 0x36ffb812, 0x4276d450, 0x437a8620,
            0x56c181e1, 0x7363bcc3, 0xdc8f9782, 0x87220fcf, 0x99d297ff, 0x69b8feb6, 0x3eeac32f, 0xb916ba1,
            0x58a42d36, 0x8d906bf7, 0xb8d381b8, 0xd367e918, 0x4606faae, 0x17469bda, 0xdc7a255b, 0xeb6e3277,
            0x993cc451, 0x56a4320a, 0x892949a2, 0x7abe2059, 0xa0c8d1ff, 0xe30a6394, 0x80e9e03d, 0x615fa4f7);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0xc1a7674e, 0xc0497aa4, 0x27b9af61, 0x813645a4, 0xc1c691a3, 0x3be4aee1, 0xa5a4164c, 0x955e83c9;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0xd6ae5ea4, 0x71ae0ae5, 0xcc9834ac, 0xbe2ecf82, 0x5ef04324, 0x753b98ff, 0x93b1d494, 0xc87400d2;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_add(
            0xdcbf00eb, 0x4c9d9d87, 0xc18d0227, 0x41b4e98b, 0xa1a30bc2, 0x49be16f6, 0x96ead4dc, 0xb89070ae,
            0x1b0e664e, 0x1b7f1bcd, 0xb6b96a67, 0xcb0d8b06, 0xc1c4a766, 0x472294e4, 0xc8a2d88f, 0x6f24c8c2,
            0xc1a7674e, 0xc0497aa4, 0x27b9af61, 0x813645a4, 0xc1c691a3, 0x3be4aee1, 0xa5a4164c, 0x955e83c9,
            0xd6ae5ea4, 0x71ae0ae5, 0xcc9834ac, 0xbe2ecf82, 0x5ef04324, 0x753b98ff, 0x93b1d494, 0xc87400d2);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0x47f62b82, 0x47e3fb06, 0xc2414cdc, 0x4d64416a, 0x9b263f25, 0x122b078f, 0xc89072f7, 0xcd9aa0b;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0x14ca295f, 0x30910574, 0x19149bd4, 0x25b0ad0, 0xd8b34884, 0x9a61a4e0, 0xb15c4d94, 0xc510e04c;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_add(
            0xb6fbe7b2, 0xb9d6ff9a, 0x458d65a3, 0x5eadedc1, 0xb2a88460, 0xf336bbb1, 0x9cb441f8, 0x26488766,
            0x21bc2a34, 0x932a78bc, 0x6a0eb603, 0x5638d981, 0xd02ddf18, 0x8f2f2dca, 0xb2014498, 0x9e15dab4,
            0x47f62b82, 0x47e3fb06, 0xc2414cdc, 0x4d64416a, 0x9b263f25, 0x122b078f, 0xc89072f7, 0xcd9aa0b,
            0x14ca295f, 0x30910574, 0x19149bd4, 0x25b0ad0, 0xd8b34884, 0x9a61a4e0, 0xb15c4d94, 0xc510e04c);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0xb0263efb, 0xcc80645c, 0x7316d99, 0x4d5abacd, 0xb542ffec, 0xc03d886c, 0x5bc3b48b, 0xb9ad22c1;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0xc37062b9, 0x474ad42a, 0x3e498d1a, 0x109e16c7, 0x9444c97b, 0x49968c1c, 0xbb4f8cca, 0xeb85392;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_add(
            0x2b038315, 0x9690d306, 0x69310e6f, 0x9cacc433, 0x9794b862, 0x1e4680e3, 0x56771222, 0xaba55687,
            0xae25fc0a, 0xf9a003f9, 0xd8b63338, 0x3fbfb532, 0x25130d6f, 0x63d570f6, 0xaa365edb, 0xa0e75d87,
            0xb0263efb, 0xcc80645c, 0x7316d99, 0x4d5abacd, 0xb542ffec, 0xc03d886c, 0x5bc3b48b, 0xb9ad22c1,
            0xc37062b9, 0x474ad42a, 0x3e498d1a, 0x109e16c7, 0x9444c97b, 0x49968c1c, 0xbb4f8cca, 0xeb85392);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0x16e97240, 0xb56720fb, 0x25349558, 0x15ab0093, 0xf2363793, 0x1f147d1a, 0x76551f7, 0x81e874bb;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0x1ceb8018, 0x3937014e, 0x6c600999, 0xaf1ce7cf, 0xfaeb8246, 0x2032d276, 0x4664bb0, 0xc2791f04;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_add(
            0x95bc15b4, 0x9cb9a134, 0x465a2ee6, 0x9275028e, 0xced7ca8d, 0xed858ee9, 0x51eeadc9, 0x10e90e2e,
            0x58aa258d, 0x34ebe609, 0x2bb6a88, 0x4ca58963, 0x16ad1f75, 0x4d57a8c6, 0x80d5e042, 0xc68a3703,
            0x16e97240, 0xb56720fb, 0x25349558, 0x15ab0093, 0xf2363793, 0x1f147d1a, 0x76551f7, 0x81e874bb,
            0x1ceb8018, 0x3937014e, 0x6c600999, 0xaf1ce7cf, 0xfaeb8246, 0x2032d276, 0x4664bb0, 0xc2791f04);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0xf94b2dfb, 0x8add44b4, 0x33978c7a, 0x5d5d4dd3, 0xd75d0b54, 0x61ca58e9, 0x97c539fd, 0xe0a6cdb7;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0xed63d567, 0x7446491b, 0xe5c5e6d3, 0x8055cb06, 0xd0165eb0, 0xae321a97, 0x2dc8eb3f, 0x8d1484c4;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_add(
            0x7a1c0a80, 0xf62abc8, 0xc65a9c74, 0x4d625158, 0x2ff9c3, 0xb17c9be7, 0xa614cca5, 0xb6b15a68,
            0x41ce0a03, 0xb6cd0110, 0x82e16ee, 0x9c9a12b3, 0xef6536d4, 0xa54e223e, 0xd6cdb61e, 0xfae62e14,
            0xf94b2dfb, 0x8add44b4, 0x33978c7a, 0x5d5d4dd3, 0xd75d0b54, 0x61ca58e9, 0x97c539fd, 0xe0a6cdb7,
            0xed63d567, 0x7446491b, 0xe5c5e6d3, 0x8055cb06, 0xd0165eb0, 0xae321a97, 0x2dc8eb3f, 0x8d1484c4);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0xe6409a8e, 0xa14dcacb, 0x2639e842, 0xece0189b, 0x1414cac6, 0x3979374e, 0xed7d382d, 0x1e3faaf7;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0xae6d0176, 0x8c62c805, 0x97c1e2e6, 0xa6e353ea, 0x70d1f1d5, 0xbd387a7, 0x7aa6fb10, 0x2eff9414;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_add(
            0x92b062d4, 0xa7caa50a, 0x9bb6a141, 0x7a5ce7e5, 0x83ea227a, 0x6fb1712, 0x3256eaca, 0x35963ea4,
            0xbbb25302, 0xa10aa4d1, 0x64de59b1, 0xd04082b9, 0xf9c08a96, 0xbfcce196, 0x4951e5c9, 0xf65be145,
            0xe6409a8e, 0xa14dcacb, 0x2639e842, 0xece0189b, 0x1414cac6, 0x3979374e, 0xed7d382d, 0x1e3faaf7,
            0xae6d0176, 0x8c62c805, 0x97c1e2e6, 0xa6e353ea, 0x70d1f1d5, 0xbd387a7, 0x7aa6fb10, 0x2eff9414);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0x2548781a, 0xe605a68, 0x1ceff047, 0xf0d2b94d, 0x45e90176, 0x3b3b64d5, 0x15169a11, 0x37e15dc4;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0x18c6306a, 0xea220b3a, 0x17dc3bc2, 0x1efec53b, 0xa03a580, 0xc329cc5b, 0x1d3d12f0, 0x4e6dfbf5;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_add(
            0x1d33fd27, 0xfa0bf5c5, 0xb646cc62, 0x445f573d, 0xda82361b, 0xd022388e, 0x2263e84c, 0x9ed73f09,
            0x2716c458, 0x5972b2de, 0xb2e44934, 0x94a823e5, 0x42467254, 0xee75b4f3, 0xebb1eeea, 0xb6318967,
            0x2548781a, 0xe605a68, 0x1ceff047, 0xf0d2b94d, 0x45e90176, 0x3b3b64d5, 0x15169a11, 0x37e15dc4,
            0x18c6306a, 0xea220b3a, 0x17dc3bc2, 0x1efec53b, 0xa03a580, 0xc329cc5b, 0x1d3d12f0, 0x4e6dfbf5);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0xdae7b802, 0xecfda59, 0x8e518ce7, 0xe5ab4c5, 0x7cd74c07, 0x2c98b4ea, 0x216c600f, 0x328a2f7a;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0xd9b7d882, 0x86610a8e, 0x29924aee, 0x9754734c, 0x6e285105, 0x607959f5, 0x34212010, 0xb9d97615;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_add(
            0xef028d83, 0x579623ae, 0xba743961, 0x6195926d, 0x15de69db, 0x6a5abe5a, 0xe3c785ec, 0xa7ebf7c4,
            0x99d0bed1, 0x9640392b, 0x4b053919, 0x47a38927, 0x7044804b, 0xcfd9c737, 0xbfe362d5, 0x6205152f,
            0xdae7b802, 0xecfda59, 0x8e518ce7, 0xe5ab4c5, 0x7cd74c07, 0x2c98b4ea, 0x216c600f, 0x328a2f7a,
            0xd9b7d882, 0x86610a8e, 0x29924aee, 0x9754734c, 0x6e285105, 0x607959f5, 0x34212010, 0xb9d97615);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0x7498cf74, 0xbb2d0fe6, 0x2d4edb8, 0x35fa8af, 0xfe19c40f, 0x1eef3c75, 0xac797f07, 0xad59a910;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0xffaadfdd, 0x84d70ced, 0xa3defb74, 0xdeee57b6, 0x5624675d, 0xa5225083, 0x9035d182, 0xd758026d;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_add(
            0x7bb61ee5, 0xf2884413, 0xfb1f0c13, 0xda4f04e2, 0x8974ae6e, 0x662638cd, 0xcc8721b8, 0xd4933230,
            0xe5d694a8, 0x662da4d0, 0x5a438ddc, 0x1ad12c8c, 0x1ecafb5e, 0xedcc5e9d, 0xf51a9d23, 0x21c09ab,
            0x7498cf74, 0xbb2d0fe6, 0x2d4edb8, 0x35fa8af, 0xfe19c40f, 0x1eef3c75, 0xac797f07, 0xad59a910,
            0xffaadfdd, 0x84d70ced, 0xa3defb74, 0xdeee57b6, 0x5624675d, 0xa5225083, 0x9035d182, 0xd758026d);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0x39cf7518, 0x5eee87ed, 0x7312f938, 0x519c909, 0x17773d04, 0x3f3fe6bc, 0x94420f9a, 0x99966667;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0xbeb56284, 0x7cb2d8ef, 0xfc349071, 0xa97b338a, 0xdbaa9dc0, 0x23089a10, 0x76ecf29, 0x6986934a;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_add(
            0xec04554a, 0x530ddcbc, 0x4688cffe, 0xaadcffbb, 0x7a10a2ec, 0x474652c2, 0x9873d1a0, 0x896f37c8,
            0x929138df, 0xd68f9fe1, 0xacc417dc, 0xe6085b61, 0x4e811bf1, 0xda622bb0, 0x224ac4ac, 0x380423e7,
            0x39cf7518, 0x5eee87ed, 0x7312f938, 0x519c909, 0x17773d04, 0x3f3fe6bc, 0x94420f9a, 0x99966667,
            0xbeb56284, 0x7cb2d8ef, 0xfc349071, 0xa97b338a, 0xdbaa9dc0, 0x23089a10, 0x76ecf29, 0x6986934a);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0xca77ca75, 0x6ffe52cb, 0x8f7ef3dd, 0x8b554eb8, 0x3b86af90, 0x387c6679, 0xc837de69, 0xc7ce6fa9;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0xd8ce2d85, 0x216c037b, 0x8f97bcee, 0xfb1f33db, 0xb4898bdf, 0x7eb02451, 0xc41130a7, 0x1d6668e2;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_add(
            0xb43fca26, 0x84077d1a, 0xa3bc2367, 0x7dfb841d, 0xbf3578a2, 0xca6c209d, 0x774b6d6c, 0x11b3b97f,
            0x5b679d58, 0xd3b27eaf, 0x4b9f9d42, 0x3bae231c, 0x2f36d3bb, 0x8cd5650c, 0xae600c50, 0x65331f9f,
            0xca77ca75, 0x6ffe52cb, 0x8f7ef3dd, 0x8b554eb8, 0x3b86af90, 0x387c6679, 0xc837de69, 0xc7ce6fa9,
            0xd8ce2d85, 0x216c037b, 0x8f97bcee, 0xfb1f33db, 0xb4898bdf, 0x7eb02451, 0xc41130a7, 0x1d6668e2);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0xbf92c52f, 0x49bbb218, 0x58dc039d, 0x4fc6d734, 0x8ad8179f, 0xdd9068ac, 0xdc747673, 0x213995c9;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0x84240dd, 0xed98f6fa, 0x3ecbf6a4, 0x907e4ae8, 0x7d55c35a, 0x2d8acdec, 0xddf22625, 0x458f0e6b;

        t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7 <== ec_add(
            0x48dfd587, 0x79361bb, 0xc9b02656, 0x5ec4ba38, 0x2cf5a12d, 0x34867aaa, 0xacf4508b, 0x5084b41b,
            0x91470e89, 0x6e79e97f, 0x6891f560, 0x5db6f560, 0x55292747, 0x619aa6c8, 0x1d980d31, 0x34a9631a,
            0xbf92c52f, 0x49bbb218, 0x58dc039d, 0x4fc6d734, 0x8ad8179f, 0xdd9068ac, 0xdc747673, 0x213995c9,
            0x84240dd, 0xed98f6fa, 0x3ecbf6a4, 0x907e4ae8, 0x7d55c35a, 0x2d8acdec, 0xddf22625, 0x458f0e6b);
        assert_eq t_0_0, t_0_1, t_0_2, t_0_3, t_0_4, t_0_5, t_0_6, t_0_7, 0xed91aaf, 0x96eb71b9, 0xe5e3ee7d, 0x7d30103b, 0xa207dafc, 0x501b2c67, 0x237542f4, 0x1159abd4;
        assert_eq t_1_0, t_1_1, t_1_2, t_1_3, t_1_4, t_1_5, t_1_6, t_1_7, 0x6a255906, 0xb1976404, 0x3c3747d4, 0xebd4f608, 0x6bd3bbf, 0x40d90c4b, 0x23adb6fd, 0xb5196132;

        // Left out these test cases, because the format is different...
        // https://github.com/0xPolygonHermez/zkevm-proverjs/blob/a4006af3d7fe4a57a85500c01dc791fb5013cef0/test/sm/sm_arith.js#L1196-L1211

        return;
    }
}
