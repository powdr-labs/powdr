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


        
    }
}