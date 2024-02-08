use std::arith::Arith;
use std::binary::Binary;

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
    
    // The x and y coordiantes of the accumulator
    reg acc_x_0;
    reg acc_x_1;
    reg acc_x_2;
    reg acc_x_3;
    reg acc_x_4;
    reg acc_x_5;
    reg acc_x_6;
    reg acc_x_7;
    reg acc_y_0;
    reg acc_y_1;
    reg acc_y_2;
    reg acc_y_3;
    reg acc_y_4;
    reg acc_y_5;
    reg acc_y_6;
    reg acc_y_7;
    
    // The x and y coordiantes of the input point
    reg p_x_0;
    reg p_x_1;
    reg p_x_2;
    reg p_x_3;
    reg p_x_4;
    reg p_x_5;
    reg p_x_6;
    reg p_x_7;
    reg p_y_0;
    reg p_y_1;
    reg p_y_2;
    reg p_y_3;
    reg p_y_4;
    reg p_y_5;
    reg p_y_6;
    reg p_y_7;

    // The scalar to multiply by
    reg s_0;
    reg s_1;
    reg s_2;
    reg s_3;
    reg s_4;
    reg s_5;
    reg s_6;
    reg s_7;

    // Only tmp_0 is used, the others are only needed because we can't ignore results of instructions...
    reg tmp_0;
    reg tmp_1;
    reg tmp_2;
    reg tmp_3;
    reg tmp_4;
    reg tmp_5;
    reg tmp_6;
    reg tmp_7;

    Arith arith;
    instr affine_256 A0, A1, A2, A3, A4, A5, A6, A7, B0, B1, B2, B3, B4, B5, B6, B7, C0, C1, C2, C3, C4, C5, C6, C7 -> D0, D1, D2, D3, D4, D5, D6, D7, E0, E1, E2, E3, E4, E5, E6, E7 = arith.affine_256;
    instr ec_add A0, A1, A2, A3, A4, A5, A6, A7, B0, B1, B2, B3, B4, B5, B6, B7, C0, C1, C2, C3, C4, C5, C6, C7, D0, D1, D2, D3, D4, D5, D6, D7 -> E0, E1, E2, E3, E4, E5, E6, E7, F0, F1, F2, F3, F4, F5, F6, F7 = arith.ec_add;
    instr ec_double A0, A1, A2, A3, A4, A5, A6, A7, B0, B1, B2, B3, B4, B5, B6, B7 -> E0, E1, E2, E3, E4, E5, E6, E7, F0, F1, F2, F3, F4, F5, F6, F7 = arith.ec_double;

    Binary binary;
    instr and A0, B0 -> C0 = binary.and;

    // ============== iszero check for A0 =======================
    col witness A0Inv;
    col witness A0IsZero;
    A0IsZero = 1 - A0 * A0Inv;
    A0IsZero * A0 = 0;
    std::utils::force_bool(A0IsZero);

    instr branch_if_zero A0, l: label { pc' = A0IsZero * l + (1 - A0IsZero) * (pc + 1) }

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

        // Initialize P:
        // x1: 55066263022277343669578718895168534326250603453777594175500187360389116729240
        //     = 0x79be667e f9dcbbac 55a06295 ce870b07 029bfcdb 2dce28d9 59f2815b 16f81798
        // y1: 32670510020758816978083085130507043184471273380659243275938904335757337482424
        //     = 0x483ada77 26a3c465 5da4fbfc 0e1108a8 fd17b448 a6855419 9c47d08f fb10d4b8

        p_x_0 <=A0= 0x16f81798;
        p_x_1 <=A0= 0x59f2815b;
        p_x_2 <=A0= 0x2dce28d9;
        p_x_3 <=A0= 0x029bfcdb;
        p_x_4 <=A0= 0xce870b07;
        p_x_5 <=A0= 0x55a06295;
        p_x_6 <=A0= 0xf9dcbbac;
        p_x_7 <=A0= 0x79be667e;

        p_y_0 <=A0= 0xfb10d4b8;
        p_y_1 <=A0= 0x9c47d08f;
        p_y_2 <=A0= 0xa6855419;
        p_y_3 <=A0= 0xfd17b448;
        p_y_4 <=A0= 0x0e1108a8;
        p_y_5 <=A0= 0x5da4fbfc;
        p_y_6 <=A0= 0x26a3c465;
        p_y_7 <=A0= 0x483ada77;

        // Initialize s (just use p_x for simplicity)
        s_0 <=A0= p_x_0;
        s_1 <=A0= p_x_1;
        s_2 <=A0= p_x_2;
        s_3 <=A0= p_x_3;
        s_4 <=A0= p_x_4;
        s_5 <=A0= p_x_5;
        s_6 <=A0= p_x_6;
        s_7 <=A0= p_x_7;

        // Initialize the accumulator
        // TODO: Explain this madness
        // x: 89565891926547004231252920425935692360644145829622209833684329913297188986597
        //    = 0xc6047f94 41ed7d6d 3045406e 95c07cd8 5c778e4b 8cef3ca7 abac09b9 5c709ee5
        // y: 12158399299693830322967808612713398636155367887041628176798871954788371653930
        //    = 0x1ae168fe a63dc339 a3c58419 466ceaee f7f63265 3266d0e1 236431a9 50cfe52a
        acc_x_0 <=A0= 0x5c709ee5;
        acc_x_1 <=A0= 0xabac09b9;
        acc_x_2 <=A0= 0x8cef3ca7;
        acc_x_3 <=A0= 0x5c778e4b;
        acc_x_4 <=A0= 0x95c07cd8;
        acc_x_5 <=A0= 0x3045406e;
        acc_x_6 <=A0= 0x41ed7d6d;
        acc_x_7 <=A0= 0xc6047f94;

        acc_y_0 <=A0= 0x50cfe52a;
        acc_y_1 <=A0= 0x236431a9;
        acc_y_2 <=A0= 0x3266d0e1;
        acc_y_3 <=A0= 0xf7f63265;
        acc_y_4 <=A0= 0x466ceaee;
        acc_y_5 <=A0= 0xa3c58419;
        acc_y_6 <=A0= 0xa63dc339;
        acc_y_7 <=A0= 0x1ae168fe;

        // Unrolled loop:
        // For each bit of s (from least to most significant):
        // - If bit is 1, add P to acc
        // - Double P

        tmp_0 <== and(s_0, 1);
        branch_if_zero tmp_0, double_bit_0;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_0:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );

        tmp_0 <== and(s_0, 2);
        branch_if_zero tmp_0, double_bit_1;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_1:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );

        tmp_0 <== and(s_0, 4);
        branch_if_zero tmp_0, double_bit_2;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_2:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_0, 8);
        branch_if_zero tmp_0, double_bit_3;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_3:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_0, 16);
        branch_if_zero tmp_0, double_bit_4;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_4:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_0, 32);
        branch_if_zero tmp_0, double_bit_5;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_5:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_0, 64);
        branch_if_zero tmp_0, double_bit_6;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_6:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_0, 128);
        branch_if_zero tmp_0, double_bit_7;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_7:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_0, 256);
        branch_if_zero tmp_0, double_bit_8;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_8:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_0, 512);
        branch_if_zero tmp_0, double_bit_9;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_9:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_0, 1024);
        branch_if_zero tmp_0, double_bit_10;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_10:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_0, 2048);
        branch_if_zero tmp_0, double_bit_11;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_11:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_0, 4096);
        branch_if_zero tmp_0, double_bit_12;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_12:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_0, 8192);
        branch_if_zero tmp_0, double_bit_13;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_13:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_0, 16384);
        branch_if_zero tmp_0, double_bit_14;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_14:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_0, 32768);
        branch_if_zero tmp_0, double_bit_15;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_15:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_0, 65536);
        branch_if_zero tmp_0, double_bit_16;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_16:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_0, 131072);
        branch_if_zero tmp_0, double_bit_17;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_17:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_0, 262144);
        branch_if_zero tmp_0, double_bit_18;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_18:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_0, 524288);
        branch_if_zero tmp_0, double_bit_19;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_19:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_0, 1048576);
        branch_if_zero tmp_0, double_bit_20;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_20:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_0, 2097152);
        branch_if_zero tmp_0, double_bit_21;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_21:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_0, 4194304);
        branch_if_zero tmp_0, double_bit_22;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_22:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_0, 8388608);
        branch_if_zero tmp_0, double_bit_23;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_23:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_0, 16777216);
        branch_if_zero tmp_0, double_bit_24;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_24:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_0, 33554432);
        branch_if_zero tmp_0, double_bit_25;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_25:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_0, 67108864);
        branch_if_zero tmp_0, double_bit_26;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_26:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_0, 134217728);
        branch_if_zero tmp_0, double_bit_27;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_27:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_0, 268435456);
        branch_if_zero tmp_0, double_bit_28;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_28:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_0, 536870912);
        branch_if_zero tmp_0, double_bit_29;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_29:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_0, 1073741824);
        branch_if_zero tmp_0, double_bit_30;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_30:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_0, 2147483648);
        branch_if_zero tmp_0, double_bit_31;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_31:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_1, 1);
        branch_if_zero tmp_0, double_bit_32;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_32:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_1, 2);
        branch_if_zero tmp_0, double_bit_33;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_33:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_1, 4);
        branch_if_zero tmp_0, double_bit_34;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_34:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_1, 8);
        branch_if_zero tmp_0, double_bit_35;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_35:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_1, 16);
        branch_if_zero tmp_0, double_bit_36;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_36:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_1, 32);
        branch_if_zero tmp_0, double_bit_37;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_37:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_1, 64);
        branch_if_zero tmp_0, double_bit_38;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_38:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_1, 128);
        branch_if_zero tmp_0, double_bit_39;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_39:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_1, 256);
        branch_if_zero tmp_0, double_bit_40;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_40:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_1, 512);
        branch_if_zero tmp_0, double_bit_41;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_41:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_1, 1024);
        branch_if_zero tmp_0, double_bit_42;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_42:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_1, 2048);
        branch_if_zero tmp_0, double_bit_43;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_43:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_1, 4096);
        branch_if_zero tmp_0, double_bit_44;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_44:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_1, 8192);
        branch_if_zero tmp_0, double_bit_45;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_45:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_1, 16384);
        branch_if_zero tmp_0, double_bit_46;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_46:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_1, 32768);
        branch_if_zero tmp_0, double_bit_47;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_47:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_1, 65536);
        branch_if_zero tmp_0, double_bit_48;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_48:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_1, 131072);
        branch_if_zero tmp_0, double_bit_49;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_49:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_1, 262144);
        branch_if_zero tmp_0, double_bit_50;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_50:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_1, 524288);
        branch_if_zero tmp_0, double_bit_51;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_51:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_1, 1048576);
        branch_if_zero tmp_0, double_bit_52;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_52:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_1, 2097152);
        branch_if_zero tmp_0, double_bit_53;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_53:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_1, 4194304);
        branch_if_zero tmp_0, double_bit_54;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_54:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_1, 8388608);
        branch_if_zero tmp_0, double_bit_55;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_55:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_1, 16777216);
        branch_if_zero tmp_0, double_bit_56;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_56:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_1, 33554432);
        branch_if_zero tmp_0, double_bit_57;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_57:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_1, 67108864);
        branch_if_zero tmp_0, double_bit_58;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_58:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_1, 134217728);
        branch_if_zero tmp_0, double_bit_59;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_59:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_1, 268435456);
        branch_if_zero tmp_0, double_bit_60;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_60:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_1, 536870912);
        branch_if_zero tmp_0, double_bit_61;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_61:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_1, 1073741824);
        branch_if_zero tmp_0, double_bit_62;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_62:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_1, 2147483648);
        branch_if_zero tmp_0, double_bit_63;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_63:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_2, 1);
        branch_if_zero tmp_0, double_bit_64;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_64:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_2, 2);
        branch_if_zero tmp_0, double_bit_65;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_65:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_2, 4);
        branch_if_zero tmp_0, double_bit_66;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_66:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_2, 8);
        branch_if_zero tmp_0, double_bit_67;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_67:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_2, 16);
        branch_if_zero tmp_0, double_bit_68;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_68:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_2, 32);
        branch_if_zero tmp_0, double_bit_69;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_69:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_2, 64);
        branch_if_zero tmp_0, double_bit_70;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_70:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_2, 128);
        branch_if_zero tmp_0, double_bit_71;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_71:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_2, 256);
        branch_if_zero tmp_0, double_bit_72;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_72:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_2, 512);
        branch_if_zero tmp_0, double_bit_73;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_73:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_2, 1024);
        branch_if_zero tmp_0, double_bit_74;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_74:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_2, 2048);
        branch_if_zero tmp_0, double_bit_75;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_75:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_2, 4096);
        branch_if_zero tmp_0, double_bit_76;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_76:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_2, 8192);
        branch_if_zero tmp_0, double_bit_77;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_77:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_2, 16384);
        branch_if_zero tmp_0, double_bit_78;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_78:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_2, 32768);
        branch_if_zero tmp_0, double_bit_79;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_79:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_2, 65536);
        branch_if_zero tmp_0, double_bit_80;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_80:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_2, 131072);
        branch_if_zero tmp_0, double_bit_81;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_81:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_2, 262144);
        branch_if_zero tmp_0, double_bit_82;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_82:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_2, 524288);
        branch_if_zero tmp_0, double_bit_83;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_83:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_2, 1048576);
        branch_if_zero tmp_0, double_bit_84;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_84:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_2, 2097152);
        branch_if_zero tmp_0, double_bit_85;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_85:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_2, 4194304);
        branch_if_zero tmp_0, double_bit_86;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_86:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_2, 8388608);
        branch_if_zero tmp_0, double_bit_87;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_87:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_2, 16777216);
        branch_if_zero tmp_0, double_bit_88;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_88:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_2, 33554432);
        branch_if_zero tmp_0, double_bit_89;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_89:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_2, 67108864);
        branch_if_zero tmp_0, double_bit_90;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_90:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_2, 134217728);
        branch_if_zero tmp_0, double_bit_91;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_91:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_2, 268435456);
        branch_if_zero tmp_0, double_bit_92;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_92:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_2, 536870912);
        branch_if_zero tmp_0, double_bit_93;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_93:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_2, 1073741824);
        branch_if_zero tmp_0, double_bit_94;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_94:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_2, 2147483648);
        branch_if_zero tmp_0, double_bit_95;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_95:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_3, 1);
        branch_if_zero tmp_0, double_bit_96;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_96:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_3, 2);
        branch_if_zero tmp_0, double_bit_97;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_97:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_3, 4);
        branch_if_zero tmp_0, double_bit_98;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_98:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_3, 8);
        branch_if_zero tmp_0, double_bit_99;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_99:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_3, 16);
        branch_if_zero tmp_0, double_bit_100;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_100:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_3, 32);
        branch_if_zero tmp_0, double_bit_101;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_101:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_3, 64);
        branch_if_zero tmp_0, double_bit_102;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_102:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_3, 128);
        branch_if_zero tmp_0, double_bit_103;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_103:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_3, 256);
        branch_if_zero tmp_0, double_bit_104;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_104:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_3, 512);
        branch_if_zero tmp_0, double_bit_105;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_105:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_3, 1024);
        branch_if_zero tmp_0, double_bit_106;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_106:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_3, 2048);
        branch_if_zero tmp_0, double_bit_107;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_107:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_3, 4096);
        branch_if_zero tmp_0, double_bit_108;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_108:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_3, 8192);
        branch_if_zero tmp_0, double_bit_109;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_109:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_3, 16384);
        branch_if_zero tmp_0, double_bit_110;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_110:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_3, 32768);
        branch_if_zero tmp_0, double_bit_111;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_111:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_3, 65536);
        branch_if_zero tmp_0, double_bit_112;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_112:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_3, 131072);
        branch_if_zero tmp_0, double_bit_113;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_113:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_3, 262144);
        branch_if_zero tmp_0, double_bit_114;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_114:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_3, 524288);
        branch_if_zero tmp_0, double_bit_115;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_115:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_3, 1048576);
        branch_if_zero tmp_0, double_bit_116;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_116:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_3, 2097152);
        branch_if_zero tmp_0, double_bit_117;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_117:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_3, 4194304);
        branch_if_zero tmp_0, double_bit_118;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_118:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_3, 8388608);
        branch_if_zero tmp_0, double_bit_119;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_119:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_3, 16777216);
        branch_if_zero tmp_0, double_bit_120;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_120:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_3, 33554432);
        branch_if_zero tmp_0, double_bit_121;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_121:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_3, 67108864);
        branch_if_zero tmp_0, double_bit_122;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_122:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_3, 134217728);
        branch_if_zero tmp_0, double_bit_123;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_123:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_3, 268435456);
        branch_if_zero tmp_0, double_bit_124;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_124:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_3, 536870912);
        branch_if_zero tmp_0, double_bit_125;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_125:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_3, 1073741824);
        branch_if_zero tmp_0, double_bit_126;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_126:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_3, 2147483648);
        branch_if_zero tmp_0, double_bit_127;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_127:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_4, 1);
        branch_if_zero tmp_0, double_bit_128;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_128:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_4, 2);
        branch_if_zero tmp_0, double_bit_129;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_129:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_4, 4);
        branch_if_zero tmp_0, double_bit_130;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_130:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_4, 8);
        branch_if_zero tmp_0, double_bit_131;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_131:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_4, 16);
        branch_if_zero tmp_0, double_bit_132;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_132:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_4, 32);
        branch_if_zero tmp_0, double_bit_133;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_133:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_4, 64);
        branch_if_zero tmp_0, double_bit_134;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_134:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_4, 128);
        branch_if_zero tmp_0, double_bit_135;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_135:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_4, 256);
        branch_if_zero tmp_0, double_bit_136;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_136:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_4, 512);
        branch_if_zero tmp_0, double_bit_137;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_137:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_4, 1024);
        branch_if_zero tmp_0, double_bit_138;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_138:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_4, 2048);
        branch_if_zero tmp_0, double_bit_139;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_139:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_4, 4096);
        branch_if_zero tmp_0, double_bit_140;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_140:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_4, 8192);
        branch_if_zero tmp_0, double_bit_141;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_141:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_4, 16384);
        branch_if_zero tmp_0, double_bit_142;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_142:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_4, 32768);
        branch_if_zero tmp_0, double_bit_143;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_143:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_4, 65536);
        branch_if_zero tmp_0, double_bit_144;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_144:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_4, 131072);
        branch_if_zero tmp_0, double_bit_145;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_145:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_4, 262144);
        branch_if_zero tmp_0, double_bit_146;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_146:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_4, 524288);
        branch_if_zero tmp_0, double_bit_147;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_147:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_4, 1048576);
        branch_if_zero tmp_0, double_bit_148;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_148:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_4, 2097152);
        branch_if_zero tmp_0, double_bit_149;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_149:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_4, 4194304);
        branch_if_zero tmp_0, double_bit_150;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_150:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_4, 8388608);
        branch_if_zero tmp_0, double_bit_151;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_151:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_4, 16777216);
        branch_if_zero tmp_0, double_bit_152;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_152:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_4, 33554432);
        branch_if_zero tmp_0, double_bit_153;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_153:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_4, 67108864);
        branch_if_zero tmp_0, double_bit_154;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_154:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_4, 134217728);
        branch_if_zero tmp_0, double_bit_155;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_155:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_4, 268435456);
        branch_if_zero tmp_0, double_bit_156;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_156:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_4, 536870912);
        branch_if_zero tmp_0, double_bit_157;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_157:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_4, 1073741824);
        branch_if_zero tmp_0, double_bit_158;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_158:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_4, 2147483648);
        branch_if_zero tmp_0, double_bit_159;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_159:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_5, 1);
        branch_if_zero tmp_0, double_bit_160;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_160:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_5, 2);
        branch_if_zero tmp_0, double_bit_161;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_161:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_5, 4);
        branch_if_zero tmp_0, double_bit_162;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_162:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_5, 8);
        branch_if_zero tmp_0, double_bit_163;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_163:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_5, 16);
        branch_if_zero tmp_0, double_bit_164;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_164:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_5, 32);
        branch_if_zero tmp_0, double_bit_165;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_165:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_5, 64);
        branch_if_zero tmp_0, double_bit_166;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_166:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_5, 128);
        branch_if_zero tmp_0, double_bit_167;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_167:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_5, 256);
        branch_if_zero tmp_0, double_bit_168;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_168:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_5, 512);
        branch_if_zero tmp_0, double_bit_169;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_169:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_5, 1024);
        branch_if_zero tmp_0, double_bit_170;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_170:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_5, 2048);
        branch_if_zero tmp_0, double_bit_171;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_171:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_5, 4096);
        branch_if_zero tmp_0, double_bit_172;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_172:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_5, 8192);
        branch_if_zero tmp_0, double_bit_173;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_173:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_5, 16384);
        branch_if_zero tmp_0, double_bit_174;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_174:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_5, 32768);
        branch_if_zero tmp_0, double_bit_175;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_175:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_5, 65536);
        branch_if_zero tmp_0, double_bit_176;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_176:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_5, 131072);
        branch_if_zero tmp_0, double_bit_177;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_177:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_5, 262144);
        branch_if_zero tmp_0, double_bit_178;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_178:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_5, 524288);
        branch_if_zero tmp_0, double_bit_179;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_179:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_5, 1048576);
        branch_if_zero tmp_0, double_bit_180;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_180:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_5, 2097152);
        branch_if_zero tmp_0, double_bit_181;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_181:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_5, 4194304);
        branch_if_zero tmp_0, double_bit_182;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_182:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_5, 8388608);
        branch_if_zero tmp_0, double_bit_183;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_183:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_5, 16777216);
        branch_if_zero tmp_0, double_bit_184;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_184:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_5, 33554432);
        branch_if_zero tmp_0, double_bit_185;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_185:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_5, 67108864);
        branch_if_zero tmp_0, double_bit_186;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_186:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_5, 134217728);
        branch_if_zero tmp_0, double_bit_187;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_187:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_5, 268435456);
        branch_if_zero tmp_0, double_bit_188;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_188:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_5, 536870912);
        branch_if_zero tmp_0, double_bit_189;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_189:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_5, 1073741824);
        branch_if_zero tmp_0, double_bit_190;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_190:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_5, 2147483648);
        branch_if_zero tmp_0, double_bit_191;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_191:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_6, 1);
        branch_if_zero tmp_0, double_bit_192;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_192:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_6, 2);
        branch_if_zero tmp_0, double_bit_193;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_193:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_6, 4);
        branch_if_zero tmp_0, double_bit_194;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_194:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_6, 8);
        branch_if_zero tmp_0, double_bit_195;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_195:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_6, 16);
        branch_if_zero tmp_0, double_bit_196;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_196:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_6, 32);
        branch_if_zero tmp_0, double_bit_197;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_197:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_6, 64);
        branch_if_zero tmp_0, double_bit_198;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_198:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_6, 128);
        branch_if_zero tmp_0, double_bit_199;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_199:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_6, 256);
        branch_if_zero tmp_0, double_bit_200;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_200:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_6, 512);
        branch_if_zero tmp_0, double_bit_201;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_201:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_6, 1024);
        branch_if_zero tmp_0, double_bit_202;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_202:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_6, 2048);
        branch_if_zero tmp_0, double_bit_203;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_203:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_6, 4096);
        branch_if_zero tmp_0, double_bit_204;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_204:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_6, 8192);
        branch_if_zero tmp_0, double_bit_205;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_205:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_6, 16384);
        branch_if_zero tmp_0, double_bit_206;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_206:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_6, 32768);
        branch_if_zero tmp_0, double_bit_207;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_207:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_6, 65536);
        branch_if_zero tmp_0, double_bit_208;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_208:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_6, 131072);
        branch_if_zero tmp_0, double_bit_209;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_209:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_6, 262144);
        branch_if_zero tmp_0, double_bit_210;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_210:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_6, 524288);
        branch_if_zero tmp_0, double_bit_211;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_211:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_6, 1048576);
        branch_if_zero tmp_0, double_bit_212;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_212:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_6, 2097152);
        branch_if_zero tmp_0, double_bit_213;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_213:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_6, 4194304);
        branch_if_zero tmp_0, double_bit_214;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_214:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_6, 8388608);
        branch_if_zero tmp_0, double_bit_215;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_215:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_6, 16777216);
        branch_if_zero tmp_0, double_bit_216;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_216:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_6, 33554432);
        branch_if_zero tmp_0, double_bit_217;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_217:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_6, 67108864);
        branch_if_zero tmp_0, double_bit_218;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_218:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_6, 134217728);
        branch_if_zero tmp_0, double_bit_219;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_219:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_6, 268435456);
        branch_if_zero tmp_0, double_bit_220;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_220:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_6, 536870912);
        branch_if_zero tmp_0, double_bit_221;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_221:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_6, 1073741824);
        branch_if_zero tmp_0, double_bit_222;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_222:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_6, 2147483648);
        branch_if_zero tmp_0, double_bit_223;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_223:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_7, 1);
        branch_if_zero tmp_0, double_bit_224;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_224:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_7, 2);
        branch_if_zero tmp_0, double_bit_225;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_225:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_7, 4);
        branch_if_zero tmp_0, double_bit_226;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_226:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_7, 8);
        branch_if_zero tmp_0, double_bit_227;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_227:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_7, 16);
        branch_if_zero tmp_0, double_bit_228;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_228:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_7, 32);
        branch_if_zero tmp_0, double_bit_229;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_229:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_7, 64);
        branch_if_zero tmp_0, double_bit_230;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_230:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_7, 128);
        branch_if_zero tmp_0, double_bit_231;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_231:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_7, 256);
        branch_if_zero tmp_0, double_bit_232;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_232:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_7, 512);
        branch_if_zero tmp_0, double_bit_233;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_233:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_7, 1024);
        branch_if_zero tmp_0, double_bit_234;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_234:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_7, 2048);
        branch_if_zero tmp_0, double_bit_235;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_235:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_7, 4096);
        branch_if_zero tmp_0, double_bit_236;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_236:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_7, 8192);
        branch_if_zero tmp_0, double_bit_237;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_237:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_7, 16384);
        branch_if_zero tmp_0, double_bit_238;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_238:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_7, 32768);
        branch_if_zero tmp_0, double_bit_239;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_239:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_7, 65536);
        branch_if_zero tmp_0, double_bit_240;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_240:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_7, 131072);
        branch_if_zero tmp_0, double_bit_241;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_241:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_7, 262144);
        branch_if_zero tmp_0, double_bit_242;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_242:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_7, 524288);
        branch_if_zero tmp_0, double_bit_243;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_243:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_7, 1048576);
        branch_if_zero tmp_0, double_bit_244;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_244:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_7, 2097152);
        branch_if_zero tmp_0, double_bit_245;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_245:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_7, 4194304);
        branch_if_zero tmp_0, double_bit_246;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_246:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_7, 8388608);
        branch_if_zero tmp_0, double_bit_247;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_247:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_7, 16777216);
        branch_if_zero tmp_0, double_bit_248;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_248:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_7, 33554432);
        branch_if_zero tmp_0, double_bit_249;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_249:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_7, 67108864);
        branch_if_zero tmp_0, double_bit_250;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_250:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_7, 134217728);
        branch_if_zero tmp_0, double_bit_251;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_251:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_7, 268435456);
        branch_if_zero tmp_0, double_bit_252;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_252:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_7, 536870912);
        branch_if_zero tmp_0, double_bit_253;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_253:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_7, 1073741824);
        branch_if_zero tmp_0, double_bit_254;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_254:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        tmp_0 <== and(s_7, 2147483648);
        branch_if_zero tmp_0, double_bit_255;
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );
        double_bit_255:
        p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7 <== ec_double(
            p_x_0, p_x_1, p_x_2, p_x_3, p_x_4, p_x_5, p_x_6, p_x_7, p_y_0, p_y_1, p_y_2, p_y_3, p_y_4, p_y_5, p_y_6, p_y_7
        );

        // Subtract initial accumulator
        // x: 89565891926547004231252920425935692360644145829622209833684329913297188986597
        //    = 0xc6047f94 41ed7d6d 3045406e 95c07cd8 5c778e4b 8cef3ca7 abac09b9 5c709ee5
        // y: p - 12158399299693830322967808612713398636155367887041628176798871954788371653930
        //    = 0xe51e9701 59c23cc6 5c3a7be6 b9931511 0809cd9a cd992f1e dc9bce55 af301705
        acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7 <== ec_add(
            acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7,
            0x5c709ee5, 0xabac09b9, 0x8cef3ca7, 0x5c778e4b, 0x95c07cd8, 0x3045406e, 0x41ed7d6d, 0xc6047f94,
            0xaf301705, 0xdc9bce55, 0xcd992f1e, 0x0809cd9a, 0xb9931511, 0x5c3a7be6, 0x59c23cc6, 0xe51e9701);

        // Assert correct result:
        // x: 41368939038460017089690463593392860417892426308765457203329747030588589193225
        //  = 0x5b75fd5f 49e78191 a45e1c94 38644fe5 d065ea98 920c63e9 eef86e15 1e99b809
        // y: 35702972027818625020095973668955176075740885849864829235584237564223564379706
        //  = 0x4eef2a82 6f1e6d13 a4dde4e5 4800e8d2 82a2089a 87307200 2e0a3a21 eae5763a
        assert_eq acc_x_0, acc_x_1, acc_x_2, acc_x_3, acc_x_4, acc_x_5, acc_x_6, acc_x_7, 0x1e99b809, 0xeef86e15, 0x920c63e9, 0xd065ea98, 0x38644fe5, 0xa45e1c94, 0x49e78191, 0x5b75fd5f;
        assert_eq acc_y_0, acc_y_1, acc_y_2, acc_y_3, acc_y_4, acc_y_5, acc_y_6, acc_y_7, 0xeae5763a, 0x2e0a3a21, 0x87307200, 0x82a2089a, 0x4800e8d2, 0xa4dde4e5, 0x6f1e6d13, 0x4eef2a82;
    }
}