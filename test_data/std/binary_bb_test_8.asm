use std::machines::binary_bb::ByteBinary;
use std::machines::binary_bb::Binary8;

machine Main with degree: 196608 {
    reg pc[@pc];
    reg X0_1[<=];
    reg X0_2[<=];
    reg X0_3[<=];
    reg X0_4[<=];
    reg X1_1[<=];
    reg X1_2[<=];
    reg X1_3[<=];
    reg X1_4[<=];
    reg X2_1[<=];
    reg X2_2[<=];
    reg X2_3[<=];
    reg X2_4[<=];
    reg A1;
    reg A2;
    reg A3;
    reg A4;

    ByteBinary byte_binary;
    Binary8 binary(byte_binary);

    instr and X0_1, X0_2, X0_3, X0_4, X1_1, X1_2, X1_3, X1_4 -> X2_1, X2_2, X2_3, X2_4 link ~> (X2_1, X2_2, X2_3, X2_4) = binary.and(X0_1, X0_2, X0_3, X0_4, X1_1, X1_2, X1_3, X1_4);
    instr or X0_1, X0_2, X0_3, X0_4, X1_1, X1_2, X1_3, X1_4 -> X2_1, X2_2, X2_3, X2_4 link ~> (X2_1, X2_2, X2_3, X2_4) = binary.or(X0_1, X0_2, X0_3, X0_4, X1_1, X1_2, X1_3, X1_4);
    instr xor X0_1, X0_2, X0_3, X0_4, X1_1, X1_2, X1_3, X1_4 -> X2_1, X2_2, X2_3, X2_4 link ~> (X2_1, X2_2, X2_3, X2_4) = binary.xor(X0_1, X0_2, X0_3, X0_4, X1_1, X1_2, X1_3, X1_4);

    instr assert_eq X0_1, X0_2, X0_3, X0_4, X1_1, X1_2, X1_3, X1_4 {
        X0_1 = X1_1,
        X0_2 = X1_2,
        X0_3 = X1_3,
        X0_4 = X1_4
    }

    function main {

        // AND
        A1, A2, A3, A4 <== and(0, 0, 0, 0, 0, 0, 0, 0);
        assert_eq A1, A2, A3, A4, 0, 0, 0, 0;
        A1, A2, A3, A4 <== and(0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff);
        assert_eq A1, A2, A3, A4, 0xff, 0xff, 0xff, 0xff;
        A1, A2, A3, A4 <== and(0xff, 0xff, 0xff, 0xff, 0xab, 0xcd, 0xef, 0x01);
        assert_eq A1, A2, A3, A4, 0xab, 0xcd, 0xef, 0x01;
        A1, A2, A3, A4 <== and(0xab, 0xcd, 0xef, 0x01, 0xff, 0xff, 0xff, 0xff);
        assert_eq A1, A2, A3, A4, 0xab, 0xcd, 0xef, 0x01;
        A1, A2, A3, A4 <== and(0, 0, 0, 0, 0xab, 0xcd, 0xef, 0x01);
        assert_eq A1, A2, A3, A4, 0, 0, 0, 0;
        A1, A2, A3, A4 <== and(0xab, 0xcd, 0xef, 0x01, 0, 0, 0, 0);
        assert_eq A1, A2, A3, A4, 0, 0, 0, 0;

  /*
        // OR
        A <== or(0, 0);
        assert_eq A, 0;
        A <== or(0xffffffff, 0xffffffff);
        assert_eq A, 0xffffffff;
        A <== or(0xffffffff, 0xabcdef01);
        assert_eq A, 0xffffffff;
        A <== or(0xabcdef01, 0xffffffff);
        assert_eq A, 0xffffffff;
        A <== or(0, 0xabcdef01);
        assert_eq A, 0xabcdef01;
        A <== or(0xabcdef01, 0);
        assert_eq A, 0xabcdef01;

        // XOR
        A <== xor(0, 0);
        assert_eq A, 0;
        A <== xor(0xffffffff, 0xffffffff);
        assert_eq A, 0;
        A <== xor(0xffffffff, 0xabcdef01);
        assert_eq A, 0x543210fe;
        A <== xor(0xabcdef01, 0xffffffff);
        assert_eq A, 0x543210fe;
        A <== xor(0, 0xabcdef01);
        assert_eq A, 0xabcdef01;
        A <== xor(0xabcdef01, 0);
        assert_eq A, 0xabcdef01;
*/
        return;
    }
}

use std::machines::binary_bb::ByteBinary;
use std::machines::binary_bb::Binary;

machine Main with degree: 196608 {
    reg pc[@pc];
    reg X0_1[<=];
    reg X0_2[<=];
    reg X0_3[<=];
    reg X0_4[<=];
    reg X1_1[<=];
    reg X1_2[<=];
    reg X1_3[<=];
    reg X1_4[<=];
    reg X2_1[<=];
    reg X2_2[<=];
    reg X2_3[<=];
    reg X2_4[<=];
    reg A1;
    reg A2;
    reg A3;
    reg A4;

    ByteBinary byte_binary;
    Binary binary(byte_binary);

    instr and X0_1, X0_2, X0_3, X0_4, X1_1, X1_2, X1_3, X1_4 -> X2_1, X2_2, X2_3, X2_4 link ~> (X2_1, X2_2, X2_3, X2_4) = binary.and(X0_1, X0_2, X0_3, X0_4, X1_1, X1_2, X1_3, X1_4);
    instr or X0_1, X0_2, X0_3, X0_4, X1_1, X1_2, X1_3, X1_4 -> X2_1, X2_2, X2_3, X2_4 link ~> (X2_1, X2_2, X2_3, X2_4) = binary.or(X0_1, X0_2, X0_3, X0_4, X1_1, X1_2, X1_3, X1_4);
    instr xor X0_1, X0_2, X0_3, X0_4, X1_1, X1_2, X1_3, X1_4 -> X2_1, X2_2, X2_3, X2_4 link ~> (X2_1, X2_2, X2_3, X2_4) = binary.xor(X0_1, X0_2, X0_3, X0_4, X1_1, X1_2, X1_3, X1_4);

    instr assert_eq X0_1, X0_2, X0_3, X0_4, X1_1, X1_2, X1_3, X1_4 {
        X0_1 = X1_1,
        X0_2 = X1_2,
        X0_3 = X1_3,
        X0_4 = X1_4
    }

    function main {

        // AND
        A1, A2, A3, A4 <== and(0, 0, 0, 0, 0, 0, 0, 0);
        assert_eq A1, A2, A3, A4, 0, 0, 0, 0;
        A1, A2, A3, A4 <== and(0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff);
        assert_eq A1, A2, A3, A4, 0xff, 0xff, 0xff, 0xff;
        A1, A2, A3, A4 <== and(0xff, 0xff, 0xff, 0xff, 0xab, 0xcd, 0xef, 0x01);
        assert_eq A1, A2, A3, A4, 0xab, 0xcd, 0xef, 0x01;
        A1, A2, A3, A4 <== and(0xab, 0xcd, 0xef, 0x01, 0xff, 0xff, 0xff, 0xff);
        assert_eq A1, A2, A3, A4, 0xab, 0xcd, 0xef, 0x01;
        A1, A2, A3, A4 <== and(0, 0, 0, 0, 0xab, 0xcd, 0xef, 0x01);
        assert_eq A1, A2, A3, A4, 0, 0, 0, 0;
        A1, A2, A3, A4 <== and(0xab, 0xcd, 0xef, 0x01, 0, 0, 0, 0);
        assert_eq A1, A2, A3, A4, 0, 0, 0, 0;

        // OR
        A1, A2, A3, A4 <== or(0, 0, 0, 0, 0, 0, 0, 0);
        assert_eq A1, A2, A3, A4, 0, 0, 0, 0;
        A1, A2, A3, A4 <== or(0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff);
        assert_eq A1, A2, A3, A4, 0xff, 0xff, 0xff, 0xff;
        A1, A2, A3, A4 <== or(0xff, 0xff, 0xff, 0xff, 0xab, 0xcd, 0xef, 0x01);
        assert_eq A1, A2, A3, A4, 0xff, 0xff, 0xff, 0xff;
        A1, A2, A3, A4 <== or(0xab, 0xcd, 0xef, 0x01, 0xff, 0xff, 0xff, 0xff);
        assert_eq A1, A2, A3, A4, 0xff, 0xff, 0xff, 0xff;
        A1, A2, A3, A4 <== or(0, 0, 0, 0, 0xab, 0xcd, 0xef, 0x01);
        assert_eq A1, A2, A3, A4, 0xab, 0xcd, 0xef, 0x01;
        A1, A2, A3, A4 <== or(0xab, 0xcd, 0xef, 0x01, 0, 0, 0, 0);
        assert_eq A1, A2, A3, A4, 0xab, 0xcd, 0xef, 0x01;

        // XOR
        A1, A2, A3, A4 <== xor(0, 0, 0, 0, 0, 0, 0, 0);
        assert_eq A1, A2, A3, A4, 0, 0, 0, 0;
        A1, A2, A3, A4 <== xor(0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff);
        assert_eq A1, A2, A3, A4, 0, 0, 0, 0;
        A1, A2, A3, A4 <== xor(0xff, 0xff, 0xff, 0xff, 0xab, 0xcd, 0xef, 0x01);
        assert_eq A1, A2, A3, A4, 0x54, 0x32, 0x10, 0xfe;
        A1, A2, A3, A4 <== xor(0xab, 0xcd, 0xef, 0x01, 0xff, 0xff, 0xff, 0xff);
        assert_eq A1, A2, A3, A4, 0x54, 0x32, 0x10, 0xfe;
        A1, A2, A3, A4 <== xor(0, 0, 0, 0, 0xab, 0xcd, 0xef, 0x01);
        assert_eq A1, A2, A3, A4, 0xab, 0xcd, 0xef, 0x01;
        A1, A2, A3, A4 <== xor(0xab, 0xcd, 0xef, 0x01, 0, 0, 0, 0);
        assert_eq A1, A2, A3, A4, 0xab, 0xcd, 0xef, 0x01;

        return;
    }
}
