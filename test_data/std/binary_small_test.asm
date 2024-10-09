use std::machines::binary::ByteBinary;
use std::machines::small_field::binary::Binary;

machine Main {
    reg pc[@pc];
    reg X0_1[<=];
    reg X0_2[<=];
    reg X1_1[<=];
    reg X1_2[<=];
    reg X2_1[<=];
    reg X2_2[<=];
    reg A1;
    reg A2;

    ByteBinary byte_binary;
    Binary binary(byte_binary);

    instr and X0_1, X0_2, X1_1, X1_2 -> X2_1, X2_2 link ~> (X2_1, X2_2) = binary.and(X0_1, X0_2, X1_1, X1_2);
    instr or X0_1, X0_2, X1_1, X1_2 -> X2_1, X2_2 link ~> (X2_1, X2_2) = binary.or(X0_1, X0_2, X1_1, X1_2);
    instr xor X0_1, X0_2, X1_1, X1_2 -> X2_1, X2_2 link ~> (X2_1, X2_2) = binary.xor(X0_1, X0_2, X1_1, X1_2);

    instr assert_eq X0_1, X0_2, X1_1, X1_2 {
        X0_1 = X1_1,
        X0_2 = X1_2
    }

    function main {

        // AND
        A1, A2 <== and(0, 0, 0, 0);
        assert_eq A1, A2, 0, 0;
        A1, A2 <== and(0xffff, 0xffff, 0xffff, 0xffff);
        assert_eq A1, A2, 0xffff, 0xffff;
        A1, A2 <== and(0xffff, 0xffff, 0xabcd, 0xef01);
        assert_eq A1, A2, 0xabcd, 0xef01;
        A1, A2 <== and(0xabcd, 0xef01, 0xffff, 0xffff);
        assert_eq A1, A2, 0xabcd, 0xef01;
        A1, A2 <== and(0, 0, 0xabcd, 0xef01);
        assert_eq A1, A2, 0, 0;
        A1, A2 <== and(0xabcd, 0xef01, 0, 0);
        assert_eq A1, A2, 0, 0;

        // OR
        A1, A2 <== or(0, 0, 0, 0);
        assert_eq A1, A2, 0, 0;
        A1, A2 <== or(0xffff, 0xffff, 0xffff, 0xffff);
        assert_eq A1, A2, 0xffff, 0xffff;
        A1, A2 <== or(0xffff, 0xffff, 0xabcd, 0xef01);
        assert_eq A1, A2, 0xffff, 0xffff;
        A1, A2 <== or(0xabcd, 0xef01, 0xffff, 0xffff);
        assert_eq A1, A2, 0xffff, 0xffff;
        A1, A2 <== or(0, 0, 0xabcd, 0xef01);
        assert_eq A1, A2, 0xabcd, 0xef01;
        A1, A2 <== or(0xabcd, 0xef01, 0, 0);
        assert_eq A1, A2, 0xabcd, 0xef01;

        // XOR
        A1, A2 <== xor(0, 0, 0, 0);
        assert_eq A1, A2, 0, 0;
        A1, A2 <== xor(0xffff, 0xffff, 0xffff, 0xffff);
        assert_eq A1, A2, 0, 0;
        A1, A2 <== xor(0xffff, 0xffff, 0xabcd, 0xef01);
        assert_eq A1, A2, 0x5432, 0x10fe;
        A1, A2 <== xor(0xabcd, 0xef01, 0xffff, 0xffff);
        assert_eq A1, A2, 0x5432, 0x10fe;
        A1, A2 <== xor(0, 0, 0xabcd, 0xef01);
        assert_eq A1, A2, 0xabcd, 0xef01;
        A1, A2 <== xor(0xabcd, 0xef01, 0, 0);
        assert_eq A1, A2, 0xabcd, 0xef01;

        return;
    }
}
