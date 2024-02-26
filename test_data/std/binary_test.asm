use std::binary::Binary;


machine Main {
    reg pc[@pc];
    reg X0[<=];
    reg X1[<=];
    reg X2[<=];
    reg A;

    degree 262144;

    Binary binary;

    instr and X0, X1 -> X2 = binary.and;
    instr or X0, X1 -> X2 = binary.or;
    instr xor X0, X1 -> X2 = binary.xor;

    instr assert_eq X0, X1 {
        X0 = X1
    }

    function main {

        // AND
        A <== and(0, 0);
        assert_eq A, 0;
        A <== and(0xffffffff, 0xffffffff);
        assert_eq A, 0xffffffff;
        A <== and(0xffffffff, 0xabcdef01);
        assert_eq A, 0xabcdef01;
        A <== and(0xabcdef01, 0xffffffff);
        assert_eq A, 0xabcdef01;
        A <== and(0, 0xabcdef01);
        assert_eq A, 0;
        A <== and(0xabcdef01, 0);
        assert_eq A, 0;

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

        return;
    }
}