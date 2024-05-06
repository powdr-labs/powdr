use std::machines::binary::Binary;

machine Main with degree: 262144 {
    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg Z[<=];
    reg A;

    Binary binary;
    WithArg sub(binary);

    instr and X, Y -> Z ~ binary.and;
    instr or X, Y -> Z ~ binary.or;
    instr xor X, Y -> Z ~ binary.xor;

    // TODO: changing these to permutation passes witgen but fails proving
    instr and1 X, Y -> Z = sub.and;
    instr or1 X, Y -> Z = sub.or;
    instr xor1 X, Y -> Z = sub.xor;

    instr assert_eq X, Y { X = Y }

    function main {
        // AND
        A <== and(0, 0);
        assert_eq A, 0;
        A <== and(0xffffffff, 0xffffffff);
        assert_eq A, 0xffffffff;
        A <== and(0xffffffff, 0xabcdef01);
        assert_eq A, 0xabcdef01;
        A <== and1(0xabcdef01, 0xffffffff);
        assert_eq A, 0xabcdef01;
        A <== and1(0, 0xabcdef01);
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
        A <== or1(0xabcdef01, 0xffffffff);
        assert_eq A, 0xffffffff;
        A <== or1(0, 0xabcdef01);
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
        A <== xor1(0xabcdef01, 0xffffffff);
        assert_eq A, 0x543210fe;
        A <== xor1(0, 0xabcdef01);
        assert_eq A, 0xabcdef01;
        A <== xor(0xabcdef01, 0);
        assert_eq A, 0xabcdef01;

        return;
    }
}

machine WithArg(bin: Binary) {
    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg Z[<=];
    reg A;
    reg B;

    instr and X, Y -> Z ~ bin.and;
    instr or X, Y -> Z ~ bin.or;
    instr xor X, Y -> Z ~ bin.xor;

    function and a, b -> c {
        A <== and(a,b);
        return A;
    }

    function or a, b -> c {
        A <== or(a,b);
        return A;
    }

    function xor a, b -> c {
        A <== xor(a,b);
        return A;
    }
}
