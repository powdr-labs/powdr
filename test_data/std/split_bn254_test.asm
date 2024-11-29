use std::machines::split::ByteCompare;
use std::machines::split::split_bn254::SplitBN254;

let main_degree: int = 2**10;
let split_degree: int = 2**12;

machine Main with degree: main_degree {
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
    reg A1;
    reg A2;
    reg A3;
    reg A4;
    reg A5;
    reg A6;
    reg A7;
    reg A8;

    ByteCompare byte_compare;
    SplitBN254 split_machine(byte_compare, split_degree, split_degree);

    instr split X0 -> X1, X2, X3, X4, X5, X6, X7, X8 link ~> (X1, X2, X3, X4, X5, X6, X7, X8) = split_machine.split(X0);

    instr assert_eq X0, X1 {
        X0 = X1
    }

    function main {

        // Min value
        // Note that this has two byte decompositions, 0 and p.
        // The second would lead to a different split value, but should be ruled
        // out by the overflow check.
        A1, A2, A3, A4, A5, A6, A7, A8 <== split(0);
        assert_eq A1, 0;
        assert_eq A2, 0;
        assert_eq A3, 0;
        assert_eq A4, 0;
        assert_eq A5, 0;
        assert_eq A6, 0;
        assert_eq A7, 0;
        assert_eq A8, 0;

        // Max value
        // On BN254, this is 0x30644e72 e131a029 b85045b6 8181585d 2833e848 79b97091 43e1f593 f0000000
        A1, A2, A3, A4, A5, A6, A7, A8 <== split(-1);
        assert_eq A1, 0xf0000000;
        assert_eq A2, 0x43e1f593;
        assert_eq A3, 0x79b97091;
        assert_eq A4, 0x2833e848;
        assert_eq A5, 0x8181585d;
        assert_eq A6, 0xb85045b6;
        assert_eq A7, 0xe131a029;
        assert_eq A8, 0x30644e72;

        // Max low values
        A1, A2, A3, A4, A5, A6, A7, A8 <== split(0x2fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff);
        assert_eq A1, 0xffffffff;
        assert_eq A2, 0xffffffff;
        assert_eq A3, 0xffffffff;
        assert_eq A4, 0xffffffff;
        assert_eq A5, 0xffffffff;
        assert_eq A6, 0xffffffff;
        assert_eq A7, 0xffffffff;
        assert_eq A8, 0x2fffffff;

        // Some other value
        A1, A2, A3, A4, A5, A6, A7, A8 <== split(0xabcdef0123456789);
        assert_eq A1, 0x23456789;
        assert_eq A2, 0xabcdef01;
        assert_eq A3, 0;
        assert_eq A4, 0;
        assert_eq A5, 0;
        assert_eq A6, 0;
        assert_eq A7, 0;
        assert_eq A8, 0;

        return;
    }
}
