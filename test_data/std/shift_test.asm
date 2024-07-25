use std::machines::shift::ByteShift;
use std::machines::shift::Shift;

machine Main with degree: 65536 {
    reg pc[@pc];
    reg X0[<=];
    reg X1[<=];
    reg X2[<=];
    reg A;

    ByteShift byte_shift;
    Shift shift(byte_shift);

    instr shl X0, X1 -> X2 link ~> X2 = shift.shl(X0, X1);
    instr shr X0, X1 -> X2 link ~> X2 = shift.shr(X0, X1);

    instr assert_eq X0, X1 {
        X0 = X1
    }

    function main {

        // SHL
        A <== shl(0x13579acf, 0);
        assert_eq A, 0x13579acf;
        A <== shl(0x13579acf, 1);
        assert_eq A, 0x26af359e;
        A <== shl(0x13579acf, 4);
        assert_eq A, 0x3579acf0;
        A <== shl(0x13579acf, 8);
        assert_eq A, 0x579acf00;
        A <== shl(0x13579acf, 12);
        assert_eq A, 0x79acf000;
        A <== shl(0x13579acf, 16);
        assert_eq A, 0x9acf0000;
        A <== shl(0x13579acf, 20);
        assert_eq A, 0xacf00000;
        A <== shl(0x13579acf, 24);
        assert_eq A, 0xcf000000;
        A <== shl(0x13579acf, 28);
        assert_eq A, 0xf0000000;
        A <== shl(0x13579acf, 31);
        assert_eq A, 0x80000000;

        // SHR
        A <== shr(0x13579acf, 0);
        assert_eq A, 0x13579acf;
        A <== shr(0x13579acf, 1);
        assert_eq A, 0x09abcd67;
        A <== shr(0x13579acf, 4);
        assert_eq A, 0x013579ac;
        A <== shr(0x13579acf, 8);
        assert_eq A, 0x0013579a;
        A <== shr(0x13579acf, 12);
        assert_eq A, 0x00013579;
        A <== shr(0x13579acf, 16);
        assert_eq A, 0x00001357;
        A <== shr(0x13579acf, 20);
        assert_eq A, 0x00000135;
        A <== shr(0x13579acf, 24);
        assert_eq A, 0x00000013;
        A <== shr(0x13579acf, 28);
        assert_eq A, 0x00000001;
        A <== shr(0x13579acf, 31);
        assert_eq A, 0x00000000;

        return;
    }
}
