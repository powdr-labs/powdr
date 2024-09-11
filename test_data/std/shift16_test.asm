use std::machines::shift16::ByteShift16;
use std::machines::shift16::Shift16;

machine Main with degree: 65536 {
    reg pc[@pc];
    reg X0_1[<=];
    reg X0_2[<=];
    reg X1[<=];
    reg X2_1[<=];
    reg X2_2[<=];
    reg ALow;
    reg AHi;

    ByteShift16 byte_shift_16;
    Shift16 shift16(byte_shift_16);

    instr shl X0_1, X0_2, X1 -> X2_1, X2_2 link ~> (X2_1, X2_2) = shift16.shl(X0_1, X0_2, X1);
    instr shr X0_1, X0_2, X1 -> X2_1, X2_2 link ~> (X2_1, X2_2) = shift16.shr(X0_1, X0_2, X1);

    instr assert_eq X0_1, X0_2, X2_1, X2_2  {
        X0_1 = X2_1,
        X0_2 = X2_2
    }

    function main {

        // SHL
        ALow, AHi <== shl(0x1357, 0x9acf, 0);
        assert_eq ALow, AHi, 0x1357, 0x9acf;
        ALow, AHi <== shl(0x1357, 0x9acf, 1);
        assert_eq ALow, AHi, 0x26ae, 0x359e;
        ALow, AHi <== shl(0x1357, 0x9acf, 4);
        assert_eq ALow, AHi, 0x3570, 0xacf1;
        ALow, AHi <== shl(0x1357, 0x9acf, 8);
        assert_eq ALow, AHi, 0x5700, 0xcf13;
        ALow, AHi <== shl(0x1357, 0x9acf, 12);
        assert_eq ALow, AHi, 0x7000, 0xf135;
        ALow, AHi <== shl(0x1357, 0x9acf, 16);
        assert_eq ALow, AHi, 0, 0x1357;
        ALow, AHi <== shl(0x1357, 0x9acf, 20);
        assert_eq ALow, AHi, 0, 0x3570;
        ALow, AHi <== shl(0x1357, 0x9acf, 24);
        assert_eq ALow, AHi, 0, 0x5700;
        ALow, AHi <== shl(0x1357, 0x9acf, 28);
        assert_eq ALow, AHi, 0, 0x7000;
        ALow, AHi <== shl(0x1357, 0x9acf, 31);
        assert_eq ALow, AHi, 0, 0x8000;

        // SHR
        ALow, AHi <== shr(0x1357, 0x9acf, 0);
        assert_eq ALow, AHi, 0x1357, 0x9acf;
        ALow, AHi <== shr(0x1357, 0x9acf, 1);
        assert_eq ALow, AHi, 0x89ab, 0x4d67;
        ALow, AHi <== shr(0x1357, 0x9acf, 4);
        assert_eq ALow, AHi, 0xf135, 0x09ac;
        ALow, AHi <== shr(0x1357, 0x9acf, 8);
        assert_eq ALow, AHi, 0xcf13, 0x009a;
        ALow, AHi <== shr(0x1357, 0x9acf, 12);
        assert_eq ALow, AHi, 0xacf1, 0x0009;
        ALow, AHi <== shr(0x1357, 0x9acf, 16);
        assert_eq ALow, AHi, 0x9acf, 0;
        ALow, AHi <== shr(0x1357, 0x9acf, 20);
        assert_eq ALow, AHi, 0x09ac, 0;
        ALow, AHi <== shr(0x1357, 0x9acf, 24);
        assert_eq ALow, AHi, 0x009a, 0;
        ALow, AHi <== shr(0x1357, 0x9acf, 28);
        assert_eq ALow, AHi, 0x0009, 0;
        ALow, AHi <== shr(0x1357, 0x9acf, 31);
        assert_eq ALow, AHi, 0x1, 0;

        return;
    }
}