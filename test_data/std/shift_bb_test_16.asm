use std::machines::shift_bb::ByteShiftBB;
use std::machines::shift_bb::ShiftBB;

machine Main with degree: 65536 {
    reg pc[@pc];
    reg X0_1[<=];
    reg X0_2[<=];
    reg X1[<=];
    reg X2_1[<=];
    reg X2_2[<=];
    reg A1;
    reg A2;

    ByteShiftBB byte_shift_bb;
    ShiftBB shift_bb(byte_shift_bb);

    instr shl X0_1, X0_2, X1 -> X2_1, X2_2 link ~> (X2_1, X2_2) = shift_bb.shl(X0_1, X0_2, X1);
    instr shr X0_1, X0_2, X1 -> X2_1, X2_2 link ~> (X2_1, X2_2) = shift_bb.shr(X0_1, X0_2, X1);

    instr assert_eq X0_1, X0_2, X2_1, X2_2  {
        X0_1 = X2_1,
        X0_2 = X2_2
    }

    function main {

        // SHL
        A1, A2 <== shl(0x1357, 0x9acf, 0);
        assert_eq A1, A2, 0x1357, 0x9acf;
        //A1, A2 <== shl(0x1357, 0x9acf, 1);
        //assert_eq A1, A2, 0x26af, 0x359e;
        A1, A2 <== shl(0x1357, 0x9acf, 4);
        //assert_eq A1, A2, 0x3579, 0xacf0;
        assert_eq A1, A2, 0x3570, 0xacf1;
        //A1, A2 <== shl(0x1357, 0x9acf, 8);
        //assert_eq A1, A2, 0x579a, 0xcf00;
        //A1, A2 <== shl(0x1357, 0x9acf, 12);
        //assert_eq A1, A2, 0x79ac, 0xf000;
        //A1, A2 <== shl(0x1357, 0x9acf, 16);
        //assert_eq A1, A2, 0x9acf, 0;
        //A1, A2 <== shl(0x1357, 0x9acf, 20);
        //assert_eq A1, A2, 0xacf0, 0;
        //A1, A2 <== shl(0x1357, 0x9acf, 24);
        //assert_eq A1, A2, 0xcf00, 0;
        //A1, A2 <== shl(0x1357, 0x9acf, 28);
        //assert_eq A1, A2, 0xf000, 0;
        //A1, A2 <== shl(0x1357, 0x9acf, 31);
        //assert_eq A1, A2, 0x8000, 0;

        // SHR
        A1, A2 <== shr(0x1357, 0x9acf, 0);
        assert_eq A1, A2, 0x1357, 0x9acf;
        //A1, A2 <== shr(0x1357, 0x9acf, 1);
        //assert_eq A1, A2, 0x09ab, 0xcd67;
        //A1, A2 <== shr(0x1357, 0x9acf, 4);
        //assert_eq A1, A2, 0x0135, 0x79ac;
        //A1, A2 <== shr(0x1357, 0x9acf, 8);
        //assert_eq A1, A2, 0x0013, 0x579a;
        //A1, A2 <== shr(0x1357, 0x9acf, 12);
        //assert_eq A1, A2, 0x0001, 0x3579;
        //A1, A2 <== shr(0x1357, 0x9acf, 16);
        //assert_eq A1, A2, 0x0000, 0x1357;
        //A1, A2 <== shr(0x1357, 0x9acf, 20);
        //assert_eq A1, A2, 0x0000, 0x0135;
        //A1, A2 <== shr(0x1357, 0x9acf, 24);
        //assert_eq A1, A2, 0x0000, 0x0013;
        //A1, A2 <== shr(0x1357, 0x9acf, 28);
        //assert_eq A1, A2, 0x0000, 0x0001;
        //A1, A2 <== shr(0x1357, 0x9acf, 31);
        //assert_eq A1, A2, 0, 0;

        return;
    }
}