use std::machines::small_field::rotate::ByteRotate;
use std::machines::small_field::rotate::Rotate;

machine Main with degree: 65536 {
    reg pc[@pc];
    reg X0_1[<=];
    reg X0_2[<=];
    reg X1[<=];
    reg X2_1[<=];
    reg X2_2[<=];
    reg A0;
    reg A1;

    ByteRotate byte_rotate;
    Rotate rotate(byte_rotate);

    instr rotl X0_1, X0_2, X1 -> X2_1, X2_2 link ~> (X2_1, X2_2) = rotate.rotl(X0_1, X0_2, X1);
    instr rotr X0_1, X0_2, X1 -> X2_1, X2_2 link ~> (X2_1, X2_2) = rotate.rotr(X0_1, X0_2, X1);

    instr assert_eq X0_1, X0_2, X2_1, X2_2  {
        X0_1 = X2_1,
        X0_2 = X2_2
    }

    function main {

        // ROTL
        A0, A1 <== rotl(0x1357, 0x9acf, 0);
        assert_eq A0, A1, 0x1357, 0x9acf;
        A0, A1 <== rotl(0x1357, 0x9acf, 1);
        assert_eq A0, A1, 0x26af, 0x359e;
        A0, A1 <== rotl(0x1357, 0x9acf, 4);
        assert_eq A0, A1, 0x3579, 0xacf1;
        A0, A1 <== rotl(0x1357, 0x9acf, 8);
        assert_eq A0, A1, 0x579a, 0xcf13;
        A0, A1 <== rotl(0x1357, 0x9acf, 12);
        assert_eq A0, A1, 0x79ac, 0xf135;
        A0, A1 <== rotl(0x1357, 0x9acf, 16);
        assert_eq A0, A1, 0x9acf, 0x1357;
        A0, A1 <== rotl(0x1357, 0x9acf, 20);
        assert_eq A0, A1, 0xacf1, 0x3579;
        A0, A1 <== rotl(0x1357, 0x9acf, 24);
        assert_eq A0, A1, 0xcf13, 0x579a;
        A0, A1 <== rotl(0x1357, 0x9acf, 28);
        assert_eq A0, A1, 0xf135, 0x79ac;
        A0, A1 <== rotl(0x1357, 0x9acf, 31);
        assert_eq A0, A1, 0x89ab, 0xcd67;

        // ROTR
        A0, A1 <== rotr(0x1357, 0x9acf, 0);
        assert_eq A0, A1, 0x1357, 0x9acf;
        A0, A1 <== rotr(0x1357, 0x9acf, 1);
        assert_eq A0, A1, 0x89ab, 0xcd67;
        A0, A1 <== rotr(0x1357, 0x9acf, 4);
        assert_eq A0, A1, 0xf135, 0x79ac;
        A0, A1 <== rotr(0x1357, 0x9acf, 8);
        assert_eq A0, A1, 0xcf13, 0x579a;
        A0, A1 <== rotr(0x1357, 0x9acf, 12);
        assert_eq A0, A1, 0xacf1, 0x3579;
        A0, A1 <== rotr(0x1357, 0x9acf, 16);
        assert_eq A0, A1, 0x9acf, 0x1357;
        A0, A1 <== rotr(0x1357, 0x9acf, 20);
        assert_eq A0, A1, 0x79ac, 0xf135;
        A0, A1 <== rotr(0x1357, 0x9acf, 24);
        assert_eq A0, A1, 0x579a, 0xcf13;
        A0, A1 <== rotr(0x1357, 0x9acf, 28);
        assert_eq A0, A1, 0x3579, 0xacf1;
        A0, A1 <== rotr(0x1357, 0x9acf, 31);
        assert_eq A0, A1, 0x26af, 0x359e;

        return;
    }
}