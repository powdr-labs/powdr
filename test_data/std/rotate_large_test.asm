use std::machines::large_field::rotate::ByteRotate;
use std::machines::large_field::rotate::Rotate;

machine Main with degree: 65536 {
    reg pc[@pc];
    reg X0[<=];
    reg X1[<=];
    reg X2[<=];
    reg A;

    ByteRotate byte_rotate;
    Rotate rotate(byte_rotate);

    instr rotl X0, X1 -> X2 link ~> X2 = rotate.rotl(X0, X1);
    instr rotr X0, X1 -> X2 link ~> X2 = rotate.rotr(X0, X1);

    instr assert_eq X0, X1 {
        X0 = X1
    }

    function main {

        // ROTL
        A <== rotl(0x13579acf, 0);
        assert_eq A, 0x13579acf;
        A <== rotl(0x13579acf, 1);
        assert_eq A, 0x26af359e;
        A <== rotl(0x13579acf, 4);
        assert_eq A, 0x3579acf1;
        A <== rotl(0x13579acf, 8);
        assert_eq A, 0x579acf13;
        A <== rotl(0x13579acf, 12);
        assert_eq A, 0x79acf135;
        A <== rotl(0x13579acf, 16);
        assert_eq A, 0x9acf1357;
        A <== rotl(0x13579acf, 20);
        assert_eq A, 0xacf13579;
        A <== rotl(0x13579acf, 24);
        assert_eq A, 0xcf13579a;
        A <== rotl(0x13579acf, 28);
        assert_eq A, 0xf13579ac;
        A <== rotl(0x13579acf, 31);
        assert_eq A, 0x89abcd67;

        // ROTR
        A <== rotr(0x13579acf, 0);
        assert_eq A, 0x13579acf;
        A <== rotr(0x13579acf, 1);
        assert_eq A, 0x89abcd67;
        A <== rotr(0x13579acf, 4);
        assert_eq A, 0xf13579ac;
        A <== rotr(0x13579acf, 8);
        assert_eq A, 0xcf13579a;
        A <== rotr(0x13579acf, 12);
        assert_eq A, 0xacf13579;
        A <== rotr(0x13579acf, 16);
        assert_eq A, 0x9acf1357;
        A <== rotr(0x13579acf, 20);
        assert_eq A, 0x79acf135;
        A <== rotr(0x13579acf, 24);
        assert_eq A, 0x579acf13;
        A <== rotr(0x13579acf, 28);
        assert_eq A, 0x3579acf1;
        A <== rotr(0x13579acf, 31);
        assert_eq A, 0x26af359e;

        return;
    }
}
