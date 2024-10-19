use std::machines::small_field::add_sub::AddSub;
use std::machines::range::Byte2;

machine Main {
    reg pc[@pc];
    reg A_h[<=];
    reg A_l[<=];
    reg B_h[<=];
    reg B_l[<=];
    reg C_h[<=];
    reg C_l[<=];

    reg A;
    reg B;

    Byte2 byte2;
    AddSub add_sub(byte2);

    instr add A_h, A_l, B_h, B_l -> C_h, C_l
      link ~> (C_h, C_l) = add_sub.add(A_h, A_l, B_h, B_l);

    instr sub A_h, A_l, B_h, B_l -> C_h, C_l
      link ~> (C_h, C_l) = add_sub.sub(A_h, A_l, B_h, B_l);

    instr gt A_h, A_l, B_h, B_l -> C_l
      link ~> C_l = add_sub.gt(A_h, A_l, B_h, B_l);

    instr assert_eq A_l, B_l {
        A_l = B_l 
    }

    function main {
        // ADD
        A, B <== add(0, 0, 0, 0);
        assert_eq A, 0;
        assert_eq B, 0;
        A, B <== add(0xffff, 0xffff, 0xffff, 0xffff);
        assert_eq A, 0xffff;
        assert_eq B, 0xfffe;
        A, B <== add(0xffff, 0xffff, 0xabcd, 0xef01);
        assert_eq A, 0xabcd;
        assert_eq B, 0xef00;
        A, B <== add(0, 0, 0xabcd, 0xef01);
        assert_eq A, 0xabcd;
        assert_eq B, 0xef01;
        A, B <== add(0xabcd, 0, 0, 0xef01);
        assert_eq A, 0xabcd;
        assert_eq B, 0xef01;
        A, B <== add(0, 0xffff, 0, 0xffff);
        assert_eq A, 1;
        assert_eq B, 0xfffe;

        // SUB
        A, B <== sub(0, 0, 0, 0);
        assert_eq A, 0;
        assert_eq B, 0;

        A, B <== sub(0, 2, 0, 1);
        assert_eq A, 0;
        assert_eq B, 1;

        A, B <== sub(0, 1, 0, 2);
        assert_eq A, 0xffff;
        assert_eq B, 0xffff;

        A, B <== add(A, B, 0, 1);
        assert_eq A, 0;
        assert_eq B, 0;

        A, B <== sub(0, 1, 0, 3);
        assert_eq A, 0xffff;
        assert_eq B, 0xfffe;

        A, B <== sub(0, 0, 0xffff, 0xffff);
        assert_eq A, 0;
        assert_eq B, 1;

        A, B <== sub(0, 1, 0xffff, 0xffff);
        assert_eq A, 0;
        assert_eq B, 2;

        A, B <== sub(0xffff, 0xffff, 0xffff, 0xffff);
        assert_eq A, 0;
        assert_eq B, 0;

        A, B <== sub(0xffff, 0xffff, 0xffff, 0xfffe);
        assert_eq A, 0;
        assert_eq B, 1;

        A, B <== sub(0xffff, 0xffff, 0, 0xffff);
        assert_eq A, 0xffff;
        assert_eq B, 0;

        A, B <== sub(0, 0, 0, 6);
        assert_eq A, 0xffff;
        assert_eq B, 0xfffa;

        A, B <== sub(0, 4, 0, 0);
        assert_eq A, 0;
        assert_eq B, 4;

        // gt
        A <== gt(0, 4, 0, 0);
        assert_eq A, 0;

        A <== gt(0, 4, 0, 8);
        assert_eq A, 1;

        A <== gt(0, 4, 0xffff, 0xffff);
        assert_eq A, 1;

        A <== gt(0xffff, 0xffff, 0, 4);
        assert_eq A, 0;

        A <== gt(0xffff, 0xffff, 0xffff, 0xfffe);
        assert_eq A, 0;

        A <== gt(0xffff, 0xfffe, 0xffff, 0xffff);
        assert_eq A, 1;

        A <== gt(0, 0, 0xffff, 0xffff);
        assert_eq A, 1;

        A <== gt(0xffff, 0xffff, 0, 0);
        assert_eq A, 0;

        A <== gt(0xffff, 0xfffe, 0, 0);
        assert_eq A, 0;

        A <== gt(0, 0, 0xffff, 0xfffe);
        assert_eq A, 1;

        return;
    }
}
