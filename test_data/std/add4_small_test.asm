use std::machines::small_field::add4::Add4;
use std::machines::range::Byte2;
use std::machines::range::Bit2;
use std::machines::range::Range9;

machine Main {
    reg pc[@pc];
    reg A_h[<=];
    reg A_l[<=];
    reg B_h[<=];
    reg B_l[<=];
    reg C_h[<=];
    reg C_l[<=];
    reg D_h[<=];
    reg D_l[<=];
    reg E_h[<=];
    reg E_l[<=];

    reg A;
    reg B;
    reg C;
    reg D;

    Byte2 byte2;
    Bit2 bit2;
    Range9 range9;
    Add4 add4(byte2, bit2, range9);

    instr add4 A_h, A_l, B_h, B_l, C_h, C_l, D_h, D_l -> E_h, E_l
      link ~> (E_h, E_l) = add4.add(A_h, A_l, B_h, B_l, C_h, C_l, D_h, D_l);

    instr assert_eq A_l, B_l {
        A_l = B_l 
    }

    function main {
        // ADD
        A, B <== add4(0, 0, 0, 0, 0, 0, 0, 0);
        assert_eq A, 0;
        assert_eq B, 0;

        A, B <== add4(0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff);
        assert_eq A, 0xffff;
        assert_eq B, 0xfffc;

        A, B <== add4(0xffff, 0xffff, 0xabcd, 0xef01, 0xffff, 0xffff, 0xabcd, 0xef01);
        assert_eq A, 0x579b;
        assert_eq B, 0xde00;

        return;
    }
}
