use std::machines::arith_bb::Arith16;

machine Main with degree: 65536 {
    reg pc[@pc];
    reg A0[<=];
    reg A1[<=];
    reg B0[<=];
    reg B1[<=];
    reg C0[<=];
    reg C1[<=];
    reg D0[<=];
    reg D1[<=];

    reg t_0_0;
    reg t_0_1;
    reg t_1_0;
    reg t_1_1;

    Arith16 arith;

    instr mul A0, A1, B0, B1 -> C0, C1, D0, D1
        link ~> (C0, C1, D0, D1) = arith.mul(A0, A1, B0, B1);

    instr div A0, A1, B0, B1 -> C0, C1, D0, D1
        link ~> (C0, C1, D0, D1) = arith.div(A0, A1, B0, B1);

    instr assert_eq A0, A1, B0, B1, C0, C1, D0, D1 {
        A0 = C0,
        A1 = C1,
        B0 = D0,
        B1 = D1
    }

    function main {
        // 2 * 3 = 6
        t_0_0, t_0_1, t_1_0, t_1_1 <== mul(2, 0, 3, 0);
        assert_eq t_0_0, t_0_1, t_1_0, t_1_1, 0, 0, 6, 0;

        // (2**32 - 1) * (2**32 - 1) = 2**64 - 2**33 + 1
        t_0_0, t_0_1, t_1_0, t_1_1 <== mul(0xffff, 0xffff, 0xffff, 0xffff);
        assert_eq t_0_0, t_0_1, t_1_0, t_1_1, 0xfffe, 0xffff, 0x0001, 0x0000;

        // 7 / 3 = 2 (remainder 1)
        t_0_0, t_0_1, t_1_0, t_1_1 <== div(7, 0, 3, 0);
        assert_eq t_0_0, t_0_1, t_1_0, t_1_1, 2, 0, 1, 0;

        // 0xffffffff / 0xfffff = 0x1000 (remainder 0xfff)
        t_0_0, t_0_1, t_1_0, t_1_1 <== div(0xffff, 0xffff, 0xffff, 0xf);
        assert_eq t_0_0, t_0_1, t_1_0, t_1_1, 0x1000, 0, 0xfff, 0;

        // 0xfffffffe / 0xff = 0x1010100 (remainder 0xfe)
        t_0_0, t_0_1, t_1_0, t_1_1 <== div(0xfffe, 0xffff, 0xff, 0);
        assert_eq t_0_0, t_0_1, t_1_0, t_1_1, 0x100, 0x101, 0xfe, 0;

        // 0xffffeff / 0xfffff = 0xff (remainder 0xffffe)
        t_0_0, t_0_1, t_1_0, t_1_1 <== div(0xfeff, 0xfff, 0xffff, 0xf);
        assert_eq t_0_0, t_0_1, t_1_0, t_1_1, 0xff, 0, 0xfffe, 0xf;
    }
}
