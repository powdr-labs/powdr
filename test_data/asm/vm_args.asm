use std::machines::shift::Shift;

machine Main with degree: 65536 {
    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg Z[<=];
    reg A;

    Shift shift;
    WithArg sub(shift);

    instr shl X, Y -> Z ~ shift.shl;
    instr shr X, Y -> Z ~ shift.shr;

    instr shl1 X, Y -> Z = sub.shl;
    instr shr1 X, Y -> Z = sub.shr;

    instr assert_eq X, Y { X = Y }

    function main {
        A <== shl(0x1, 3);
        assert_eq A, 0x8;
        A <== shl1(0x1, 4);
        assert_eq A, 0x10;

        A <== shr(0x11, 2);
        assert_eq A, 0x4;
        A <== shr1(0x22, 3);
        assert_eq A, 0x4;

        return;
    }
}

machine WithArg(shift: Shift) {
    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg Z[<=];
    reg A;
    reg B;

    instr shl X, Y -> Z ~ shift.shl;
    instr shr X, Y -> Z ~ shift.shr;

    function shl a, b -> c {
        A <== shl(a,b);
        return A;
    }

    function shr a, b -> c {
        A <== shr(a,b);
        return A;
    }
}
