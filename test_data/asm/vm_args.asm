use std::machines::large_field::shift::Shift;
use std::machines::large_field::shift::ByteShift;

let N: int = 65536;

machine Main with degree: N {
    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg Z[<=];
    reg A;

    ByteShift byte_shift;
    Shift shift(byte_shift);
    WithArg sub(shift);

    instr shl X, Y -> Z link ~> Z = shift.shl(X, Y);
    instr shr X, Y -> Z link ~> Z = shift.shr(X, Y);

    instr shl1 X, Y -> Z link => Z = sub.shl(X, Y);
    instr shr1 X, Y -> Z link => Z = sub.shr(X, Y);

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

machine WithArg(shift: Shift) with degree: N {
    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg Z[<=];
    reg A;
    reg B;

    instr shl X, Y -> Z link ~> Z = shift.shl(X, Y);
    instr shr X, Y -> Z link ~> Z = shift.shr(X, Y);

    function shl a, b -> c {
        A <== shl(a,b);
        return A;
    }

    function shr a, b -> c {
        A <== shr(a,b);
        return A;
    }
}
