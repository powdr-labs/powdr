machine BitAccess with degree: 1024 {
    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg A;
    reg B;

    col witness XInv;
    col witness XIsZero;
    XIsZero  = 1 - X * XInv;
    XIsZero * X = 0;
    XIsZero * (1 - XIsZero) = 0;

    // Wraps a value in Y to 32 bits.
    // Requires 0 <= Y < 2**33
    instr wrap Y -> X { Y = X + wrap_bit * 2**32, X = XB1 + 0x100 * XB2 + 0x10000 * XB3 + 0x1000000 * XB4 }

    col fixed BYTE(i) { i & 0xff };
    col witness XB1;
    col witness XB2;
    col witness XB3;
    col witness XB4;
    [ XB1 ] in [ BYTE ];
    [ XB2 ] in [ BYTE ];
    [ XB3 ] in [ BYTE ];
    [ XB4 ] in [ BYTE ];
    col commit wrap_bit;
    wrap_bit * (1 - wrap_bit) = 0;

    instr assert_zero X { XIsZero = 1 }

    function main {
        B <=X= ${ std::prover::Query::Input(0) };
        wrap B + 0xffffffec, A;
        assert_zero A;
        return;
    }
}