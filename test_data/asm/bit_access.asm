use std::array;

machine BitAccess with degree: 32 {
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
    instr wrap Y -> X { Y = X + wrap_bit * 2**32, X = array::sum(array::map_enumerated(NIB, |i, nib| (2 ** (i * 4)) * nib)) }

    col fixed NIBBLES(i) { i & 0xf };
    let NIB = std::array::new(8, constr |i| { let XN; [XN] in [NIBBLES]; XN });
    col commit wrap_bit;
    wrap_bit * (1 - wrap_bit) = 0;

    instr assert_zero X { XIsZero = 1 }

    function main {
        B <=X= ${ std::prelude::Query::DataIdentifier(1, 0) };
        wrap B + 0xffffffec, A;
        assert_zero A;
        return;
    }
}
