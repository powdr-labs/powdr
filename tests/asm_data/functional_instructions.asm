reg pc[@pc];
reg X[<=];
reg Y[<=];
reg A;
reg B;
reg CNT;

pil{
    col witness XInv;
    col witness XIsZero;
    XIsZero  = 1 - X * XInv;
    XIsZero * X = 0;
    XIsZero * (1 - XIsZero) = 0;
}

// Wraps a value in Y to 32 bits.
// Requires 0 <= Y < 2**33
// TODO we need better syntax for defining instructions that are functions.
// Maybe like instr wrap <=Y= v -> X { Y = X + wrap_bit * 2**32, X = Xhi * 2**16 + Xlo }
instr wrap <=Y= v, x <=X= { Y = X + wrap_bit * 2**32, X = XB1 + 0x100 * XB2 + 0x10000 * XB3 + 0x1000000 * XB4 }
pil{
    col fixed BYTES(i) { i & 0xff };
    col commit XB1;
    col commit XB2;
    col commit XB3;
    col commit XB4;
    { XB1 } in { BYTES };
    { XB2 } in { BYTES };
    { XB3 } in { BYTES };
    { XB4 } in { BYTES };
    col commit wrap_bit;
    wrap_bit * (1 - wrap_bit) = 0;
}

instr assert_zero <=X= a { XIsZero = 1 }

B <=X= ${ ("input", 0) };
A <=X= wrap(B + 0xffffffec);
assert_zero A;
