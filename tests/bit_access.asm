reg pc[@pc];
reg X[<=];
reg Y[<=];
reg A;
reg B;

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
instr wrap <=Y= v, x <=X= { Y = X + wrap_bit * 2**32, X = Xhi * 2**16 + Xlo }
pil{
    col fixed BYTES2(i) { i & 0xffff };
    col witness Xlo;
    col witness Xhi;
    { Xlo } in { BYTES2 };
    { Xhi } in { BYTES2 };
    col commit wrap_bit;
    wrap_bit * (1 - wrap_bit) = 0;
}

instr assert_zero <=X= a { XIsZero = 1 }

B <=X= ${ ("input", 0) };
wrap B + 0xffffffec, A;
assert_zero A;
