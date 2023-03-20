reg pc[@pc];
reg X[<=];
reg Y[<=];
reg A;

pil{
    col witness XInv;
    col witness XIsZero;
    XIsZero  = 1 - X * XInv;
    XIsZero * X = 0;
    XIsZero * (1 - XIsZero) = 0;
}

instr assert_zero <=X= a { XIsZero = 1 }

A <=X= ${ ("input", 0) };
A <=Y= A - 7;
assert_zero A;
