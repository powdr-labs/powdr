machine MultiAssign {
    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg A;

    constraints {
        col witness XInv;
        col witness XIsZero;
        XIsZero  = 1 - X * XInv;
        XIsZero * X = 0;
        XIsZero * (1 - XIsZero) = 0;
    }

    instr assert_zero X { XIsZero = 1 }
    instr loop { pc' = pc }

    function main {
        A <=X= ${ ("input", 0) };
        A <=Y= A - 7;
        assert_zero A;
        loop;
    }
}