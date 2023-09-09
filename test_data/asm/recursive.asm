machine VM {
    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg A;

    instr factorial X -> Y = self.factorial

    constraints {
        col witness XIsZero;
        col witness XInv;
        XIsZero = (1 - (X * XInv));
        (XIsZero * X) = 0;
        (XIsZero * (1 - XIsZero)) = 0;
    }

    instr jmpz X, l: label {
        pc' = XIsZero * (pc + 1) + (1 - XIsZero) * l
    }

    instr mult_A X {
        A' = A * X
    }

    instr assert_eq X, Y {
        X = Y
    }

    function factorial x: field -> field {
        jmpz x, recursive;
        return 1;
        recursive::
        A <== factorial(x - 1);
        mult_A(x);
        return A;
    }

    function main {
        A <=X= 5;
        A <== factorial(A);
        assert_eq A, 120;
        return;
    }
}