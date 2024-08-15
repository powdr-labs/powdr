machine Main with degree: 128 {
    Pow pow;

    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg Z[<=];
    reg A;

    instr pow X, Y -> Z link => Z = pow::pow(X, Y);
    instr assert_eq X, Y { X = Y }

    function main {
        A <== pow(7, 0);
        assert_eq A, 1;

        A <== pow(7, 1);
        assert_eq A, 7;

        A <== pow(7, 2);
        assert_eq A, 49;

        A <== pow(2, 2);
        assert_eq A, 4;

        A <== pow(2, 10);
        assert_eq A, 1024;

        return;
    }
}

// Computes X^Y by multiplying X by itself Y times
machine Pow {

    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg Z[<=];
    reg A;
    reg CNT;

    col witness XInv;
    col witness XIsZero;
    XIsZero  = 1 - X * XInv;
    XIsZero * X = 0;
    XIsZero * (1 - XIsZero) = 0;

    instr mul X, Y -> Z { X * Y = Z }
    instr jmpz X, l: label { pc' = XIsZero * l + (1 - XIsZero) * (pc + 1) }
    instr jmp l: label { pc' = l }

    function pow x: field, y: field -> field {
        A <=X= 1;
        CNT <=X= y;

        start:
        jmpz CNT, done;
        A <== mul(A, x);
        CNT <=X= CNT - 1;
        jmp start;

        done:
        return A;
    }

}
