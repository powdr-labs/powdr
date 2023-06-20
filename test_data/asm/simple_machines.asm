// a machine to add numbers between 0 and 1, with preprocessing
machine PreprocessedAdder {
    instr add A, B -> C {}

    constraints {
        col fixed A = [0, 0, 1, 1] + [0]*;
        col fixed B = [0, 1, 0, 1] + [0]*;
        col fixed C = [0, 1, 1, 2] + [0]*;
    }
}

// a machine to add numbers between 0 and 1, without preprocessing
machine Adder {
    instr<LATCH> add a, b -> c {}

    constraints {

        // this latch is required for the executor to detect this machine as a block machine. 
        // It could be removed in this edge case of a block machine with blocks of size 1
        col fixed LATCH = [1]*;

        col witness a;
        col witness b;
        col witness c;
        // boolean constrain a, b, c
        a * (1 - a) = 0;
        b * (1 - b) = 0;
        // check a + b - 2 against 0
        col witness X;
        X = a + b - 2;
        col witness XInv;
        col witness XIsZero;
        XIsZero = 1 - X * XInv;
        XIsZero * X = 0;
        XIsZero * (1 - XIsZero) = 0;

        // constrain c: if a + b == 2 { 0 } else { a + b }
        c = XIsZero * 0 + (1 - XIsZero) * (a + b);
    }
}


machine Main {

    Adder adder;
    PreprocessedAdder preprocessed_adder;

    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg Z[<=];
    reg A;
    reg B;

    instr assert_two X { X = 2 }
    instr add X, Y -> Z = adder.add
    instr add_preprocessed X, Y -> Z = preprocessed_adder.add

    instr loop { pc' = pc }

    program {
        A <=X= 0;
        B <=Z= add(A, 1);
        B <=Z= add_preprocessed(B, 1);
        assert_two(B);
        loop;
    }
}
