machine NovaZero {

    degree 32;

    // this simple machine does not have submachines

    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg Z[<=];
    reg x0;
    reg x1;

    constraints {
        col witness XInv;
        col witness XIsZero;
        XIsZero = 1 - X * XInv;
        XIsZero * X = 0;
        XIsZero * (1 - XIsZero) = 0;
    }

    constraints {
        col witness x_b0;
        col witness x_b1;
        col witness x_b2;
        col witness x_b3;

        // constraints bit
        x_b0 * (1-x_b0) = 0;
        x_b1 * (1-x_b1) = 0;
        x_b2 * (1-x_b2) = 0;
        x_b3 * (1-x_b3) = 0;
        // ...
    }

    instr incr X -> Y {
        Y = X + 1,
        X = x_b0 + x_b1 * 2 + x_b2 * 2**2 + x_b3 * 2**3,
        pc' = pc + 1
    }

    instr add X, Y -> Z {
        Z = X + Y,
        pc' = pc + 1
    }

    instr sub X, Y -> Z {
        Z = X - Y,
        pc' = pc + 1
    }

    instr decr X -> Y {
        Y = X - 1
    }

    instr addi X, Y:signed -> Z {
        Z = X + Y,
        pc' = pc + 1
    }

    // an instruction to loop forever, as we must fill the whole execution trace
    instr loop {
        pc' = pc
    }

    // an instruction only proceed pc + 1 if X = 0
    instr bnz X, Y: label {
        pc' = (1 - XIsZero) * (Y) + XIsZero * (pc + 1)
    }

    instr assert_zero X {
        X = 0
    }

    constraints {
    }

    // the main function assigns the first prover input to A, increments it, decrements it, and loops forever
    function main {
        x0 <=X= ${ ("input", 0) };
        x1 <=Z= addi(x0, 1); // x1 = 1
        x0 <=Y= decr(x1); // x0 = 0
        x1 <=Y= incr(x0); // x1 = 1
        x0 <=Z= add(x1, x1); // x0 = 1 + 1
        x0 <=Z= addi(x0, 1); // x0 = 2 + 1
        x0 <=Z= addi(x0, -2); // x0 = 3 - 2
        x0 <=Z= sub(x0, x0); // x0 - x0 = 0
        assert_zero x0; // x0 == 0
        x1 <=X= ${ ("input", 1) };
        LOOP::
        x1 <=Z= addi(x1, -1);
        bnz x1, LOOP;
        assert_zero x1; // x1 == 0
        loop;
    }
}
