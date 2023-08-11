machine NovaZero {

    degree 20;

    // this simple machine does not have submachines

    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg Z[<=];
    reg x0;
    reg x1;

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
        Y = x_b0 + x_b1 * 2 + x_b2 * 2**2 + x_b3 * 2**3,
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
        x1 <=Z= sub(x1, x1); // x1 - x1 = 0
        assert_zero x1; // x1 == 0
        loop;
    }
}
