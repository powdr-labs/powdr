machine HelloWorld {

    degree 8;

    // this simple machine does not have submachines

    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg A;

    instr incr X -> Y {
        Y = X + 1
    }

    instr decr X -> Y {
        Y = X - 1
    }

    // an instruction to loop forever, as we must fill the whole execution trace
    instr loop {
        pc' = pc
    }

    instr assert_zero X {
        X = 0
    }

    constraints {
        // in this machine, we do not add more constraints
    }

    // the main function assigns the first prover input to A, increments it, decrements it, and loops forever
    function main {
        A <=X= ${ ("input", 0) };
        A <=Y= incr(A);
        A <=Y= decr(A);
        assert_zero A;
        loop;
    }
}