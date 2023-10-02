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

    instr assert_zero X {
        X = 0
    }

    // the main function assigns the first prover input to A, increments it, decrements it, and loops forever
    function main {
        A <=X= ${ ("input", 0) };
        A <== incr(A);
        A <== decr(A);
        assert_zero A;
        return;
    }
}