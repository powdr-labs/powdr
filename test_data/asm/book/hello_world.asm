machine HelloWorld with degree: 8 {
    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg A;

    instr decr X -> Y {
        Y = X - 1
    }

    instr assert_zero X {
        X = 0
    }

    function main {
        // assign the first prover input to A
        A <=X= ${ std::prelude::Query::Input(0, 1) };

        // decrement A
        A <== decr(A);

        // assert that A is zero
        assert_zero A;

        return;
    }
}
