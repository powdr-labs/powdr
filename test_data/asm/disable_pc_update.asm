machine Main with degree: 128, pc_update_disabled {
    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg A;

    // custom pc update
    pol constant FIRST = [1] + [0]*;
    pc' = (1 - FIRST') * pc_update;

    instr assert_eq X, Y { X = Y }

    function main {
        A <=X= 2 + 3;
        assert_eq A, 5;
        A <=X= A + 2;
        assert_eq A, 7;

        return;
    }
}
