machine Binary {
    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg Z[<=];
    reg A;

    instr add X, Y -> Z {
       Z = X + Y
    }

    function add a, b -> c {
        A <== add(a,b);
        return A;
    }
}

machine Main with degree: 65536 {
    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg Z[<=];
    reg A;
    reg B;

    Binary bin;

    // two permutations to bin machine
    instr add X, Y -> Z ~ bin.add;
    instr add_into_B X, Y ~ bin.add X, Y -> B';

    instr assert_eq X, Y { X = Y }

    function main {
        A <== add(2,3);
        assert_eq A, 3;
        A <== add(1,2);
        assert_eq A, 3;

        add_into_B 2,3;
        assert_eq B, 3;
        add_into_B 1,2;
        assert_eq B, 3;

        return;
    }
}
