machine SubMachine with
    latch: latch,
    operation_id: operation_id
{
    col witness operation_id;
    col fixed latch = [1]*;

    operation add<0> x, y -> z;
    operation sub<1> z, x -> y;

    col witness x;
    col witness y;
    col witness z;
    z = y + x;
}

machine Main {
    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg Z[<=];
    reg W[<=];
    reg A;
    reg B;
    reg C;
    col witness tmp;

    SubMachine submachine;

    // these are merged into 1 link
    instr add X, Y -> Z link => Z = submachine.add(X, Y);
    instr sub_with_add X, Y -> Z link => X = submachine.add(Y, Z);
    instr addAB -> X link => X = submachine.add(A, B);

    // one of these will be merged into the previous link, the other will be separate
    instr add3 X, Y, Z -> W
        link => tmp = submachine.add(X, Y)
        link => W = submachine.add(tmp, Z);

    // these will be separate links, as we disallow merging links with next references due to a witgen limitation
    instr add_to_A X, Y link => A' = submachine.add(X, Y);
    instr add_BC_to_A link => A' = submachine.add(B, C);

    // these are merged into 1 link
    instr sub X, Y -> Z link => Z = submachine.sub(X, Y);
    instr add_with_sub X, Y -> Z link => Y = submachine.sub(Z, X);

    instr assert_eq X, Y { X = Y }

    function main {
        A <== add(2, 3);
        assert_eq A, 5;

        add_to_A 6, 7;
        assert_eq A, 13;

        A <== sub_with_add(6, 5);
        assert_eq A, 1;

        A <== sub(6, 5);
        assert_eq A, 1;

        B <=X= 20;
        C <== addAB();
        assert_eq C, 21;

        A <== add3(1, 2, 3);
        assert_eq A, 6;

        A <== add_with_sub(1, 2);
        assert_eq A, 3;

        return;
    }
}
// ANCHOR_END: main
