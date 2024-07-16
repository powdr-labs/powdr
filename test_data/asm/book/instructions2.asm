// ANCHOR: submachine
machine SubMachine with
    latch: latch,
    operation_id: operation_id
{
    let operation_id;
    col fixed latch = [1]*;

    operation add<0> x, y -> z;
    operation sub<1> x, y -> z;

    col witness x;
    col witness y;
    col witness z;
    z = (1 - operation_id) * (x + y) + operation_id * (x - y);
}
// ANCHOR_END: submachine

// ANCHOR: main
machine Main with degree: 16 {
    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg Z[<=];
    reg W[<=];
    reg A;
    col witness B;
    col witness C;

    SubMachine submachine;

    // multiple links can be activated by a single instruction,
    // witness columns can be used for temporary values,
    // and additional constraints can be used
    instr double_then_mul X, Y -> Z
        link => B = submachine.add(X, X)
        link => C = submachine.add(Y, Y)
    {
        Z = B * C
    }

    // links activated conditional on a boolean flag
    instr add_or_sub W, X, Y -> Z
        link if W => Z = submachine.add(X, Y)
        link if (1 - W) => Z = submachine.sub(X, Y);

    instr assert_eq X, Y { X = Y }

    function main {
        A <== double_then_mul(3, 2);
        assert_eq A, 24;

        A <== add_or_sub(1, 3, 2);
        assert_eq A, 5;
        A <== add_or_sub(0, 3, 2);
        assert_eq A, 1;

        return;
    }
}
