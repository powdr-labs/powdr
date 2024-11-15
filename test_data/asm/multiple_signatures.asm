machine Add with
    degree: 32,
    latch: latch,
    operation_id: operation_id,
    call_selectors: sel,
{
    col witness A, B, C;
    A + B = C;

    // The compiler enforces that there is an operation ID if there are
    // multiple operations, even though we want the constraints to be
    // the same in both cases...
    col witness operation_id;
    let latch = 1;

    // A and B provided => C will be the sum.
    operation add<0> A, B -> C;
    // A and C provided => B must equal C - A, for A + B = C to be valid.
    operation sub<0> C, A -> B;
}

machine Main with degree: 32 {
    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg Z[<=];
    reg A;
    reg B;

    Add add(32, 32);

    instr add X, Y -> Z link ~> Z = add.add(X, Y);
    instr sub X, Y -> Z link ~> Z = add.sub(X, Y);

    instr assert_eq X, Y { X = Y }

    function main {
        A <== add(3, 2);
        assert_eq A, 5;

        A <== sub(3, 2);
        assert_eq A, 1;

        return;
    }
}
