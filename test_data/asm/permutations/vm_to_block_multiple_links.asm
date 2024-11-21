machine Main with degree: 32 {
    Arith arith;

    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg Z[<=];
    reg A;
    col witness B;
    reg ADD;

    // basic external instructions
    instr add X, Y -> Z link ~> Z = arith.add(X, Y);
    instr sub X, Y -> Z link ~> Z = arith.sub(X, Y);
    instr assert_eq X, Y { X = Y }

    function main {
        A <== add(1, 1);
        A <== add(A, 1);
        A <== sub(A, 1);
        assert_eq A, 2;

        return;
    }
}

machine Arith with
    latch: latch,
    operation_id: operation_id,
    call_selectors: selectors
{
    operation add<0> x, y -> z;

    operation sub<1> z, x -> y;

    col witness operation_id;
    col fixed latch = [1]*;
    col witness x;
    col witness y;
    col witness z;
    z = x + y;
}
