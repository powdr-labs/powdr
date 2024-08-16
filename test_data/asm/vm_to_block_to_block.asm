machine Inc with
    degree: 8,
    latch: latch,
    operation_id: operation_id
{
    operation inc<0> x -> y;

    col witness operation_id;
    col fixed latch = [1]*;
    col witness x;
    col witness y;
    y = x + 1;
}

machine Assert1 with
    degree: 8,
    latch: latch,
    operation_id: operation_id
{
    Inc inc;

    operation assert1<0> x ->;

    // Increment x by calling into inc machine
    link => y = inc.inc(x);

    col witness operation_id;
    col fixed latch = [1]*;
    col witness x;
    col witness y;

    y = 2;
}

machine Main with degree: 8 {
    Assert1 assert1;

    reg pc[@pc];
    reg X[<=];
    reg A;

    instr assert1 X -> link => assert1.assert1(X);

    instr loop {
        pc' = pc
    }

    function main {
        assert1(1);
        loop;
    }
}
