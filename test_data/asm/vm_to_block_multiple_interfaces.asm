// calls two functions in a submachine whose interface is different: one is `x, y, z` while the other one is `z, x, y`

machine Arith with
    degree: 8,
    latch: latch,
    operation_id: operation_id
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

machine Main with degree: 8 {
    Arith arith;

    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg Z[<=];
    reg A;

    instr add X, Y -> Z link => Z = arith::add(X, Y);
    instr sub X, Y -> Z link => Z = arith::sub(X, Y);
    instr assert_eq X, Y { X = Y }

    function main {
        A <== add(2, 1);
        A <== sub(A, 1);
        assert_eq A, 2;
        return;
    }
}
