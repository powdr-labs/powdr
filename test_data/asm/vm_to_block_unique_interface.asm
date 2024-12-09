machine Binary with
    degree: 32,
    latch: latch,
    operation_id: operation_id,
{

    operation and<0> x, y -> z;

    operation or<1> x, y -> z;

    col witness operation_id;
    col fixed latch = [1]*;
    col witness x;
    col witness y;
    col witness z;
    col fixed P_FUNCTION = [0, 0, 0, 0, 1, 1, 1, 1] + [1]*;
    col fixed P_X = [0, 0, 1, 1, 0, 0, 1, 1] + [1]*;
    col fixed P_Y = [0, 1, 0, 1, 0, 1, 0, 1] + [1]*;
    col fixed P_Z = [0, 0, 0, 1, 0, 1, 1, 1] + [1]*;
    [ operation_id, x, y, z ] in [ P_FUNCTION, P_X, P_Y, P_Z ];
}

machine Arith with
    degree: 32,
    latch: latch,
    operation_id: operation_id,
{

    operation add<0> x, y -> z;

    operation sub<1> x, y -> z;

    col witness operation_id;
    col fixed latch = [1]*;
    col witness x;
    col witness y;
    col witness z;
    z = (1 - operation_id) * (x + y) + operation_id * (x - y);
}

machine Main with degree: 32 {
    Arith arith;
    Binary binary;

    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg Z[<=];
    reg A;

    instr add X, Y -> Z link => Z = arith.add(X, Y);
    instr sub X, Y -> Z link => Z = arith.sub(X, Y);
    instr and X, Y -> Z link => Z = binary.and(X, Y);
    instr or X, Y -> Z link => Z = binary.or(X, Y);
    instr assert_eq X, Y { X = Y }

    function main {
        A <== add(2, 1);
        A <== sub(A, 1);
        assert_eq A, 2;
        A <== and(1, 1);
        A <== or(A, 0);
        assert_eq A, 1;
        return;
    }
}
