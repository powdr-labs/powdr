machine Main {

    degree 256;

    Arith arith;

    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg Z[<=];
    reg A;

    instr add X, Y -> Z = arith.add
    instr mul X, Y -> Z = arith.mul
    instr assert_eq X, Y { X = Y }

    function main {
        A <== add(2, 1);
        A <== mul(A, 9);
        assert_eq A, 27;
        return;
    }
}

machine Arith(latch, operation_id) {

    operation add<0> x[0], x[1] -> y;
    operation mul<1> x[0], x[1] -> y;

    col fixed latch = [1]*;
    col witness operation_id;
    col witness x[2];
    col witness y;

    y = operation_id * (x[0] * x[1]) + (1 - operation_id) * (x[0] + x[1]);
}
