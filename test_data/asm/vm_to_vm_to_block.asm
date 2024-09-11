let N: int = 64;

machine Main with degree: N {
    Pythagoras pythagoras;

    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg Z[<=];
    reg A;

    instr pythagoras X, Y -> Z link => Z = pythagoras.pythagoras(X, Y);
    instr assert_eq X, Y { X = Y }

    function main {
        A <== pythagoras(3, 4);
        assert_eq A, 25;

        A <== pythagoras(4, 3);
        assert_eq A, 25;

        A <== pythagoras(1, 2);
        assert_eq A, 5;

        return;
    }
}


machine Pythagoras with degree: N {

    Arith arith;

    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg Z[<=];
    reg A;
    reg B;


    instr add X, Y -> Z link => Z = arith.add(X, Y);
    instr mul X, Y -> Z link => Z = arith.mul(X, Y);

    function pythagoras a: field, b: field -> field {
        A <== mul(a, a);
        B <== mul(b, b);
        A <== add(A, B);
        return A;
    }
}

machine Arith with
    latch: latch,
    operation_id: operation_id,
{

    operation add<0> x1, x2 -> y;
    operation mul<1> x1, x2 -> y;

    let latch: col = |i| 1;
    let operation_id;
    let x1;
    let x2;
    let y;

    y = operation_id * (x1 * x2) + (1 - operation_id) * (x1 + x2);
}
