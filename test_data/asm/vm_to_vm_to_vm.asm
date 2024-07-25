machine Main with degree: 16 {
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

machine Pythagoras {

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

machine Arith {

    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg Z[<=];
    reg A;

    instr add X, Y -> Z { X + Y = Z }
    instr mul X, Y -> Z { X * Y = Z }

    function add x: field, y: field -> field {
        A <== add(x, y);
        return A;
    }

    function mul x: field, y: field -> field {
        A <== mul(x, y);
        return A;
    }
}
