machine Arith {

    // some constraints to chose the right operation
    constraints {
        col witness XInv;
        col witness XIsZero;
        XIsZero = 1 - _operation_id * XInv;
        XIsZero * _operation_id = 0;
        XIsZero * (1 - XIsZero) = 0;
    }

    operation add _input_0, _input_1 -> _output_0 {
    }

    operation sub _input_0, _input_1 -> _output_0 {
    }

    constraints {
        col fixed _latch = [1]*;
        _output_0 = XIsZero * (_input_0 + _input_1) + (1 - XIsZero) * (_input_0 - _input_1);
    }
}

machine Main {
    Arith arith;

    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg Z[<=];
    reg A;

    instr add X, Y -> Z = arith.add
    instr sub X, Y -> Z = arith.sub

    // this is the identity function: x + 1 - 1 = x
    operation main x: field -> field {
        A <=Z= add(x, 1);
        A <=Z= sub(A, 1);
        return A;
    }
}