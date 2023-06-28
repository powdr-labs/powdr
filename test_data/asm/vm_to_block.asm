machine Arith {

    degree 8;

    operation noop _input_0, _input_1 -> _output_0 {
    }

    operation add_plus_three _input_0, _input_1 -> _output_0 {
    }

    operation add_plus_seven _input_0, _input_1 -> _output_0 {
    }

    constraints {
        col fixed latch = [1]*;
        col fixed OPERATION = [0, 1, 2] + [2]*;
        col fixed RESULT = [0, 3, 7] + [7]*;
        col witness plus;
        { _operation_id, plus } in { OPERATION, RESULT };
        _output_0 = _input_0 + _input_1 + plus;
    }
}

machine Main {

    degree 8;

    Arith arith;

    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg Z[<=];
    reg A;

    instr add_plus_three X, Y -> Z = arith.add_plus_three
    instr add_plus_seven X, Y -> Z = arith.add_plus_seven

    // this is the identity function: x + 1 - 1 = x
    operation main x: field {
        A <=Z= add_plus_three(x, 1);
        A <=Z= add_plus_seven(A, 1);
        return;
    }
}