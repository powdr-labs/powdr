machine NotUsed {
    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg Z[<=];
// ANCHOR: local
instr add X, Y -> Z {
    X + Y = Z
}
// ANCHOR_END: local
}

// ANCHOR: submachine
machine SubMachine with
    latch: latch,
    operation_id: operation_id
{
    col witness operation_id;
    col fixed latch = [1]*;

    operation add<0> x, y -> z;

    col witness x;
    col witness y;
    col witness z;
    z = y + x;
}
// ANCHOR_END: submachine

// ANCHOR: main
machine Main {
    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg Z[<=];
    reg A;
    reg B;
    reg C;

// ANCHOR: trivial
    SubMachine submachine;

    instr add X, Y -> Z = submachine.add; // - trivial usage, equivalent to:
                                          //   instr add X, Y -> Z = submachine.add X, Y -> Z;
// ANCHOR_END: trivial
    instr add_to_A X, Y = submachine.add X, Y -> A';// - output to a regular register
    instr addAB -> X = submachine.add A, B -> X;    // - inputs from regular registers
    instr addAB_to_C = submachine.add A, B -> C';   // - inputs and output from regular registers
    instr addAB_to_A = submachine.add A, B -> A';   // - reusing an input register as output
    instr sub X, Y -> Z = submachine.add Y, Z -> X; // - swapping input/output
    // any expression can be used in the external call
    instr add5 X -> Z = submachine.add X, 3+2 -> Z; // - literal expression as argument
    col fixed STEP(i) { i };
    instr add_current_time_step X -> Z = submachine.add X, STEP -> Z;// - columns can be referenced
    let arr = [1,2,3,4,5];                          // - functions can be used
    instr add_arr_sum X -> Z = submachine.add X, std::array::sum(arr) -> Z;

    instr assert_eq X, Y { X = Y }

    function main {
        A <== add(2, 3);
        assert_eq A, 5;
        add_to_A 6, 7;
        assert_eq A, 13;

        A <== sub(6, 5);
        assert_eq A, 1;
        B <=X= 20;
        C <== addAB();
        assert_eq C, 21;

        A <=X= 2;
        B <=X= 3;
        addAB_to_C;
        assert_eq C, 5;

        A <=X= 33;
        B <=X= 44;
        addAB_to_A;
        assert_eq A, 77;

        A <== add5(2);
        assert_eq A, 7;
        A <== add_arr_sum(3);
        assert_eq A, 18;

        // Note that the result of this operation depends on when it executed
        A <== add_current_time_step(42);
        B <== add_current_time_step(42);
        assert_eq B - A, 1;

        return;
    }
}
// ANCHOR_END: main
