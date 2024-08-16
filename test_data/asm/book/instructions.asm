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
machine Main with degree: 32 {
    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg Z[<=];
    reg A;
    reg B;
    reg C;

// ANCHOR: trivial
    SubMachine submachine;

    instr add X, Y -> Z link => Z = submachine.add(X, Y); // - trivial usage: only instruction inputs/outputs used in the call
// ANCHOR_END: trivial
    instr add_to_A X, Y link => A' = submachine.add(X, Y);// - output to a regular register
    instr addAB -> X link => X = submachine.add(A, B);    // - inputs from regular registers
    instr addAB_to_C link => C' = submachine.add(A, B);   // - inputs and output from regular registers
    instr addAB_to_A link => A' = submachine.add(A, B);   // - reusing an input register as output
    instr sub X, Y -> Z link => X = submachine.add(Y, Z); // - swapping input/output
    // expressions can also be used as call parameters
    instr add5 X -> Z link => Z = submachine.add(X, 3+2); // - literal expression as argument
    col fixed STEP(i) { i };
    instr add_current_time_step X -> Z link => Z = submachine.add(X, STEP);// - machine columns can be referenced
    let arr = [1,2,3,4,5];                          // - functions can be used
    instr add_arr_sum X -> Z link => Z = submachine.add(X, std::array::sum(arr));

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

        // Note that the result of this operation depends on when it executed (STEP column)
        A <== add_current_time_step(42);
        B <== add_current_time_step(42);
        assert_eq B - A, 1;

        return;
    }
}
// ANCHOR_END: main
