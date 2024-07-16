let N: int = 64;

machine SubVM with degree: N {
    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg Z[<=];
    reg A;
    reg B;

    instr sub X, Y -> Z { X - Y = Z }

    function sub x: field, y: field -> field {
        A <== sub(x, y);
        return A;
    }
}

machine AddVM with
    degree: N,
    latch: latch,
    operation_id: operation_id
{
    operation add<0> x,y -> z;

    let operation_id;
    col fixed latch = [1]*;

    col witness x;
    col witness y;
    col witness z;

    z = y + x;
}

machine Main with degree: N {
    SubVM subvm;
    AddVM addvm;

    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg Z[<=];
    reg A;
    reg B;
    reg C;

    instr add X, Y -> Z link => Z = addvm.add(X, Y);
    instr add_to_A X, Y link => A' = addvm.add(X, Y);
    instr addAB -> X link => X = addvm.add(A, B);
    instr addAB_to_C link => C' = addvm.add(A, B);
    instr addAB_to_A link => A' = addvm.add(A, B);
    instr sub_from_add X, Y -> Z link => X = addvm.add(Y, Z);
    instr sub_from_add_into_A X, Y link => X = addvm.add(Y, A');
    instr add5 X -> Z link => Z = addvm.add(X, 5);
    col fixed NUM(i) { 42 };
    instr add42 X -> Z link => Z = addvm.add(X, NUM);
    let arr = [1,2,3,4,5];
    instr add_arr_sum X -> Z link => Z = addvm.add(X, std::array::sum(arr));
    instr jump X link => pc' = addvm.add(pc, X);

    instr sub X, Y -> Z link => Z = subvm.sub(X, Y);
    instr sub_to_C X, Y link => C' = subvm.sub(X, Y);

    instr assert_eq X, Y { X = Y }

    function main {
        A <== add(2, 3);
        assert_eq A, 5;

        A <== add5(2);
        assert_eq A, 7;

        A <== add42(3);
        assert_eq A, 45;

        add_to_A 6, 7;
        assert_eq A, 13;

        A <== sub_from_add(6, 5);
        assert_eq A, 1;
        B <=X= 20;
        C <== addAB();
        assert_eq C, 21;

        A <=X= 2;
        B <=X= 3;
        addAB_to_C;
        assert_eq C, 5;

        A <== add_arr_sum(3);
        assert_eq A, 18;

        A <=X= 33;
        B <=X= 44;
        C <== sub(B, A);
        assert_eq C, 11;
        addAB_to_A;
        assert_eq A, 77;

        sub_to_C 6, 3;
        assert_eq C, 3;

        jump 3;
        assert_eq 0,1; // asserts will be jumped over
        assert_eq 0,1;
        A <=X= 42;
        assert_eq A, 42;

        return;
    }
}
