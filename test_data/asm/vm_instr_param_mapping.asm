machine SubVM {
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
    latch: latch,
    operation_id: operation_id
{
    operation add<0> x,y -> z;

    col witness operation_id;
    col fixed latch = [1]*;

    col witness x;
    col witness y;
    col witness z;

    z = y + x;
}

machine Main with degree: 256 {
    SubVM subvm;
    AddVM addvm;

    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg Z[<=];
    reg A;
    reg B;
    reg C;

    instr add X, Y -> Z = addvm.add;
    instr add_to_A X, Y = addvm.add X, Y -> A';
    instr addAB -> X = addvm.add A, B -> X;
    instr addAB_to_C = addvm.add A, B -> C';
    instr addAB_to_A = addvm.add A, B -> A';
    instr sub_from_add X, Y -> Z = addvm.add Y, Z -> X;
    instr sub_from_add_into_A X, Y = addvm.add Y, A' -> X;
    instr add5 X -> Z = addvm.add X, 5 -> Z;
    col fixed NUM(i) { 42 };
    instr add42 X -> Z = addvm.add X, NUM -> Z;
    let arr = [1,2,3,4,5];
    instr add_arr_sum X -> Z = addvm.add X, std::array::sum(arr) -> Z;
    instr jump X = addvm.add pc, X -> pc';

    instr sub X, Y -> Z = subvm.sub;
    instr sub_to_C X, Y = subvm.sub X, Y -> C';

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
