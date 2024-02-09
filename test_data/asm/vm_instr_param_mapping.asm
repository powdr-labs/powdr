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

machine AddVM(latch, operation_id) {
    operation add<0> x,y -> z;

    col witness operation_id;
    col fixed latch = [1]*;

    col witness x;
    col witness y;
    col witness z;

    z = y + x;
}

machine Main {

    degree 256;

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
    instr add_to_A X, Y = addvm.add X, Y -> A;
    instr sub_from_add X, Y -> Z = addvm.add Y, Z -> X;
    instr addAB -> X = addvm.add A, B -> X;
    instr addAB_to_C = addvm.add A, B -> C;
    instr addAB_to_A = addvm.add A, B -> A;

    instr sub X, Y -> Z = subvm.sub;
    instr sub_to_C X, Y = subvm.sub X, Y -> C;

    instr assert_eq X, Y { X = Y }

    function main {
        A <== add(2, 3);
        assert_eq A, 5;
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

        A <=X= 33;
        B <=X= 44;

        C <== sub(B, A);
        assert_eq C, 11;

        addAB_to_A;
        assert_eq A, 77;

        sub_to_C 6, 3;
        assert_eq C, 3;

        return;
    }
}
