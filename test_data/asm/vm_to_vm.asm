machine Main {

    inputs 1;
    outputs 1;

    Add add;
    VM vm;

    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg Z[<=];
    reg A;

    instr loop { pc' = pc }

    instr mul X, Y -> Z = vm.mul
    instr add X, Y -> Z = vm.add

    // this should be the identity function: x + 1 - 1 = x
    operation main {
        load_input_0_into_A;
        A <=Z= add(A, 1);
        A <=Z= sub(A, 1);
        store_A_into_output_0;
    }
}

machine VM {

    inputs 2;
    outputs 1;

    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg Z[<=];
    reg A;
    reg B;

    instr add X, Y -> Z { X + Y = Z }

    instr sub X, Y -> Z { X - Y = Z }

    operation add {
        load_input_0_into_A;
        load_input_1_into_B;
        A <=Z= add(A, B);
        store_A_into_output_0;
    }

    operation sub {
        load_input_0_into_A;
        load_input_1_into_B;
        A <=Z= sub(A, B);
        store_A_into_output_0;
    }
}