machine Main {

    VM vm;

    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg Z[<=];
    reg A;

    instr add X, Y -> Z = vm.add
    instr sub X, Y -> Z = vm.sub

    // this should be the identity function: x + 1 - 1 = x
    operation main x: field -> field {
        A <=Z= add(x, 1);
        A <=Z= sub(A, 1);
        return A;
    }
}

machine VM {

    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg Z[<=];
    reg A;
    reg B;

    instr add X, Y -> Z { X + Y = Z }

    instr sub X, Y -> Z { X - Y = Z }

    operation add x: field, y: field -> field {
        A <=Z= add(x, y);
        return A;
    }

    operation sub x: field, y: field -> field {
        A <=Z= sub(x, y);
        return A;
    }
}