machine Main {

    VM vm;

    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg Z[<=];
    reg A;

    instr add X, Y -> Z = vm.add
    instr sub X, Y -> Z = vm.sub
    instr assert_eq X, Y { X = Y }

    // this should be the identity function: x + 1 - 1 = x
    function main {
        A <=Z= add(1, 1);
        A <=Z= sub(A, 1);
        assert_eq A, 1;
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

    function add x: field, y: field -> field {
        A <=Z= add(x, y);
        return A;
    }

    function sub x: field, y: field -> field {
        A <=Z= sub(x, y);
        return A;
    }
}