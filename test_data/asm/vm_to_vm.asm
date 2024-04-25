machine Main with degree: 256 {

    VM vm;

    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg Z[<=];
    reg A;

    instr add X, Y -> Z = vm.add;
    instr sub X, Y -> Z = vm.sub;
    instr assert_eq X, Y { X = Y }

    function main {
        A <== add(1, 1);
        A <== add(A, 1);
        A <== sub(A, 1);
        assert_eq A, 2;
        return;
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
        A <== add(x, y);
        return A;
    }

    function sub x: field, y: field -> field {
        A <== sub(x, y);
        return A;
    }
}
