machine Binary(latch, _) {
    // fails because incoming permutation links to block machine requires call_selectors
    operation add A, B -> C;
    col fixed latch = [1]*;

    col witness A;
    col witness B;
    col witness C;

    C = A + B;
}

machine Main {
    degree 65536;

    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg Z[<=];
    reg A;

    Binary bin;

    // permutation into Binary
    instr add X, Y -> Z ~ bin.add;

    instr assert_eq X, Y { X = Y }

    function main {
        A <== add(2,3);
        assert_eq A, 5;
        return;
    }
}
