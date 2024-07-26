machine Binary with
    latch: latch
{
    // fails because incoming permutation links to block machine requires call_selectors
    operation add A, B -> C;
    col fixed latch = [1]*;

    col witness A;
    col witness B;
    col witness C;

    C = A + B;
}

machine Main with degree: 32 {
    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg Z[<=];
    reg A;

    Binary bin;

    // permutation into Binary
    instr add X, Y -> Z link ~> Z = bin.add(X, Y);

    instr assert_eq X, Y { X = Y }

    function main {
        A <== add(2,3);
        assert_eq A, 5;
        return;
    }
}
