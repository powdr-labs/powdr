machine Binary(latch, operation_id) {
    operation or<0> A, B -> C;
    col witness operation_id;
    col fixed latch(i) { if (i % 4) == 3 { 1 } else { 0 } };

    // check that we can reference the call_selectors
    call_selectors sel;
    let sum_sel = std::array::sum(sel);

    col fixed FACTOR(i) { 1 << (((i + 1) % 4) * 8) };

    let a = |i| i % 256;
    col fixed P_A(i) { a(i) };
    let b = |i| (i >> 8) % 256;
    col fixed P_B(i) { b(i) };
    col fixed P_C(i) { (a(i) | b(i)) & 0xff };

    col witness A_byte;
    col witness B_byte;
    col witness C_byte;

    col witness A;
    col witness B;
    col witness C;

    A' = A * (1 - latch) + A_byte * FACTOR;
    B' = B * (1 - latch) + B_byte * FACTOR;
    C' = C * (1 - latch) + C_byte * FACTOR;

    {A_byte, B_byte, C_byte} in {P_A, P_B, P_C};
}

machine Main {
    degree 65536;

    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg Z[<=];
    reg A;

    Binary bin;

    // permutation to machine bin
    instr or X, Y -> Z ~ bin.or;

    instr assert_eq X, Y { X = Y }

    function main {
        A <== or(2,3);
        assert_eq A, 3;
        A <== or(1,2);
        assert_eq A, 3;
        A <== or(3,4);
        assert_eq A, 7;

        return;
    }
}
