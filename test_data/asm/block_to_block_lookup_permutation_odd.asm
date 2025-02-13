machine Or with 
  latch: latch, 
  degree: 256,
{
    col fixed latch = [1]*;

    operation or P_A, P_B -> P_C;

    let a = |i| i % 16;
    col fixed P_A(i) { a(i) };
    let b = |i| (i >> 4) % 16;
    col fixed P_B(i) { b(i) };
    col fixed P_C(i) { (a(i) | b(i)) & 0xf };
}

machine Binary4 with
    degree: 32,
    latch: latch,
    operation_id: operation_id,
    call_selectors: sel,
{
    Or or;
    operation or4<0> A, B, C, D -> E;

    // Permutation links to Binary machine.
    // Only enable the links in rows that have been 'used' by a call into this machine.
    let used = std::array::sum(sel);
    std::utils::force_bool(used);
    link if used => X = or.or(A, B);
    link if used => Y = or.or(C, D);
    link if used => E = or.or(X, Y);

    col fixed operation_id = [0]*;
    col fixed latch = [1]*;

    col witness A;
    col witness B;
    col witness C;
    col witness D;
    col witness E;
    col witness X;
    col witness Y;
}

machine Main with degree: 256 {
    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg Z[<=];
    reg W[<=];
    reg R[<=];
    reg A;
    reg B;

    Binary4 bin;
    Or or;

    instr or X, Y -> Z link => Z = or.or(X, Y);

    // permutation to machine bin4
    instr or4 X, Y, Z, W -> R link ~> R = bin.or4(X, Y, Z, W);

    instr assert_eq X, Y { X = Y }

    function main {
        A <== or(2,3);
        assert_eq A, 3;
        A <== or(1,2);
        assert_eq A, 3;

        A <== or4(1,2,4,8);
        assert_eq A, 15;
        A <== or4(15,16,32,64);
        assert_eq A, 127;

        return;
    }
}
