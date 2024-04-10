machine Binary with
    latch: latch,
    operation_id: operation_id,
{
    col fixed FACTOR(i) { 1 << (((i + 1) % 4) * 8) };

    operation or<0> A, B -> C;
    call_selectors sel;

    col witness operation_id;
    col fixed latch(i) { if (i % 4) == 3 { 1 } else { 0 } };

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

machine Binary4 with
    latch: latch,
    operation_id: operation_id,
{
    Binary bin;

    operation or4<0> A, B, C, D -> E;
    call_selectors sel;

    // Permutation links to Binary machine.
    // Only enable the links in rows that have been 'used' by a call into this machine.
    let used = std::array::sum(sel);
    std::utils::force_bool(used);
    link used ~> bin.or A, B -> X;
    link used ~> bin.or C, D -> Y;
    link used ~> bin.or X, Y -> E;

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

machine Main {
    degree 65536;

    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg Z[<=];
    reg W[<=];
    reg R[<=];
    reg A;
    reg B;

    Binary bin;
    Binary4 bin4;

    // two permutations to machine bin
    instr or X, Y -> Z ~ bin.or;
    instr or_into_B X, Y ~ bin.or X, Y -> B';

    // permutation to machine bin4
    instr or4 X, Y, Z, W -> R ~ bin4.or4;

    instr assert_eq X, Y { X = Y }

    function main {
        A <== or(2,3);
        assert_eq A, 3;
        A <== or(1,2);
        assert_eq A, 3;

        or_into_B 2,3;
        assert_eq B, 3;
        or_into_B 1,2;
        assert_eq B, 3;

        A <== or4(1,2,4,8);
        assert_eq A, 15;
        A <== or4(15,16,32,64);
        assert_eq A, 127;

        return;
    }
}
