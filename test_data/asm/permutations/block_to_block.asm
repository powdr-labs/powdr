mod binary4;
use binary4::Binary4;

machine Binary4x with
    latch: latch,
    operation_id: operation_id,
    call_selectors: sel,
{
    Binary4 bin(256, 256);

    operation or4<0> A, B, C, D -> E;

    // Permutation links to Binary machine.
    // Only enable the links in rows that have been 'used' by a call into this machine.
    let used = std::array::sum(sel);
    std::utils::force_bool(used);
    link if used ~> X = bin.or(A, B);
    link if used ~> Y = bin.or(C, D);
    link if used ~> E = bin.or(X, Y);

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

    Binary4 bin(256, 256);
    Binary4x bin4(256, 256);

    // two permutations to machine bin
    instr or X, Y -> Z link ~> Z = bin.or(X, Y);
    instr or_into_B X, Y link ~> B' = bin.or(X, Y);

    // permutation to machine bin4
    instr or4 X, Y, Z, W -> R link ~> R = bin4.or4(X, Y, Z, W);

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
