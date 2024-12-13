mod binary4;
use binary4::Main as Binary4;

machine Main with degree: 128 {
    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg Z[<=];
    reg A;
    reg B;

    Binary4 bin(128, 128);

    instr or X, Y -> Z link ~> Z = bin.or(X, Y);

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
