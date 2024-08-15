machine Main with degree: 32 {
    Arith arith;

    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg Z[<=];
    reg A;
    col witness B;
    reg ADD;

    // basic external instructions
    instr add X, Y -> Z link ~> Z = arith::add(X, Y);
    instr sub X, Y -> Z link ~> Z = arith::sub(X, Y);
    instr assert_eq X, Y { X = Y }

    // instructions activating multiple permutations
    instr add_one X -> Y
    link ~> B = arith::add(X, 2)
    link ~> Y = arith::sub(B, 1)
    {
    }

    // multiple links with flags
    instr add_or_sub X, Y -> Z
    link if ADD ~> Z = arith::add(X, Y)
    link if (1 - ADD) ~> Z = arith::sub(X, Y);

    function main {
        A <== add(1, 1);
        A <== add(A, 1);
        A <== sub(A, 1);
        assert_eq A, 2;
        A <== add_one(A);
        assert_eq A, 3;

        ADD <=X= 1;
        A <== add_or_sub(2,5);
        assert_eq A, 7;
        ADD <=X= 0;
        A <== add_or_sub(A,5);
        assert_eq A, 2;

        return;
    }
}

machine Arith with
    latch: latch,
    operation_id: operation_id,
    call_selectors: selectors
{
    operation add<0> x, y -> z;

    operation sub<1> z, x -> y;

    col witness operation_id;
    col fixed latch = [1]*;
    col witness x;
    col witness y;
    col witness z;
    z = x + y;
}
