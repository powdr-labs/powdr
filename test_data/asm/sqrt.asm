machine Sqrt(latch, operation_id) {

    operation sqrt<0> x -> y;

    col fixed operation_id = [0]*;
    col fixed latch = [1]*;
    // Only works for small results, to keep the degree of this example small.
    col fixed range(i) { i % 8 };
    col witness x;

    // Witness generation is not smart enough to figure out that
    // there is a unique witness, so we provide it as a hint.
    // This is a dummy example that hard-codes the answer for an input of 4.
    // Once we have a sqrt function that we can run to compute the query result,
    // this can be used to compute the hint from x.
    col witness y(i) query ("hint", 2);
    
    y * y = x;
    
    // Note that this is required to make the witness unique
    // (y := -y would also satisfy y * y = x, but we want the positive solution).
    { y } in { range };
}


machine Main {
    degree 8;

    Sqrt sqrt;

    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg A;

    col witness XInv;
    col witness XIsZero;
    XIsZero  = 1 - X * XInv;
    XIsZero * X = 0;
    XIsZero * (1 - XIsZero) = 0;

    instr assert_zero X { XIsZero = 1 }

    instr sqrt X -> Y = sqrt.sqrt;


    function main {

        A <== sqrt(4);
        assert_zero A - 2;

        return;
    }
}