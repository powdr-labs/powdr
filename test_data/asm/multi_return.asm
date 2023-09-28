machine MultiAssign {
    degree 16;

    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg Z[<=];
    reg A;
    reg B;

    instr assert_eq X, Y { X = Y }

    instr square_and_double X -> Y, Z {
        Y = X * X,
        Z = 2 * X
    }

    function main {

        // Different ways of expressing the same thing...
        A, B <== square_and_double(3);
        A, B <=Y,Z= square_and_double(3);
        A, B <=Y,_= square_and_double(3);
        A, B <=_,Z= square_and_double(3);

        assert_eq A, 9;
        assert_eq B, 6;
 
        return;
    }
}