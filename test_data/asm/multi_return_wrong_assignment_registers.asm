machine MultiAssign {
    degree 16;

    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg Z[<=];
    reg A;
    reg B;

    instr square_and_double X -> Y, Z {
        Y = X * X,
        Z = 2 * X
    }

    function main {

        // Should be using assignment registers Y, Z
        A, B <=Z,Y= square_and_double(3);
 
        return;
    }
}