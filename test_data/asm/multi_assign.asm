machine MultiAssign with degree: 8 {
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

    function main {
        A <=X= ${ std::prelude::Query::DataIdentifier(1, 0) };
        A <=Y= A - 7;
        assert_zero A;
        return;
    }
}
