machine Square with degree: 8 {
    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg A;

    // Expose the register value of A in the last time step
    public N = A(7);

    instr square X -> Y {
        Y = X * X
    }
	
    function main {
        A <=X= ${ std::prelude::Query::DataIdentifier(1, 0) };
        A <== square(A);
    }
}
