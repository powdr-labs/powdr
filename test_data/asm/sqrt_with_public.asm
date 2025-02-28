machine Square with degree: 8 {
    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg A;

    // Expose the register value of A in the last time step
    public N = A(7);

    // TODO: this example isn't re-written using the syntax of public references,
    // because it's run on halo2, which we haven't adapted to accept public references.

    instr square X -> Y {
        Y = X * X
    }
	
    function main {
        A <=X= ${ std::prelude::Query::Input(0, 1) };
        A <== square(A);
    }
}
