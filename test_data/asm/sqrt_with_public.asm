machine Square with degree: 8 {
    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg A;

    // Expose the register value of A in the last time step
    public N = A(7);

    col fixed ISLAST = [0]* + [1];
    ISLAST * (N - A) = 0;

    instr square X -> Y {
        Y = X * X
    }
	
    function main {
        A <=X= ${ std::prelude::Query::Input(0, 1) };
        A <== square(A);
    }
}
