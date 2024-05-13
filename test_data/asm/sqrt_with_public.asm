machine Square with degree: 8 {
    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg A;

    public N = A(7);

    instr j l: label {
        pc' = l
    }

    instr square X -> Y {
        Y = X * X
    }
	
    function main {
        A <=X= ${ std::prover::Query::Input(0) };
        A <== square(A);
    loop:
        j loop;
        return;
    }
}
