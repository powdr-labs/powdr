machine Zero {
	degree 2;

	reg pc[@pc];
	reg X[<=];

	reg A;

	instr ASSERT_IS_ZERO_A { A = 0 }

	function main {
		A <=X= ${ ("input", 0) }; // pc = 0 => input[0] = X && X = A
		ASSERT_IS_ZERO_A;         // pc = 1 => A = 0
	}
}
