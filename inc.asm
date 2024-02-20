machine Main {
	degree 8;

	reg pc[@pc];
	reg X[<=];
	reg Y[<=];
	reg A;

	instr inc {
		A' = A + 1
	}

	instr dec {
		A' = A - 1
	}

	instr assert_eq X, Y {
		X = Y
	}

	function main {
		A <=X= 1;
		inc;
		dec;
		assert_eq A, 1;
	}
}
