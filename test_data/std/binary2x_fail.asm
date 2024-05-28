use std::machines::binary::Binary2x;

/*
The "Binary" machine has a lower bound of 2**18 because of the byte lookup and
a block size of 4 rows.
That means that if the Main machine has degree D, the original Binary machine
can only be called D/4 times.
The Binary2x machine can be called D/2 times.
In the test below, the Main machine has degree 2**20 and the Binary machine is
called ~ 2/3 of the rows, so the Binary2x machine runs out of rows.
Therefore, the test should fail.
*/

machine Main with degree: 1048576 {
	Binary2x binary;

	reg pc[@pc];
	reg X[<=];
	reg Y[<=];
	reg Z[<=];
	reg tmp1;
	reg tmp2;
	reg tmp3;

	reg addr;

	// ================= binary/bitwise instructions =================

	instr and Y, Z -> X ~ binary.and;
	instr or Y, Z -> X ~ binary.or;
	instr xor Y, Z -> X ~ binary.xor;

	instr j l: label {
		pc' = l
	}

	function main {
		tmp1 <=X= 1;
		tmp2 <=X= 0;
	loop:
		tmp3 <=X= and(tmp1, tmp2);
		tmp2 <=X= xor(tmp1, tmp3);
		j loop;
	}
}
