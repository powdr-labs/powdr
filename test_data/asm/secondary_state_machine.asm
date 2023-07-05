reg pc[@pc];
reg X[<=];
reg Y[<=];
reg A;
reg B;


pil{
    // Binary SM for 2 4-bit numbers
    // 0: AND
    // 1: OR
    // 2: XOR
	macro is_nonzero(X) { match X { 0 => 0, _ => 1, } };
	macro is_zero(X) { 1 - is_nonzero(X) };

	col fixed binary_RESET(i) { is_zero((i % 2) - 1) };
	col fixed binary_FACTOR(i) { 1 << (((i + 1) % 2) * 4) };

	col fixed binary_P_A(i) { i % 16 };
	col fixed binary_P_B(i) {
        (binary_P_A(i) + 1) & 0xf
    };

	col witness binary_A_byte;
	col witness binary_B_byte;

	col witness binary_A;
	col witness binary_B;

	binary_A' = binary_A * (1 - binary_RESET) + binary_A_byte * binary_FACTOR;
	binary_B' = binary_B * (1 - binary_RESET) + binary_B_byte * binary_FACTOR;

	{binary_A_byte, binary_B_byte} in {binary_P_A, binary_P_B};
}


instr and Y -> X {
    {Y, X} in binary_RESET { binary_A, binary_B }
}
instr loop { pc' = pc }

// Set input
A <=X= 0x12;
B <=X= 0xff;

B <=X= and(A);

loop;