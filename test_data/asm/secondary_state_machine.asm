reg pc[@pc];
reg X[<=];
reg A;
reg B;
reg C;


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
	col fixed binary_P_B(i) { (i >> 4) % 16 };
    col fixed binary_P_operation(i) { (i / (16 * 16)) % 3 };
	col fixed binary_P_C(i) {
        match binary_P_operation(i) {
            0 => binary_P_A(i) & binary_P_B(i),
            1 => binary_P_A(i) | binary_P_B(i),
            2 => binary_P_A(i) ^ binary_P_B(i),
        } & 0xf
    };

	col witness binary_A_byte;
	col witness binary_B_byte;
	col witness binary_C_byte;

	col witness binary_A;
	col witness binary_B;
	col witness binary_C;
    col witness binary_operation;

	binary_A' = binary_A * (1 - binary_RESET) + binary_A_byte * binary_FACTOR;
	binary_B' = binary_B * (1 - binary_RESET) + binary_B_byte * binary_FACTOR;
	binary_C' = binary_C * (1 - binary_RESET) + binary_C_byte * binary_FACTOR;
	(binary_operation' - binary_operation) * (1 - binary_RESET) = 0;

	{binary_operation', binary_A_byte, binary_B_byte, binary_C_byte} in {binary_P_operation, binary_P_A, binary_P_B, binary_P_C};
}


instr and {
    {A, B, C, 0} in binary_RESET { binary_A, binary_B, binary_C, binary_operation }
}

instr or {
    {A, B, C, 1} in binary_RESET { binary_A, binary_B, binary_C, binary_operation }
}

instr xor {
    {A, B, C, 2} in binary_RESET { binary_A, binary_B, binary_C, binary_operation }
}
instr loop { pc' = pc }

// Set input
A <=X= 0xf0;
B <=X= 0x11;
C <=X= 0;

and; // -> 0x10
or;  // -> 0xf1
xor; // -> 0xe1

loop;