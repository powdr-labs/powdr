machine Binary(latch, operation_id) {

    degree 262144;

    operation and<0> A, B -> C;

    operation or<1> A, B -> C;

    operation xor<2> A, B -> C;

    col witness operation_id;

    macro is_nonzero(X) { match X { 0 => 0, _ => 1, } };
    macro is_zero(X) { 1 - is_nonzero(X) };

    col fixed latch(i) { is_zero((i % 4) - 3) };
    col fixed FACTOR(i) { 1 << (((i + 1) % 4) * 8) };

    col fixed P_A(i) { i % 256 };
    col fixed P_B(i) { (i >> 8) % 256 };
    col fixed P_operation(i) { (i / (256 * 256)) % 3 };
    col fixed P_C(i) {
        match P_operation(i) {
            0 => P_A(i) & P_B(i),
            1 => P_A(i) | P_B(i),
            2 => P_A(i) ^ P_B(i),
        } & 0xff
    };

    col witness A_byte;
    col witness B_byte;
    col witness C_byte;

    col witness A;
    col witness B;
    col witness C;

    A' = A * (1 - latch) + A_byte * FACTOR;
    B' = B * (1 - latch) + B_byte * FACTOR;
    C' = C * (1 - latch) + C_byte * FACTOR;

    {operation_id', A_byte, B_byte, C_byte} in {P_operation, P_A, P_B, P_C};
}