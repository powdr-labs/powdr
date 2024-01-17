use std::utils::unchanged_until;

machine Shift(latch, operation_id) {
    // lower bound degree is 262144

    operation shl<0> A, B -> C;

    operation shr<1> A, B -> C;

    col witness operation_id;

    col fixed latch(i) { (i % 4) == 3 };
    col fixed FACTOR_ROW(i) { (i + 1) % 4 };
    col fixed FACTOR(i) { 1 << (((i + 1) % 4) * 8) };

    col fixed P_A(i) { i % 256 };
    col fixed P_B(i) { (i / 256) % 32 };
    col fixed P_ROW(i) { (i / (256 * 32)) % 4 };
    col fixed P_operation(i) { (i / (256 * 32 * 4)) % 2 };
    col fixed P_C(i) {
        match P_operation(i) {
            0 => (P_A(i) << (P_B(i) + (P_ROW(i) * 8))),
            1 => (P_A(i) << (P_ROW(i) * 8)) >> P_B(i),
        } & 0xffffffff
    };

    col witness A_byte;
    col witness C_part;

    col witness A;
    col witness B;
    col witness C;

    A' = A * (1 - latch) + A_byte * FACTOR;
    unchanged_until(B, latch);
    C' = C * (1 - latch) + C_part;

    // TODO this way, we cannot prove anything that shifts by more than 31 bits.
    {operation_id', A_byte, B', FACTOR_ROW, C_part} in {P_operation, P_A, P_B, P_ROW, P_C};
}