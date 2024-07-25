/// Binary machine that works on chunks of 4 bits,
/// to be used with permutation lookups.
machine Binary4 with
    latch: latch,
    operation_id: operation_id,
    // Allow this machine to be connected via a permutation
    call_selectors: sel,
{
    operation or<0> A, B -> C;
    col witness operation_id;
    col fixed latch(i) { if (i % 8) == 7 { 1 } else { 0 } };

    // check that we can reference the call_selectors
    let sum_sel = std::array::sum(sel);

    col fixed FACTOR(i) { 1 << (((i + 1) % 8) * 4) };

    let a = |i| i % 16;
    col fixed P_A(i) { a(i) };
    let b = |i| (i >> 4) % 16;
    col fixed P_B(i) { b(i) };
    col fixed P_C(i) { (a(i) | b(i)) & 0xf };

    col witness A_byte;
    col witness B_byte;
    col witness C_byte;

    col witness A;
    col witness B;
    col witness C;

    A' = A * (1 - latch) + A_byte * FACTOR;
    B' = B * (1 - latch) + B_byte * FACTOR;
    C' = C * (1 - latch) + C_byte * FACTOR;

    [A_byte, B_byte, C_byte] in [P_A, P_B, P_C];
}