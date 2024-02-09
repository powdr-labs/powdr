use std::convert::int;
use std::utils::cross_product;

machine Binary(latch, operation_id) {

    // lower bound degree is 262144

    operation and<0> A, B -> C;

    operation or<1> A, B -> C;

    operation xor<2> A, B -> C;

    col witness operation_id;

    col fixed latch(i) { if (i % 4) == 3 { 1 } else { 0 } };
    col fixed FACTOR(i) { 1 << (((i + 1) % 4) * 8) };

    // TOOD would be nice with destructuring assignment for arrays.
    let inputs: (int -> int)[] = cross_product([3, 256, 256]);
    let a = inputs[2];
    let b = inputs[1];
    let op = inputs[0];
    col fixed P_A(i) { a(i) };
    col fixed P_B(i) { b(i) };
    col fixed P_operation(i) { op(i)};
    col fixed P_C(i) {
        match op(i) {
            0 => a(i) & b(i),
            1 => a(i) | b(i),
            2 => a(i) ^ b(i),
        }
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