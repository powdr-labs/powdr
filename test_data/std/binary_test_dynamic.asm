use std::convert::int;
use std::utils::cross_product;
use std::utils::unchanged_until;

// Binary for single bytes using an exhaustive table
machine ByteBinary with
    latch: latch,
    operation_id: operation_id
{
    operation run<0> P_operation, P_A, P_B -> P_C;

    col fixed latch = [1]*;
    col fixed operation_id = [0]*;

    let bit_counts = [256, 256, 3];
    let min_degree = std::array::product(bit_counts);
    std::check::assert(std::prover::degree() >= std::array::product(bit_counts), || "The binary machine needs at least 196608 rows to work.");
    // TODO would be nice with destructuring assignment for arrays.
    let inputs: (int -> int)[] = cross_product(bit_counts);
    let a = inputs[0];
    let b = inputs[1];
    let op = inputs[2];
    let P_A: col = a;
    let P_B: col = b;
    let P_operation: col = op;
    col fixed P_C(i) {
        match op(i) {
            0 => a(i) & b(i),
            1 => a(i) | b(i),
            2 => a(i) ^ b(i),
        }
    };
}

machine Binary with
    latch: latch,
    operation_id: operation_id,
    // Allow this machine to be connected via a permutation
    call_selectors: sel,
{

    ByteBinary byte_binary;

    operation and<0> A, B -> C;

    operation or<1> A, B -> C;

    operation xor<2> A, B -> C;

    col witness operation_id;
    unchanged_until(operation_id, latch);

    col fixed latch(i) { if (i % 4) == 3 { 1 } else { 0 } };
    col fixed FACTOR(i) { 1 << (((i + 1) % 4) * 8) };

    col witness A_byte;
    col witness B_byte;
    col witness C_byte;

    col witness A;
    col witness B;
    col witness C;

    A' = A * (1 - latch) + A_byte * FACTOR;
    B' = B * (1 - latch) + B_byte * FACTOR;
    C' = C * (1 - latch) + C_byte * FACTOR;

    link => C_byte = byte_binary.run(operation_id', A_byte, B_byte);
}


machine Main with degree: 262144 {
    reg pc[@pc];
    reg X0[<=];
    reg X1[<=];
    reg X2[<=];
    reg A;

    Binary binary;

    instr and X0, X1 -> X2 link ~> X2 = binary.and(X0, X1);
    instr or X0, X1 -> X2 link ~> X2 = binary.or(X0, X1);
    instr xor X0, X1 -> X2 link ~> X2 = binary.xor(X0, X1);

    instr assert_eq X0, X1 {
        X0 = X1
    }

    function main {

        // AND
        A <== and(0, 0);
        assert_eq A, 0;
        A <== and(0xffffffff, 0xffffffff);
        assert_eq A, 0xffffffff;
        A <== and(0xffffffff, 0xabcdef01);
        assert_eq A, 0xabcdef01;
        A <== and(0xabcdef01, 0xffffffff);
        assert_eq A, 0xabcdef01;
        A <== and(0, 0xabcdef01);
        assert_eq A, 0;
        A <== and(0xabcdef01, 0);
        assert_eq A, 0;

        // OR
        A <== or(0, 0);
        assert_eq A, 0;
        A <== or(0xffffffff, 0xffffffff);
        assert_eq A, 0xffffffff;
        A <== or(0xffffffff, 0xabcdef01);
        assert_eq A, 0xffffffff;
        A <== or(0xabcdef01, 0xffffffff);
        assert_eq A, 0xffffffff;
        A <== or(0, 0xabcdef01);
        assert_eq A, 0xabcdef01;
        A <== or(0xabcdef01, 0);
        assert_eq A, 0xabcdef01;

        // XOR
        A <== xor(0, 0);
        assert_eq A, 0;
        A <== xor(0xffffffff, 0xffffffff);
        assert_eq A, 0;
        A <== xor(0xffffffff, 0xabcdef01);
        assert_eq A, 0x543210fe;
        A <== xor(0xabcdef01, 0xffffffff);
        assert_eq A, 0x543210fe;
        A <== xor(0, 0xabcdef01);
        assert_eq A, 0xabcdef01;
        A <== xor(0xabcdef01, 0);
        assert_eq A, 0xabcdef01;

        return;
    }
}
