use std::convert::int;
use std::utils::cross_product;
use std::utils::unchanged_until;

machine Binary with
    latch: latch,
    operation_id: operation_id,
    // Allow this machine to be connected via a permutation
    call_selectors: sel,
{
    operation and<0> A, B -> C;

    operation or<1> A, B -> C;

    operation xor<2> A, B -> C;

    col witness operation_id;
    unchanged_until(operation_id, latch);

    col fixed latch(i) { if (i % 4) == 3 { 1 } else { 0 } };
    col fixed FACTOR(i) { 1 << (((i + 1) % 4) * 8) };

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

machine Binary2x with
	latch: latch,
	operation_id: operation_id,
	call_selectors: sel,
{
	Binary bin0;
	Binary bin1;

    // Unfortunately, this needs to be a witness column for witgen to work...
    let used;
    used = std::array::sum(sel);
    std::utils::force_bool(used);
	link instr_and_m0 ~> bin0.and A, B -> C;
	link instr_and_m1 ~> bin1.and A, B -> C;

	link instr_or_m0 ~> bin0.or A, B -> C;
	link instr_or_m1 ~> bin1.or A, B -> C;

	link instr_xor_m0 ~> bin0.xor A, B -> C;
	link instr_xor_m1 ~> bin1.xor A, B -> C;

	operation and<0> A, B -> C;
	operation or<1> A, B -> C;
	operation xor<2> A, B -> C;

	col witness A, B, C;

	col witness operation_id;

	col fixed M(i) { i % 2 };

	col fixed OPERATION_ID(i) { i % 3 };
	col fixed IS_AND = [1, 0, 0]*;
	col fixed IS_OR = [0, 1, 0]*;
	col fixed IS_XOR = [0, 0, 1]*;

	col witness is_and, is_or, is_xor;

	{operation_id, is_and, is_or, is_xor} in {OPERATION_ID, IS_AND, IS_OR, IS_XOR};

	col witness instr_and_m0;
	col witness instr_and_m1;

	col witness instr_or_m0;
	col witness instr_or_m1;

	col witness instr_xor_m0;
	col witness instr_xor_m1;

    let is_and_and_used = used * is_and;
	instr_and_m0 = is_and_and_used * (1 - M);
	instr_and_m1 = is_and_and_used * M;

    let is_or_and_used = used * is_or;
	instr_or_m0 = is_or_and_used * (1 - M);
	instr_or_m1 = is_or_and_used * M;

    let is_xor_and_used = used * is_xor;
	instr_xor_m0 = is_xor_and_used * (1 - M);
	instr_xor_m1 = is_xor_and_used * M;

	col fixed latch = [1]*;
}
