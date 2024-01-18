machine Binary(latch, operation_id) {

    // lower bound degree is 196608 // 65536 * 3

    operation and<0> A[7],A[6],A[5],A[4],A[3],A[2],A[1],A[0], B[7],B[6],B[5],B[4],B[3],B[2],B[1],B[0] -> C[7],C[6],C[5],C[4],C[3],C[2],C[1],C[0];

    operation or<1> A[7],A[6],A[5],A[4],A[3],A[2],A[1],A[0], B[7],B[6],B[5],B[4],B[3],B[2],B[1],B[0] -> C[7],C[6],C[5],C[4],C[3],C[2],C[1],C[0];

    operation xor<2> A[7],A[6],A[5],A[4],A[3],A[2],A[1],A[0], B[7],B[6],B[5],B[4],B[3],B[2],B[1],B[0] -> C[7],C[6],C[5],C[4],C[3],C[2],C[1],C[0];

    col witness operation_id;
    col fixed latch(i) { (i % 4) == 3 };

    let fold = |length, f, initial, folder| match length {
        0 => initial,
        _ => folder(fold(length - 1, f, initial, folder), f(length - 1))
    };

    let make_array = |length, f| fold(length, f, [], |acc, e| acc + [e]);

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

    col witness A_byte[8];
    col witness B_byte[8];
    col witness C_byte[8];

    col witness A[8];
    col witness B[8];
    col witness C[8];

    let byte_decompose_constr = [|x,y| x' == x * (1 - latch) + y * FACTOR][0];

    make_array(8,|i| byte_decompose_constr(A[i],A_byte[i]));
    make_array(8,|i| byte_decompose_constr(B[i],B_byte[i]));
    make_array(8,|i| byte_decompose_constr(C[i],C_byte[i]));

    {operation_id', A_byte[0], B_byte[0], C_byte[0]} in {P_operation, P_A, P_B, P_C};
    {operation_id', A_byte[1], B_byte[1], C_byte[1]} in {P_operation, P_A, P_B, P_C};
    {operation_id', A_byte[2], B_byte[2], C_byte[2]} in {P_operation, P_A, P_B, P_C};
    {operation_id', A_byte[3], B_byte[3], C_byte[3]} in {P_operation, P_A, P_B, P_C};
    {operation_id', A_byte[4], B_byte[4], C_byte[4]} in {P_operation, P_A, P_B, P_C};
    {operation_id', A_byte[5], B_byte[5], C_byte[5]} in {P_operation, P_A, P_B, P_C};
    {operation_id', A_byte[6], B_byte[6], C_byte[6]} in {P_operation, P_A, P_B, P_C};
    {operation_id', A_byte[7], B_byte[7], C_byte[7]} in {P_operation, P_A, P_B, P_C};

}
