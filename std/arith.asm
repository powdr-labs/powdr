use std::array;
use std::utils::unchanged_until;
use std::utils::force_bool;
use std::utils::sum;

// Arithmetic machine, ported mainly from Polygon: https://github.com/0xPolygonHermez/zkevm-proverjs/blob/main/pil/arith.pil
// Currently only supports "Equation 0", i.e., 256-Bit addition and multiplication.
machine Arith(CLK32_31, operation_id){
    
    // The operation ID will be bit-decomosed to yield selEq[], controlling which equations are activated.
    col witness operation_id;

    // Computes x1 * y1 + x2, where all inputs / outputs are 256-bit words (represented as 32-Bit limbs in little-endian order).
    // More precisely, affine_256(x1, y1, x2) = (y2, y3), where x1 * y1 + x2 = 2**256 * y2 + y3
    // Operation ID is 1 = 0b0001, i.e., we activate equation 0.
    operation affine_256<1> x1_0, x1_1, x1_2, x1_3, x1_4, x1_5, x1_6, x1_7, y1_0, y1_1, y1_2, y1_3, y1_4, y1_5, y1_6, y1_7, x2_0, x2_1, x2_2, x2_3, x2_4, x2_5, x2_6, x2_7 -> y2_0, y2_1, y2_2, y2_3, y2_4, y2_5, y2_6, y2_7, y3_0, y3_1, y3_2, y3_3, y3_4, y3_5, y3_6, y3_7;
    
    // Performs elliptic curve addition of points (x1, y2) and (x2, y2).
    // Operation ID is 10 = 0b1010, i.e., we activate equations 1, 3, and 4.
    // TODO: Implement these equations
    operation ec_add<10> x1_0, x1_1, x1_2, x1_3, x1_4, x1_5, x1_6, x1_7, y1_0, y1_1, y1_2, y1_3, y1_4, y1_5, y1_6, y1_7, x2_0, x2_1, x2_2, x2_3, x2_4, x2_5, x2_6, x2_7, y2_0, y2_1, y2_2, y2_3, y2_4, y2_5, y2_6, y2_7 -> x3_0, x3_1, x3_2, x3_3, x3_4, x3_5, x3_6, x3_7, y3_0, y3_1, y3_2, y3_3, y3_4, y3_5, y3_6, y3_7;
    
    // Performs elliptic curve doubling of point (x1, y2).
    // Operation ID is 12 = 0b1100, i.e., we activate equations 2, 3, and 4.
    // TODO: Implement these equations
    operation ec_double<12> x1_0, x1_1, x1_2, x1_3, x1_4, x1_5, x1_6, x1_7, y1_0, y1_1, y1_2, y1_3, y1_4, y1_5, y1_6, y1_7 -> x3_0, x3_1, x3_2, x3_3, x3_4, x3_5, x3_6, x3_7, y3_0, y3_1, y3_2, y3_3, y3_4, y3_5, y3_6, y3_7;
    
    let BYTE = |i| i & 0xff;
    let BYTE2 = |i| i & 0xffff;

    pol commit x1[16], y1[16], x2[16], y2[16], x3[16], y3[16];

    // Intermediate polynomials, 32-Bit each
    pol x1_0 = x1[1] * 2**16 + x1[0];
    pol x1_1 = x1[3] * 2**16 + x1[2];
    pol x1_2 = x1[5] * 2**16 + x1[4];
    pol x1_3 = x1[7] * 2**16 + x1[6];
    pol x1_4 = x1[9] * 2**16 + x1[8];
    pol x1_5 = x1[11] * 2**16 + x1[10];
    pol x1_6 = x1[13] * 2**16 + x1[12];
    pol x1_7 = x1[15] * 2**16 + x1[14];

    pol x2_0 = x2[1] * 2**16 + x2[0];
    pol x2_1 = x2[3] * 2**16 + x2[2];
    pol x2_2 = x2[5] * 2**16 + x2[4];
    pol x2_3 = x2[7] * 2**16 + x2[6];
    pol x2_4 = x2[9] * 2**16 + x2[8];
    pol x2_5 = x2[11] * 2**16 + x2[10];
    pol x2_6 = x2[13] * 2**16 + x2[12];
    pol x2_7 = x2[15] * 2**16 + x2[14];

    pol y1_0 = y1[1] * 2**16 + y1[0];
    pol y1_1 = y1[3] * 2**16 + y1[2];
    pol y1_2 = y1[5] * 2**16 + y1[4];
    pol y1_3 = y1[7] * 2**16 + y1[6];
    pol y1_4 = y1[9] * 2**16 + y1[8];
    pol y1_5 = y1[11] * 2**16 + y1[10];
    pol y1_6 = y1[13] * 2**16 + y1[12];
    pol y1_7 = y1[15] * 2**16 + y1[14];

    pol y2_0 = y2[1] * 2**16 + y2[0];
    pol y2_1 = y2[3] * 2**16 + y2[2];
    pol y2_2 = y2[5] * 2**16 + y2[4];
    pol y2_3 = y2[7] * 2**16 + y2[6];
    pol y2_4 = y2[9] * 2**16 + y2[8];
    pol y2_5 = y2[11] * 2**16 + y2[10];
    pol y2_6 = y2[13] * 2**16 + y2[12];
    pol y2_7 = y2[15] * 2**16 + y2[14];

    pol x3_0 = x3[1] * 2**16 + x3[0];
    pol x3_1 = x3[3] * 2**16 + x3[2];
    pol x3_2 = x3[5] * 2**16 + x3[4];
    pol x3_3 = x3[7] * 2**16 + x3[6];
    pol x3_4 = x3[9] * 2**16 + x3[8];
    pol x3_5 = x3[11] * 2**16 + x3[10];
    pol x3_6 = x3[13] * 2**16 + x3[12];
    pol x3_7 = x3[15] * 2**16 + x3[14];

    pol y3_0 = y3[1] * 2**16 + y3[0];
    pol y3_1 = y3[3] * 2**16 + y3[2];
    pol y3_2 = y3[5] * 2**16 + y3[4];
    pol y3_3 = y3[7] * 2**16 + y3[6];
    pol y3_4 = y3[9] * 2**16 + y3[8];
    pol y3_5 = y3[11] * 2**16 + y3[10];
    pol y3_6 = y3[13] * 2**16 + y3[12];
    pol y3_7 = y3[15] * 2**16 + y3[14];

    let CLK32: col[32] = array::new(32, |i| |row| if row % 32 == i { 1 } else { 0 });
    let CLK32_31 = CLK32[31];

    /****
    *
    * LATCH POLS: x1,y1,x2,y2,y3
    *
    *****/

    array::map(x1, |e| unchanged_until(e, CLK32[31]));
    array::map(y1, |e| unchanged_until(e, CLK32[31]));
    array::map(x2, |e| unchanged_until(e, CLK32[31]));
    array::map(y2, |e| unchanged_until(e, CLK32[31]));
    array::map(x3, |e| unchanged_until(e, CLK32[31]));
    array::map(y3, |e| unchanged_until(e, CLK32[31]));

    /****
    *
    * RANGE CHECK x1,y1,x2,y2,y3
    *
    *****/

    sum(16, |i| x1[i] * CLK32[i]) + sum(16, |i| y1[i] * CLK32[16 + i]) in BYTE2;
    sum(16, |i| x2[i] * CLK32[i]) + sum(16, |i| y2[i] * CLK32[16 + i]) in BYTE2;
    sum(16, |i| x3[i] * CLK32[i]) + sum(16, |i| y3[i] * CLK32[16 + i]) in BYTE2;


    /*******
    *
    * EQ0: A(x1) * B(y1) + C(x2) = D (y2) * 2 ** 256 + op (y3)
    *        x1 * y1 + x2 - y2 * 2**256 - y3 = 0
    *
    *******/

    /// returns a(0) * b(0) + ... + a(n - 1) * b(n - 1)
    let dot_prod = |n, a, b| sum(n, |i| a(i) * b(i));
    /// returns |n| a(0) * b(n) + ... + a(n) * b(0)
    let product = |a, b| |n| dot_prod(n + 1, a, |i| b(n - i));
    /// Converts array to function, extended by zeros.
    let array_as_fun: expr[] -> (int -> expr) = |arr| |i| if 0 <= i && i < array::len(arr) {
        arr[i]
    } else {
        0
    };
    let shift_right = |fn, amount| |i| fn(i - amount);

    let x1f = array_as_fun(x1);
    let y1f = array_as_fun(y1);
    let x2f = array_as_fun(x2);
    let y2f = array_as_fun(y2);
    let y3f = array_as_fun(y3);

    // Defined for arguments from 0 to 31 (inclusive)
    let eq0: int -> expr = |nr|
        product(x1f, y1f)(nr)
        + x2f(nr)
        - shift_right(y2f, 16)(nr)
        - y3f(nr);

    // Binary selectors for the equations that are activated. Determined from the operation ID via bit-decomposition.
    // Note that there are only 4 selectors because equation 4 is activated iff. equation 3 is activated, so we can
    // re-use the same selector.
    pol commit selEq[4];
    // Note that this is not necessary, because the operation ID is already constant within the block
    // array::map(selEq, |e| unchanged_until(e, CLK32[31]));
    array::map(selEq, |c| force_bool(c));
    sum(4, |i| 2 ** i * selEq[i]) = operation_id;
    
    // Note that Polygon uses a single 22-Bit column. However, this approach allows for a lower degree (2**16)
    // while still preventing overflows: The 32-bit carry gets added to 32 16-Bit values, which can't overflow
    // the Goldilocks field.
    pol witness carry_low[3], carry_high[3];
    { carry_low[0] } in { BYTE2 };
    { carry_low[1] } in { BYTE2 };
    { carry_low[2] } in { BYTE2 };
    { carry_high[0] } in { BYTE2 };
    { carry_high[1] } in { BYTE2 };
    { carry_high[2] } in { BYTE2 };

    // Carries can be any integer in the range [-2**31, 2**31 - 1)
    pol carry0 = carry_high[0] * 2**16 + carry_low[0] - 2 ** 31;
    pol carry1 = carry_high[1] * 2**16 + carry_low[1] - 2 ** 31;
    pol carry2 = carry_high[2] * 2**16 + carry_low[2] - 2 ** 31;
    let carry = [carry0, carry1, carry2];
    
    array::map(carry, |c| c * CLK32[0] == 0);

    // TODO: Somehow witgen fails if a carry is unconstrained
    selEq[0] * carry[1] = 0;
    selEq[0] * carry[2] = 0;

    col eq0_sum = sum(32, |i| eq0(i) * CLK32[i]);
    selEq[0] * (eq0_sum + carry[0]) = selEq[0] * carry[0]' * 2**16;
}