use std::array;
use std::utils::unchanged_until;
use std::utils::sum;

// Arithmetic machine, ported mainly from Polygon: https://github.com/0xPolygonHermez/zkevm-proverjs/blob/main/pil/arith.pil
// Currently only supports "Equation 0", i.e., 256-Bit addition and multiplication.
machine Arith(CLK32[31], operation_id){
    operation eq0<0> x1_0, x1_1, x1_2, x1_3, x1_4, x1_5, x1_6, x1_7, y1_0, y1_1, y1_2, y1_3, y1_4, y1_5, y1_6, y1_7, x2_0, x2_1, x2_2, x2_3, x2_4, x2_5, x2_6, x2_7 -> y2_0, y2_1, y2_2, y2_3, y2_4, y2_5, y2_6, y2_7, y3_0, y3_1, y3_2, y3_3, y3_4, y3_5, y3_6, y3_7;
    col witness operation_id;

    let BYTE = |i| i & 0xff;
    let BYTE2 = |i| i & 0xffff;

    pol commit x1[16], y1[16], x2[16], y2[16], y3[16];

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

    pol y3_0 = y3[1] * 2**16 + y3[0];
    pol y3_1 = y3[3] * 2**16 + y3[2];
    pol y3_2 = y3[5] * 2**16 + y3[4];
    pol y3_3 = y3[7] * 2**16 + y3[6];
    pol y3_4 = y3[9] * 2**16 + y3[8];
    pol y3_5 = y3[11] * 2**16 + y3[10];
    pol y3_6 = y3[13] * 2**16 + y3[12];
    pol y3_7 = y3[15] * 2**16 + y3[14];

    let clock = |j, row| if row % 32 == j { 1 } else { 0 };
    // Arrays of fixed columns are not supported yet.
    // These need an explicit lambda, otherwise they are not recognized as fixed columns.
    // The type system will handle that in the future.
    let CLK32_0 = |row| clock(0, row);
    let CLK32_1 = |row| clock(1, row);
    let CLK32_2 = |row| clock(2, row);
    let CLK32_3 = |row| clock(3, row);
    let CLK32_4 = |row| clock(4, row);
    let CLK32_5 = |row| clock(5, row);
    let CLK32_6 = |row| clock(6, row);
    let CLK32_7 = |row| clock(7, row);
    let CLK32_8 = |row| clock(8, row);
    let CLK32_9 = |row| clock(9, row);
    let CLK32_10 = |row| clock(10, row);
    let CLK32_11 = |row| clock(11, row);
    let CLK32_12 = |row| clock(12, row);
    let CLK32_13 = |row| clock(13, row);
    let CLK32_14 = |row| clock(14, row);
    let CLK32_15 = |row| clock(15, row);
    let CLK32_16 = |row| clock(16, row);
    let CLK32_17 = |row| clock(17, row);
    let CLK32_18 = |row| clock(18, row);
    let CLK32_19 = |row| clock(19, row);
    let CLK32_20 = |row| clock(20, row);
    let CLK32_21 = |row| clock(21, row);
    let CLK32_22 = |row| clock(22, row);
    let CLK32_23 = |row| clock(23, row);
    let CLK32_24 = |row| clock(24, row);
    let CLK32_25 = |row| clock(25, row);
    let CLK32_26 = |row| clock(26, row);
    let CLK32_27 = |row| clock(27, row);
    let CLK32_28 = |row| clock(28, row);
    let CLK32_29 = |row| clock(29, row);
    let CLK32_30 = |row| clock(30, row);
    let CLK32_31 = |row| clock(31, row);
    let CLK32 = [
        CLK32_0, CLK32_1, CLK32_2, CLK32_3, CLK32_4, CLK32_5, CLK32_6, CLK32_7,
        CLK32_8, CLK32_9, CLK32_10, CLK32_11, CLK32_12, CLK32_13, CLK32_14, CLK32_15,
        CLK32_16, CLK32_17, CLK32_18, CLK32_19, CLK32_20, CLK32_21, CLK32_22, CLK32_23,
        CLK32_24, CLK32_25, CLK32_26, CLK32_27, CLK32_28, CLK32_29, CLK32_30, CLK32_31
    ];

    /****
    *
    * LATCH POLS: x1,y1,x2,y2,y3
    *
    *****/

    array::map(x1, |e| unchanged_until(e, CLK32[31]));
    array::map(y1, |e| unchanged_until(e, CLK32[31]));
    array::map(x2, |e| unchanged_until(e, CLK32[31]));
    array::map(y2, |e| unchanged_until(e, CLK32[31]));
    array::map(y3, |e| unchanged_until(e, CLK32[31]));

    /****
    *
    * RANGE CHECK x1,y1,x2,y2,y3
    *
    *****/

    sum(16, |i| x1[i] * CLK32[i]) + sum(16, |i| y1[i] * CLK32[16 + i]) in BYTE2;
    sum(16, |i| x2[i] * CLK32[i]) in BYTE2;
    sum(16, |i| y3[i] * CLK32[i]) + sum(16, |i| y2[i] * CLK32[16 + i]) in BYTE2;


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
    
    // Note that Polygon uses a single 22-Bit column. However, this approach allows for a lower degree (2**16)
    // while still preventing overflows: The 32-bit carry gets added to 32 16-Bit values, which can't overflow
    // the Goldilocks field.
    pol witness carry_low, carry_high;
    { carry_high } in { BYTE2 };
    { carry_low } in { BYTE2 };

    pol carry = carry_high * 2**16 + carry_low;
    
    carry * CLK32[0] = 0;

    let eq0_sum = sum(32, |i| eq0(i) * CLK32[i]);
    carry + eq0_sum = carry' * 2**16;
}