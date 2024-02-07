use std::array;
use std::utils::unchanged_until;
use std::utils::sum;

// Arithmetic machine, ported mainly from Polygon: https://github.com/0xPolygonHermez/zkevm-proverjs/blob/main/pil/arith.pil
// Currently only supports "Equation 0", i.e., 256-Bit addition and multiplication.
machine Arith(CLK32_31, operation_id){
    operation eq0<0> x1_combined, y1_combined, x2_combined -> y2_combined, y3_combined;
    col witness operation_id;

    let BYTE = |i| i & 0xff;
    let BYTE2 = |i| i & 0xffff;

    pol commit x1[16], y1[16], x2[16], y2[16], y3[16];

    let combine: expr[] -> expr[] = |x| array::new(array::len(x) / 2, |i| x[2 * i + 1] * 2**16 + x[2 * i]);
    // Intermediate polynomials, arrays of 8 columns, 32 bit per column.
    let x1_combined: expr[] = combine(x1);
    let x2_combined: expr[] = combine(x2);
    let y1_combined: expr[] = combine(y1);
    let y2_combined: expr[] = combine(y2);
    let y3_combined: expr[] = combine(y3);

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