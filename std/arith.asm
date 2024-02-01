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
    operation affine_256<1> x1c[0], x1c[1], x1c[2], x1c[3], x1c[4], x1c[5], x1c[6], x1c[7], y1c[0], y1c[1], y1c[2], y1c[3], y1c[4], y1c[5], y1c[6], y1c[7], x2c[0], x2c[1], x2c[2], x2c[3], x2c[4], x2c[5], x2c[6], x2c[7] -> y2c[0], y2c[1], y2c[2], y2c[3], y2c[4], y2c[5], y2c[6], y2c[7], y3c[0], y3c[1], y3c[2], y3c[3], y3c[4], y3c[5], y3c[6], y3c[7];
    
    // Performs elliptic curve addition of points (x1, y2) and (x2, y2).
    // Operation ID is 10 = 0b1010, i.e., we activate equations 1, 3, and 4.
    // TODO: Witgen doesn't work for that yet.
    operation ec_add<10> x1c[0], x1c[1], x1c[2], x1c[3], x1c[4], x1c[5], x1c[6], x1c[7], y1c[0], y1c[1], y1c[2], y1c[3], y1c[4], y1c[5], y1c[6], y1c[7], x2c[0], x2c[1], x2c[2], x2c[3], x2c[4], x2c[5], x2c[6], x2c[7], y2c[0], y2c[1], y2c[2], y2c[3], y2c[4], y2c[5], y2c[6], y2c[7] -> x3c[0], x3c[1], x3c[2], x3c[3], x3c[4], x3c[5], x3c[6], x3c[7], y3c[0], y3c[1], y3c[2], y3c[3], y3c[4], y3c[5], y3c[6], y3c[7];
    
    // Performs elliptic curve doubling of point (x1, y2).
    // Operation ID is 12 = 0b1100, i.e., we activate equations 2, 3, and 4.
    // TODO: Witgen doesn't work for that yet.
    operation ec_double<12> x1c[0], x1c[1], x1c[2], x1c[3], x1c[4], x1c[5], x1c[6], x1c[7], y1c[0], y1c[1], y1c[2], y1c[3], y1c[4], y1c[5], y1c[6], y1c[7] -> x3c[0], x3c[1], x3c[2], x3c[3], x3c[4], x3c[5], x3c[6], x3c[7], y3c[0], y3c[1], y3c[2], y3c[3], y3c[4], y3c[5], y3c[6], y3c[7];
    
    let BYTE: col = |i| i & 0xff;
    let BYTE2: col = |i| i & 0xffff;

    pol commit x1[16], y1[16], x2[16], y2[16], x3[16], y3[16], s[16], q0[16], q1[16], q2[16];

    let combine: expr[] -> expr[] = |x| array::new(array::len(x) / 2, |i| x[2 * i + 1] * 2**16 + x[2 * i]);
    // Intermediate polynomials, arrays of 8 columns, 32 bit per column.
    let x1c: expr[8] = combine(x1);
    let y1c: expr[8] = combine(y1);
    let x2c: expr[8] = combine(x2);
    let y2c: expr[8] = combine(y2);
    let x3c: expr[8] = combine(x3);
    let y3c: expr[8] = combine(y3);

    let CLK32: col[32] = array::new(32, |i| |row| if row % 32 == i { 1 } else { 0 });
    let CLK32_31: expr = CLK32[31];

    // TODO: Add the equivalent of these constraints for soundness: https://github.com/0xPolygonHermez/zkevm-proverjs/blob/main/pil/arith.pil#L43-L243

    /****
    *
    * LATCH POLS: x1,y1,x2,y2,x3,y3,s,q0,q1,q2
    *
    *****/

    let fixed_inside_32_block = [|e| unchanged_until(e, CLK32[31])][0];

    array::map(x1, fixed_inside_32_block);
    array::map(y1, fixed_inside_32_block);
    array::map(x2, fixed_inside_32_block);
    array::map(y2, fixed_inside_32_block);
    array::map(x3, fixed_inside_32_block);
    array::map(y3, fixed_inside_32_block);
    array::map(s, fixed_inside_32_block);
    array::map(q0, fixed_inside_32_block);
    array::map(q1, fixed_inside_32_block);
    array::map(q2, fixed_inside_32_block);

    /****
    *
    * RANGE CHECK x1,y1,x2,y2,x3,y3,s,q0,q1,q2
    *
    *****/

    sum(16, |i| x1[i] * CLK32[i]) + sum(16, |i| y1[i] * CLK32[16 + i]) in BYTE2;
    sum(16, |i| x2[i] * CLK32[i]) + sum(16, |i| y2[i] * CLK32[16 + i]) in BYTE2;
    sum(16, |i| x3[i] * CLK32[i]) + sum(16, |i| y3[i] * CLK32[16 + i]) in BYTE2;
    // Note that for q0-q2, we only range-constrain the first 15 limbs here
    sum(16, |i| s[i] * CLK32[i]) + sum(15, |i| q0[i] * CLK32[16 + i]) in BYTE2;
    sum(15, |i| q1[i] * CLK32[i]) + sum(15, |i| q2[i] * CLK32[16 + i]) in BYTE2;

    // The most significant limbs of q0-q2 are constrained to be 32 bits
    // In Polygon's version they are 19 bits, but that requires increasing the minimum degree
    // to fit the lookup.
    // Instead, we decompose the most significant limb into two 16-Bit limbs.
    // Having a larger range-constraint is fine, because we're only multiplying it with 16-bit
    // limbs of the prime, so the result is within 48 bits, still far from overflowing the
    // Goldilocks field.
    pol witness q0_15_high, q0_15_low, q1_15_high, q1_15_low, q2_15_high, q2_15_low;
    q0_15_high * CLK32[0] + q0_15_low * CLK32[1] + q1_15_high * CLK32[2] + q1_15_low * CLK32[3] + q2_15_high * CLK32[4] + q2_15_low * CLK32[5] in BYTE2;

    fixed_inside_32_block(q0_15_high);
    fixed_inside_32_block(q0_15_low);
    fixed_inside_32_block(q1_15_high);
    fixed_inside_32_block(q1_15_low);
    fixed_inside_32_block(q2_15_high);
    fixed_inside_32_block(q2_15_low);

    q0[15] = 2**16 * q0_15_high + q0_15_low;
    q1[15] = 2**16 * q1_15_high + q1_15_low;
    q2[15] = 2**16 * q2_15_high + q2_15_low;

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
    let x3f = array_as_fun(x3);
    let y3f = array_as_fun(y3);
    let sf = array_as_fun(s);
    let q0f = array_as_fun(q0);
    let q1f = array_as_fun(q1);
    let q2f = array_as_fun(q2);

    // Defined for arguments from 0 to 31 (inclusive)
    let eq0: int -> expr = |nr|
        product(x1f, y1f)(nr)
        + x2f(nr)
        - shift_right(y2f, 16)(nr)
        - y3f(nr);

    /*******
    *
    * EQ1: s * x2 - s * x1 - y2 + y1 + (q0 * p)
    *
    *******/

    // 0xffffffffffffffffffffffffffffffffffffffffffffffffffff fffe ffff fc2f
    let p = array_as_fun([
        0xfc2f, 0xffff, 0xfffe, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff,
        0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff
    ]);

    // The "- 4 * shift_right(p, 16)" effectively subtracts 4 * (p << 16 * 16) = 2 ** 258 * p
    // As a result, the term computes `(x - 2 ** 258) * p`.
    let product_with_p = (|| |x| |nr| product(p, x)(nr) - 4 * shift_right(p, 16)(nr))();

    let eq1 = (|| |nr| product(sf, x2f)(nr) - product(sf, x1f)(nr) - y2f(nr) + y1f(nr) + product_with_p(q0f)(nr))();

    /*******
    *
    * EQ2:  2 * s * y1 - 3 * x1 * x1 + (q0 * p)
    *
    *******/

    let eq2 = (|| |nr| 2 * product(sf, y1f)(nr) - 3 * product(x1f, x1f)(nr) + product_with_p(q0f)(nr))();

    /*******
    *
    * EQ3:  s * s - x1 - x2 - x3 + (q1 * p)
    *
    *******/

    // If we're doing the ec_double operation (selEq[2] == 1), x2 is so far unconstrained and should be set to x1
    array::new(16, |i| selEq[2] * (x1[i] - x2[i]) = 0);

    let eq3 = (|| |nr| product(sf, sf)(nr) - x1f(nr) - x2f(nr) - x3f(nr) + product_with_p(q1f)(nr))();


    /*******
    *
    * EQ4:  s * x1 - s * x3 - y1 - y3 + (q2 * p)
    *
    *******/

    let eq4 = (|| |nr| product(sf, x1f)(nr) - product(sf, x3f)(nr) - y1f(nr) - y3f(nr) + product_with_p(q2f)(nr))();


    /*******
    *
    * Equation Selectors
    *
    *******/

    // Binary selectors for the equations that are activated. Determined from the operation ID via bit-decomposition.
    // Note that there are only 4 selectors because equation 4 is activated iff. equation 3 is activated, so we can
    // re-use the same selector.
    pol commit selEq[4];
    // Note that this implies that the selEq[] columns are also constant within the block.
    fixed_inside_32_block(operation_id);
    array::map(selEq, |c| force_bool(c));
    sum(4, |i| 2 ** i * selEq[i]) = operation_id;

    /*******
    *
    * Carry
    *
    *******/
    
    // Note that Polygon uses a single 22-Bit column. However, this approach allows for a lower degree (2**16)
    // while still preventing overflows: The 32-bit carry gets added to 32 48-Bit values, which can't overflow
    // the Goldilocks field.
    pol witness carry_low[3], carry_high[3];
    { carry_low[0] } in { BYTE2 };
    { carry_low[1] } in { BYTE2 };
    { carry_low[2] } in { BYTE2 };
    { carry_high[0] } in { BYTE2 };
    { carry_high[1] } in { BYTE2 };
    { carry_high[2] } in { BYTE2 };

    // Carries can be any integer in the range [-2**31, 2**31 - 1)
    let carry: expr[3] = array::new(3, |i| carry_high[i] * 2**16 + carry_low[i] - 2 ** 31);
    
    array::map(carry, |c| c * CLK32[0] = 0);

    /*******
    *
    * Putting everything together
    *
    *******/
    
    col eq0_sum = sum(32, |i| eq0(i) * CLK32[i]);
    col eq1_sum = sum(32, |i| eq1(i) * CLK32[i]);
    col eq2_sum = sum(32, |i| eq2(i) * CLK32[i]);
    col eq3_sum = sum(32, |i| eq3(i) * CLK32[i]);
    col eq4_sum = sum(32, |i| eq4(i) * CLK32[i]);

    selEq[0] * (eq0_sum + carry[0]) = selEq[0] * carry[0]' * 2**16;
    selEq[1] * (eq1_sum + carry[0]) = selEq[1] * carry[0]' * 2**16;
    selEq[2] * (eq2_sum + carry[0]) = selEq[2] * carry[0]' * 2**16;
    selEq[3] * (eq3_sum + carry[1]) = selEq[3] * carry[1]' * 2**16;
    selEq[3] * (eq4_sum + carry[2]) = selEq[3] * carry[2]' * 2**16;
}