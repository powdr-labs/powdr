use std::array;
use std::utils::unchanged_until;
use std::utils::force_bool;
use std::utils::sum;
use std::math::ff;
use std::check::panic;
use std::convert::int;
use std::convert::fe;
use std::convert::expr;
use std::prover::eval;
use std::prelude::Query;
use std::machines::range::Byte;

// Arithmetic machine, ported mainly from Polygon: https://github.com/0xPolygonHermez/zkevm-proverjs/blob/main/pil/arith.pil
// Currently only supports "Equation 0", i.e., 256-Bit addition and multiplication.
machine Arith256 with
    latch: CLK64_63,
    operation_id: operation_id,
    // Allow this machine to be connected via a permutation
    call_selectors: sel,
{
    Byte byte;
    
    // The operation ID will be bit-decomposed to yield selEq[], controlling which equations are activated.
    col witness operation_id;

    // Computes x1 * y1 + x2, where all inputs / outputs are 256-bit words (represented as 16-Bit limbs in little-endian order).
    // More precisely, affine_256(x1, y1, x2) = (y2, y3), where x1 * y1 + x2 = 2**256 * y2 + y3
    // Operation ID is 1 = 0b0001, i.e., we activate equation 0.
    operation affine_256<1> x1c[0], x1c[1], x1c[2], x1c[3], x1c[4], x1c[5], x1c[6], x1c[7], x1c[8], x1c[9], x1c[10], x1c[11], x1c[12], x1c[13], x1c[14], x1c[15], y1c[0], y1c[1], y1c[2], y1c[3], y1c[4], y1c[5], y1c[6], y1c[7], y1c[8], y1c[9], y1c[10], y1c[11], y1c[12], y1c[13], y1c[14], y1c[15], x2c[0], x2c[1], x2c[2], x2c[3], x2c[4], x2c[5], x2c[6], x2c[7], x2c[8], x2c[9], x2c[10], x2c[11], x2c[12], x2c[13], x2c[14], x2c[15] -> y2c[0], y2c[1], y2c[2], y2c[3], y2c[4], y2c[5], y2c[6], y2c[7], y2c[8], y2c[9], y2c[10], y2c[11], y2c[12], y2c[13], y2c[14], y2c[15], y3c[0], y3c[1], y3c[2], y3c[3], y3c[4], y3c[5], y3c[6], y3c[7], y3c[8], y3c[9], y3c[10], y3c[11], y3c[12], y3c[13], y3c[14], y3c[15];

    pol commit x1[32], y1[32], x2[32], y2[32], y3[32];

    // Selects the ith limb of x (little endian)
    // All limbs are 8 bits
    let select_limb = |x, i| if i >= 0 {
        (x >> (i * 8)) & 0xff
    } else {
        0
    };

    let limbs_to_int: expr[] -> int = query |limbs| array::sum(array::map_enumerated(limbs, |i, limb| int(eval(limb)) << (i * 8)));

    let x1_int = query || limbs_to_int(x1);
    let y1_int = query || limbs_to_int(y1);
    let x2_int = query || limbs_to_int(x2);
    let y2_int = query || limbs_to_int(y2);
    let y3_int = query || limbs_to_int(y3);

    let combine: expr[] -> expr[] = |x| array::new(array::len(x) / 2, |i| x[2 * i + 1] * 2**8 + x[2 * i]);
    // Intermediate polynomials, arrays of 16 columns, 16 bit per column.
    col x1c[16] = combine(x1);
    col y1c[16] = combine(y1);
    col x2c[16] = combine(x2);
    col y2c[16] = combine(y2);
    col y3c[16] = combine(y3);

    let CLK64: col[64] = array::new(64, |i| |row| if row % 64 == i { 1 } else { 0 });
    let CLK64_63: expr = CLK64[63];

    // TODO: Add the equivalent of these constraints for soundness: https://github.com/0xPolygonHermez/zkevm-proverjs/blob/main/pil/arith.pil#L43-L243

    /****
    *
    * LATCH POLS: x1,y1,x2,y2,x3,y3,s,q0,q1,q2
    *
    *****/

    let fixed_inside_64_block = |e| unchanged_until(e, CLK64[63]);

    array::map(x1, fixed_inside_64_block);
    array::map(y1, fixed_inside_64_block);
    array::map(x2, fixed_inside_64_block);
    array::map(y2, fixed_inside_64_block);
    array::map(y3, fixed_inside_64_block);

    /****
    *
    * RANGE CHECK x1,y1,x2,y2,x3,y3,s,q0,q1,q2
    *
    *****/

    link => byte.check(sum(32, |i| x1[i] * CLK64[i]) + sum(32, |i| y1[i] * CLK64[32 + i]));
    link => byte.check(sum(32, |i| x2[i] * CLK64[i]) + sum(32, |i| y2[i] * CLK64[32 + i]));
    link => byte.check(sum(32, |i| y3[i] * CLK64[i]) + sum(32, |i| y3[i] * CLK64[32 + i]));

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

    // Defined for arguments from 0 to 63 (inclusive)
    let eq0 = |nr|
        product(x1f, y1f)(nr)
        + x2f(nr)
        - shift_right(y2f, 32)(nr)
        - y3f(nr);

    /*******
    *
    * Carry
    *
    *******/
    
    // Note that Polygon uses a single 22-Bit column. However, this approach allows for a lower degree (2**16)
    // while still preventing overflows: The 32-bit carry gets added to 32 48-Bit values, which can't overflow
    // the Goldilocks field.
    pol witness carry_low, carry_high;
    link => byte.check(carry_low);
    link => byte.check(carry_high);

    // Carries can be any integer in the range [-2**31, 2**31 - 1)
    let carry = carry_high * 2**8 + carry_low;
    
    carry * CLK64[0] = 0;

    /*******
    *
    * Putting everything together
    *
    *******/
    
    col eq0_sum = sum(64, |i| eq0(i) * CLK64[i]);

    eq0_sum + carry = carry' * 2**8;
}
