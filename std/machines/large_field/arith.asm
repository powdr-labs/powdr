use std::array;
use std::utils::unchanged_until;
use std::utils::force_bool;
use std::utils::sum;
use std::math::ff;
use std::field::modulus;
use std::check::panic;
use std::check::require_field_bits;
use std::convert::int;
use std::convert::fe;
use std::convert::expr;
use std::prover::eval;
use std::prelude::Query;
use std::machines::range::Byte2;

// Implements 256-Bit addition and multiplication.
// Ported mainly from Polygon: https://github.com/0xPolygonHermez/zkevm-proverjs/blob/main/pil/arith.pil
// Requires the field to contain at least 48 bits.
machine Arith with
    latch: CLK32_31,
    operation_id: operation_id,
    // Allow this machine to be connected via a permutation
    call_selectors: sel,
{
    require_field_bits(48, || "Arith requires a field that fits any 48-Bit value.");

    Byte2 byte2;
    
    // The operation ID will be bit-decomposed to yield selEq[], controlling which equations are activated.
    col witness operation_id;

    // Computes x1 * y1 + x2, where all inputs / outputs are 256-bit words (represented as 32-Bit limbs in little-endian order).
    // More precisely, affine_256(x1, y1, x2) = (y2, y3), where x1 * y1 + x2 = 2**256 * y2 + y3
    // Operation ID is 1 = 0b0001, i.e., we activate equation 0.
    operation affine_256<1> x1c[0], x1c[1], x1c[2], x1c[3], x1c[4], x1c[5], x1c[6], x1c[7], y1c[0], y1c[1], y1c[2], y1c[3], y1c[4], y1c[5], y1c[6], y1c[7], x2c[0], x2c[1], x2c[2], x2c[3], x2c[4], x2c[5], x2c[6], x2c[7] -> y2c[0], y2c[1], y2c[2], y2c[3], y2c[4], y2c[5], y2c[6], y2c[7], y3c[0], y3c[1], y3c[2], y3c[3], y3c[4], y3c[5], y3c[6], y3c[7];
    
    // mod_256(y2, y3, x1) = x2 computes (2 ** 256 * y2 + y3) % x1, where all inputs / outputs are 256-bit words.
    // While hint computes the modulus, there's no guarantee from user generated witness input that the remainder is smaller than the modulus.
    // In fact, the remainder can contain any multiples of modulus.
    operation mod_256<1> y2c[0], y2c[1], y2c[2], y2c[3], y2c[4], y2c[5], y2c[6], y2c[7], y3c[0], y3c[1], y3c[2], y3c[3], y3c[4], y3c[5], y3c[6], y3c[7], x1c[0], x1c[1], x1c[2], x1c[3], x1c[4], x1c[5], x1c[6], x1c[7] -> x2c[0], x2c[1], x2c[2], x2c[3], x2c[4], x2c[5], x2c[6], x2c[7];

    // Performs elliptic curve addition of points (x1, y2) and (x2, y2).
    // Operation ID is 10 = 0b1010, i.e., we activate equations 1, 3, and 4.
    operation ec_add<10> x1c[0], x1c[1], x1c[2], x1c[3], x1c[4], x1c[5], x1c[6], x1c[7], y1c[0], y1c[1], y1c[2], y1c[3], y1c[4], y1c[5], y1c[6], y1c[7], x2c[0], x2c[1], x2c[2], x2c[3], x2c[4], x2c[5], x2c[6], x2c[7], y2c[0], y2c[1], y2c[2], y2c[3], y2c[4], y2c[5], y2c[6], y2c[7] -> x3c[0], x3c[1], x3c[2], x3c[3], x3c[4], x3c[5], x3c[6], x3c[7], y3c[0], y3c[1], y3c[2], y3c[3], y3c[4], y3c[5], y3c[6], y3c[7];
    
    // Performs elliptic curve doubling of point (x1, y2).
    // Operation ID is 12 = 0b1100, i.e., we activate equations 2, 3, and 4.
    operation ec_double<12> x1c[0], x1c[1], x1c[2], x1c[3], x1c[4], x1c[5], x1c[6], x1c[7], y1c[0], y1c[1], y1c[2], y1c[3], y1c[4], y1c[5], y1c[6], y1c[7] -> x3c[0], x3c[1], x3c[2], x3c[3], x3c[4], x3c[5], x3c[6], x3c[7], y3c[0], y3c[1], y3c[2], y3c[3], y3c[4], y3c[5], y3c[6], y3c[7];

    let secp_modulus = 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2f;

    let inverse: int -> int = |x| ff::inverse(x, secp_modulus);
    let add = |x, y| ff::add(x, y, secp_modulus);
    let sub = |x, y| ff::sub(x, y, secp_modulus);
    let mul = |x, y| ff::mul(x, y, secp_modulus);
    let div = |x, y| ff::div(x, y, secp_modulus);

    pol commit x1[16], x2[16], x3[16];
    pol commit y1[16], y2[16], y3[16];
    pol commit s[16], q0[16], q1[16], q2[16];

    let limbs_to_int: fe[] -> int = |limbs| array::sum(array::map_enumerated(limbs, |i, limb| int(limb) << (i * 16)));
    let limbs_to_ints: fe[] -> int[] = |l| array::new(array::len(l) / 16, |i| limbs_to_int(array::sub_array(l, i * 16, 16)));

    // Selects the ith limb of x (little endian)
    // Note that the most significant limb can be up to 32 bits; all others are 16 bits.
    let select_limb = |x, i| if i >= 0 {
        (x >> (i * 16)) & if i < 15 { 0xffff } else { 0xffffffff }
    } else {
        0
    };
    let int_to_limbs: int -> fe[] = |x| array::new(16, |i| fe(select_limb(x, i)));

    // Prover function in case we are computing division or modulo (y1 is not an input).
    query |i| std::prover::compute_from_multi_if(
        operation_id = 1,
        y1 + x2,
        i,
        y2 + y3 + x1,
        |values| match limbs_to_ints(values) {
            [y2, y3, x1] => {
                let dividend = (y2 << 256) + y3;
                int_to_limbs(dividend / x1) + int_to_limbs(dividend % x1)
            },
            _ => panic("Unexpected number of values")
        }
    );

    // Prover function for elliptic curve addition.
    query |i| std::prover::compute_from_multi_if(
        operation_id = 10, // ec_add
        s + q0 + q1 + q2 + x3 + y3,
        i,
        x1 + x2 + y1 + y2,
        |values| match limbs_to_ints(values) {
            [x1, x2, y1, y2] => {
                let s = div(sub(y2, y1), sub(x2, x1));
                // Compute quotients.
                // Note that we add 2**258 to it, to move it from the (-2**258, 2**258) to the (0, 2**259) range, so it can
                // be represented as an unsigned 272-bit integer.
                // See the comment for `product_with_p` below.
                let q0 = -(s * x2 - s * x1 - y2 + y1) / secp_modulus + (1 << 258);
                // Adding secp_modulus to make sure that all numbers are positive when % is applied to it.
                let x3 = (s * s - x1 - x2 + 2 * secp_modulus) % secp_modulus;
                let y3 = (s * ((x1 - x3) + secp_modulus) - y1 + secp_modulus) % secp_modulus;
                let q1 = -(s * s - x1 - x2 - x3) / secp_modulus + (1 << 258);
                let q2 = -(s * x1 - s * x3 - y1 - y3) / secp_modulus + (1 << 258);
                int_to_limbs(s) + int_to_limbs(q0) + int_to_limbs(q1) + int_to_limbs(q2) + int_to_limbs(x3) + int_to_limbs(y3)
            },
            _ => panic("Unexpected number of values")
        }
    );
    
    // Prover function for elliptic curve doubling.
    query |i| std::prover::compute_from_multi_if(
        operation_id = 12, // ec_double
        s + q0 + q1 + q2 + x3 + y3,
        i,
        x1 + x2 + y1,
        |values| match limbs_to_ints(values) {
            [x1, x2, y1] => {
                let s = div(mul(3, mul(x1, x1)), mul(2, y1));
                // Compute quotients.
                // Note that we add 2**258 to it, to move it from the (-2**258, 2**258) to the (0, 2**259) range, so it can
                // be represented as an unsigned 272-bit integer.
                // See the comment for `product_with_p` below.
                let q0 = -(2 * s * y1 - 3 * x1 * x1) / secp_modulus + (1 << 258);
                // Adding secp_modulus to make sure that all numbers are positive when % is applied to it.
                let x3 = (s * s - x1 - x2 + 2 * secp_modulus) % secp_modulus;
                let y3 = (s * ((x1 - x3) + secp_modulus) - y1 + secp_modulus) % secp_modulus;
                let q1 = -(s * s - x1 - x2 - x3) / secp_modulus + (1 << 258);
                let q2 = -(s * x1 - s * x3 - y1 - y3) / secp_modulus + (1 << 258);
                int_to_limbs(s) + int_to_limbs(q0) + int_to_limbs(q1) + int_to_limbs(q2) + int_to_limbs(x3) + int_to_limbs(y3)
            },
            _ => panic("Unexpected number of values")
        }
    );
    
    let combine: expr[] -> expr[] = |x| array::new(array::len(x) / 2, |i| x[2 * i + 1] * 2**16 + x[2 * i]);
    // Intermediate polynomials, arrays of 8 columns, 32 bit per column.
    col x1c[8] = combine(x1);
    col y1c[8] = combine(y1);
    col x2c[8] = combine(x2);
    col y2c[8] = combine(y2);
    col x3c[8] = combine(x3);
    col y3c[8] = combine(y3);

    let CLK32: col[32] = array::new(32, |i| |row| if row % 32 == i { 1 } else { 0 });
    let CLK32_31: expr = CLK32[31];

    // TODO: Add the equivalent of these constraints for soundness: https://github.com/0xPolygonHermez/zkevm-proverjs/blob/main/pil/arith.pil#L43-L243

    /****
    *
    * LATCH POLS: x1,y1,x2,y2,x3,y3,s,q0,q1,q2
    *
    *****/

    let fixed_inside_32_block = |e| unchanged_until(e, CLK32[31]);

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

    // sum0 (and the others) used to be inlined inside `byte2.check(...)`,
    // but that causes an issue in the bus linker mode due to the expressions
    // being copied syntactically before resolving the closures.
    // Moving these expressions out of the link fixes it.
    let sum0 = sum(16, |i| x1[i] * CLK32[i]) + sum(16, |i| y1[i] * CLK32[16 + i]);
    link => byte2.check(sum0);
    let sum1 = sum(16, |i| x2[i] * CLK32[i]) + sum(16, |i| y2[i] * CLK32[16 + i]);
    link => byte2.check(sum1);
    let sum2 = sum(16, |i| x3[i] * CLK32[i]) + sum(16, |i| y3[i] * CLK32[16 + i]);
    link => byte2.check(sum2);
    // Note that for q0-q2, we only range-constrain the first 15 limbs here
    let sum3 = sum(16, |i| s[i] * CLK32[i]) + sum(15, |i| q0[i] * CLK32[16 + i]);
    link => byte2.check(sum3);
    let sum4 = sum(15, |i| q1[i] * CLK32[i]) + sum(15, |i| q2[i] * CLK32[16 + i]);
    link => byte2.check(sum4);

    // The most significant limbs of q0-q2 are constrained to be 32 bits
    // In Polygon's version they are 19 bits, but that requires increasing the minimum degree
    // to fit the lookup.
    // Instead, we decompose the most significant limb into two 16-Bit limbs.
    // Having a larger range-constraint is fine, because we're only multiplying it with 16-bit
    // limbs of the prime, so the result is within 48 bits, still far from overflowing the
    // Goldilocks field.
    pol witness q0_15_high, q0_15_low, q1_15_high, q1_15_low, q2_15_high, q2_15_low;
    link => byte2.check(q0_15_high * CLK32[0] + q0_15_low * CLK32[1] + q1_15_high * CLK32[2] + q1_15_low * CLK32[3] + q2_15_high * CLK32[4] + q2_15_low * CLK32[5]);

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
    let product = constr |a, b| constr |n| {
        // TODO: To reduce the degree of the constraints, we materialize the intermediate result here.
        // this introduces ~256 additional witness columns & constraints.
        let product_res;
        product_res = dot_prod(n + 1, a, |i| b(n - i));
        product_res
    };
    // Same as `product`, but does not materialize the result. Use this to multiply by constants (like `p`).
    let product_inline = |a, b| |n| dot_prod(n + 1, a, |i| b(n - i));
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
    let eq0 = constr |nr|
        product(x1f, y1f)(nr)
        + x2f(nr)
        - shift_right(y2f, 16)(nr)
        - y3f(nr);

    /*******
    *
    * EQ1: s * x2 - s * x1 - y2 + y1 + (q0 * p)
    *
    *******/

    let p = |i| expr(select_limb(secp_modulus, i));

    // The "- 4 * shift_right(p, 16)" effectively subtracts 4 * (p << 16 * 16) = 2 ** 258 * p
    // As a result, the term computes `(x - 2 ** 258) * p`.
    let product_with_p = |x| |nr| product_inline(p, x)(nr) - 4 * shift_right(p, 16)(nr);

    let eq1 = constr |nr| product(sf, x2f)(nr) - product(sf, x1f)(nr) - y2f(nr) + y1f(nr) + product_with_p(q0f)(nr);

    /*******
    *
    * EQ2:  2 * s * y1 - 3 * x1 * x1 + (q0 * p)
    *
    *******/

    let eq2 = constr |nr| 2 * product(sf, y1f)(nr) - 3 * product(x1f, x1f)(nr) + product_with_p(q0f)(nr);

    /*******
    *
    * EQ3:  s * s - x1 - x2 - x3 + (q1 * p)
    *
    *******/

    // If we're doing the ec_double operation (selEq[2] == 1), x2 is so far unconstrained and should be set to x1
    array::new(16, |i| selEq[2] * (x1[i] - x2[i]) = 0);

    let eq3 = constr |nr| product(sf, sf)(nr) - x1f(nr) - x2f(nr) - x3f(nr) + product_with_p(q1f)(nr);


    /*******
    *
    * EQ4:  s * x1 - s * x3 - y1 - y3 + (q2 * p)
    *
    *******/

    let eq4 = constr |nr| product(sf, x1f)(nr) - product(sf, x3f)(nr) - y1f(nr) - y3f(nr) + product_with_p(q2f)(nr);


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
    link => byte2.check(carry_low[0]);
    link => byte2.check(carry_low[1]);
    link => byte2.check(carry_low[2]);
    link => byte2.check(carry_high[0]);
    link => byte2.check(carry_high[1]);
    link => byte2.check(carry_high[2]);

    // Carries can be any integer in the range [-2**31, 2**31 - 1)
    let carry = array::new(3, |i| carry_high[i] * 2**16 + carry_low[i] - 2 ** 31);
    
    array::map(carry, |c| c * CLK32[0] = 0);

    /*******
    *
    * Putting everything together
    *
    *******/
    
    let eq0_sum = sum(32, |i| eq0(i) * CLK32[i]);
    let eq1_sum = sum(32, |i| eq1(i) * CLK32[i]);
    let eq2_sum = sum(32, |i| eq2(i) * CLK32[i]);
    let eq3_sum = sum(32, |i| eq3(i) * CLK32[i]);
    let eq4_sum = sum(32, |i| eq4(i) * CLK32[i]);

    selEq[0] * (eq0_sum + carry[0]) = selEq[0] * carry[0]' * 2**16;
    selEq[1] * (eq1_sum + carry[0]) = selEq[1] * carry[0]' * 2**16;
    selEq[2] * (eq2_sum + carry[0]) = selEq[2] * carry[0]' * 2**16;
    selEq[3] * (eq3_sum + carry[1]) = selEq[3] * carry[1]' * 2**16;
    selEq[3] * (eq4_sum + carry[2]) = selEq[3] * carry[2]' * 2**16;
}
