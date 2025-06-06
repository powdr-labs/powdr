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
use std::machines::range::Byte;
use std::machines::range::Byte2;

// Implements 32-Bit multiplication and division.
// Inspired by Polygon's 256-Bit Arith machine: https://github.com/0xPolygonHermez/zkevm-proverjs/blob/main/pil/arith.pil
// Requires the field to contain at least 18 bits.
machine Arith(byte: Byte, byte2: Byte2) with
    latch: CLK8_7,
    operation_id: is_division,
    // Allow this machine to be connected via a permutation
    call_selectors: sel,
{
    require_field_bits(18, || "Arith requires a field that fits any 18-Bit value.");

    col witness is_division;

    // Computes x1 * y1 + x2, where all inputs / outputs are 32-bit words (represented as 16-bit limbs in big-endian order).
    // More precisely, affine(x1, y1, x2) = (y2, y3), where x1 * y1 + x2 = 2**16 * y2 + y3
    operation mul<0> x1c[1], x1c[0], x2c[1], x2c[0], y1c[1], y1c[0] -> y2c[1], y2c[0], y3c[1], y3c[0];

    // y3 / x1 = y1 (remainder x2)
    // WARNING: it's not constrained that remainder is less than the divisor.
    // WARNING: For division by zero, the quotient is unconstrained.
    // Both need to be handled by any machine calling into this one.
    operation div<1> y3c[1], y3c[0], x1c[1], x1c[0] -> y1c[1], y1c[0], x2c[1], x2c[0];

    // Constrain that y2 = 0 when operation is div.
    array::new(4, |i| is_division * y2[i] = 0);

    // Selects the ith limb of x (little endian)
    // All limbs are 8 bits
    let select_limb = |x, i| if i >= 0 {
        (x >> (i * 8)) & 0xff
    } else {
        0
    };

    let limbs_to_int: fe[] -> int = |limbs| array::sum(array::map_enumerated(limbs, |i, limb| int(limb) << (i * 8)));
    let int_to_limbs: int -> fe[] = |x| array::new(4, |i| fe(select_limb(x, i)));

    // We need to provide hints for the quotient and remainder, because they are not unique under our current constraints.
    // They are unique given additional main machine constraints, but it's still good to provide hints for the solver.
    query |i| std::prover::compute_from_multi_if(
        is_division = 1,
        y1 + x2,
        i,
        y3 + x1,
        |values| {
            let y3_value = limbs_to_int([values[0], values[1], values[2], values[3]]);
            let x1_value = limbs_to_int([values[4], values[5], values[6], values[7]]);
            if x1_value == 0 {
                // Quotient is unconstrained, use zero for y1
                // and set remainder x2 = y3.
                [0, 0, 0, 0] + int_to_limbs(y3_value)
            } else {
                let quotient = y3_value / x1_value;
                let remainder = y3_value % x1_value;
                int_to_limbs(quotient) + int_to_limbs(remainder)
            }
        }
    );

    pol commit x1[4], x2[4], y1[4], y2[4], y3[4];

    let combine: expr[] -> expr[] = |x| array::new(array::len(x) / 2, |i| x[2 * i + 1] * 2**8 + x[2 * i]);
    // Intermediate polynomials, arrays of 16 columns, 16 bit per column.
    col x1c[2] = combine(x1);
    col y1c[2] = combine(y1);
    col x2c[2] = combine(x2);
    col y2c[2] = combine(y2);
    col y3c[2] = combine(y3);

    let CLK8: col[8] = array::new(8, |i| |row| if row % 8 == i { 1 } else { 0 });
    let CLK8_7: expr = CLK8[7];

    // All inputs & outputs are kept constant within a block.

    let fixed_inside_8_block = |e| unchanged_until(e, CLK8[7]);

    array::map(x1, fixed_inside_8_block);
    array::map(y1, fixed_inside_8_block);
    array::map(x2, fixed_inside_8_block);
    array::map(y2, fixed_inside_8_block);
    array::map(y3, fixed_inside_8_block);

    // All input & output limbs are range-constrained to be bytes.

    // sum0 (and the others) used to be inlined inside `byte2.check(...)`,
    // but that causes an issue in the bus linker mode due to the expressions
    // being copied syntactically before resolving the closures.
    // Moving these expressions out of the link fixes it.
    let sum0 = sum(4, |i| x1[i] * CLK8[i]) + sum(4, |i| y1[i] * CLK8[4 + i]);
    link => byte.check(sum0);
    let sum1 = sum(4, |i| x2[i] * CLK8[i]) + sum(4, |i| y2[i] * CLK8[4 + i]);
    link => byte.check(sum1);
    let sum2 = sum(4, |i| y3[i] * CLK8[i]);
    link => byte.check(sum2);

    // Constrain x1 * y1 + x2 - y2 * 2**16 - y3 = 0

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

    // Defined for arguments from 0 to 7 (inclusive)
    let eq0 = |nr|
        product(x1f, y1f)(nr)
        + x2f(nr)
        - shift_right(y2f, 4)(nr)
        - y3f(nr);

    // Carry: Constrained to be 16-Bit and zero in the first row of the block.
    col witness carry;
    link => byte2.check(carry);
    carry * CLK8[0] = 0;

    // Putting everything together    
    let eq0_sum = sum(8, |i| eq0(i) * CLK8[i]);
    eq0_sum + carry = carry' * 2**8;
}
