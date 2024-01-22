let base_field_modulus = 1 << 256 - 1 << 32 - 1 << 9 - 1 << 8 - 1 << 7 - 1 << 6 - 1 << 4 - 1;


// These are utilities specific to polygon's arithmetic machine.
// Inputs are in (x1, y1), and (x2, y2). The output point is in (x3, y3).
// s is an intermediate value (computed through EQ1 or EQ2).
// q0, q1 and q2 are the integer multipliers for the modulo reduction.
// p is the secp256k1 base field modulus.
// EQ3 and EQ4 compute x3 and y3, respectively, from x1, x2 (x3) and s.
//
//    EQ1: s * x2 - s * x1 - y2 + y1 + (q0 * p)   lambda - ADD
//    EQ2: 2 * s * y1 - 3 * x1 * x1 + (q0 * p)    lambda - DBL
//    EQ3: s * s - x1 - x2 - x3 + (q1 * p)        x3
//    EQ4: s * x1 - s * x3 - y1 - y3 + (q2 * p)   y3

use super::ff;
let inverse = |x| ff::inverse(x, base_field_modulus);
let add = |x, y| ff::add(x, y, base_field_modulus);
let sub = |x, y| ff::sub(x, y, base_field_modulus);
let mul = |x, y| ff::mul(x, y, base_field_modulus);
let div = |x, y| ff::div(x, y, base_field_modulus);


let s_for_eq1_add = |x1, y1, x2, y2| div(sub(y2, y1), sub(x2, x1));
// Yes, these are regular integer computations.
// TODO the weird term at the end, I don't really know.
let q0_for_eq1_add = |x1, y1, x2, y2, s| -(s * x2 - s * x1 - y2 + y1) / base_field_modulus + 1 << 258;

let s_for_eq2_double = |x1, y1, x2, y2| div(mul(3, mul(x1, x1)), add(y1, y1));
// Yes, these are regular integer computations.
// TODO the weird term at the end, I don't really know.
let q0_for_eq2_double = |x1, y1, x2, y2, s| -(s * 2 + y1 - 3 * x1 * x1) / base_field_modulus + 1 << 258;

let q1_for_eq3 = |x1, y1, x2, y2, s| -(s * s - x1 - x2 - x3) / base_field_modulus + 1 << 258;
let q2_for_eq4 = |x1, y1, x2, y2, s| -(s * x1 - s * x3 - y1 - y3) / base_field_modulus + 1 << 258;
