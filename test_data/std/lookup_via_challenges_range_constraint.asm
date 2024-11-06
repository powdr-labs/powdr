use std::convert::fe;
use std::protocols::lookup::lookup;
use std::math::fp2::from_base;
use std::prover::challenge;

machine Main with degree: 8 {

    // Prove a correct decomposition of x into 3-bit limbs
    col fixed x = [1, 6, 23, 55, 63, 4, 1, 0];
    col witness x_low, x_high;

    col fixed BIT3 = [0, 1, 2, 3, 4, 5, 6, 7];

    lookup([x_low] in [BIT3]);
    lookup([x_high] in [BIT3]);

    x = x_low + 8 * x_high;
}