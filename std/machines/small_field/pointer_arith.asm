/// Increments a pointer by one full 32-bit word.
///
/// The pointers are given in 2 16-bit limbs. This function return the polynomials constraining
/// (int(post_high) * 0x10000 + int(post_low)) to (int(pre_high) * 0x10000 + int(pre_low) + 4).
///
/// The returned polynomials are not automatically added as constraints. This is so the caller
/// will be able to latch-disable them in rows they aren't needed.
///
/// I think it can be used in both directions: to increment and decrement. Just swap the inputs.
///
/// This constr function introduces 2 new witness column and some helper constraints.
///
/// Preconditions: pre_low and post_low must be 16-bit aligned and in range [0, 0xfffc].
let word_increment_ptr: expr, expr, expr, expr -> expr[] = constr |pre_high, pre_low, post_high, post_low| {
    // How far away from overflowing the low limb is:
    let low_diff = pre_low - (0x10000 - 4);
    // Is one if low limb is about to overflow:
    let low_overflow;
    // Helper to allow low_overflow to be boolean
    let low_diff_inv;

    low_overflow = 1 - low_diff_inv * low_diff;

    // Ensures that (low_diff_inv * low_diff) is boolean,
    // and that low_diff_inv is not 0 when low_diff is not 0:
    (low_diff_inv * low_diff - 1) * low_diff = 0;

    // Increment polynomials, to be used by the caller in constraints:
    [
        // If low limb is about to overflow, next value must be 0:
        low_overflow * post_low +
        // Otherwise, next value is current plus 4:
        (1 - low_overflow) * (pre_low + 4 - post_low),

        // Set high limb, incremented if low overflowed:
        post_high - pre_high - low_overflow
    ]
};
