/// Creates constraints that increments a pointer by one full 32-bit word.
///
/// Preconditions: pre_low must be 4-byte aligned and in range [0, 0xfffc].
///
/// The pointers are given in 2 16-bit limbs. This function returns a set of constraints
/// ensuring that the following are equal:
/// - (int(post_high) * 0x10000 + int(post_low))
/// - (int(pre_high) * 0x10000 + int(pre_low) + 4)
///
/// The returned constraints are not automatically added to the constraints set. This is so the
/// caller will be able to latch-disable them in rows they aren't needed.
///
/// This constr function introduces 2 new witness column and some helper constraints.
let word_increment_ptr: expr, expr, expr, expr -> Constr[] = constr |pre_high, pre_low, post_high, post_low| {
    // Is the low limb at its maximum value?
    let carry;
    carry = std::utils::is_zero(pre_low - (0x10000 - 4));

    // The increment constraints returned to the caller:
    [
        // If low limb is about to overflow, next value must be 0,
        // otherwise, next value is current plus 4.
        std::constraints::if_else(carry, post_low = 0, post_low = pre_low + 4),

        // Set high limb, incremented if low overflowed:
        post_high = pre_high + carry
    ]
};
