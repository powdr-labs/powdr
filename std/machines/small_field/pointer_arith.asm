/// Creates constraints that increments a pointer by amount.
///
/// Preconditions:
/// - amount must be a power of two,
/// - amount must be in range [1, 0x10000],
/// - pre_low must be a multiple of amount,
/// - pre_low must be in range [0, 0xffff].
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
let increment_ptr: expr, expr, expr, expr, expr -> Constr[] = constr |
    amount,
    pre_high,
    pre_low,
    post_high,
    post_low
| {
    // Is the low limb at its maximum value?
    let carry;
    carry = std::utils::is_zero(pre_low - (0x10000 - amount));

    // The increment constraints returned to the caller:
    [
        // If low limb is about to overflow, next value must be 0,
        // otherwise, next value is current plus 4.
        std::constraints::if_else(carry, post_low = 0, post_low = pre_low + amount),

        // Set high limb, incremented if low overflowed:
        post_high = pre_high + carry
    ]
};
