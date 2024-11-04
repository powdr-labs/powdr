use std::array;

/// Creates constraints that increments a 32-bits pointer by `amount` bytes.
///
/// Preconditions:
/// - `amount` must be a power of two,
/// - `amount` must be in range [1, 0x10000],
/// - `pre_low` must be a multiple of amount,
/// - `pre_low` must be in range [0, 0xffff].
/// - `int(pre_high) * 0x10000 + int(pre_low) + amount` must be in range [0, 0xffffffff].
///
/// The pointers are given in 2 16-bit limbs. This function returns a set of constraints
/// ensuring that the following are equal:
/// - (int(post_high) * 0x10000 + int(post_low))
/// - (int(pre_high) * 0x10000 + int(pre_low) + amount)
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

// Creates a sequence of 4-byte sparsed addresses.
//
// Takes two arrays of 16-bit limbs addresses, and constrains all the elements of
// the arrays to 4-bytes after the previous one.
//
// Returns an array of constraints.
let address_array_elems = constr |addr_high, addr_low| {
    let addr = array::zip(
        addr_high,
        addr_low,
        |high, low| (high, low)
    );

    array::fold(
        array::zip(
            array::sub_array(addr, 0, array::len(addr) - 1),
            array::sub_array(addr, 1, array::len(addr) - 1),
            constr |(high, low), (next_high, next_low)| {
                increment_ptr(4, high, low, next_high, next_low)
            }
        ), [],
        |a, b| a + b
    )
};
