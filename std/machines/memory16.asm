use std::array;
use std::field::modulus;
use std::check::assert;
use std::machines::range::Bit12;
use std::machines::range::Byte2;

// A read/write memory, similar to that of Polygon:
// https://github.com/0xPolygonHermez/zkevm-proverjs/blob/main/pil/mem.pil
machine Memory16(bit12: Bit12, byte2: Byte2) with
    latch: LATCH,
    operation_id: m_is_write,
    call_selectors: selectors,
{
    // We compute m_step_high (12-Bit) * 2**16 + m_step_low (16-Bit), which fits into 28 Bits.
    assert(modulus() > 2**28, || "Memory16 requires a field that fits any 28-Bit value.");

    operation mload<0> m_addr_high, m_addr_low, m_step -> m_value1, m_value2;
    operation mstore<1> m_addr_high, m_addr_low, m_step, m_value1, m_value2 ->;

    let LATCH = 1;

    // =============== read-write memory =======================
    // Read-write memory. Columns are sorted by addr and
    // then by step. change is 1 if and only if addr changes
    // in the next row.
    // Note that these column names are used by witgen to detect
    // this machine...
    col witness m_addr_high, m_addr_low;
    col witness m_step_high, m_step_low;
    col witness m_change;
    col witness m_value1, m_value2;

    link => bit12.check(m_step_high);
    link => byte2.check(m_step_low);
    let m_step = m_step_high * 2**16 + m_step_low;

    link => byte2.check(m_value1);
    link => byte2.check(m_value2);

    // Memory operation flags
    col witness m_is_write;
    std::utils::force_bool(m_is_write);

    // is_write can only be 1 if a selector is active
    let is_mem_op = array::sum(selectors);
    std::utils::force_bool(is_mem_op);
    (1 - is_mem_op) * m_is_write = 0;

    // If the next line is a not a write and we have an address change,
    // then the value is zero.
    (1 - m_is_write') * m_change * m_value1' = 0;
    (1 - m_is_write') * m_change * m_value2' = 0;

    // change has to be 1 in the last row, so that a first read on row zero is constrained to return 0
    (1 - m_change) * LAST = 0;

    // If the next line is a read and we stay at the same address, then the
    // value cannot change.
    (1 - m_is_write') * (1 - m_change) * (m_value1' - m_value1) = 0;
    (1 - m_is_write') * (1 - m_change) * (m_value2' - m_value2) = 0;

    col fixed FIRST = [1] + [0]*;
    let LAST = FIRST';

    std::utils::force_bool(m_change);

    // if change is zero, addr has to stay the same.
    (m_addr_low' - m_addr_low) * (1 - m_change) = 0;
    (m_addr_high' - m_addr_high) * (1 - m_change) = 0;

    // Except for the last row, if m_change is 1, then addr has to increase,
    // if it is zero, step has to increase.

    // The prover provides which limb to compare and the diff *minus one*
    col witness m_high_limb_equal, m_diff_minus_one;
    link => byte2.check(m_diff_minus_one);
    m_high_limb_equal * (m_high_limb_equal - 1) = 0;

    // Select the actual diff.
    // In the last row we'll use a diff_high of 1, so that the prover can satisfy the constraints
    // below by setting m_high_limb_equal = 0 & m_diff_minus_one = 0.
    // Note that `(m_change - LAST)` is the same as `m_change * (1 - LAST)` (but of lower degree),
    // because m_change is constrained to equal 1 in the last row.
    let diff_high = (m_change - LAST) * (m_addr_high' - m_addr_high)
                    + (1 - m_change) * (m_step_high' - m_step_high)
                    + LAST * 1;
    let diff_low = m_change * (m_addr_low' - m_addr_low)
                     + (1 - m_change) * (m_step_low' - m_step_low);

    // On the last row, we're not doing any diffing. We force m_high_limb_equal
    // to be zero in the last row, in order to deactivate the next constraint.
    LAST * m_high_limb_equal = 0;

    // If m_high_limb_equal is 1, the higher limbs should be equal.
    m_high_limb_equal * diff_high = 0;

    // Assert that m_diff_minus_one stores the actual diff - 1.
    let actual_limb_diff = m_high_limb_equal * diff_low
                           + (1 - m_high_limb_equal) * diff_high;
    (m_diff_minus_one + 1 - actual_limb_diff) = 0;
}
