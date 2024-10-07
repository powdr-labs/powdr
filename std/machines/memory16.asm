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
    // The diff for the step is a 28-Bit value.
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
    col witness m_step;
    col witness m_change;
    col witness m_value1, m_value2;

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
    // The diff has to be equal to the difference **minus one**.

    // These two helper columns have different semantics, depending on
    // whether we're comparing addresses or time steps.
    // In both cases, m_tmp2 needs to be of 16 Bits.
    col witness m_tmp1, m_tmp2;
    link => byte2.check(m_diff);

    // When comparing time steps, a 28-Bit diff is sufficient assuming a maximum step
    // of 2**28.
    // The difference is computed on the field, which is larger than 2**28.
    // We prove that m_step' - m_step > 0 by letting the prover provide a 28-Bit value
    // such that claimed_diff + 1 == m_step' - m_step.
    // TODO: This does assume that m_step is a 28-Bit value. For rows that come from the
    //       permutation (i.e., have a matching LHS in the main VM), this is guaranteed,
    //       because the value comes from a fixed column. However, the prover can
    //       "insert" reads without a matching LHS at any time (setting the selector to 0).
    //       This allows the prover to increase the step by 2**28 in each row, over-flowing
    //       eventually.
    let m_diff_upper = m_tmp1;
    let m_diff_lower = m_tmp2;
    link if (1 - m_change) => bit12.check(m_diff_upper);
    let claimed_time_step_diff = m_diff_upper * 2**16 + m_diff_lower;
    let actual_time_step_diff = m_step' - m_step;
    (1 - m_change) * (claimed_time_step_diff + 1 - actual_time_step_diff) = 0;

    // When comparing addresses, we let the prover indicate whether the upper or lower
    // limb needs to be compared and then assert that the diff is positive.
    let address_high_unequal = m_tmp1;
    let m_diff = m_tmp2;

    // address_high_unequal is binary.
    m_change * address_high_unequal * (address_high_unequal - 1) = 0;

    // Whether to do any comparison.
    // We want to compare whenever m_change == 1, but not in the last row.
    // Because we constrained m_change to be 1 in the last row, this will just
    // be equal to m_change, except that the last entry is 0.
    // (`m_change * (1 - LAST)` would be the same, but of higher degree.) 
    let do_comparison = m_change - LAST;

    // If address_high_unequal is 0, the higher limbs should be equal.
    do_comparison * (1 - address_high_unequal) * (m_addr_high' - m_addr_high) = 0;

    // Assert that m_diff stores the actual diff - 1.
    let actual_addr_limb_diff = address_high_unequal * (m_addr_high' - m_addr_high)
                                + (1 - address_high_unequal) * (m_addr_low' - m_addr_low);
    do_comparison * (m_diff + 1 - actual_addr_limb_diff) = 0;
}
