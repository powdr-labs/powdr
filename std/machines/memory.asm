use std::convert::int;
use std::utils::cross_product;
use std::utils::unchanged_until;
use std::array;

// A read/write memory, similar to that of Polygon:
// https://github.com/0xPolygonHermez/zkevm-proverjs/blob/main/pil/mem.pil
machine Memory with
    latch: LATCH,
    operation_id: m_is_write,
    call_selectors: selectors,
{
    // lower bound degree is 65536

    operation mload<0> m_addr, m_step -> m_value;
    operation mstore<1> m_addr, m_step, m_value ->;

    let LATCH = 1;

    // =============== read-write memory =======================
    // Read-write memory. Columns are sorted by addr and
    // then by step. change is 1 if and only if addr changes
    // in the next row.
    // Note that these column names are used by witgen to detect
    // this machine...
    col witness m_addr;
    col witness m_step;
    col witness m_change;
    col witness m_value;

    // Memory operation flags
    col witness m_is_write;
    std::utils::force_bool(m_is_write);

    // is_write can only be 1 if a selector is active
    let is_mem_op = array::sum(selectors);
    std::utils::force_bool(is_mem_op);
    (1 - is_mem_op) * m_is_write = 0;

    // If the next line is a not a write and we have an address change,
    // then the value is zero.
    (1 - m_is_write') * m_change * m_value' = 0;

    // change has to be 1 in the last row, so that a first read on row zero is constrained to return 0
    (1 - m_change) * LAST = 0;

    // If the next line is a read and we stay at the same address, then the
    // value cannot change.
    (1 - m_is_write') * (1 - m_change) * (m_value' - m_value) = 0;

    col witness m_diff_lower;
    col witness m_diff_upper;

    col fixed FIRST = [1] + [0]*;
    let LAST = FIRST';
    col fixed STEP(i) { i };
    col fixed BIT16(i) { i & 0xffff };

    {m_diff_lower} in {BIT16};
    {m_diff_upper} in {BIT16};

    std::utils::force_bool(m_change);

    // if change is zero, addr has to stay the same.
    (m_addr' - m_addr) * (1 - m_change) = 0;

    // Except for the last row, if change is 1, then addr has to increase,
    // if it is zero, step has to increase.
    // `m_diff_upper * 2**16 + m_diff_lower` has to be equal to the difference **minus one**.
    // Since we know that both addr and step can only be 32-Bit, this enforces that
    // the values are strictly increasing.
    col diff = (m_change * (m_addr' - m_addr) + (1 - m_change) * (m_step' - m_step));
    (1 - LAST) * (diff - 1 - m_diff_upper * 2**16 - m_diff_lower) = 0;
}
