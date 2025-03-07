use std::array;
use std::machines::range::Byte2;
use std::field::modulus;
use std::check::require_field_bits;

// A read/write memory, similar to that of Polygon:
// https://github.com/0xPolygonHermez/zkevm-proverjs/blob/main/pil/mem.pil
machine Memory(byte2: Byte2) with
    latch: LATCH,
    operation_id: m_is_write,
    call_selectors: selectors,
{
    require_field_bits(32, || "Memory requires a field that fits any 32-Bit value.");
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
    let m_addr;
    let m_step;
    let m_change;
    let m_value;

    // Memory operation flags
    let m_is_write;
    std::utils::force_bool(m_is_write);

    // is_write can only be 1 if a selector is active
    let is_mem_op = array::sum(selectors);
    std::utils::force_bool(is_mem_op);
    (1 - is_mem_op) * m_is_write = 0;

    // If the next line is a not a write and we have an address change,
    // then the value is zero.
    (1 - m_is_write') * m_change * m_value' = 0;

    // m_change has to be 1 in the last row, so that a first read on row zero is constrained to return 0
    (1 - m_change) * LAST = 0;

    // If the next line is a read and we stay at the same address, then the
    // value cannot change.
    (1 - m_is_write') * (1 - m_change) * (m_value' - m_value) = 0;

    let m_diff_lower;
    let m_diff_upper;

    col fixed FIRST = [1] + [0]*;
    let LAST = FIRST';

    link => byte2.check(m_diff_lower);
    link => byte2.check(m_diff_upper);

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
