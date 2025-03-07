use std::array;
use std::machines::range::Byte2;
use std::field::modulus;
use std::check::require_field_bits;

/// This is a slightly different version of std::machines::memory::Memory,
/// where initial read is unconstrained unconstrained instead of returning 0, but
/// it can only happen when m_step is 0 (i.e., from the initial memory verification,
/// inside the bootloader).
machine MemoryWithBootloaderWrite(byte2: Byte2) with
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

    // The first operation of a new address has to either have
    // m_step = 0 (to mean it is inside the bootloader) or be a write.
    m_change * m_step * (1 - m_is_write)= 0;

    // m_change has to be 1 in the last row, so that the above constraint is triggered.
    // An exception to this when the last address is -1, which is only possible if there is
    // no memory operation in the entire chunk (because addresses are 32 bit unsigned).
    // This exception is necessary so that there can be valid assignment in this case.
    pol m_change_or_no_memory_operations = (1 - m_change) * (m_addr + 1);
    LAST * m_change_or_no_memory_operations = 0;

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
