use std::array;
use std::machines::byte2::Byte2;

/// This machine is a slightly extended version of std::machines::memory::Memory,
/// where in addition to mstore, there is an mstore_bootloader operation. It behaves
/// just like mstore, except that the first access to each memory cell must come
/// from the mstore_bootloader operation.
machine MemoryWithBootloaderWrite with
    latch: LATCH,
    operation_id: operation_id,
    call_selectors: selectors,
{
    // lower bound degree is 65536

    Byte2 byte2;

    operation mload<0> m_addr, m_step -> m_value;
    operation mstore<1> m_addr, m_step, m_value ->;
    operation mstore_bootloader<2> m_addr, m_step, m_value ->;

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
    col witness m_is_bootloader_write;
    std::utils::force_bool(m_is_write);
    std::utils::force_bool(m_is_bootloader_write);
    col operation_id = m_is_write + 2 * m_is_bootloader_write;

    // is_write can only be 1 if a selector is active
    let is_mem_op = array::sum(selectors);
    std::utils::force_bool(is_mem_op);
    (1 - is_mem_op) * m_is_write = 0;
    (1 - is_mem_op) * m_is_bootloader_write = 0;

    // The first operation of a new address has to be a bootloader write
    m_change * (1 - m_is_bootloader_write') = 0;

    // m_change has to be 1 in the last row, so that the above constraint is triggered.
    // An exception to this when the last address is -1, which is only possible if there is
    // no memory operation in the entire chunk (because addresses are 32 bit unsigned).
    // This exception is necessary so that there can be valid assignment in this case.
    pol m_change_or_no_memory_operations = (1 - m_change) * (m_addr + 1);
    LAST * m_change_or_no_memory_operations = 0;

    // If the next line is a read and we stay at the same address, then the
    // value cannot change.
    (1 - m_is_write' - m_is_bootloader_write') * (1 - m_change) * (m_value' - m_value) = 0;

    col witness m_diff_lower;
    col witness m_diff_upper;

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
