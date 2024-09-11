use std::utils::force_bool;

machine MemReadWrite with degree: 256 {
    reg pc[@pc];
    reg X[<=];
    reg A;
    reg B;
    reg I;
    reg CNT;
    reg ADDR;

    let XInv;
    let XIsZero;
    XIsZero  = 1 - X * XInv;
    XIsZero * X = 0;
    XIsZero * (1 - XIsZero) = 0;

    // Read-write memory. Columns are sorted by m_addr and
    // then by m_step. m_change is 1 if and only if m_addr changes
    // in the next row.
    let m_addr;
    let m_step;
    let m_change;
    let m_value;

    // Memory operation flags
    let m_is_write;
    let m_is_bootloader_write;

    let m_selector1;
    let m_selector2;
    let m_selector3;
    force_bool(m_selector1);
    force_bool(m_selector2);
    force_bool(m_selector3);
    (1 - m_selector1 - m_selector2 - m_selector3) * m_is_write = 0;
    (1 - m_selector1 - m_selector2 - m_selector3) * m_is_bootloader_write = 0;

    // positive numbers (assumed to be much smaller than the field order)
    let POSITIVE: col = |i| { i + 1 };
    col fixed FIRST = [1] + [0]*;
    col fixed LAST  = [0]* + [1];
    let STEP: col = |i| { i };

    m_change * (1 - m_change) = 0;

    // if m_change is zero, m_addr has to stay the same.
    (m_addr' - m_addr) * (1 - m_change) = 0;

    // Except for the last row, if m_change is 1, then m_addr has to increase,
    // if it is zero, m_step has to increase.
    (1 - LAST) $ [ m_change * (m_addr' - m_addr) + (1 - m_change) * (m_step' - m_step) ] in [POSITIVE];

    // m_change has to be 1 in the last row, so that the above constraint is triggered.
    // An exception to this when the last address is -1, which is only possible if there is
    // no memory operation in the entire chunk (because addresses are 32 bit unsigned).
    // This exception is necessary so that there can be valid assignment in this case.
    pol m_change_or_no_memory_operations = (1 - m_change) * (m_addr + 1);
    LAST * m_change_or_no_memory_operations = 0;

    // All operation flags are boolean and either all 0 or exactly 1 is set.
    m_is_write * (1 - m_is_write) = 0;
    m_is_bootloader_write * (1 - m_is_bootloader_write) = 0;

    // If the next line is a read and we stay at the same address, then the
    // value cannot change.
    (1 - m_is_write' - m_is_bootloader_write') * (1 - m_change) * (m_value' - m_value) = 0;

    // The first operation of a new address has to be a bootloader write
    m_change * (1 - m_is_bootloader_write') = 0;

    instr assert_zero X { XIsZero = 1 }
    let operation_id = m_is_write + 2 * m_is_bootloader_write;
    instr mload -> X { [ 0, ADDR, STEP, X ] is m_selector1 $ [ operation_id, m_addr, m_step, m_value ] }
    instr mstore X { [ 1, ADDR, STEP, X ] is m_selector2 $ [ operation_id, m_addr, m_step, m_value ] }
    instr mstore_bootloader X { [ 2, ADDR, STEP, X ] is m_selector3 $ [ operation_id, m_addr, m_step, m_value ] }

    function main {
        ADDR <=X= 4;
        mstore_bootloader 1;
        ADDR <=X= 8;
        mstore_bootloader 4;
        mload A;
        assert_zero A - 4;
        ADDR <=X= 4;
        mload A;
        assert_zero A - 1;
        mstore 5;
        mload A;
        assert_zero A - 5;
        return;
    }
}