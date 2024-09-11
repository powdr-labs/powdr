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
    // If the operation is a write operation.
    let m_is_write;

    let m_selector1;
    let m_selector2;
    force_bool(m_selector1);
    force_bool(m_selector2);
    (1 - m_selector1 - m_selector2) * m_is_write = 0;

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

    // m_change has to be 1 in the last row, so that a first read on row zero is constrained to return 0
    (1 - m_change) * LAST = 0;

    m_is_write * (1 - m_is_write) = 0;


    // If the next line is a read and we stay at the same address, then the
    // value cannot change.
    (1 - m_is_write') * (1 - m_change) * (m_value' - m_value) = 0;

    // If the next line is a read and we have an address change,
    // then the value is zero.
    (1 - m_is_write') * m_change * m_value' = 0;

    instr assert_zero X { XIsZero = 1 }
    instr mload -> X { [ 0, ADDR, STEP, X ] is m_selector1 $ [ m_is_write, m_addr, m_step, m_value ] }
    instr mstore X { [ 1, ADDR, STEP, X ] is m_selector2 $ [ m_is_write, m_addr, m_step, m_value ] }

    function main {
        return;
    }
}