use std::utils::force_bool;

// A variant of the `mem_read_write` machine which does not have the limitation that
// gaps between accessed memory cells must not be larger than the degree.
// This test uses two 8-bit digits to represent the diff, so the diff has to be
// representable in 16 bits.
machine MemReadWrite with degree: 256 {
    reg pc[@pc];
    reg X[<=];
    reg A;
    reg B;
    reg I;
    reg CNT;
    reg ADDR;

    col witness XInv;
    col witness XIsZero;
    XIsZero  = 1 - X * XInv;
    XIsZero * X = 0;
    XIsZero * (1 - XIsZero) = 0;

    // Read-write memory. Columns are sorted by m_addr and
    // then by m_step. m_change is 1 if and only if m_addr changes
    // in the next row.
    col witness m_addr;
    col witness m_step;
    col witness m_change;
    col witness m_value;
    // If the operation is a write operation.
    col witness m_is_write;
    col witness m_diff_lower;
    col witness m_diff_upper;

    col witness m_selector1;
    col witness m_selector2;
    force_bool(m_selector1);
    force_bool(m_selector2);
    (1 - m_selector1 - m_selector2) * m_is_write = 0;

    col fixed FIRST = [1] + [0]*;
    col fixed LAST  = [0]* + [1];
    col fixed STEP(i) { i };
    col fixed BYTE(i) { i & 0xff };

    [m_diff_lower] in [BYTE];
    [m_diff_upper] in [BYTE];

    m_change * (1 - m_change) = 0;

    // if m_change is zero, m_addr has to stay the same.
    (m_addr' - m_addr) * (1 - m_change) = 0;

    // Except for the last row, if m_change is 1, then m_addr has to increase,
    // if it is zero, m_step has to increase.
    // `m_diff_upper * 2**8 + m_diff_lower` has to be equal to the difference **minus one**.
    col diff = (m_change * (m_addr' - m_addr) + (1 - m_change) * (m_step' - m_step));
    (1 - LAST) * (diff - 1 - m_diff_upper * 2**8 - m_diff_lower) = 0;

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
        ADDR <=X= 4;
        mstore 1;
        // Write to the largest aligned memory cell.
        // This wouldn't be possible in the `mem_read_write` machine.
        ADDR <=X= 0xfffc;
        mstore 4;
        mload A;
        assert_zero A - 4;
        ADDR <=X= 4;
        mload A;
        assert_zero A - 1;
        return;
    }
}