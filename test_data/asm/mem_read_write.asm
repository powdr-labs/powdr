reg pc[@pc];
reg X[<=];
reg A;
reg B;
reg I;
reg CNT;
reg ADDR;

pil{
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
    // If we have an operation at all (needed because this needs to be a permutation)
    col witness m_op;
    // If the operation is a write operation.
    col witness m_is_write;
    col witness m_is_read;

    // positive numbers (assumed to be much smaller than the field order)
    col fixed POSITIVE(i) { i + 1 };
    col fixed FIRST = [1] + [0]*;
    col fixed LAST(i) { FIRST(i + 1) };
    col fixed STEP(i) { i };

    m_change * (1 - m_change) = 0;

    // if m_change is zero, m_addr has to stay the same.
    (m_addr' - m_addr) * (1 - m_change) = 0;

    // Except for the last row, if m_change is 1, then m_addr has to increase,
    // if it is zero, m_step has to increase.
    (1 - LAST) { m_change * (m_addr' - m_addr) + (1 - m_change) * (m_step' - m_step) } in POSITIVE;

    m_op * (1 - m_op) = 0;
    m_is_write * (1 - m_is_write) = 0;
    m_is_read * (1 - m_is_read) = 0;
    // m_is_write can only be 1 if m_op is 1.
    m_is_write * (1 - m_op) = 0;
    m_is_read * (1 - m_op) = 0;
    m_is_read * m_is_write = 0;


    // If the next line is a read and we stay at the same address, then the
    // value cannot change.
    (1 - m_is_write') * (1 - m_change) * (m_value' - m_value) = 0;

    // If the next line is a read and we have an address change,
    // then the value is zero.
    (1 - m_is_write') * m_change * m_value' = 0;

}

instr assert_zero X { XIsZero = 1 }
instr mstore X { { ADDR, STEP, X } is m_is_write { m_addr, m_step, m_value } }
instr mload -> X { { ADDR, STEP, X } is m_is_read { m_addr, m_step, m_value } }


ADDR <=X= 4;
mstore 1;
ADDR <=X= 8;
mstore 4;
mload A;
assert_zero A - 4;
ADDR <=X= 4;
mload A;
assert_zero A - 1;
