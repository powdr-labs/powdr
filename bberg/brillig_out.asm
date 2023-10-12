
machine Main {


    degree 256;

    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg Z[<=];

    reg jump_ptr;  // Store the location of a call
    reg addr;       // used for memory operations
    reg tmp;    // used for temporary storage
    reg r0;         // 12 registers for now
    reg r1;
    reg r2;
    reg r3;
    reg r4;
    reg r5;
    reg r6;
    reg r7;
    reg r8;
    reg r9;
    reg r10;
    reg r11;

    // ============== iszero check for X =======================
    col witness XInv;
    col witness XIsZero;
    XIsZero = 1 - X * XInv;
    XIsZero * X = 0;
    XIsZero * (1 - XIsZero) = 0;


    // =============== read-write memory =======================
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


    // ============== memory instructions ==============
    instr store X { { addr, STEP, X } is m_is_write { m_addr, m_step, m_value } }
    instr load -> X { { addr, STEP, X } is m_is_read { m_addr, m_step, m_value } }



    /// Add
    /// Take in two input registers, send the result into the output register
    instr add Y, Z -> X {
        X = Y + Z
    }

    /// Sub
    instr sub Y, Z -> X {
        X = Y - Z
    }

    /// Is the value equal to 0 - uses only assignment registers
    instr eq X -> Y { Y = XIsZero }

    /// Mul
    instr mul Y, Z -> X {
        X = Y * Z
    }
    
    /// move
    // TODO: move should zero out the sending register?
    instr mov Y -> X {
        X = Y
    }

    // When we get a call, we want
    instr call l: label { pc' = l, jump_ptr' = pc + 1 }
    instr ret { pc' = jump_ptr }

    /// Jumps
    instr jump l: label { pc' = l }
    instr jumpi X, l: label { pc' = (1 - XIsZero) * l + XIsZero * (pc + 1) }
    instr jumpni X, l: label { pc' = XIsZero * l + (1 - XIsZero) * (pc + 1) }



    function main {
		r0 <=X= 0;
		r1 <=X= 0;
		call bbdf;
		return;
		bbdf::
		r3 <=X= 3;
		ret;
    }
}    
