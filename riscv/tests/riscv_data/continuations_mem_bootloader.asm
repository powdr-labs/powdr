
use std::binary::Binary;
use std::shift::Shift;
machine Main {
		Binary binary;
		Shift shift;

degree 262144;
    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg Z[<=];
    reg W[<=];

    reg tmp1;
    reg tmp2;
    reg tmp3;
    reg lr_sc_reservation;
		reg x0;
		reg x1;
		reg x2;
		reg x3;
		reg x4;
		reg x5;
		reg x6;
		reg x7;
		reg x8;
		reg x9;
		reg x10;
		reg x11;
		reg x12;
		reg x13;
		reg x14;
		reg x15;
		reg x16;
		reg x17;
		reg x18;
		reg x19;
		reg x20;
		reg x21;
		reg x22;
		reg x23;
		reg x24;
		reg x25;
		reg x26;
		reg x27;
		reg x28;
		reg x29;
		reg x30;
		reg x31;


    x0 = 0;

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

    let up_to_three = |i| i % 4;
    let six_bits = |i| i % 2**6;
    /// Loads one word from an address Y, where Y can be between 0 and 2**33 (sic!),
    /// wraps the address to 32 bits and rounds it down to the next multiple of 4.
    /// Returns the loaded word and the remainder of the division by 4.
    instr mload Y -> X, Z {
        // Z * (Z - 1) * (Z - 2) * (Z - 3) = 0,
        { Z } in { up_to_three },
        Y = wrap_bit * 2**32 + X_b4 * 0x1000000 + X_b3 * 0x10000 + X_b2 * 0x100 + X_b1 * 4 + Z,
        { X_b1 } in { six_bits },
        {
            X_b4 * 0x1000000 + X_b3 * 0x10000 + X_b2 * 0x100 + X_b1 * 4,
            STEP,
            X
        } is m_is_read { m_addr, m_step, m_value }
        // If we could access the shift machine here, we
        // could even do the following to complete the mload:
        // { W, X, Z} in { shr.value, shr.amount, shr.amount}
    }

    /// Stores Z at address Y % 2**32. Y can be between 0 and 2**33.
    /// Y should be a multiple of 4, but this instruction does not enforce it.
    instr mstore Y, Z {
        { X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000, STEP, Z } is m_is_write { m_addr, m_step, m_value },
        // Wrap the addr value
        Y = (X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000) + wrap_bit * 2**32
    }

    // ============== control-flow instructions ==============

    instr jump l: label { pc' = l }
    instr load_label l: label -> X { X = l }
    instr jump_dyn X { pc' = X }
    instr jump_and_link_dyn X { pc' = X, x1' = pc + 1 }
    instr call l: label { pc' = l, x1' = pc + 1 }
    // TODO x6 actually stores some relative address, but only part of it.
    instr tail l: label { pc' = l, x6' = l }
    instr ret { pc' = x1 }

    instr branch_if_nonzero X, l: label { pc' = (1 - XIsZero) * l + XIsZero * (pc + 1) }
    instr branch_if_zero X, l: label { pc' = XIsZero * l + (1 - XIsZero) * (pc + 1) }

    // Skips Y instructions if X is zero
    instr skip_if_zero X, Y { pc' = pc + 1 + (XIsZero * Y) }

    // input X is required to be the difference of two 32-bit unsigend values.
    // i.e. -2**32 < X < 2**32
    instr branch_if_positive X, l: label {
        X + 2**32 - 1 = X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000 + wrap_bit * 2**32,
        pc' = wrap_bit * l + (1 - wrap_bit) * (pc + 1)
    }
    // input X is required to be the difference of two 32-bit unsigend values.
    // i.e. -2**32 < X < 2**32
    instr is_positive X -> Y {
        X + 2**32 - 1 = X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000 + wrap_bit * 2**32,
        Y = wrap_bit
    }

    // ================= logical instructions =================

    instr is_equal_zero X -> Y { Y = XIsZero }
    instr is_not_equal_zero X -> Y { Y = 1 - XIsZero }

    // ================= coprocessor substitution instructions =================

    // ================= binary/bitwise instructions =================
    instr and Y, Z -> X = binary.and
    instr or Y, Z -> X = binary.or
    instr xor Y, Z -> X = binary.xor

            
    // ================= shift instructions =================
    instr shl Y, Z -> X = shift.shl
    instr shr Y, Z -> X = shift.shr

            
    // Wraps a value in Y to 32 bits.
    // Requires 0 <= Y < 2**33
    instr wrap Y -> X { Y = X + wrap_bit * 2**32, X = X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000 }
    // Requires -2**32 <= Y < 2**32
    instr wrap_signed Y -> X { Y + 2**32 = X + wrap_bit * 2**32, X = X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000 }
    col fixed bytes(i) { i & 0xff };
    col witness X_b1;
    col witness X_b2;
    col witness X_b3;
    col witness X_b4;
    { X_b1 } in { bytes };
    { X_b2 } in { bytes };
    { X_b3 } in { bytes };
    { X_b4 } in { bytes };
    col witness wrap_bit;
    wrap_bit * (1 - wrap_bit) = 0;

    // Input is a 32 bit unsigned number. We check bit 7 and set all higher bits to that value.
    instr sign_extend_byte Y -> X {
        // wrap_bit is used as sign_bit here.
        Y = Y_7bit + wrap_bit * 0x80 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000,
        X = Y_7bit + wrap_bit * 0xffffff80
    }
    col fixed seven_bit(i) { i & 0x7f };
    col witness Y_7bit;
    { Y_7bit } in { seven_bit };

    // Input is a 32 bit unsigned number. We check bit 15 and set all higher bits to that value.
    instr sign_extend_16_bits Y -> X {
        Y_15bit = X_b1 + Y_7bit * 0x100,

        // wrap_bit is used as sign_bit here.
        Y = Y_15bit + wrap_bit * 0x8000 + X_b3 * 0x10000 + X_b4 * 0x1000000,
        X = Y_15bit + wrap_bit * 0xffff8000
    }
    col witness Y_15bit;

    // Input is a 32 but unsigned number (0 <= Y < 2**32) interpreted as a two's complement numbers.
    // Returns a signed number (-2**31 <= X < 2**31).
    instr to_signed Y -> X {
        // wrap_bit is used as sign_bit here.
        Y = X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + Y_7bit * 0x1000000 + wrap_bit * 0x80000000,
        X = Y - wrap_bit * 2**32
    }

    // ======================= assertions =========================

    instr fail { 1 = 0 }

    // Removes up to 16 bits beyond 32
    // TODO is this really safe?
    instr wrap16 Y -> X { Y = Y_b5 * 2**32 + Y_b6 * 2**40 + X, X = X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000 }
    col witness Y_b5;
    col witness Y_b6;
    col witness Y_b7;
    col witness Y_b8;
    { Y_b5 } in { bytes };
    { Y_b6 } in { bytes };
    { Y_b7 } in { bytes };
    { Y_b8 } in { bytes };

    col witness REM_b1;
    col witness REM_b2;
    col witness REM_b3;
    col witness REM_b4;
    { REM_b1 } in { bytes };
    { REM_b2 } in { bytes };
    { REM_b3 } in { bytes };
    { REM_b4 } in { bytes };

    // implements Z = Y / X and W = Y % X.
    instr divremu Y, X -> Z, W {
        // main division algorithm:
        // Y is the known dividend
        // X is the known divisor
        // Z is the unknown quotient
        // W is the unknown remainder
        // if X is zero, remainder is set to dividend, as per RISC-V specification:
        X * Z + W = Y,

        // remainder >= 0:
        W = REM_b1 + REM_b2 * 0x100 + REM_b3 * 0x10000 + REM_b4 * 0x1000000,

        // remainder < divisor, conditioned to X not being 0:
        (1 - XIsZero) * (X - W - 1 - Y_b5 - Y_b6 * 0x100 - Y_b7 * 0x10000 - Y_b8 * 0x1000000) = 0,

        // in case X is zero, we set quotient according to RISC-V specification
        XIsZero * (Z - 0xffffffff) = 0,

        // quotient is 32 bits:
        Z = X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000
    }

    // Multiply two 32-bits unsigned, return the upper and lower unsigned 32-bit
    // halves of the result.
    // X is the lower half (least significant bits)
    // Y is the higher half (most significant bits)
    instr mul Z, W -> X, Y {
        Z * W = X + Y * 2**32,
        X = X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000,
        Y = Y_b5 + Y_b6 * 0x100 + Y_b7 * 0x10000 + Y_b8 * 0x1000000
    }


    function main {
        // Number of pages
        x1 <=X= ${ ("input", 36) };
        x1 <== wrap(x1);

        branch_if_zero x1, end_page_loop;

        // Current page index
        x2 <=X= 0;

        start_page_loop::

        // Start address
        x3 <=X= ${ ("input", x2 * (256 + 1) + 36 + 1) };
        x3 <== wrap(x3);

        // Current word index
        x4 <=X= 0;

        start_word_loop::

        // Store word
        mstore x3 + x4 * 4, ${ ("input", x2 * (256 + 1) + 36 + 2 + x4) };

        // Increment word index
        x4 <=X= x4 + 1;

        branch_if_nonzero x4 - 256, start_word_loop;

        end_word_loop::

        // Increment page index
        x2 <=X= x2 + 1;

        branch_if_nonzero x2 - x1, start_page_loop;

        end_page_loop::

        // Initialize registers, starting with index 0
        x1 <=X= ${ ("input", 0) };
        x2 <=X= ${ ("input", 1) };
        x3 <=X= ${ ("input", 2) };
        x4 <=X= ${ ("input", 3) };
        x5 <=X= ${ ("input", 4) };
        x6 <=X= ${ ("input", 5) };
        x7 <=X= ${ ("input", 6) };
        x8 <=X= ${ ("input", 7) };
        x9 <=X= ${ ("input", 8) };
        x10 <=X= ${ ("input", 9) };
        x11 <=X= ${ ("input", 10) };
        x12 <=X= ${ ("input", 11) };
        x13 <=X= ${ ("input", 12) };
        x14 <=X= ${ ("input", 13) };
        x15 <=X= ${ ("input", 14) };
        x16 <=X= ${ ("input", 15) };
        x17 <=X= ${ ("input", 16) };
        x18 <=X= ${ ("input", 17) };
        x19 <=X= ${ ("input", 18) };
        x20 <=X= ${ ("input", 19) };
        x21 <=X= ${ ("input", 20) };
        x22 <=X= ${ ("input", 21) };
        x23 <=X= ${ ("input", 22) };
        x24 <=X= ${ ("input", 23) };
        x25 <=X= ${ ("input", 24) };
        x26 <=X= ${ ("input", 25) };
        x27 <=X= ${ ("input", 26) };
        x28 <=X= ${ ("input", 27) };
        x29 <=X= ${ ("input", 28) };
        x30 <=X= ${ ("input", 29) };
        x31 <=X= ${ ("input", 30) };
        tmp1 <=X= ${ ("input", 31) };
        tmp2 <=X= ${ ("input", 32) };
        tmp3 <=X= ${ ("input", 33) };
        lr_sc_reservation <=X= ${ ("input", 34) };

        // Commented out for now, because there is no remaining program...
        // jump ${ ("input", 35) };

        // Return for now, as the RISC-V executor requires it
        return;
    }
}    
