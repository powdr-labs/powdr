
use std::binary::Binary;
use std::hash::poseidon_gl::PoseidonGL;
use std::shift::Shift;
use std::split::split_gl::SplitGL;
machine Main {
		Binary binary;
		PoseidonGL poseidon_gl;
		Shift shift;
		SplitGL split_gl;

degree 262144;
    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg Z[<=];
    reg W[<=];
reg A0[<=];
reg A1[<=];
reg A2[<=];
reg A3[<=];
reg A4[<=];
reg A5[<=];
reg A6[<=];
reg A7[<=];
reg A8[<=];
reg A9[<=];
reg A10[<=];
reg A11[<=];
reg P0;
reg P1;
reg P2;
reg P3;
reg P4;
reg P5;
reg P6;
reg P7;
reg P8;
reg P9;
reg P10;
reg P11;
    reg tmp1;
    reg tmp2;
    reg tmp3;
    reg tmp4;
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

    // ============== bootloader-specific instructions =======================
    // Write-once memory
    let BOOTLOADER_INPUT_ADDRESS = |i| i;
    let bootloader_input_value;
    // Loads a value. If the cell is empty, the prover can choose a value.
    instr load_bootloader_input X -> Y { {X, Y} in {BOOTLOADER_INPUT_ADDRESS, bootloader_input_value} }

    // HACK: We should not use these columns & constraints and instead be able to use X.
    // However, this seems to currently violate some constraints, because X is neither
    // an input nor an output of the jump_to_bootloader_input_if_nonzero instruction.
    col witness tmp_addr;
    col witness tmp_addrInv;
    col witness tmp_addrIsZero;
    tmp_addrIsZero = 1 - tmp_addr * tmp_addrInv;
    tmp_addrIsZero * tmp_addr = 0;
    tmp_addrIsZero * (1 - tmp_addrIsZero) = 0;
    (1 - instr_jump_to_bootloader_input_if_nonzero) * tmp_addr = 0;

    // Sets the PC to the bootloader input at the provided index if it is nonzero
    instr jump_to_bootloader_input_if_nonzero Y {
        // Atomically reads the bootloader input at index Y and jumps to it if it is nonzero
        // (otherwise, the PC is incremented by 1).
        {Y, tmp_addr} in {BOOTLOADER_INPUT_ADDRESS, bootloader_input_value},
        pc' = (1 - tmp_addrIsZero) * tmp_addr + tmp_addrIsZero * (pc + 1)
    }

    // Expose initial register values as public outputs
    public initial_x1 = bootloader_input_value(0);
    public initial_x2 = bootloader_input_value(1);
    public initial_x3 = bootloader_input_value(2);
    public initial_x4 = bootloader_input_value(3);
    public initial_x5 = bootloader_input_value(4);
    public initial_x6 = bootloader_input_value(5);
    public initial_x7 = bootloader_input_value(6);
    public initial_x8 = bootloader_input_value(7);
    public initial_x9 = bootloader_input_value(8);
    public initial_x10 = bootloader_input_value(9);
    public initial_x11 = bootloader_input_value(10);
    public initial_x12 = bootloader_input_value(11);
    public initial_x13 = bootloader_input_value(12);
    public initial_x14 = bootloader_input_value(13);
    public initial_x15 = bootloader_input_value(14);
    public initial_x16 = bootloader_input_value(15);
    public initial_x17 = bootloader_input_value(16);
    public initial_x18 = bootloader_input_value(17);
    public initial_x19 = bootloader_input_value(18);
    public initial_x20 = bootloader_input_value(19);
    public initial_x21 = bootloader_input_value(20);
    public initial_x22 = bootloader_input_value(21);
    public initial_x23 = bootloader_input_value(22);
    public initial_x24 = bootloader_input_value(23);
    public initial_x25 = bootloader_input_value(24);
    public initial_x26 = bootloader_input_value(25);
    public initial_x27 = bootloader_input_value(26);
    public initial_x28 = bootloader_input_value(27);
    public initial_x29 = bootloader_input_value(28);
    public initial_x30 = bootloader_input_value(29);
    public initial_x31 = bootloader_input_value(30);
    public initial_tmp1 = bootloader_input_value(31);
    public initial_tmp2 = bootloader_input_value(32);
    public initial_tmp3 = bootloader_input_value(33);
    public initial_tmp4 = bootloader_input_value(34);
    public initial_lr_sc_reservation = bootloader_input_value(35);
    public initial_P0 = bootloader_input_value(36);
    public initial_P1 = bootloader_input_value(37);
    public initial_P2 = bootloader_input_value(38);
    public initial_P3 = bootloader_input_value(39);
    public initial_P4 = bootloader_input_value(40);
    public initial_P5 = bootloader_input_value(41);
    public initial_P6 = bootloader_input_value(42);
    public initial_P7 = bootloader_input_value(43);
    public initial_P8 = bootloader_input_value(44);
    public initial_P9 = bootloader_input_value(45);
    public initial_P10 = bootloader_input_value(46);
    public initial_P11 = bootloader_input_value(47);
    public initial_pc = bootloader_input_value(48);

    public initial_memory_hash_1 = bootloader_input_value(49);
    public initial_memory_hash_2 = bootloader_input_value(50);
    public initial_memory_hash_3 = bootloader_input_value(51);
    public initial_memory_hash_4 = bootloader_input_value(52);

    // ============== Constraint on x0 =======================

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

    // m_change has to be 1 in the last row, so that a first read on row zero is constrained to return 0
    (1 - m_change) * LAST = 0;

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

            
// ================== hashing instructions ==============
instr poseidon_gl A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11 -> X, Y, Z, W = poseidon_gl.poseidon_permutation


    // ================= shift instructions =================
    instr shl Y, Z -> X = shift.shl
    instr shr Y, Z -> X = shift.shr

            
// ================== wrapping instructions ==============
instr split_gl Z -> X, Y = split_gl.split

    
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
		debug file 1 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/fmt" "mod.rs";
		debug file 2 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/fmt" "num.rs";
		debug file 3 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/alloc" "src/ffi/c_str.rs";
		debug file 4 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/slice/iter" "macros.rs";
		debug file 5 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/ptr" "mut_ptr.rs";
		debug file 6 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/fmt" "builders.rs";
		debug file 7 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/alloc" "src/vec/mod.rs";
		debug file 8 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/alloc" "src/collections/mod.rs";
		debug file 9 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/ptr" "mod.rs";
		debug file 10 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/alloc" "src/raw_vec.rs";
		debug file 11 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/alloc" "src/alloc.rs";
		debug file 12 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src" "error.rs";
		debug file 13 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/alloc" "src/string.rs";
		debug file 14 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/num" "uint_macros.rs";
		debug file 15 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src" "intrinsics.rs";
		debug file 16 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/alloc" "src/vec/spec_extend.rs";
		debug file 17 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src" "cmp.rs";
		debug file 18 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/alloc" "layout.rs";
		debug file 19 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src" "result.rs";
		debug file 20 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/alloc" "src/boxed.rs";
		debug file 21 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/convert" "mod.rs";
		debug file 22 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/alloc" "src/slice.rs";
		debug file 23 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/alloc" "src/str.rs";
		debug file 24 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/ptr" "const_ptr.rs";
		debug file 25 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/alloc" "src/borrow.rs";
		debug file 26 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/alloc" "src/collections/btree/mem.rs";
		debug file 27 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/alloc" "src/collections/btree/node.rs";
		debug file 28 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/alloc" "src/collections/btree/set_val.rs";
		debug file 29 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src" "option.rs";
		debug file 30 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/slice" "memchr.rs";
		debug file 31 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/slice" "mod.rs";
		debug file 32 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/mem" "mod.rs";
		debug file 33 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/ffi" "c_str.rs";
		debug file 34 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/alloc" "src/fmt.rs";
		debug file 35 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/iter/traits" "iterator.rs";
		debug file 36 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/iter/traits" "accum.rs";
		debug file 37 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/str" "mod.rs";
		debug file 38 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/iter/adapters" "map.rs";
		debug file 39 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/alloc" "src/rc.rs";
		debug file 40 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/mem" "manually_drop.rs";
		debug file 41 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/mem" "maybe_uninit.rs";
		debug file 42 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/num" "mod.rs";
		debug file 43 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/str" "validations.rs";
		debug file 44 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/str" "iter.rs";
		debug file 45 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/str" "traits.rs";
		debug file 46 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/iter/traits" "double_ended.rs";
		debug file 47 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/iter/adapters" "skip_while.rs";
		debug file 48 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/iter/adapters" "rev.rs";
		debug file 49 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/char" "decode.rs";
		debug file 50 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/slice" "iter.rs";
		debug file 51 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/iter/adapters" "cloned.rs";
		debug file 52 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/char" "methods.rs";
		debug file 53 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/str" "pattern.rs";
		debug file 54 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/alloc" "src/vec/drain.rs";
		debug file 55 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/alloc" "src/sync.rs";
		debug file 56 "/Users/georg/.cargo/registry/src/github.com-1ecc6299db9ec823/compiler_builtins-0.1.85" "src/macros.rs";
		debug file 57 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/ops" "bit.rs";
		debug file 58 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src" "cmp.rs";
		debug file 59 "/Users/georg/.cargo/registry/src/github.com-1ecc6299db9ec823/compiler_builtins-0.1.85" "src/float/add.rs";
		debug file 60 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/num" "uint_macros.rs";
		debug file 61 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/num" "int_macros.rs";
		debug file 62 "/Users/georg/.cargo/registry/src/github.com-1ecc6299db9ec823/compiler_builtins-0.1.85" "src/float/mod.rs";
		debug file 63 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/ops" "arith.rs";
		debug file 64 "/Users/georg/.cargo/registry/src/github.com-1ecc6299db9ec823/compiler_builtins-0.1.85" "src/int/mod.rs";
		debug file 65 "/Users/georg/.cargo/registry/src/github.com-1ecc6299db9ec823/compiler_builtins-0.1.85" "src/float/cmp.rs";
		debug file 66 "/Users/georg/.cargo/registry/src/github.com-1ecc6299db9ec823/compiler_builtins-0.1.85" "src/float/conv.rs";
		debug file 67 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/num" "f64.rs";
		debug file 68 "/Users/georg/.cargo/registry/src/github.com-1ecc6299db9ec823/compiler_builtins-0.1.85" "src/float/div.rs";
		debug file 69 "/Users/georg/.cargo/registry/src/github.com-1ecc6299db9ec823/compiler_builtins-0.1.85" "src/float/extend.rs";
		debug file 70 "/Users/georg/.cargo/registry/src/github.com-1ecc6299db9ec823/compiler_builtins-0.1.85" "src/float/mul.rs";
		debug file 71 "/Users/georg/.cargo/registry/src/github.com-1ecc6299db9ec823/compiler_builtins-0.1.85" "src/float/pow.rs";
		debug file 72 "/Users/georg/.cargo/registry/src/github.com-1ecc6299db9ec823/compiler_builtins-0.1.85" "src/float/sub.rs";
		debug file 73 "/Users/georg/.cargo/registry/src/github.com-1ecc6299db9ec823/compiler_builtins-0.1.85" "src/float/trunc.rs";
		debug file 74 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/num" "f32.rs";
		debug file 75 "/Users/georg/.cargo/registry/src/github.com-1ecc6299db9ec823/compiler_builtins-0.1.85" "src/int/specialized_div_rem/delegate.rs";
		debug file 76 "/Users/georg/.cargo/registry/src/github.com-1ecc6299db9ec823/compiler_builtins-0.1.85" "src/int/specialized_div_rem/norm_shift.rs";
		debug file 77 "/Users/georg/.cargo/registry/src/github.com-1ecc6299db9ec823/compiler_builtins-0.1.85" "src/int/specialized_div_rem/mod.rs";
		debug file 78 "/Users/georg/.cargo/registry/src/github.com-1ecc6299db9ec823/compiler_builtins-0.1.85" "src/int/specialized_div_rem/binary_long.rs";
		debug file 79 "/Users/georg/.cargo/registry/src/github.com-1ecc6299db9ec823/compiler_builtins-0.1.85" "src/int/addsub.rs";
		debug file 80 "/Users/georg/.cargo/registry/src/github.com-1ecc6299db9ec823/compiler_builtins-0.1.85" "src/int/leading_zeros.rs";
		debug file 81 "/Users/georg/.cargo/registry/src/github.com-1ecc6299db9ec823/compiler_builtins-0.1.85" "src/int/mul.rs";
		debug file 82 "/Users/georg/.cargo/registry/src/github.com-1ecc6299db9ec823/compiler_builtins-0.1.85" "src/int/sdiv.rs";
		debug file 83 "/Users/georg/.cargo/registry/src/github.com-1ecc6299db9ec823/compiler_builtins-0.1.85" "src/int/udiv.rs";
		debug file 84 "/Users/georg/.cargo/registry/src/github.com-1ecc6299db9ec823/compiler_builtins-0.1.85" "src/int/shift.rs";
		debug file 85 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/convert" "num.rs";
		debug file 86 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/convert" "mod.rs";
		debug file 87 "/Users/georg/.cargo/registry/src/github.com-1ecc6299db9ec823/compiler_builtins-0.1.85" "src/mem/impls.rs";
		debug file 88 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/ptr" "mut_ptr.rs";
		debug file 89 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/ptr" "const_ptr.rs";
		debug file 90 "/Users/georg/.cargo/registry/src/github.com-1ecc6299db9ec823/compiler_builtins-0.1.85" "src/mem/mod.rs";
		debug file 91 "/Users/georg/.cargo/registry/src/github.com-1ecc6299db9ec823/compiler_builtins-0.1.85" "src/riscv.rs";
		debug file 92 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/num/flt2dec/strategy/dragon.rs";
		debug file 93 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/num/flt2dec/strategy/grisu.rs";
		debug file 94 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/fmt/num.rs";
		debug file 95 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/fmt/mod.rs";
		debug file 96 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/num/dec2flt/table.rs";
		debug file 97 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/unicode/unicode_data.rs";
		debug file 98 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/ops/function.rs";
		debug file 99 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/ptr/mod.rs";
		debug file 100 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/num/f32.rs";
		debug file 101 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/num/f64.rs";
		debug file 102 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/num/dec2flt/common.rs";
		debug file 103 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/num/dec2flt/decimal.rs";
		debug file 104 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/iter/range.rs";
		debug file 105 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/slice/index.rs";
		debug file 106 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/ptr/const_ptr.rs";
		debug file 107 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/iter/adapters/take.rs";
		debug file 108 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/slice/iter/macros.rs";
		debug file 109 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/iter/adapters/enumerate.rs";
		debug file 110 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/array/mod.rs";
		debug file 111 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/option.rs";
		debug file 112 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/cmp.rs";
		debug file 113 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/slice/mod.rs";
		debug file 114 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/num/uint_macros.rs";
		debug file 115 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/intrinsics.rs";
		debug file 116 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/ptr/mut_ptr.rs";
		debug file 117 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/iter/adapters/rev.rs";
		debug file 118 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/num/dec2flt/float.rs";
		debug file 119 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/num/dec2flt/lemire.rs";
		debug file 120 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/num/int_macros.rs";
		debug file 121 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/num/dec2flt/parse.rs";
		debug file 122 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/num/mod.rs";
		debug file 123 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/iter/traits/iterator.rs";
		debug file 124 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/iter/adapters/zip.rs";
		debug file 125 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/num/dec2flt/mod.rs";
		debug file 126 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/num/diy_float.rs";
		debug file 127 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/num/flt2dec/decoder.rs";
		debug file 128 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/num/flt2dec/estimator.rs";
		debug file 129 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/num/bignum.rs";
		debug file 130 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/iter/traits/double_ended.rs";
		debug file 131 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/macros/mod.rs";
		debug file 132 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/num/flt2dec/mod.rs";
		debug file 133 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/iter/adapters/cloned.rs";
		debug file 134 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/slice/iter.rs";
		debug file 135 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/num/fmt.rs";
		debug file 136 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/num/error.rs";
		debug file 137 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/ops/range.rs";
		debug file 138 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/char/methods.rs";
		debug file 139 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/ops/arith.rs";
		debug file 140 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/mem/transmutability.rs";
		debug file 141 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/ptr/alignment.rs";
		debug file 142 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/num/nonzero.rs";
		debug file 143 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/convert/mod.rs";
		debug file 144 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/error.rs";
		debug file 145 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/char/convert.rs";
		debug file 146 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/ffi/c_str.rs";
		debug file 147 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/any.rs";
		debug file 148 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/result.rs";
		debug file 149 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/fmt/builders.rs";
		debug file 150 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/ascii.rs";
		debug file 151 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/cell.rs";
		debug file 152 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/char/decode.rs";
		debug file 153 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/char/mod.rs";
		debug file 154 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/clone.rs";
		debug file 155 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/slice/ascii.rs";
		debug file 156 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/slice/memchr.rs";
		debug file 157 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/slice/cmp.rs";
		debug file 158 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/ffi/mod.rs";
		debug file 159 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/iter/adapters/filter.rs";
		debug file 160 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/iter/traits/collect.rs";
		debug file 161 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/panic/location.rs";
		debug file 162 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/panic/panic_info.rs";
		debug file 163 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/panicking.rs";
		debug file 164 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/sync/atomic.rs";
		debug file 165 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/str/iter.rs";
		debug file 166 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/str/pattern.rs";
		debug file 167 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/str/mod.rs";
		debug file 168 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/fmt/float.rs";
		debug file 169 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/str/count.rs";
		debug file 170 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/str/validations.rs";
		debug file 171 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/iter/traits/accum.rs";
		debug file 172 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/iter/adapters/map.rs";
		debug file 173 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/mem/mod.rs";
		debug file 174 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/str/traits.rs";
		debug file 175 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/iter/traits/exact_size.rs";
		debug file 176 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/mem/maybe_uninit.rs";
		debug file 177 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/iter/adapters/fuse.rs";
		debug file 178 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/iter/adapters/flatten.rs";
		debug file 179 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/internal_macros.rs";
		debug file 180 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/str/converts.rs";
		debug file 181 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/str/error.rs";
		debug file 182 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/str/lossy.rs";
		debug file 183 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/time.rs";
		debug file 184 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/num/int_log10.rs";
		debug file 185 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/unicode/printable.rs";
		debug file 186 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/task/wake.rs";
		debug file 187 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/alloc/layout.rs";
		debug file 188 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/alloc/mod.rs";
		debug file 189 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/num/dec2flt/number.rs";
		debug file 190 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/intrinsics/mir.rs";
		debug file 191 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/marker.rs";
		debug file 192 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/ops/index_range.rs";
		debug file 193 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/hash/sip.rs";
		debug file 194 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/ptr/non_null.rs";
		debug file 195 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/iter/adapters/copied.rs";
		debug file 196 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/iter/adapters/chain.rs";
		debug file 197 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/ops/bit.rs";
		debug file 198 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/future/mod.rs";
		debug file 199 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/../../stdarch/crates/core_arch/src/simd.rs";
		debug file 200 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/../../portable-simd/crates/core_simd/src/swizzle.rs";
		debug file 201 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/../../portable-simd/crates/core_simd/src/masks.rs";
		debug file 202 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/alloc/src" "raw_vec.rs";
		debug file 203 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src" "result.rs";
		debug file 204 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/alloc/src" "alloc.rs";
		debug file 205 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/num" "uint_macros.rs";
		debug file 206 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src" "cmp.rs";
		debug file 207 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/alloc" "layout.rs";
		debug file 208 "/private/var/folders/sm/xh2t696x06zfh9q5m4xxg3y00000gn/T/40d967f993b24a8d963c1a89fc9ce196" "src/lib.rs";
		debug file 209 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/alloc/src/vec" "mod.rs";
		debug file 210 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/ptr" "mut_ptr.rs";
		debug file 211 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/ptr" "mod.rs";
		debug file 212 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/iter" "range.rs";
		debug file 213 "/private/var/folders/sm/xh2t696x06zfh9q5m4xxg3y00000gn/T/40d967f993b24a8d963c1a89fc9ce196" "runtime/src/lib.rs";
		debug file 214 "/private/var/folders/sm/xh2t696x06zfh9q5m4xxg3y00000gn/T/40d967f993b24a8d963c1a89fc9ce196" "runtime/src/allocator.rs";
		debug file 215 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/fmt" "mod.rs";
		debug file 216 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/char" "methods.rs";
		debug file 217 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/ptr" "mut_ptr.rs";
		debug file 218 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src" "option.rs";
		debug file 219 "/private/var/folders/sm/xh2t696x06zfh9q5m4xxg3y00000gn/T/40d967f993b24a8d963c1a89fc9ce196" "runtime/src/fmt.rs";
		debug file 220 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/slice/iter" "macros.rs";
		debug file 221 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/iter/adapters" "copied.rs";
		debug file 222 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/str" "iter.rs";
		debug file 223 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/ptr" "mod.rs";
		debug file 224 "/private/var/folders/sm/xh2t696x06zfh9q5m4xxg3y00000gn/T/40d967f993b24a8d963c1a89fc9ce196" "runtime/src/coprocessors.rs";
		debug file 225 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/slice" "mod.rs";
		debug file 226 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/iter/adapters" "enumerate.rs";
		debug file 227 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src" "result.rs";
		debug file 228 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src" "cell.rs";
		debug file 229 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/mem" "mod.rs";
		debug file 230 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/alloc" "global.rs";
		debug file 231 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src" "cmp.rs";
		debug file 232 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src" "intrinsics.rs";
		debug file 233 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/alloc" "layout.rs";
		debug file 234 "/Users/georg/.rustup/toolchains/nightly-2023-01-03-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/ptr" "alignment.rs";
		x10 <== and(x10, x10);
		x10 <== shl(x10, x10);
		x10, x11 <== split_gl(x10);
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		P0 <=X= 0;
		P1 <=X= 0;
		P2 <=X= 0;
		P3 <=X= 0;
		x10 <=X= 0;
		x11 <=X= 0;
		
		// START OF BOOTLOADER
		
		// Number of pages
		x1 <== load_bootloader_input(53);
		x1 <== wrap(x1);
		
		// Current page index
		x2 <=X= 0;
		
		branch_if_zero x1, end_page_loop;
		
		start_page_loop:
		
		// Page number
		x3 <== load_bootloader_input(x2 * 345 + 54);
		x3 <== and(x3, 4194303);
		
		// Store & hash 256 page words. This is an unrolled loop that for each each word:
		// - Loads the word into the P{(i % 4) + 4} register
		// - Stores the word at the address x3 * 1024 + i * 4
		// - If i % 4 == 3: Hashes registers P0-P11, storing the result in P0-P3
		//
		// At the end of the loop, we'll have a linear hash of the page in P0-P3, using a Merkle-Damgard
		// construction. The initial P0-P3 values are 0, and the capacity (P8-P11) is 0 throughout the
		// booloader execution.
		
		P0 <=X= 0;
		P1 <=X= 0;
		P2 <=X= 0;
		P3 <=X= 0;
		P4 <=X= 0;
		P5 <=X= 0;
		P6 <=X= 0;
		P7 <=X= 0;
		
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 0);
		mstore x3 * 1024 + 0 * 4, P4;
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 1);
		mstore x3 * 1024 + 1 * 4, P5;
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 2);
		mstore x3 * 1024 + 2 * 4, P6;
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 3);
		mstore x3 * 1024 + 3 * 4, P7;
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 4);
		mstore x3 * 1024 + 4 * 4, P4;
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 5);
		mstore x3 * 1024 + 5 * 4, P5;
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 6);
		mstore x3 * 1024 + 6 * 4, P6;
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 7);
		mstore x3 * 1024 + 7 * 4, P7;
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 8);
		mstore x3 * 1024 + 8 * 4, P4;
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 9);
		mstore x3 * 1024 + 9 * 4, P5;
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 10);
		mstore x3 * 1024 + 10 * 4, P6;
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 11);
		mstore x3 * 1024 + 11 * 4, P7;
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 12);
		mstore x3 * 1024 + 12 * 4, P4;
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 13);
		mstore x3 * 1024 + 13 * 4, P5;
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 14);
		mstore x3 * 1024 + 14 * 4, P6;
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 15);
		mstore x3 * 1024 + 15 * 4, P7;
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 16);
		mstore x3 * 1024 + 16 * 4, P4;
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 17);
		mstore x3 * 1024 + 17 * 4, P5;
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 18);
		mstore x3 * 1024 + 18 * 4, P6;
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 19);
		mstore x3 * 1024 + 19 * 4, P7;
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 20);
		mstore x3 * 1024 + 20 * 4, P4;
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 21);
		mstore x3 * 1024 + 21 * 4, P5;
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 22);
		mstore x3 * 1024 + 22 * 4, P6;
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 23);
		mstore x3 * 1024 + 23 * 4, P7;
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 24);
		mstore x3 * 1024 + 24 * 4, P4;
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 25);
		mstore x3 * 1024 + 25 * 4, P5;
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 26);
		mstore x3 * 1024 + 26 * 4, P6;
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 27);
		mstore x3 * 1024 + 27 * 4, P7;
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 28);
		mstore x3 * 1024 + 28 * 4, P4;
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 29);
		mstore x3 * 1024 + 29 * 4, P5;
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 30);
		mstore x3 * 1024 + 30 * 4, P6;
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 31);
		mstore x3 * 1024 + 31 * 4, P7;
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 32);
		mstore x3 * 1024 + 32 * 4, P4;
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 33);
		mstore x3 * 1024 + 33 * 4, P5;
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 34);
		mstore x3 * 1024 + 34 * 4, P6;
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 35);
		mstore x3 * 1024 + 35 * 4, P7;
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 36);
		mstore x3 * 1024 + 36 * 4, P4;
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 37);
		mstore x3 * 1024 + 37 * 4, P5;
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 38);
		mstore x3 * 1024 + 38 * 4, P6;
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 39);
		mstore x3 * 1024 + 39 * 4, P7;
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 40);
		mstore x3 * 1024 + 40 * 4, P4;
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 41);
		mstore x3 * 1024 + 41 * 4, P5;
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 42);
		mstore x3 * 1024 + 42 * 4, P6;
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 43);
		mstore x3 * 1024 + 43 * 4, P7;
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 44);
		mstore x3 * 1024 + 44 * 4, P4;
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 45);
		mstore x3 * 1024 + 45 * 4, P5;
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 46);
		mstore x3 * 1024 + 46 * 4, P6;
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 47);
		mstore x3 * 1024 + 47 * 4, P7;
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 48);
		mstore x3 * 1024 + 48 * 4, P4;
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 49);
		mstore x3 * 1024 + 49 * 4, P5;
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 50);
		mstore x3 * 1024 + 50 * 4, P6;
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 51);
		mstore x3 * 1024 + 51 * 4, P7;
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 52);
		mstore x3 * 1024 + 52 * 4, P4;
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 53);
		mstore x3 * 1024 + 53 * 4, P5;
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 54);
		mstore x3 * 1024 + 54 * 4, P6;
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 55);
		mstore x3 * 1024 + 55 * 4, P7;
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 56);
		mstore x3 * 1024 + 56 * 4, P4;
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 57);
		mstore x3 * 1024 + 57 * 4, P5;
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 58);
		mstore x3 * 1024 + 58 * 4, P6;
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 59);
		mstore x3 * 1024 + 59 * 4, P7;
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 60);
		mstore x3 * 1024 + 60 * 4, P4;
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 61);
		mstore x3 * 1024 + 61 * 4, P5;
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 62);
		mstore x3 * 1024 + 62 * 4, P6;
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 63);
		mstore x3 * 1024 + 63 * 4, P7;
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 64);
		mstore x3 * 1024 + 64 * 4, P4;
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 65);
		mstore x3 * 1024 + 65 * 4, P5;
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 66);
		mstore x3 * 1024 + 66 * 4, P6;
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 67);
		mstore x3 * 1024 + 67 * 4, P7;
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 68);
		mstore x3 * 1024 + 68 * 4, P4;
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 69);
		mstore x3 * 1024 + 69 * 4, P5;
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 70);
		mstore x3 * 1024 + 70 * 4, P6;
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 71);
		mstore x3 * 1024 + 71 * 4, P7;
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 72);
		mstore x3 * 1024 + 72 * 4, P4;
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 73);
		mstore x3 * 1024 + 73 * 4, P5;
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 74);
		mstore x3 * 1024 + 74 * 4, P6;
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 75);
		mstore x3 * 1024 + 75 * 4, P7;
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 76);
		mstore x3 * 1024 + 76 * 4, P4;
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 77);
		mstore x3 * 1024 + 77 * 4, P5;
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 78);
		mstore x3 * 1024 + 78 * 4, P6;
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 79);
		mstore x3 * 1024 + 79 * 4, P7;
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 80);
		mstore x3 * 1024 + 80 * 4, P4;
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 81);
		mstore x3 * 1024 + 81 * 4, P5;
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 82);
		mstore x3 * 1024 + 82 * 4, P6;
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 83);
		mstore x3 * 1024 + 83 * 4, P7;
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 84);
		mstore x3 * 1024 + 84 * 4, P4;
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 85);
		mstore x3 * 1024 + 85 * 4, P5;
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 86);
		mstore x3 * 1024 + 86 * 4, P6;
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 87);
		mstore x3 * 1024 + 87 * 4, P7;
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 88);
		mstore x3 * 1024 + 88 * 4, P4;
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 89);
		mstore x3 * 1024 + 89 * 4, P5;
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 90);
		mstore x3 * 1024 + 90 * 4, P6;
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 91);
		mstore x3 * 1024 + 91 * 4, P7;
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 92);
		mstore x3 * 1024 + 92 * 4, P4;
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 93);
		mstore x3 * 1024 + 93 * 4, P5;
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 94);
		mstore x3 * 1024 + 94 * 4, P6;
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 95);
		mstore x3 * 1024 + 95 * 4, P7;
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 96);
		mstore x3 * 1024 + 96 * 4, P4;
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 97);
		mstore x3 * 1024 + 97 * 4, P5;
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 98);
		mstore x3 * 1024 + 98 * 4, P6;
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 99);
		mstore x3 * 1024 + 99 * 4, P7;
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 100);
		mstore x3 * 1024 + 100 * 4, P4;
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 101);
		mstore x3 * 1024 + 101 * 4, P5;
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 102);
		mstore x3 * 1024 + 102 * 4, P6;
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 103);
		mstore x3 * 1024 + 103 * 4, P7;
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 104);
		mstore x3 * 1024 + 104 * 4, P4;
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 105);
		mstore x3 * 1024 + 105 * 4, P5;
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 106);
		mstore x3 * 1024 + 106 * 4, P6;
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 107);
		mstore x3 * 1024 + 107 * 4, P7;
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 108);
		mstore x3 * 1024 + 108 * 4, P4;
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 109);
		mstore x3 * 1024 + 109 * 4, P5;
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 110);
		mstore x3 * 1024 + 110 * 4, P6;
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 111);
		mstore x3 * 1024 + 111 * 4, P7;
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 112);
		mstore x3 * 1024 + 112 * 4, P4;
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 113);
		mstore x3 * 1024 + 113 * 4, P5;
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 114);
		mstore x3 * 1024 + 114 * 4, P6;
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 115);
		mstore x3 * 1024 + 115 * 4, P7;
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 116);
		mstore x3 * 1024 + 116 * 4, P4;
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 117);
		mstore x3 * 1024 + 117 * 4, P5;
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 118);
		mstore x3 * 1024 + 118 * 4, P6;
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 119);
		mstore x3 * 1024 + 119 * 4, P7;
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 120);
		mstore x3 * 1024 + 120 * 4, P4;
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 121);
		mstore x3 * 1024 + 121 * 4, P5;
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 122);
		mstore x3 * 1024 + 122 * 4, P6;
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 123);
		mstore x3 * 1024 + 123 * 4, P7;
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 124);
		mstore x3 * 1024 + 124 * 4, P4;
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 125);
		mstore x3 * 1024 + 125 * 4, P5;
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 126);
		mstore x3 * 1024 + 126 * 4, P6;
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 127);
		mstore x3 * 1024 + 127 * 4, P7;
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 128);
		mstore x3 * 1024 + 128 * 4, P4;
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 129);
		mstore x3 * 1024 + 129 * 4, P5;
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 130);
		mstore x3 * 1024 + 130 * 4, P6;
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 131);
		mstore x3 * 1024 + 131 * 4, P7;
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 132);
		mstore x3 * 1024 + 132 * 4, P4;
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 133);
		mstore x3 * 1024 + 133 * 4, P5;
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 134);
		mstore x3 * 1024 + 134 * 4, P6;
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 135);
		mstore x3 * 1024 + 135 * 4, P7;
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 136);
		mstore x3 * 1024 + 136 * 4, P4;
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 137);
		mstore x3 * 1024 + 137 * 4, P5;
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 138);
		mstore x3 * 1024 + 138 * 4, P6;
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 139);
		mstore x3 * 1024 + 139 * 4, P7;
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 140);
		mstore x3 * 1024 + 140 * 4, P4;
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 141);
		mstore x3 * 1024 + 141 * 4, P5;
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 142);
		mstore x3 * 1024 + 142 * 4, P6;
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 143);
		mstore x3 * 1024 + 143 * 4, P7;
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 144);
		mstore x3 * 1024 + 144 * 4, P4;
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 145);
		mstore x3 * 1024 + 145 * 4, P5;
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 146);
		mstore x3 * 1024 + 146 * 4, P6;
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 147);
		mstore x3 * 1024 + 147 * 4, P7;
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 148);
		mstore x3 * 1024 + 148 * 4, P4;
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 149);
		mstore x3 * 1024 + 149 * 4, P5;
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 150);
		mstore x3 * 1024 + 150 * 4, P6;
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 151);
		mstore x3 * 1024 + 151 * 4, P7;
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 152);
		mstore x3 * 1024 + 152 * 4, P4;
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 153);
		mstore x3 * 1024 + 153 * 4, P5;
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 154);
		mstore x3 * 1024 + 154 * 4, P6;
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 155);
		mstore x3 * 1024 + 155 * 4, P7;
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 156);
		mstore x3 * 1024 + 156 * 4, P4;
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 157);
		mstore x3 * 1024 + 157 * 4, P5;
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 158);
		mstore x3 * 1024 + 158 * 4, P6;
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 159);
		mstore x3 * 1024 + 159 * 4, P7;
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 160);
		mstore x3 * 1024 + 160 * 4, P4;
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 161);
		mstore x3 * 1024 + 161 * 4, P5;
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 162);
		mstore x3 * 1024 + 162 * 4, P6;
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 163);
		mstore x3 * 1024 + 163 * 4, P7;
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 164);
		mstore x3 * 1024 + 164 * 4, P4;
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 165);
		mstore x3 * 1024 + 165 * 4, P5;
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 166);
		mstore x3 * 1024 + 166 * 4, P6;
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 167);
		mstore x3 * 1024 + 167 * 4, P7;
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 168);
		mstore x3 * 1024 + 168 * 4, P4;
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 169);
		mstore x3 * 1024 + 169 * 4, P5;
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 170);
		mstore x3 * 1024 + 170 * 4, P6;
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 171);
		mstore x3 * 1024 + 171 * 4, P7;
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 172);
		mstore x3 * 1024 + 172 * 4, P4;
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 173);
		mstore x3 * 1024 + 173 * 4, P5;
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 174);
		mstore x3 * 1024 + 174 * 4, P6;
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 175);
		mstore x3 * 1024 + 175 * 4, P7;
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 176);
		mstore x3 * 1024 + 176 * 4, P4;
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 177);
		mstore x3 * 1024 + 177 * 4, P5;
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 178);
		mstore x3 * 1024 + 178 * 4, P6;
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 179);
		mstore x3 * 1024 + 179 * 4, P7;
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 180);
		mstore x3 * 1024 + 180 * 4, P4;
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 181);
		mstore x3 * 1024 + 181 * 4, P5;
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 182);
		mstore x3 * 1024 + 182 * 4, P6;
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 183);
		mstore x3 * 1024 + 183 * 4, P7;
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 184);
		mstore x3 * 1024 + 184 * 4, P4;
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 185);
		mstore x3 * 1024 + 185 * 4, P5;
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 186);
		mstore x3 * 1024 + 186 * 4, P6;
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 187);
		mstore x3 * 1024 + 187 * 4, P7;
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 188);
		mstore x3 * 1024 + 188 * 4, P4;
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 189);
		mstore x3 * 1024 + 189 * 4, P5;
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 190);
		mstore x3 * 1024 + 190 * 4, P6;
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 191);
		mstore x3 * 1024 + 191 * 4, P7;
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 192);
		mstore x3 * 1024 + 192 * 4, P4;
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 193);
		mstore x3 * 1024 + 193 * 4, P5;
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 194);
		mstore x3 * 1024 + 194 * 4, P6;
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 195);
		mstore x3 * 1024 + 195 * 4, P7;
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 196);
		mstore x3 * 1024 + 196 * 4, P4;
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 197);
		mstore x3 * 1024 + 197 * 4, P5;
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 198);
		mstore x3 * 1024 + 198 * 4, P6;
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 199);
		mstore x3 * 1024 + 199 * 4, P7;
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 200);
		mstore x3 * 1024 + 200 * 4, P4;
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 201);
		mstore x3 * 1024 + 201 * 4, P5;
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 202);
		mstore x3 * 1024 + 202 * 4, P6;
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 203);
		mstore x3 * 1024 + 203 * 4, P7;
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 204);
		mstore x3 * 1024 + 204 * 4, P4;
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 205);
		mstore x3 * 1024 + 205 * 4, P5;
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 206);
		mstore x3 * 1024 + 206 * 4, P6;
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 207);
		mstore x3 * 1024 + 207 * 4, P7;
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 208);
		mstore x3 * 1024 + 208 * 4, P4;
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 209);
		mstore x3 * 1024 + 209 * 4, P5;
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 210);
		mstore x3 * 1024 + 210 * 4, P6;
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 211);
		mstore x3 * 1024 + 211 * 4, P7;
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 212);
		mstore x3 * 1024 + 212 * 4, P4;
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 213);
		mstore x3 * 1024 + 213 * 4, P5;
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 214);
		mstore x3 * 1024 + 214 * 4, P6;
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 215);
		mstore x3 * 1024 + 215 * 4, P7;
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 216);
		mstore x3 * 1024 + 216 * 4, P4;
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 217);
		mstore x3 * 1024 + 217 * 4, P5;
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 218);
		mstore x3 * 1024 + 218 * 4, P6;
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 219);
		mstore x3 * 1024 + 219 * 4, P7;
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 220);
		mstore x3 * 1024 + 220 * 4, P4;
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 221);
		mstore x3 * 1024 + 221 * 4, P5;
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 222);
		mstore x3 * 1024 + 222 * 4, P6;
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 223);
		mstore x3 * 1024 + 223 * 4, P7;
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 224);
		mstore x3 * 1024 + 224 * 4, P4;
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 225);
		mstore x3 * 1024 + 225 * 4, P5;
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 226);
		mstore x3 * 1024 + 226 * 4, P6;
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 227);
		mstore x3 * 1024 + 227 * 4, P7;
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 228);
		mstore x3 * 1024 + 228 * 4, P4;
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 229);
		mstore x3 * 1024 + 229 * 4, P5;
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 230);
		mstore x3 * 1024 + 230 * 4, P6;
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 231);
		mstore x3 * 1024 + 231 * 4, P7;
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 232);
		mstore x3 * 1024 + 232 * 4, P4;
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 233);
		mstore x3 * 1024 + 233 * 4, P5;
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 234);
		mstore x3 * 1024 + 234 * 4, P6;
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 235);
		mstore x3 * 1024 + 235 * 4, P7;
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 236);
		mstore x3 * 1024 + 236 * 4, P4;
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 237);
		mstore x3 * 1024 + 237 * 4, P5;
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 238);
		mstore x3 * 1024 + 238 * 4, P6;
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 239);
		mstore x3 * 1024 + 239 * 4, P7;
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 240);
		mstore x3 * 1024 + 240 * 4, P4;
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 241);
		mstore x3 * 1024 + 241 * 4, P5;
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 242);
		mstore x3 * 1024 + 242 * 4, P6;
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 243);
		mstore x3 * 1024 + 243 * 4, P7;
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 244);
		mstore x3 * 1024 + 244 * 4, P4;
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 245);
		mstore x3 * 1024 + 245 * 4, P5;
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 246);
		mstore x3 * 1024 + 246 * 4, P6;
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 247);
		mstore x3 * 1024 + 247 * 4, P7;
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 248);
		mstore x3 * 1024 + 248 * 4, P4;
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 249);
		mstore x3 * 1024 + 249 * 4, P5;
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 250);
		mstore x3 * 1024 + 250 * 4, P6;
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 251);
		mstore x3 * 1024 + 251 * 4, P7;
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 252);
		mstore x3 * 1024 + 252 * 4, P4;
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 253);
		mstore x3 * 1024 + 253 * 4, P5;
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 254);
		mstore x3 * 1024 + 254 * 4, P6;
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 255);
		mstore x3 * 1024 + 255 * 4, P7;
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		// == Merkle proof validation ==
		// We commit to the memory content by hashing it in pages of 256 words each.
		// These hashes are stored in a binary Merkle tree of depth 23.
		// At this point, the current page hash is in P0-P3. In order to validate the Merkle proof,
		// we need to re-compute the Merkle root from the prover-provided sibling page hashes.
		//
		// This is an unrolled loop that for each level:
		// - If the ith bit of the page number is 0:
		//   - Load sibling into registers P4-P7
		// - Else:
		//   - Write registers P0-P3 to registers P4-P7
		//   - Load sibling into registers P0-P3
		// - Hash registers P0-P11, storing the result in P0-P3
		//
		// At the end of the loop, we'll have the Merkle root in P0-P3.
		
		x4 <== and(x3, 1);
		branch_if_nonzero x4, level_0_is_right;
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 0 * 4 + 0);
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 0 * 4 + 1);
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 0 * 4 + 2);
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 0 * 4 + 3);
		jump level_0_end;
		level_0_is_right:
		P4 <=X= P0;
		P5 <=X= P1;
		P6 <=X= P2;
		P7 <=X= P3;
		P0 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 0 * 4 + 0);
		P1 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 0 * 4 + 1);
		P2 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 0 * 4 + 2);
		P3 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 0 * 4 + 3);
		level_0_end:
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		x4 <== and(x3, 2);
		branch_if_nonzero x4, level_1_is_right;
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 1 * 4 + 0);
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 1 * 4 + 1);
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 1 * 4 + 2);
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 1 * 4 + 3);
		jump level_1_end;
		level_1_is_right:
		P4 <=X= P0;
		P5 <=X= P1;
		P6 <=X= P2;
		P7 <=X= P3;
		P0 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 1 * 4 + 0);
		P1 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 1 * 4 + 1);
		P2 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 1 * 4 + 2);
		P3 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 1 * 4 + 3);
		level_1_end:
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		x4 <== and(x3, 4);
		branch_if_nonzero x4, level_2_is_right;
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 2 * 4 + 0);
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 2 * 4 + 1);
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 2 * 4 + 2);
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 2 * 4 + 3);
		jump level_2_end;
		level_2_is_right:
		P4 <=X= P0;
		P5 <=X= P1;
		P6 <=X= P2;
		P7 <=X= P3;
		P0 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 2 * 4 + 0);
		P1 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 2 * 4 + 1);
		P2 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 2 * 4 + 2);
		P3 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 2 * 4 + 3);
		level_2_end:
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		x4 <== and(x3, 8);
		branch_if_nonzero x4, level_3_is_right;
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 3 * 4 + 0);
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 3 * 4 + 1);
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 3 * 4 + 2);
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 3 * 4 + 3);
		jump level_3_end;
		level_3_is_right:
		P4 <=X= P0;
		P5 <=X= P1;
		P6 <=X= P2;
		P7 <=X= P3;
		P0 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 3 * 4 + 0);
		P1 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 3 * 4 + 1);
		P2 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 3 * 4 + 2);
		P3 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 3 * 4 + 3);
		level_3_end:
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		x4 <== and(x3, 16);
		branch_if_nonzero x4, level_4_is_right;
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 4 * 4 + 0);
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 4 * 4 + 1);
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 4 * 4 + 2);
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 4 * 4 + 3);
		jump level_4_end;
		level_4_is_right:
		P4 <=X= P0;
		P5 <=X= P1;
		P6 <=X= P2;
		P7 <=X= P3;
		P0 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 4 * 4 + 0);
		P1 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 4 * 4 + 1);
		P2 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 4 * 4 + 2);
		P3 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 4 * 4 + 3);
		level_4_end:
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		x4 <== and(x3, 32);
		branch_if_nonzero x4, level_5_is_right;
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 5 * 4 + 0);
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 5 * 4 + 1);
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 5 * 4 + 2);
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 5 * 4 + 3);
		jump level_5_end;
		level_5_is_right:
		P4 <=X= P0;
		P5 <=X= P1;
		P6 <=X= P2;
		P7 <=X= P3;
		P0 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 5 * 4 + 0);
		P1 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 5 * 4 + 1);
		P2 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 5 * 4 + 2);
		P3 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 5 * 4 + 3);
		level_5_end:
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		x4 <== and(x3, 64);
		branch_if_nonzero x4, level_6_is_right;
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 6 * 4 + 0);
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 6 * 4 + 1);
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 6 * 4 + 2);
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 6 * 4 + 3);
		jump level_6_end;
		level_6_is_right:
		P4 <=X= P0;
		P5 <=X= P1;
		P6 <=X= P2;
		P7 <=X= P3;
		P0 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 6 * 4 + 0);
		P1 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 6 * 4 + 1);
		P2 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 6 * 4 + 2);
		P3 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 6 * 4 + 3);
		level_6_end:
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		x4 <== and(x3, 128);
		branch_if_nonzero x4, level_7_is_right;
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 7 * 4 + 0);
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 7 * 4 + 1);
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 7 * 4 + 2);
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 7 * 4 + 3);
		jump level_7_end;
		level_7_is_right:
		P4 <=X= P0;
		P5 <=X= P1;
		P6 <=X= P2;
		P7 <=X= P3;
		P0 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 7 * 4 + 0);
		P1 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 7 * 4 + 1);
		P2 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 7 * 4 + 2);
		P3 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 7 * 4 + 3);
		level_7_end:
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		x4 <== and(x3, 256);
		branch_if_nonzero x4, level_8_is_right;
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 8 * 4 + 0);
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 8 * 4 + 1);
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 8 * 4 + 2);
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 8 * 4 + 3);
		jump level_8_end;
		level_8_is_right:
		P4 <=X= P0;
		P5 <=X= P1;
		P6 <=X= P2;
		P7 <=X= P3;
		P0 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 8 * 4 + 0);
		P1 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 8 * 4 + 1);
		P2 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 8 * 4 + 2);
		P3 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 8 * 4 + 3);
		level_8_end:
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		x4 <== and(x3, 512);
		branch_if_nonzero x4, level_9_is_right;
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 9 * 4 + 0);
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 9 * 4 + 1);
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 9 * 4 + 2);
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 9 * 4 + 3);
		jump level_9_end;
		level_9_is_right:
		P4 <=X= P0;
		P5 <=X= P1;
		P6 <=X= P2;
		P7 <=X= P3;
		P0 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 9 * 4 + 0);
		P1 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 9 * 4 + 1);
		P2 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 9 * 4 + 2);
		P3 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 9 * 4 + 3);
		level_9_end:
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		x4 <== and(x3, 1024);
		branch_if_nonzero x4, level_10_is_right;
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 10 * 4 + 0);
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 10 * 4 + 1);
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 10 * 4 + 2);
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 10 * 4 + 3);
		jump level_10_end;
		level_10_is_right:
		P4 <=X= P0;
		P5 <=X= P1;
		P6 <=X= P2;
		P7 <=X= P3;
		P0 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 10 * 4 + 0);
		P1 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 10 * 4 + 1);
		P2 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 10 * 4 + 2);
		P3 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 10 * 4 + 3);
		level_10_end:
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		x4 <== and(x3, 2048);
		branch_if_nonzero x4, level_11_is_right;
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 11 * 4 + 0);
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 11 * 4 + 1);
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 11 * 4 + 2);
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 11 * 4 + 3);
		jump level_11_end;
		level_11_is_right:
		P4 <=X= P0;
		P5 <=X= P1;
		P6 <=X= P2;
		P7 <=X= P3;
		P0 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 11 * 4 + 0);
		P1 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 11 * 4 + 1);
		P2 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 11 * 4 + 2);
		P3 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 11 * 4 + 3);
		level_11_end:
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		x4 <== and(x3, 4096);
		branch_if_nonzero x4, level_12_is_right;
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 12 * 4 + 0);
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 12 * 4 + 1);
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 12 * 4 + 2);
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 12 * 4 + 3);
		jump level_12_end;
		level_12_is_right:
		P4 <=X= P0;
		P5 <=X= P1;
		P6 <=X= P2;
		P7 <=X= P3;
		P0 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 12 * 4 + 0);
		P1 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 12 * 4 + 1);
		P2 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 12 * 4 + 2);
		P3 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 12 * 4 + 3);
		level_12_end:
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		x4 <== and(x3, 8192);
		branch_if_nonzero x4, level_13_is_right;
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 13 * 4 + 0);
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 13 * 4 + 1);
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 13 * 4 + 2);
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 13 * 4 + 3);
		jump level_13_end;
		level_13_is_right:
		P4 <=X= P0;
		P5 <=X= P1;
		P6 <=X= P2;
		P7 <=X= P3;
		P0 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 13 * 4 + 0);
		P1 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 13 * 4 + 1);
		P2 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 13 * 4 + 2);
		P3 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 13 * 4 + 3);
		level_13_end:
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		x4 <== and(x3, 16384);
		branch_if_nonzero x4, level_14_is_right;
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 14 * 4 + 0);
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 14 * 4 + 1);
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 14 * 4 + 2);
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 14 * 4 + 3);
		jump level_14_end;
		level_14_is_right:
		P4 <=X= P0;
		P5 <=X= P1;
		P6 <=X= P2;
		P7 <=X= P3;
		P0 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 14 * 4 + 0);
		P1 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 14 * 4 + 1);
		P2 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 14 * 4 + 2);
		P3 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 14 * 4 + 3);
		level_14_end:
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		x4 <== and(x3, 32768);
		branch_if_nonzero x4, level_15_is_right;
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 15 * 4 + 0);
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 15 * 4 + 1);
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 15 * 4 + 2);
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 15 * 4 + 3);
		jump level_15_end;
		level_15_is_right:
		P4 <=X= P0;
		P5 <=X= P1;
		P6 <=X= P2;
		P7 <=X= P3;
		P0 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 15 * 4 + 0);
		P1 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 15 * 4 + 1);
		P2 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 15 * 4 + 2);
		P3 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 15 * 4 + 3);
		level_15_end:
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		x4 <== and(x3, 65536);
		branch_if_nonzero x4, level_16_is_right;
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 16 * 4 + 0);
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 16 * 4 + 1);
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 16 * 4 + 2);
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 16 * 4 + 3);
		jump level_16_end;
		level_16_is_right:
		P4 <=X= P0;
		P5 <=X= P1;
		P6 <=X= P2;
		P7 <=X= P3;
		P0 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 16 * 4 + 0);
		P1 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 16 * 4 + 1);
		P2 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 16 * 4 + 2);
		P3 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 16 * 4 + 3);
		level_16_end:
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		x4 <== and(x3, 131072);
		branch_if_nonzero x4, level_17_is_right;
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 17 * 4 + 0);
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 17 * 4 + 1);
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 17 * 4 + 2);
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 17 * 4 + 3);
		jump level_17_end;
		level_17_is_right:
		P4 <=X= P0;
		P5 <=X= P1;
		P6 <=X= P2;
		P7 <=X= P3;
		P0 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 17 * 4 + 0);
		P1 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 17 * 4 + 1);
		P2 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 17 * 4 + 2);
		P3 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 17 * 4 + 3);
		level_17_end:
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		x4 <== and(x3, 262144);
		branch_if_nonzero x4, level_18_is_right;
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 18 * 4 + 0);
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 18 * 4 + 1);
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 18 * 4 + 2);
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 18 * 4 + 3);
		jump level_18_end;
		level_18_is_right:
		P4 <=X= P0;
		P5 <=X= P1;
		P6 <=X= P2;
		P7 <=X= P3;
		P0 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 18 * 4 + 0);
		P1 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 18 * 4 + 1);
		P2 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 18 * 4 + 2);
		P3 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 18 * 4 + 3);
		level_18_end:
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		x4 <== and(x3, 524288);
		branch_if_nonzero x4, level_19_is_right;
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 19 * 4 + 0);
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 19 * 4 + 1);
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 19 * 4 + 2);
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 19 * 4 + 3);
		jump level_19_end;
		level_19_is_right:
		P4 <=X= P0;
		P5 <=X= P1;
		P6 <=X= P2;
		P7 <=X= P3;
		P0 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 19 * 4 + 0);
		P1 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 19 * 4 + 1);
		P2 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 19 * 4 + 2);
		P3 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 19 * 4 + 3);
		level_19_end:
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		x4 <== and(x3, 1048576);
		branch_if_nonzero x4, level_20_is_right;
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 20 * 4 + 0);
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 20 * 4 + 1);
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 20 * 4 + 2);
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 20 * 4 + 3);
		jump level_20_end;
		level_20_is_right:
		P4 <=X= P0;
		P5 <=X= P1;
		P6 <=X= P2;
		P7 <=X= P3;
		P0 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 20 * 4 + 0);
		P1 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 20 * 4 + 1);
		P2 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 20 * 4 + 2);
		P3 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 20 * 4 + 3);
		level_20_end:
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		x4 <== and(x3, 2097152);
		branch_if_nonzero x4, level_21_is_right;
		P4 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 21 * 4 + 0);
		P5 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 21 * 4 + 1);
		P6 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 21 * 4 + 2);
		P7 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 21 * 4 + 3);
		jump level_21_end;
		level_21_is_right:
		P4 <=X= P0;
		P5 <=X= P1;
		P6 <=X= P2;
		P7 <=X= P3;
		P0 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 21 * 4 + 0);
		P1 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 21 * 4 + 1);
		P2 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 21 * 4 + 2);
		P3 <== load_bootloader_input(x2 * 345 + 54 + 1 + 256 + 21 * 4 + 3);
		level_21_end:
		P0, P1, P2, P3 <== poseidon_gl(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
		
		// Assert Correct Merkle Root
		// At this point, the re-computed Merkle root is in P0-P3 and P4-P7 are not needed anymore.
		P4 <== load_bootloader_input(49);
		P5 <== load_bootloader_input(49 + 1);
		P6 <== load_bootloader_input(49 + 2);
		P7 <== load_bootloader_input(49 + 3);
		branch_if_nonzero P0 - P4, memory_hash_mismatch;
		branch_if_nonzero P1 - P5, memory_hash_mismatch;
		branch_if_nonzero P2 - P6, memory_hash_mismatch;
		branch_if_nonzero P3 - P7, memory_hash_mismatch;
		jump memory_hash_ok;
		memory_hash_mismatch:
		fail;
		memory_hash_ok:
		
		// Increment page index
		x2 <=X= x2 + 1;
		
		branch_if_nonzero x2 - x1, start_page_loop;
		
		end_page_loop:
		
		// Initialize registers, starting with index 0
		x1 <== load_bootloader_input(0);
		x2 <== load_bootloader_input(1);
		x3 <== load_bootloader_input(2);
		x4 <== load_bootloader_input(3);
		x5 <== load_bootloader_input(4);
		x6 <== load_bootloader_input(5);
		x7 <== load_bootloader_input(6);
		x8 <== load_bootloader_input(7);
		x9 <== load_bootloader_input(8);
		x10 <== load_bootloader_input(9);
		x11 <== load_bootloader_input(10);
		x12 <== load_bootloader_input(11);
		x13 <== load_bootloader_input(12);
		x14 <== load_bootloader_input(13);
		x15 <== load_bootloader_input(14);
		x16 <== load_bootloader_input(15);
		x17 <== load_bootloader_input(16);
		x18 <== load_bootloader_input(17);
		x19 <== load_bootloader_input(18);
		x20 <== load_bootloader_input(19);
		x21 <== load_bootloader_input(20);
		x22 <== load_bootloader_input(21);
		x23 <== load_bootloader_input(22);
		x24 <== load_bootloader_input(23);
		x25 <== load_bootloader_input(24);
		x26 <== load_bootloader_input(25);
		x27 <== load_bootloader_input(26);
		x28 <== load_bootloader_input(27);
		x29 <== load_bootloader_input(28);
		x30 <== load_bootloader_input(29);
		x31 <== load_bootloader_input(30);
		tmp1 <== load_bootloader_input(31);
		tmp2 <== load_bootloader_input(32);
		tmp3 <== load_bootloader_input(33);
		tmp4 <== load_bootloader_input(34);
		lr_sc_reservation <== load_bootloader_input(35);
		P0 <== load_bootloader_input(36);
		P1 <== load_bootloader_input(37);
		P2 <== load_bootloader_input(38);
		P3 <== load_bootloader_input(39);
		P4 <== load_bootloader_input(40);
		P5 <== load_bootloader_input(41);
		P6 <== load_bootloader_input(42);
		P7 <== load_bootloader_input(43);
		P8 <== load_bootloader_input(44);
		P9 <== load_bootloader_input(45);
		P10 <== load_bootloader_input(46);
		P11 <== load_bootloader_input(47);
		
		// Default PC is 0, but we already started from 0, so in that case we do nothing.
		// Otherwise, we jump to the PC.
		jump_to_bootloader_input_if_nonzero 48;
		
		// END OF BOOTLOADER
		
		call __data_init;
		// Set stack pointer
x2 <=X= 65536;
		call __runtime_start;
		return;
		_ZN5alloc7raw_vec17capacity_overflow17h2c1c04a797021fabE:
		alloc_dash_0c41f8373e14af19___dot_Lfunc_begin24:
		  debug loc 10 517 0;
		  debug insn "addi x2, x2, -32";
		  x2 <== wrap(x2 + 4294967264);
		alloc_dash_0c41f8373e14af19___dot_Ltmp214:
		  debug loc 1 398 9;
		  debug insn "lui x10, 16";
		  x10 <=X= 65536;
		  debug insn "addi x10, x10, 400";
		  x10 <== wrap(x10 + 400);
		alloc_dash_0c41f8373e14af19___dot_Ltmp215:
		  debug insn "sw x10, 16(x2)";
		  mstore x2 + 16, x10;
		  debug insn "li x10, 1";
		  x10 <=X= 1;
		alloc_dash_0c41f8373e14af19___dot_Ltmp216:
		  debug insn "sw x10, 20(x2)";
		  mstore x2 + 20, x10;
		  debug insn "sw x0, 8(x2)";
		  mstore x2 + 8, x0;
		  debug insn "lui x10, 16";
		  x10 <=X= 65536;
		  debug insn "addi x10, x10, 256";
		  x10 <== wrap(x10 + 256);
		alloc_dash_0c41f8373e14af19___dot_Ltmp217:
		  debug insn "sw x10, 24(x2)";
		  mstore x2 + 24, x10;
		  debug insn "sw x0, 28(x2)";
		  mstore x2 + 28, x0;
		alloc_dash_0c41f8373e14af19___dot_Ltmp218:
		  debug loc 10 518 5;
		  debug insn "lui x10, 16";
		  x10 <=X= 65536;
		alloc_dash_0c41f8373e14af19___dot_Ltmp219:
		  debug insn "addi x11, x10, 408";
		  x11 <== wrap(x10 + 408);
		  debug insn "addi x10, x2, 8";
		  x10 <== wrap(x2 + 8);
		  debug insn "call _ZN4core9panicking9panic_fmt17h38a84a0a59f0bde7E";
		  call _ZN4core9panicking9panic_fmt17h38a84a0a59f0bde7E;
		  debug insn "unimp ";
		  fail;
		_ZN5alloc5alloc18handle_alloc_error17hd9333cd203b4ffb7E:
		alloc_dash_0c41f8373e14af19___dot_Lfunc_begin25:
		  debug loc 11 378 0;
		  debug loc 11 389 14;
		  debug insn "call _ZN5alloc5alloc18handle_alloc_error8rt_error17hbce31673c38e46b3E";
		  call _ZN5alloc5alloc18handle_alloc_error8rt_error17hbce31673c38e46b3E;
		alloc_dash_0c41f8373e14af19___dot_Ltmp221:
		  debug insn "unimp ";
		  fail;
		_ZN5alloc5alloc18handle_alloc_error8rt_error17hbce31673c38e46b3E:
		alloc_dash_0c41f8373e14af19___dot_Lfunc_begin27:
		  debug loc 11 383 0;
		  debug loc 11 385 13;
		  debug insn "call __rg_oom";
		  call __rg_oom;
		alloc_dash_0c41f8373e14af19___dot_Ltmp232:
		  debug insn "unimp ";
		  fail;
		compiler_builtins_dash_4c4cb3cfee5e9dcf___ZN17compiler_builtins3mem6memcpy17ha1e5a026136a52a6E:
		compiler_builtins_dash_4c4cb3cfee5e9dcf___dot_Lfunc_begin333:
		  debug loc 56 297 0;
		  debug insn "li x13, 15";
		  x13 <=X= 15;
		compiler_builtins_dash_4c4cb3cfee5e9dcf___dot_Ltmp6146:
		  debug loc 87 100 8;
		  debug insn "bgeu x13, x12, compiler_builtins_dash_4c4cb3cfee5e9dcf__.LBB333_8";
		  branch_if_positive x13 - x12 + 1, compiler_builtins_dash_4c4cb3cfee5e9dcf___dot_LBB333_8;
		compiler_builtins_dash_4c4cb3cfee5e9dcf___dot_Ltmp6147:
		  debug loc 60 1203 13;
		  debug insn "neg x13, x10";
		  x13 <== wrap_signed(0 - x10);
		compiler_builtins_dash_4c4cb3cfee5e9dcf___dot_Ltmp6148:
		  debug loc 87 103 33;
		  debug insn "andi x16, x13, 3";
		  x16 <== and(x13, 3);
		compiler_builtins_dash_4c4cb3cfee5e9dcf___dot_Ltmp6149:
		  debug loc 88 499 18;
		  debug insn "add x7, x10, x16";
		  x7 <== wrap(x10 + x16);
		compiler_builtins_dash_4c4cb3cfee5e9dcf___dot_Ltmp6150:
		  debug loc 87 32 15;
		  debug insn "beqz x16, compiler_builtins_dash_4c4cb3cfee5e9dcf__.LBB333_4";
		  branch_if_zero x16, compiler_builtins_dash_4c4cb3cfee5e9dcf___dot_LBB333_4;
		compiler_builtins_dash_4c4cb3cfee5e9dcf___dot_Ltmp6151:
		  debug loc 87 0 15;
		  debug insn "mv x15, x10";
		  x15 <=X= x10;
		  debug insn "mv x13, x11";
		  x13 <=X= x11;
		compiler_builtins_dash_4c4cb3cfee5e9dcf___dot_Ltmp6152:
		compiler_builtins_dash_4c4cb3cfee5e9dcf___dot_LBB333_3:
		  debug loc 87 33 21;
		  debug insn "lb x14, 0(x13)";
		  x14, tmp2 <== mload(x13 + 0);
		  x14 <== shr(x14, 8 * tmp2);
		  x14 <== sign_extend_byte(x14);
		  debug loc 87 33 13;
		  debug insn "sb x14, 0(x15)";
		  tmp1, tmp2 <== mload(x15 + 0);
		  tmp3 <== shl(0xff, 8 * tmp2);
		  tmp3 <== xor(tmp3, 0xffffffff);
		  tmp1 <== and(tmp1, tmp3);
		  tmp3 <== and(x14, 0xff);
		  tmp3 <== shl(tmp3, 8 * tmp2);
		  tmp1 <== or(tmp1, tmp3);
		  mstore x15 + 0 - tmp2, tmp1;
		compiler_builtins_dash_4c4cb3cfee5e9dcf___dot_Ltmp6153:
		  debug loc 88 499 18;
		  debug insn "addi x15, x15, 1";
		  x15 <== wrap(x15 + 1);
		compiler_builtins_dash_4c4cb3cfee5e9dcf___dot_Ltmp6154:
		  debug loc 89 485 18;
		  debug insn "addi x13, x13, 1";
		  x13 <== wrap(x13 + 1);
		compiler_builtins_dash_4c4cb3cfee5e9dcf___dot_Ltmp6155:
		  debug loc 87 32 15;
		  debug insn "bltu x15, x7, compiler_builtins_dash_4c4cb3cfee5e9dcf__.LBB333_3";
		  branch_if_positive x7 - x15, compiler_builtins_dash_4c4cb3cfee5e9dcf___dot_LBB333_3;
		compiler_builtins_dash_4c4cb3cfee5e9dcf___dot_Ltmp6156:
		compiler_builtins_dash_4c4cb3cfee5e9dcf___dot_LBB333_4:
		  debug loc 89 485 18;
		  debug insn "add x17, x11, x16";
		  x17 <== wrap(x11 + x16);
		compiler_builtins_dash_4c4cb3cfee5e9dcf___dot_Ltmp6157:
		  debug loc 87 107 9;
		  debug insn "sub x16, x12, x16";
		  x16 <== wrap_signed(x12 - x16);
		compiler_builtins_dash_4c4cb3cfee5e9dcf___dot_Ltmp6158:
		  debug loc 87 109 23;
		  debug insn "andi x5, x16, -4";
		  x5 <== and(x16, 4294967292);
		compiler_builtins_dash_4c4cb3cfee5e9dcf___dot_Ltmp6159:
		  debug loc 87 110 32;
		  debug insn "andi x11, x17, 3";
		  x11 <== and(x17, 3);
		compiler_builtins_dash_4c4cb3cfee5e9dcf___dot_Ltmp6160:
		  debug loc 87 0 0;
		  debug insn "add x13, x7, x5";
		  x13 <== wrap(x7 + x5);
		  debug loc 87 111 12;
		  debug insn "beqz x11, compiler_builtins_dash_4c4cb3cfee5e9dcf__.LBB333_9";
		  branch_if_zero x11, compiler_builtins_dash_4c4cb3cfee5e9dcf___dot_LBB333_9;
		compiler_builtins_dash_4c4cb3cfee5e9dcf___dot_Ltmp6161:
		  debug loc 87 72 15;
		  debug insn "blez x5, compiler_builtins_dash_4c4cb3cfee5e9dcf__.LBB333_12";
		  tmp1 <== to_signed(x5);
		  branch_if_positive -tmp1 + 1, compiler_builtins_dash_4c4cb3cfee5e9dcf___dot_LBB333_12;
		compiler_builtins_dash_4c4cb3cfee5e9dcf___dot_Ltmp6162:
		  debug loc 87 0 0;
		  debug insn "slli x6, x11, 3";
		  x6 <== wrap16(x11 * 8);
		compiler_builtins_dash_4c4cb3cfee5e9dcf___dot_Ltmp6163:
		  debug loc 87 63 31;
		  debug insn "andi x14, x17, -4";
		  x14 <== and(x17, 4294967292);
		compiler_builtins_dash_4c4cb3cfee5e9dcf___dot_Ltmp6164:
		  debug loc 87 68 29;
		  debug insn "lw x12, 0(x14)";
		  x12, tmp1 <== mload(x14 + 0);
		compiler_builtins_dash_4c4cb3cfee5e9dcf___dot_Ltmp6165:
		  debug loc 87 0 29;
		  debug insn "neg x11, x6";
		  x11 <== wrap_signed(0 - x6);
		compiler_builtins_dash_4c4cb3cfee5e9dcf___dot_Ltmp6166:
		  debug insn "andi x28, x11, 24";
		  x28 <== and(x11, 24);
		compiler_builtins_dash_4c4cb3cfee5e9dcf___dot_Ltmp6167:
		  debug loc 87 72 15;
		  debug insn "addi x15, x14, 4";
		  x15 <== wrap(x14 + 4);
		compiler_builtins_dash_4c4cb3cfee5e9dcf___dot_Ltmp6168:
		compiler_builtins_dash_4c4cb3cfee5e9dcf___dot_LBB333_7:
		  debug loc 87 74 28;
		  debug insn "lw x14, 0(x15)";
		  x14, tmp1 <== mload(x15 + 0);
		compiler_builtins_dash_4c4cb3cfee5e9dcf___dot_Ltmp6169:
		  debug loc 87 76 29;
		  debug insn "srl x12, x12, x6";
		  tmp1 <== and(x6, 0x1f);
		  x12 <== shr(x12, tmp1);
		compiler_builtins_dash_4c4cb3cfee5e9dcf___dot_Ltmp6170:
		  debug loc 87 76 50;
		  debug insn "sll x11, x14, x28";
		  tmp1 <== and(x28, 0x1f);
		  x11 <== shl(x14, tmp1);
		  debug loc 87 76 29;
		  debug insn "or x11, x11, x12";
		  x11 <== or(x11, x12);
		compiler_builtins_dash_4c4cb3cfee5e9dcf___dot_Ltmp6171:
		  debug loc 87 81 13;
		  debug insn "sw x11, 0(x7)";
		  mstore x7 + 0, x11;
		compiler_builtins_dash_4c4cb3cfee5e9dcf___dot_Ltmp6172:
		  debug loc 88 499 18;
		  debug insn "addi x7, x7, 4";
		  x7 <== wrap(x7 + 4);
		compiler_builtins_dash_4c4cb3cfee5e9dcf___dot_Ltmp6173:
		  debug loc 87 72 15;
		  debug insn "addi x15, x15, 4";
		  x15 <== wrap(x15 + 4);
		compiler_builtins_dash_4c4cb3cfee5e9dcf___dot_Ltmp6174:
		  debug loc 87 0 15;
		  debug insn "mv x12, x14";
		  x12 <=X= x14;
		compiler_builtins_dash_4c4cb3cfee5e9dcf___dot_Ltmp6175:
		  debug loc 87 72 15;
		  debug insn "bltu x7, x13, compiler_builtins_dash_4c4cb3cfee5e9dcf__.LBB333_7";
		  branch_if_positive x13 - x7, compiler_builtins_dash_4c4cb3cfee5e9dcf___dot_LBB333_7;
		  debug insn "j compiler_builtins_dash_4c4cb3cfee5e9dcf__.LBB333_12";
		  jump compiler_builtins_dash_4c4cb3cfee5e9dcf___dot_LBB333_12;
		compiler_builtins_dash_4c4cb3cfee5e9dcf___dot_LBB333_8:
		  debug loc 87 0 15;
		  debug insn "mv x13, x10";
		  x13 <=X= x10;
		compiler_builtins_dash_4c4cb3cfee5e9dcf___dot_Ltmp6177:
		  debug loc 87 32 15;
		  debug insn "bnez x12, compiler_builtins_dash_4c4cb3cfee5e9dcf__.LBB333_13";
		  branch_if_nonzero x12, compiler_builtins_dash_4c4cb3cfee5e9dcf___dot_LBB333_13;
		  debug insn "j compiler_builtins_dash_4c4cb3cfee5e9dcf__.LBB333_15";
		  jump compiler_builtins_dash_4c4cb3cfee5e9dcf___dot_LBB333_15;
		compiler_builtins_dash_4c4cb3cfee5e9dcf___dot_LBB333_9:
		  debug loc 87 45 15;
		  debug insn "blez x5, compiler_builtins_dash_4c4cb3cfee5e9dcf__.LBB333_12";
		  tmp1 <== to_signed(x5);
		  branch_if_positive -tmp1 + 1, compiler_builtins_dash_4c4cb3cfee5e9dcf___dot_LBB333_12;
		compiler_builtins_dash_4c4cb3cfee5e9dcf___dot_Ltmp6179:
		  debug loc 87 0 15;
		  debug insn "mv x11, x17";
		  x11 <=X= x17;
		compiler_builtins_dash_4c4cb3cfee5e9dcf___dot_Ltmp6180:
		compiler_builtins_dash_4c4cb3cfee5e9dcf___dot_LBB333_11:
		  debug loc 87 46 27;
		  debug insn "lw x12, 0(x11)";
		  x12, tmp1 <== mload(x11 + 0);
		  debug loc 87 46 13;
		  debug insn "sw x12, 0(x7)";
		  mstore x7 + 0, x12;
		compiler_builtins_dash_4c4cb3cfee5e9dcf___dot_Ltmp6181:
		  debug loc 88 499 18;
		  debug insn "addi x7, x7, 4";
		  x7 <== wrap(x7 + 4);
		compiler_builtins_dash_4c4cb3cfee5e9dcf___dot_Ltmp6182:
		  debug loc 88 499 18;
		  debug insn "addi x11, x11, 4";
		  x11 <== wrap(x11 + 4);
		compiler_builtins_dash_4c4cb3cfee5e9dcf___dot_Ltmp6183:
		  debug loc 87 45 15;
		  debug insn "bltu x7, x13, compiler_builtins_dash_4c4cb3cfee5e9dcf__.LBB333_11";
		  branch_if_positive x13 - x7, compiler_builtins_dash_4c4cb3cfee5e9dcf___dot_LBB333_11;
		compiler_builtins_dash_4c4cb3cfee5e9dcf___dot_Ltmp6184:
		compiler_builtins_dash_4c4cb3cfee5e9dcf___dot_LBB333_12:
		  debug loc 89 485 18;
		  debug insn "add x11, x17, x5";
		  x11 <== wrap(x17 + x5);
		compiler_builtins_dash_4c4cb3cfee5e9dcf___dot_Ltmp6185:
		  debug loc 87 118 9;
		  debug insn "andi x12, x16, 3";
		  x12 <== and(x16, 3);
		compiler_builtins_dash_4c4cb3cfee5e9dcf___dot_Ltmp6186:
		  debug loc 87 32 15;
		  debug insn "beqz x12, compiler_builtins_dash_4c4cb3cfee5e9dcf__.LBB333_15";
		  branch_if_zero x12, compiler_builtins_dash_4c4cb3cfee5e9dcf___dot_LBB333_15;
		compiler_builtins_dash_4c4cb3cfee5e9dcf___dot_Ltmp6187:
		compiler_builtins_dash_4c4cb3cfee5e9dcf___dot_LBB333_13:
		  debug loc 87 0 15;
		  debug insn "add x12, x12, x13";
		  x12 <== wrap(x12 + x13);
		compiler_builtins_dash_4c4cb3cfee5e9dcf___dot_Ltmp6188:
		compiler_builtins_dash_4c4cb3cfee5e9dcf___dot_LBB333_14:
		  debug loc 87 33 21;
		  debug insn "lb x14, 0(x11)";
		  x14, tmp2 <== mload(x11 + 0);
		  x14 <== shr(x14, 8 * tmp2);
		  x14 <== sign_extend_byte(x14);
		  debug loc 87 33 13;
		  debug insn "sb x14, 0(x13)";
		  tmp1, tmp2 <== mload(x13 + 0);
		  tmp3 <== shl(0xff, 8 * tmp2);
		  tmp3 <== xor(tmp3, 0xffffffff);
		  tmp1 <== and(tmp1, tmp3);
		  tmp3 <== and(x14, 0xff);
		  tmp3 <== shl(tmp3, 8 * tmp2);
		  tmp1 <== or(tmp1, tmp3);
		  mstore x13 + 0 - tmp2, tmp1;
		compiler_builtins_dash_4c4cb3cfee5e9dcf___dot_Ltmp6189:
		  debug loc 88 499 18;
		  debug insn "addi x13, x13, 1";
		  x13 <== wrap(x13 + 1);
		compiler_builtins_dash_4c4cb3cfee5e9dcf___dot_Ltmp6190:
		  debug loc 89 485 18;
		  debug insn "addi x11, x11, 1";
		  x11 <== wrap(x11 + 1);
		compiler_builtins_dash_4c4cb3cfee5e9dcf___dot_Ltmp6191:
		  debug loc 89 0 18;
		  debug insn "bltu x13, x12, compiler_builtins_dash_4c4cb3cfee5e9dcf__.LBB333_14";
		  branch_if_positive x12 - x13, compiler_builtins_dash_4c4cb3cfee5e9dcf___dot_LBB333_14;
		compiler_builtins_dash_4c4cb3cfee5e9dcf___dot_Ltmp6192:
		compiler_builtins_dash_4c4cb3cfee5e9dcf___dot_LBB333_15:
		  debug loc 56 299 10;
		  debug insn "ret ";
		  ret;
		memcpy:
		compiler_builtins_dash_4c4cb3cfee5e9dcf___dot_Lfunc_begin334:
		  debug loc 56 305 0;
		  debug loc 56 306 17;
		  debug insn "tail compiler_builtins_dash_4c4cb3cfee5e9dcf___ZN17compiler_builtins3mem6memcpy17ha1e5a026136a52a6E";
		  jump compiler_builtins_dash_4c4cb3cfee5e9dcf___ZN17compiler_builtins3mem6memcpy17ha1e5a026136a52a6E;
		core_dash_e8b3f51d8f75870a___ZN4core3ops8function6FnOnce9call_once17ha862edba306640dcE:
		core_dash_e8b3f51d8f75870a___dot_Lfunc_begin0:
		  debug loc 98 250 0;
		  debug loc 99 1521 9;
		  debug insn "lw x10, 0(x10)";
		  x10, tmp1 <== mload(x10 + 0);
		core_dash_e8b3f51d8f75870a___dot_Ltmp0:
		core_dash_e8b3f51d8f75870a___dot_LBB0_1:
		  debug loc 95 314 5;
		  debug insn "j core_dash_e8b3f51d8f75870a__.LBB0_1";
		  jump core_dash_e8b3f51d8f75870a___dot_LBB0_1;
		core_dash_e8b3f51d8f75870a___ZN4core3ptr102drop_in_place$LT$$RF$core_dot__dot_iter_dot__dot_adapters_dot__dot_copied_dot__dot_Copied$LT$core_dot__dot_slice_dot__dot_iter_dot__dot_Iter$LT$u8$GT$$GT$$GT$17hc185e3e6ab652ffaE:
		core_dash_e8b3f51d8f75870a___dot_Lfunc_begin1:
		  debug loc 99 490 0;
		  debug loc 99 490 1;
		  debug insn "ret ";
		  ret;
		core_dash_e8b3f51d8f75870a___ZN36_$LT$T$u20$as$u20$core_dot__dot_any_dot__dot_Any$GT$7type_id17h8092b8f856eee998E:
		core_dash_e8b3f51d8f75870a___dot_Lfunc_begin101:
		  debug loc 147 201 0;
		  debug insn "lui x10, 387778";
		  x10 <=X= 1588338688;
		  debug insn "addi x10, x10, 909";
		  x10 <== wrap(x10 + 909);
		  debug insn "lui x11, 259849";
		  x11 <=X= 1064341504;
		  debug insn "addi x11, x11, -1408";
		  x11 <== wrap(x11 + 4294965888);
		core_dash_e8b3f51d8f75870a___dot_Ltmp4369:
		  debug loc 147 203 6;
		  debug insn "ret ";
		  ret;
		_ZN73_$LT$core_dot__dot_panic_dot__dot_panic_info_dot__dot_PanicInfo$u20$as$u20$core_dot__dot_fmt_dot__dot_Display$GT$3fmt17h0360d72aed724b29E:
		core_dash_e8b3f51d8f75870a___dot_Lfunc_begin174:
		  debug loc 162 152 0;
		  debug insn "addi x2, x2, -80";
		  x2 <== wrap(x2 + 4294967216);
		core_dash_e8b3f51d8f75870a___dot_Ltmp5164:
		  debug loc 95 1639 9;
		  debug insn "sw x1, 76(x2)";
		  mstore x2 + 76, x1;
		  debug insn "sw x8, 72(x2)";
		  mstore x2 + 72, x8;
		  debug insn "sw x9, 68(x2)";
		  mstore x2 + 68, x9;
		  debug insn "sw x18, 64(x2)";
		  mstore x2 + 64, x18;
		  debug insn "sw x19, 60(x2)";
		  mstore x2 + 60, x19;
		  debug insn "lw x19, 4(x11)";
		  x19, tmp1 <== mload(x11 + 4);
		  debug insn "lw x9, 0(x11)";
		  x9, tmp1 <== mload(x11 + 0);
		  debug insn "lw x13, 12(x19)";
		  x13, tmp1 <== mload(x19 + 12);
		  debug insn "mv x18, x10";
		  x18 <=X= x10;
		core_dash_e8b3f51d8f75870a___dot_Ltmp5165:
		  debug insn "lui x10, 16";
		  x10 <=X= 65536;
		  debug insn "addi x11, x10, 452";
		  x11 <== wrap(x10 + 452);
		core_dash_e8b3f51d8f75870a___dot_Ltmp5166:
		  debug insn "li x12, 12";
		  x12 <=X= 12;
		  debug insn "mv x10, x9";
		  x10 <=X= x9;
		  debug insn "jalr x13";
		  jump_and_link_dyn x13;
		core_dash_e8b3f51d8f75870a___dot_Ltmp5167:
		  debug loc 95 0 9;
		  debug insn "li x8, 1";
		  x8 <=X= 1;
		core_dash_e8b3f51d8f75870a___dot_Ltmp5168:
		  debug loc 162 153 9;
		  debug insn "bnez x10, core_dash_e8b3f51d8f75870a__.LBB174_7";
		  branch_if_nonzero x10, core_dash_e8b3f51d8f75870a___dot_LBB174_7;
		core_dash_e8b3f51d8f75870a___dot_Ltmp5169:
		  debug loc 162 154 16;
		  debug insn "lw x10, 8(x18)";
		  x10, tmp1 <== mload(x18 + 8);
		  debug insn "beqz x10, core_dash_e8b3f51d8f75870a__.LBB174_3";
		  branch_if_zero x10, core_dash_e8b3f51d8f75870a___dot_LBB174_3;
		core_dash_e8b3f51d8f75870a___dot_Ltmp5170:
		  debug loc 162 154 21;
		  debug insn "sw x10, 4(x2)";
		  mstore x2 + 4, x10;
		  debug insn "addi x10, x2, 4";
		  x10 <== wrap(x2 + 4);
		core_dash_e8b3f51d8f75870a___dot_Ltmp5171:
		  debug loc 162 155 13;
		  debug insn "sw x10, 8(x2)";
		  mstore x2 + 8, x10;
		  debug insn "load_dynamic x10, core_dash_e8b3f51d8f75870a___ZN44_$LT$$RF$T$u20$as$u20$core..fmt..Display$GT$3fmt17hd10a92ec7570104aE";
		  x10 <== load_label(core_dash_e8b3f51d8f75870a___ZN44_$LT$$RF$T$u20$as$u20$core_dot__dot_fmt_dot__dot_Display$GT$3fmt17hd10a92ec7570104aE);
		  debug insn "j core_dash_e8b3f51d8f75870a__.LBB174_5";
		  jump core_dash_e8b3f51d8f75870a___dot_LBB174_5;
		core_dash_e8b3f51d8f75870a___dot_LBB174_3:
		  debug loc 162 156 39;
		  debug insn "lw x10, 4(x18)";
		  x10, tmp1 <== mload(x18 + 4);
		core_dash_e8b3f51d8f75870a___dot_Ltmp5173:
		  debug insn "lw x8, 0(x18)";
		  x8, tmp1 <== mload(x18 + 0);
		core_dash_e8b3f51d8f75870a___dot_Ltmp5174:
		  debug loc 147 439 9;
		  debug insn "lw x11, 12(x10)";
		  x11, tmp1 <== mload(x10 + 12);
		core_dash_e8b3f51d8f75870a___dot_Ltmp5175:
		  debug loc 147 260 24;
		  debug insn "mv x10, x8";
		  x10 <=X= x8;
		core_dash_e8b3f51d8f75870a___dot_Ltmp5176:
		  debug insn "jalr x11";
		  jump_and_link_dyn x11;
		  debug insn "lui x12, 563831";
		  x12 <=X= 2309451776;
		  debug insn "addi x12, x12, -972";
		  x12 <== wrap(x12 + 4294966324);
		core_dash_e8b3f51d8f75870a___dot_Ltmp5177:
		  debug loc 147 668 5;
		  debug insn "xor x11, x11, x12";
		  x11 <== xor(x11, x12);
		core_dash_e8b3f51d8f75870a___dot_Ltmp5178:
		  debug loc 147 0 5;
		  debug insn "lui x12, 696194";
		  x12 <=X= 2851610624;
		  debug insn "addi x12, x12, -1336";
		  x12 <== wrap(x12 + 4294965960);
		  debug loc 147 668 5;
		  debug insn "xor x10, x10, x12";
		  x10 <== xor(x10, x12);
		core_dash_e8b3f51d8f75870a___dot_Ltmp5179:
		  debug insn "or x10, x10, x11";
		  x10 <== or(x10, x11);
		core_dash_e8b3f51d8f75870a___dot_Ltmp5180:
		  debug loc 162 156 23;
		  debug insn "bnez x10, core_dash_e8b3f51d8f75870a__.LBB174_6";
		  branch_if_nonzero x10, core_dash_e8b3f51d8f75870a___dot_LBB174_6;
		core_dash_e8b3f51d8f75870a___dot_Ltmp5181:
		  debug loc 162 156 28;
		  debug insn "sw x8, 4(x2)";
		  mstore x2 + 4, x8;
		  debug insn "addi x10, x2, 4";
		  x10 <== wrap(x2 + 4);
		  debug loc 162 157 13;
		  debug insn "sw x10, 8(x2)";
		  mstore x2 + 8, x10;
		  debug insn "load_dynamic x10, core_dash_e8b3f51d8f75870a___ZN44_$LT$$RF$T$u20$as$u20$core..fmt..Display$GT$3fmt17h1e2de74ca37ee72aE";
		  x10 <== load_label(core_dash_e8b3f51d8f75870a___ZN44_$LT$$RF$T$u20$as$u20$core_dot__dot_fmt_dot__dot_Display$GT$3fmt17h1e2de74ca37ee72aE);
		core_dash_e8b3f51d8f75870a___dot_Ltmp5182:
		core_dash_e8b3f51d8f75870a___dot_LBB174_5:
		  debug loc 162 0 0;
		  debug insn "sw x10, 12(x2)";
		  mstore x2 + 12, x10;
		core_dash_e8b3f51d8f75870a___dot_Ltmp5183:
		  debug insn "sw x0, 32(x2)";
		  mstore x2 + 32, x0;
		  debug insn "lui x10, 16";
		  x10 <=X= 65536;
		  debug insn "addi x10, x10, 472";
		  x10 <== wrap(x10 + 472);
		core_dash_e8b3f51d8f75870a___dot_Ltmp5184:
		  debug insn "sw x10, 40(x2)";
		  mstore x2 + 40, x10;
		  debug insn "li x10, 2";
		  x10 <=X= 2;
		core_dash_e8b3f51d8f75870a___dot_Ltmp5185:
		  debug insn "sw x10, 44(x2)";
		  mstore x2 + 44, x10;
		  debug insn "addi x10, x2, 8";
		  x10 <== wrap(x2 + 8);
		  debug insn "sw x10, 48(x2)";
		  mstore x2 + 48, x10;
		  debug insn "li x8, 1";
		  x8 <=X= 1;
		  debug insn "sw x8, 52(x2)";
		  mstore x2 + 52, x8;
		  debug insn "addi x12, x2, 32";
		  x12 <== wrap(x2 + 32);
		  debug insn "mv x10, x9";
		  x10 <=X= x9;
		  debug insn "mv x11, x19";
		  x11 <=X= x19;
		  debug insn "call _ZN4core3fmt5write17h87a75934cc3b911aE";
		  call _ZN4core3fmt5write17h87a75934cc3b911aE;
		  debug insn "bnez x10, core_dash_e8b3f51d8f75870a__.LBB174_7";
		  branch_if_nonzero x10, core_dash_e8b3f51d8f75870a___dot_LBB174_7;
		core_dash_e8b3f51d8f75870a___dot_Ltmp5186:
		core_dash_e8b3f51d8f75870a___dot_LBB174_6:
		  debug loc 162 164 9;
		  debug insn "lw x10, 12(x18)";
		  x10, tmp1 <== mload(x18 + 12);
		core_dash_e8b3f51d8f75870a___dot_Ltmp5187:
		  debug loc 161 198 9;
		  debug insn "addi x11, x10, 8";
		  x11 <== wrap(x10 + 8);
		  debug insn "addi x12, x10, 12";
		  x12 <== wrap(x10 + 12);
		  debug insn "sw x10, 8(x2)";
		  mstore x2 + 8, x10;
		core_dash_e8b3f51d8f75870a___dot_Ltmp5188:
		  debug insn "load_dynamic x10, core_dash_e8b3f51d8f75870a___ZN44_$LT$$RF$T$u20$as$u20$core..fmt..Display$GT$3fmt17hbb08b750573e0112E";
		  x10 <== load_label(core_dash_e8b3f51d8f75870a___ZN44_$LT$$RF$T$u20$as$u20$core_dot__dot_fmt_dot__dot_Display$GT$3fmt17hbb08b750573e0112E);
		  debug insn "sw x10, 12(x2)";
		  mstore x2 + 12, x10;
		  debug insn "sw x11, 16(x2)";
		  mstore x2 + 16, x11;
		  debug insn "load_dynamic x10, _ZN4core3fmt3num3imp52_$LT$impl$u20$core..fmt..Display$u20$for$u20$u32$GT$3fmt17h361d4a177e5aa0a5E";
		  x10 <== load_label(_ZN4core3fmt3num3imp52_$LT$impl$u20$core_dot__dot_fmt_dot__dot_Display$u20$for$u20$u32$GT$3fmt17h361d4a177e5aa0a5E);
		  debug insn "sw x10, 20(x2)";
		  mstore x2 + 20, x10;
		  debug insn "sw x12, 24(x2)";
		  mstore x2 + 24, x12;
		  debug insn "sw x10, 28(x2)";
		  mstore x2 + 28, x10;
		core_dash_e8b3f51d8f75870a___dot_Ltmp5189:
		  debug loc 95 1662 25;
		  debug insn "sw x0, 32(x2)";
		  mstore x2 + 32, x0;
		  debug insn "lui x10, 16";
		  x10 <=X= 65536;
		  debug insn "addi x10, x10, 428";
		  x10 <== wrap(x10 + 428);
		core_dash_e8b3f51d8f75870a___dot_Ltmp5190:
		  debug insn "sw x10, 40(x2)";
		  mstore x2 + 40, x10;
		  debug insn "li x10, 3";
		  x10 <=X= 3;
		core_dash_e8b3f51d8f75870a___dot_Ltmp5191:
		  debug insn "sw x10, 44(x2)";
		  mstore x2 + 44, x10;
		  debug insn "addi x11, x2, 8";
		  x11 <== wrap(x2 + 8);
		  debug insn "sw x11, 48(x2)";
		  mstore x2 + 48, x11;
		  debug insn "sw x10, 52(x2)";
		  mstore x2 + 52, x10;
		  debug loc 95 1662 9;
		  debug insn "addi x12, x2, 32";
		  x12 <== wrap(x2 + 32);
		  debug insn "mv x10, x9";
		  x10 <=X= x9;
		  debug insn "mv x11, x19";
		  x11 <=X= x19;
		  debug insn "call _ZN4core3fmt5write17h87a75934cc3b911aE";
		  call _ZN4core3fmt5write17h87a75934cc3b911aE;
		  debug insn "mv x8, x10";
		  x8 <=X= x10;
		core_dash_e8b3f51d8f75870a___dot_Ltmp5192:
		core_dash_e8b3f51d8f75870a___dot_LBB174_7:
		  debug loc 162 165 6;
		  debug insn "mv x10, x8";
		  x10 <=X= x8;
		  debug insn "lw x1, 76(x2)";
		  x1, tmp1 <== mload(x2 + 76);
		  debug insn "lw x8, 72(x2)";
		  x8, tmp1 <== mload(x2 + 72);
		  debug insn "lw x9, 68(x2)";
		  x9, tmp1 <== mload(x2 + 68);
		  debug insn "lw x18, 64(x2)";
		  x18, tmp1 <== mload(x2 + 64);
		core_dash_e8b3f51d8f75870a___dot_Ltmp5193:
		  debug insn "lw x19, 60(x2)";
		  x19, tmp1 <== mload(x2 + 60);
		  debug insn "addi x2, x2, 80";
		  x2 <== wrap(x2 + 80);
		  debug insn "ret ";
		  ret;
		_ZN4core9panicking9panic_fmt17h38a84a0a59f0bde7E:
		core_dash_e8b3f51d8f75870a___dot_Lfunc_begin175:
		  debug loc 163 49 0;
		  debug insn "addi x2, x2, -32";
		  x2 <== wrap(x2 + 4294967264);
		core_dash_e8b3f51d8f75870a___dot_Ltmp5195:
		  debug loc 162 51 9;
		  debug insn "lui x12, 16";
		  x12 <=X= 65536;
		  debug insn "addi x12, x12, 424";
		  x12 <== wrap(x12 + 424);
		  debug insn "sw x12, 8(x2)";
		  mstore x2 + 8, x12;
		  debug insn "lui x12, 16";
		  x12 <=X= 65536;
		  debug insn "addi x12, x12, 488";
		  x12 <== wrap(x12 + 488);
		  debug insn "sw x12, 12(x2)";
		  mstore x2 + 12, x12;
		  debug insn "sw x10, 16(x2)";
		  mstore x2 + 16, x10;
		  debug insn "sw x11, 20(x2)";
		  mstore x2 + 20, x11;
		  debug insn "li x10, 1";
		  x10 <=X= 1;
		core_dash_e8b3f51d8f75870a___dot_Ltmp5196:
		  debug insn "sb x10, 24(x2)";
		  tmp1, tmp2 <== mload(x2 + 24);
		  tmp3 <== shl(0xff, 8 * tmp2);
		  tmp3 <== xor(tmp3, 0xffffffff);
		  tmp1 <== and(tmp1, tmp3);
		  tmp3 <== and(x10, 0xff);
		  tmp3 <== shl(tmp3, 8 * tmp2);
		  tmp1 <== or(tmp1, tmp3);
		  mstore x2 + 24 - tmp2, tmp1;
		core_dash_e8b3f51d8f75870a___dot_Ltmp5197:
		  debug loc 163 64 14;
		  debug insn "addi x10, x2, 8";
		  x10 <== wrap(x2 + 8);
		  debug insn "call rust_begin_unwind";
		  call rust_begin_unwind;
		core_dash_e8b3f51d8f75870a___dot_Ltmp5198:
		  debug insn "unimp ";
		  fail;
		_ZN4core9panicking5panic17h83c2c4098b58b628E:
		core_dash_e8b3f51d8f75870a___dot_Lfunc_begin177:
		  debug loc 163 105 0;
		  debug insn "addi x2, x2, -32";
		  x2 <== wrap(x2 + 4294967264);
		core_dash_e8b3f51d8f75870a___dot_Ltmp5210:
		  debug loc 163 112 39;
		  debug insn "sw x10, 24(x2)";
		  mstore x2 + 24, x10;
		  debug insn "sw x11, 28(x2)";
		  mstore x2 + 28, x11;
		  debug insn "addi x10, x2, 24";
		  x10 <== wrap(x2 + 24);
		core_dash_e8b3f51d8f75870a___dot_Ltmp5211:
		  debug loc 95 398 9;
		  debug insn "sw x10, 8(x2)";
		  mstore x2 + 8, x10;
		  debug insn "li x10, 1";
		  x10 <=X= 1;
		  debug insn "sw x10, 12(x2)";
		  mstore x2 + 12, x10;
		  debug insn "sw x0, 0(x2)";
		  mstore x2 + 0, x0;
		  debug insn "lui x10, 16";
		  x10 <=X= 65536;
		  debug insn "addi x10, x10, 424";
		  x10 <== wrap(x10 + 424);
		core_dash_e8b3f51d8f75870a___dot_Ltmp5212:
		  debug insn "sw x10, 16(x2)";
		  mstore x2 + 16, x10;
		  debug insn "sw x0, 20(x2)";
		  mstore x2 + 20, x0;
		core_dash_e8b3f51d8f75870a___dot_Ltmp5213:
		  debug loc 163 112 5;
		  debug insn "mv x10, x2";
		  x10 <=X= x2;
		core_dash_e8b3f51d8f75870a___dot_Ltmp5214:
		  debug insn "mv x11, x12";
		  x11 <=X= x12;
		core_dash_e8b3f51d8f75870a___dot_Ltmp5215:
		  debug insn "call _ZN4core9panicking9panic_fmt17h38a84a0a59f0bde7E";
		  call _ZN4core9panicking9panic_fmt17h38a84a0a59f0bde7E;
		  debug insn "unimp ";
		  fail;
		_ZN4core6result13unwrap_failed17h3809f1cd94940bfeE:
		core_dash_e8b3f51d8f75870a___dot_Lfunc_begin187:
		  debug loc 148 1789 0;
		  debug insn "addi x2, x2, -64";
		  x2 <== wrap(x2 + 4294967232);
		  debug insn "sw x10, 8(x2)";
		  mstore x2 + 8, x10;
		  debug insn "sw x11, 12(x2)";
		  mstore x2 + 12, x11;
		  debug insn "sw x12, 16(x2)";
		  mstore x2 + 16, x12;
		  debug insn "sw x13, 20(x2)";
		  mstore x2 + 20, x13;
		  debug insn "addi x10, x2, 8";
		  x10 <== wrap(x2 + 8);
		core_dash_e8b3f51d8f75870a___dot_Ltmp5286:
		  debug loc 148 1790 5;
		  debug insn "sw x10, 48(x2)";
		  mstore x2 + 48, x10;
		  debug insn "load_dynamic x10, core_dash_e8b3f51d8f75870a___ZN44_$LT$$RF$T$u20$as$u20$core..fmt..Display$GT$3fmt17hbb08b750573e0112E";
		  x10 <== load_label(core_dash_e8b3f51d8f75870a___ZN44_$LT$$RF$T$u20$as$u20$core_dot__dot_fmt_dot__dot_Display$GT$3fmt17hbb08b750573e0112E);
		  debug insn "sw x10, 52(x2)";
		  mstore x2 + 52, x10;
		  debug insn "addi x10, x2, 16";
		  x10 <== wrap(x2 + 16);
		  debug insn "sw x10, 56(x2)";
		  mstore x2 + 56, x10;
		  debug insn "load_dynamic x10, core_dash_e8b3f51d8f75870a___ZN42_$LT$$RF$T$u20$as$u20$core..fmt..Debug$GT$3fmt17h1906125bc95027d2E";
		  x10 <== load_label(core_dash_e8b3f51d8f75870a___ZN42_$LT$$RF$T$u20$as$u20$core_dot__dot_fmt_dot__dot_Debug$GT$3fmt17h1906125bc95027d2E);
		  debug insn "sw x10, 60(x2)";
		  mstore x2 + 60, x10;
		core_dash_e8b3f51d8f75870a___dot_Ltmp5287:
		  debug loc 95 398 9;
		  debug insn "lui x10, 16";
		  x10 <=X= 65536;
		  debug insn "addi x10, x10, 508";
		  x10 <== wrap(x10 + 508);
		core_dash_e8b3f51d8f75870a___dot_Ltmp5288:
		  debug insn "sw x10, 32(x2)";
		  mstore x2 + 32, x10;
		  debug insn "li x10, 2";
		  x10 <=X= 2;
		core_dash_e8b3f51d8f75870a___dot_Ltmp5289:
		  debug insn "sw x10, 36(x2)";
		  mstore x2 + 36, x10;
		  debug insn "sw x0, 24(x2)";
		  mstore x2 + 24, x0;
		  debug insn "addi x11, x2, 48";
		  x11 <== wrap(x2 + 48);
		  debug insn "sw x11, 40(x2)";
		  mstore x2 + 40, x11;
		  debug insn "sw x10, 44(x2)";
		  mstore x2 + 44, x10;
		core_dash_e8b3f51d8f75870a___dot_Ltmp5290:
		  debug loc 148 1790 5;
		  debug insn "addi x10, x2, 24";
		  x10 <== wrap(x2 + 24);
		  debug insn "mv x11, x14";
		  x11 <=X= x14;
		  debug insn "call _ZN4core9panicking9panic_fmt17h38a84a0a59f0bde7E";
		  call _ZN4core9panicking9panic_fmt17h38a84a0a59f0bde7E;
		  debug insn "unimp ";
		  fail;
		_ZN4core3fmt5write17h87a75934cc3b911aE:
		core_dash_e8b3f51d8f75870a___dot_Lfunc_begin224:
		  debug loc 95 1194 0;
		  debug insn "addi x2, x2, -80";
		  x2 <== wrap(x2 + 4294967216);
		  debug insn "sw x1, 76(x2)";
		  mstore x2 + 76, x1;
		  debug insn "sw x8, 72(x2)";
		  mstore x2 + 72, x8;
		  debug insn "sw x9, 68(x2)";
		  mstore x2 + 68, x9;
		  debug insn "sw x18, 64(x2)";
		  mstore x2 + 64, x18;
		  debug insn "sw x19, 60(x2)";
		  mstore x2 + 60, x19;
		  debug insn "sw x20, 56(x2)";
		  mstore x2 + 56, x20;
		  debug insn "sw x21, 52(x2)";
		  mstore x2 + 52, x21;
		  debug insn "sw x22, 48(x2)";
		  mstore x2 + 48, x22;
		  debug insn "mv x19, x12";
		  x19 <=X= x12;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6641:
		  debug loc 95 243 9;
		  debug insn "sw x0, 32(x2)";
		  mstore x2 + 32, x0;
		  debug insn "li x12, 32";
		  x12 <=X= 32;
		  debug insn "sw x12, 36(x2)";
		  mstore x2 + 36, x12;
		  debug insn "li x12, 3";
		  x12 <=X= 3;
		  debug insn "sb x12, 40(x2)";
		  tmp1, tmp2 <== mload(x2 + 40);
		  tmp3 <== shl(0xff, 8 * tmp2);
		  tmp3 <== xor(tmp3, 0xffffffff);
		  tmp1 <== and(tmp1, tmp3);
		  tmp3 <== and(x12, 0xff);
		  tmp3 <== shl(tmp3, 8 * tmp2);
		  tmp1 <== or(tmp1, tmp3);
		  mstore x2 + 40 - tmp2, tmp1;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6642:
		  debug loc 95 1198 11;
		  debug insn "lw x12, 0(x19)";
		  x12, tmp1 <== mload(x19 + 0);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6643:
		  debug loc 95 243 9;
		  debug insn "sw x0, 16(x2)";
		  mstore x2 + 16, x0;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6644:
		  debug insn "sw x0, 24(x2)";
		  mstore x2 + 24, x0;
		  debug insn "sw x10, 8(x2)";
		  mstore x2 + 8, x10;
		  debug insn "sw x11, 12(x2)";
		  mstore x2 + 12, x11;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6645:
		  debug loc 95 1198 5;
		  debug insn "beqz x12, core_dash_e8b3f51d8f75870a__.LBB224_19";
		  branch_if_zero x12, core_dash_e8b3f51d8f75870a___dot_LBB224_19;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6646:
		  debug loc 95 1212 14;
		  debug insn "lw x10, 4(x19)";
		  x10, tmp1 <== mload(x19 + 4);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6647:
		  debug loc 108 146 24;
		  debug insn "beqz x10, core_dash_e8b3f51d8f75870a__.LBB224_26";
		  branch_if_zero x10, core_dash_e8b3f51d8f75870a___dot_LBB224_26;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6648:
		  debug loc 108 0 24;
		  debug insn "lw x11, 8(x19)";
		  x11, tmp1 <== mload(x19 + 8);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6649:
		  debug loc 108 146 24;
		  debug insn "addi x13, x10, -1";
		  x13 <== wrap(x10 + 4294967295);
		  debug insn "slli x13, x13, 5";
		  x13 <== wrap16(x13 * 32);
		  debug insn "srli x13, x13, 5";
		  x13 <== shr(x13, 5);
		  debug insn "addi x18, x13, 1";
		  x18 <== wrap(x13 + 1);
		  debug insn "addi x9, x11, 4";
		  x9 <== wrap(x11 + 4);
		  debug insn "slli x20, x10, 5";
		  x20 <== wrap16(x10 * 32);
		  debug insn "addi x8, x12, 16";
		  x8 <== wrap(x12 + 16);
		  debug insn "li x21, 1";
		  x21 <=X= 1;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6650:
		  debug loc 108 0 24;
		  debug insn "load_dynamic x22, core_dash_e8b3f51d8f75870a___ZN4core3ops8function6FnOnce9call_once17ha862edba306640dcE";
		  x22 <== load_label(core_dash_e8b3f51d8f75870a___ZN4core3ops8function6FnOnce9call_once17ha862edba306640dcE);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6651:
		core_dash_e8b3f51d8f75870a___dot_LBB224_3:
		  debug loc 95 1219 21;
		  debug insn "lw x12, 0(x9)";
		  x12, tmp1 <== mload(x9 + 0);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6652:
		  debug loc 95 1219 20;
		  debug insn "beqz x12, core_dash_e8b3f51d8f75870a__.LBB224_5";
		  branch_if_zero x12, core_dash_e8b3f51d8f75870a___dot_LBB224_5;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6653:
		  debug loc 95 1220 21;
		  debug insn "lw x13, 12(x2)";
		  x13, tmp1 <== mload(x2 + 12);
		  debug insn "lw x10, 8(x2)";
		  x10, tmp1 <== mload(x2 + 8);
		  debug loc 95 1220 45;
		  debug insn "lw x11, -4(x9)";
		  x11, tmp1 <== mload(x9 + 4294967292);
		  debug loc 95 1220 21;
		  debug insn "lw x13, 12(x13)";
		  x13, tmp1 <== mload(x13 + 12);
		  debug insn "jalr x13";
		  jump_and_link_dyn x13;
		  debug insn "bnez x10, core_dash_e8b3f51d8f75870a__.LBB224_28";
		  branch_if_nonzero x10, core_dash_e8b3f51d8f75870a___dot_LBB224_28;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6654:
		core_dash_e8b3f51d8f75870a___dot_LBB224_5:
		  debug loc 95 1239 16;
		  debug insn "lw x10, 8(x8)";
		  x10, tmp1 <== mload(x8 + 8);
		  debug loc 95 1239 5;
		  debug insn "sw x10, 36(x2)";
		  mstore x2 + 36, x10;
		  debug loc 95 1240 17;
		  debug insn "lb x10, 12(x8)";
		  x10, tmp2 <== mload(x8 + 12);
		  x10 <== shr(x10, 8 * tmp2);
		  x10 <== sign_extend_byte(x10);
		  debug loc 95 1240 5;
		  debug insn "sb x10, 40(x2)";
		  tmp1, tmp2 <== mload(x2 + 40);
		  tmp3 <== shl(0xff, 8 * tmp2);
		  tmp3 <== xor(tmp3, 0xffffffff);
		  tmp1 <== and(tmp1, tmp3);
		  tmp3 <== and(x10, 0xff);
		  tmp3 <== shl(tmp3, 8 * tmp2);
		  tmp1 <== or(tmp1, tmp3);
		  mstore x2 + 40 - tmp2, tmp1;
		  debug loc 95 1241 17;
		  debug insn "lw x11, 4(x8)";
		  x11, tmp1 <== mload(x8 + 4);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6655:
		  debug loc 95 1224 51;
		  debug insn "lw x10, 16(x19)";
		  x10, tmp1 <== mload(x19 + 16);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6656:
		  debug loc 95 1241 5;
		  debug insn "sw x11, 32(x2)";
		  mstore x2 + 32, x11;
		  debug loc 95 1245 21;
		  debug insn "lw x13, -4(x8)";
		  x13, tmp1 <== mload(x8 + 4294967292);
		  debug insn "lw x11, 0(x8)";
		  x11, tmp1 <== mload(x8 + 0);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6657:
		  debug loc 95 1260 5;
		  debug insn "beqz x13, core_dash_e8b3f51d8f75870a__.LBB224_10";
		  branch_if_zero x13, core_dash_e8b3f51d8f75870a___dot_LBB224_10;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6658:
		  debug loc 95 0 5;
		  debug insn "li x12, 0";
		  x12 <=X= 0;
		  debug loc 95 1260 5;
		  debug insn "bne x13, x21, core_dash_e8b3f51d8f75870a__.LBB224_11";
		  branch_if_nonzero x13 - x21, core_dash_e8b3f51d8f75870a___dot_LBB224_11;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6659:
		  debug loc 106 485 18;
		  debug insn "slli x11, x11, 3";
		  x11 <== wrap16(x11 * 8);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6660:
		  debug insn "add x11, x11, x10";
		  x11 <== wrap(x11 + x10);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6661:
		  debug loc 95 1267 22;
		  debug insn "lw x12, 4(x11)";
		  x12, tmp1 <== mload(x11 + 4);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6662:
		  debug loc 95 366 12;
		  debug insn "beq x12, x22, core_dash_e8b3f51d8f75870a__.LBB224_9";
		  branch_if_zero x12 - x22, core_dash_e8b3f51d8f75870a___dot_LBB224_9;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6663:
		  debug loc 95 0 12;
		  debug insn "li x12, 0";
		  x12 <=X= 0;
		  debug insn "j core_dash_e8b3f51d8f75870a__.LBB224_11";
		  jump core_dash_e8b3f51d8f75870a___dot_LBB224_11;
		core_dash_e8b3f51d8f75870a___dot_LBB224_9:
		  debug loc 95 1267 22;
		  debug insn "lw x11, 0(x11)";
		  x11, tmp1 <== mload(x11 + 0);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6665:
		  debug loc 95 369 27;
		  debug insn "lw x11, 0(x11)";
		  x11, tmp1 <== mload(x11 + 0);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6666:
		core_dash_e8b3f51d8f75870a___dot_LBB224_10:
		  debug loc 95 0 27;
		  debug insn "li x12, 1";
		  x12 <=X= 1;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6667:
		core_dash_e8b3f51d8f75870a___dot_LBB224_11:
		  debug loc 95 1245 9;
		  debug insn "sw x12, 16(x2)";
		  mstore x2 + 16, x12;
		  debug insn "sw x11, 20(x2)";
		  mstore x2 + 20, x11;
		  debug loc 95 1246 25;
		  debug insn "lw x13, -12(x8)";
		  x13, tmp1 <== mload(x8 + 4294967284);
		  debug insn "lw x11, -8(x8)";
		  x11, tmp1 <== mload(x8 + 4294967288);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6668:
		  debug loc 95 1260 5;
		  debug insn "beqz x13, core_dash_e8b3f51d8f75870a__.LBB224_16";
		  branch_if_zero x13, core_dash_e8b3f51d8f75870a___dot_LBB224_16;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6669:
		  debug loc 95 0 5;
		  debug insn "li x12, 0";
		  x12 <=X= 0;
		  debug loc 95 1260 5;
		  debug insn "bne x13, x21, core_dash_e8b3f51d8f75870a__.LBB224_17";
		  branch_if_nonzero x13 - x21, core_dash_e8b3f51d8f75870a___dot_LBB224_17;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6670:
		  debug loc 106 485 18;
		  debug insn "slli x11, x11, 3";
		  x11 <== wrap16(x11 * 8);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6671:
		  debug insn "add x11, x11, x10";
		  x11 <== wrap(x11 + x10);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6672:
		  debug loc 95 1267 22;
		  debug insn "lw x12, 4(x11)";
		  x12, tmp1 <== mload(x11 + 4);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6673:
		  debug loc 95 366 12;
		  debug insn "beq x12, x22, core_dash_e8b3f51d8f75870a__.LBB224_15";
		  branch_if_zero x12 - x22, core_dash_e8b3f51d8f75870a___dot_LBB224_15;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6674:
		  debug loc 95 0 12;
		  debug insn "li x12, 0";
		  x12 <=X= 0;
		  debug insn "j core_dash_e8b3f51d8f75870a__.LBB224_17";
		  jump core_dash_e8b3f51d8f75870a___dot_LBB224_17;
		core_dash_e8b3f51d8f75870a___dot_LBB224_15:
		  debug loc 95 1267 22;
		  debug insn "lw x11, 0(x11)";
		  x11, tmp1 <== mload(x11 + 0);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6676:
		  debug loc 95 369 27;
		  debug insn "lw x11, 0(x11)";
		  x11, tmp1 <== mload(x11 + 0);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6677:
		core_dash_e8b3f51d8f75870a___dot_LBB224_16:
		  debug loc 95 0 27;
		  debug insn "li x12, 1";
		  x12 <=X= 1;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6678:
		core_dash_e8b3f51d8f75870a___dot_LBB224_17:
		  debug loc 95 1246 9;
		  debug insn "sw x12, 24(x2)";
		  mstore x2 + 24, x12;
		  debug insn "sw x11, 28(x2)";
		  mstore x2 + 28, x11;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6679:
		  debug loc 95 1253 45;
		  debug insn "lw x11, -16(x8)";
		  x11, tmp1 <== mload(x8 + 4294967280);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6680:
		  debug loc 106 485 18;
		  debug insn "slli x11, x11, 3";
		  x11 <== wrap16(x11 * 8);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6681:
		  debug insn "add x10, x10, x11";
		  x10 <== wrap(x10 + x11);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6682:
		  debug loc 95 1256 5;
		  debug insn "lw x12, 4(x10)";
		  x12, tmp1 <== mload(x10 + 4);
		  debug loc 95 1256 23;
		  debug insn "lw x10, 0(x10)";
		  x10, tmp1 <== mload(x10 + 0);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6683:
		  debug loc 95 1256 5;
		  debug insn "addi x11, x2, 8";
		  x11 <== wrap(x2 + 8);
		  debug insn "jalr x12";
		  jump_and_link_dyn x12;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6684:
		  debug loc 95 1224 17;
		  debug insn "bnez x10, core_dash_e8b3f51d8f75870a__.LBB224_28";
		  branch_if_nonzero x10, core_dash_e8b3f51d8f75870a___dot_LBB224_28;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6685:
		  debug loc 108 146 24;
		  debug insn "addi x9, x9, 8";
		  x9 <== wrap(x9 + 8);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6686:
		  debug insn "addi x20, x20, -32";
		  x20 <== wrap(x20 + 4294967264);
		  debug insn "addi x8, x8, 32";
		  x8 <== wrap(x8 + 32);
		  debug insn "bnez x20, core_dash_e8b3f51d8f75870a__.LBB224_3";
		  branch_if_nonzero x20, core_dash_e8b3f51d8f75870a___dot_LBB224_3;
		  debug insn "j core_dash_e8b3f51d8f75870a__.LBB224_25";
		  jump core_dash_e8b3f51d8f75870a___dot_LBB224_25;
		core_dash_e8b3f51d8f75870a___dot_LBB224_19:
		  debug loc 95 1201 29;
		  debug insn "lw x10, 20(x19)";
		  x10, tmp1 <== mload(x19 + 20);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6688:
		  debug loc 108 146 24;
		  debug insn "beqz x10, core_dash_e8b3f51d8f75870a__.LBB224_26";
		  branch_if_zero x10, core_dash_e8b3f51d8f75870a___dot_LBB224_26;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6689:
		  debug loc 108 0 24;
		  debug insn "lw x11, 16(x19)";
		  x11, tmp1 <== mload(x19 + 16);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6690:
		  debug insn "lw x12, 8(x19)";
		  x12, tmp1 <== mload(x19 + 8);
		  debug loc 108 146 24;
		  debug insn "addi x13, x10, -1";
		  x13 <== wrap(x10 + 4294967295);
		  debug insn "slli x13, x13, 3";
		  x13 <== wrap16(x13 * 8);
		  debug insn "srli x13, x13, 3";
		  x13 <== shr(x13, 3);
		  debug insn "addi x18, x13, 1";
		  x18 <== wrap(x13 + 1);
		  debug insn "addi x8, x12, 4";
		  x8 <== wrap(x12 + 4);
		  debug insn "addi x9, x11, 4";
		  x9 <== wrap(x11 + 4);
		  debug insn "slli x20, x10, 3";
		  x20 <== wrap16(x10 * 8);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6691:
		core_dash_e8b3f51d8f75870a___dot_LBB224_21:
		  debug loc 95 1205 21;
		  debug insn "lw x12, 0(x8)";
		  x12, tmp1 <== mload(x8 + 0);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6692:
		  debug loc 95 1205 20;
		  debug insn "beqz x12, core_dash_e8b3f51d8f75870a__.LBB224_23";
		  branch_if_zero x12, core_dash_e8b3f51d8f75870a___dot_LBB224_23;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6693:
		  debug loc 95 1206 21;
		  debug insn "lw x13, 12(x2)";
		  x13, tmp1 <== mload(x2 + 12);
		  debug insn "lw x10, 8(x2)";
		  x10, tmp1 <== mload(x2 + 8);
		  debug loc 95 1206 45;
		  debug insn "lw x11, -4(x8)";
		  x11, tmp1 <== mload(x8 + 4294967292);
		  debug loc 95 1206 21;
		  debug insn "lw x13, 12(x13)";
		  x13, tmp1 <== mload(x13 + 12);
		  debug insn "jalr x13";
		  jump_and_link_dyn x13;
		  debug insn "bnez x10, core_dash_e8b3f51d8f75870a__.LBB224_28";
		  branch_if_nonzero x10, core_dash_e8b3f51d8f75870a___dot_LBB224_28;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6694:
		core_dash_e8b3f51d8f75870a___dot_LBB224_23:
		  debug loc 95 1208 17;
		  debug insn "lw x12, 0(x9)";
		  x12, tmp1 <== mload(x9 + 0);
		  debug loc 95 1208 33;
		  debug insn "lw x10, -4(x9)";
		  x10, tmp1 <== mload(x9 + 4294967292);
		  debug loc 95 1208 17;
		  debug insn "addi x11, x2, 8";
		  x11 <== wrap(x2 + 8);
		  debug insn "jalr x12";
		  jump_and_link_dyn x12;
		  debug insn "bnez x10, core_dash_e8b3f51d8f75870a__.LBB224_28";
		  branch_if_nonzero x10, core_dash_e8b3f51d8f75870a___dot_LBB224_28;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6695:
		  debug loc 108 146 24;
		  debug insn "addi x8, x8, 8";
		  x8 <== wrap(x8 + 8);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6696:
		  debug insn "addi x20, x20, -8";
		  x20 <== wrap(x20 + 4294967288);
		  debug insn "addi x9, x9, 8";
		  x9 <== wrap(x9 + 8);
		  debug insn "bnez x20, core_dash_e8b3f51d8f75870a__.LBB224_21";
		  branch_if_nonzero x20, core_dash_e8b3f51d8f75870a___dot_LBB224_21;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6697:
		core_dash_e8b3f51d8f75870a___dot_LBB224_25:
		  debug loc 95 1231 26;
		  debug insn "lw x10, 12(x19)";
		  x10, tmp1 <== mload(x19 + 12);
		  debug loc 95 1231 12;
		  debug insn "bltu x18, x10, core_dash_e8b3f51d8f75870a__.LBB224_27";
		  branch_if_positive x10 - x18, core_dash_e8b3f51d8f75870a___dot_LBB224_27;
		  debug insn "j core_dash_e8b3f51d8f75870a__.LBB224_29";
		  jump core_dash_e8b3f51d8f75870a___dot_LBB224_29;
		core_dash_e8b3f51d8f75870a___dot_LBB224_26:
		  debug loc 95 0 12;
		  debug insn "li x18, 0";
		  x18 <=X= 0;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6699:
		  debug loc 95 1231 26;
		  debug insn "lw x10, 12(x19)";
		  x10, tmp1 <== mload(x19 + 12);
		  debug loc 95 1231 12;
		  debug insn "bgeu x18, x10, core_dash_e8b3f51d8f75870a__.LBB224_29";
		  branch_if_positive x18 - x10 + 1, core_dash_e8b3f51d8f75870a___dot_LBB224_29;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6700:
		core_dash_e8b3f51d8f75870a___dot_LBB224_27:
		  debug loc 95 1231 26;
		  debug insn "lw x10, 8(x19)";
		  x10, tmp1 <== mload(x19 + 8);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6701:
		  debug loc 105 219 12;
		  debug insn "slli x11, x18, 3";
		  x11 <== wrap16(x18 * 8);
		  debug insn "add x12, x10, x11";
		  x12 <== wrap(x10 + x11);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6702:
		  debug loc 95 1232 9;
		  debug insn "lw x13, 12(x2)";
		  x13, tmp1 <== mload(x2 + 12);
		  debug insn "lw x10, 8(x2)";
		  x10, tmp1 <== mload(x2 + 8);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6703:
		  debug loc 95 1232 33;
		  debug insn "lw x11, 0(x12)";
		  x11, tmp1 <== mload(x12 + 0);
		  debug insn "lw x12, 4(x12)";
		  x12, tmp1 <== mload(x12 + 4);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6704:
		  debug loc 95 1232 9;
		  debug insn "lw x13, 12(x13)";
		  x13, tmp1 <== mload(x13 + 12);
		  debug insn "jalr x13";
		  jump_and_link_dyn x13;
		  debug insn "beqz x10, core_dash_e8b3f51d8f75870a__.LBB224_29";
		  branch_if_zero x10, core_dash_e8b3f51d8f75870a___dot_LBB224_29;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6705:
		core_dash_e8b3f51d8f75870a___dot_LBB224_28:
		  debug loc 95 0 9;
		  debug insn "li x10, 1";
		  x10 <=X= 1;
		  debug insn "j core_dash_e8b3f51d8f75870a__.LBB224_30";
		  jump core_dash_e8b3f51d8f75870a___dot_LBB224_30;
		core_dash_e8b3f51d8f75870a___dot_LBB224_29:
		  debug insn "li x10, 0";
		  x10 <=X= 0;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6707:
		core_dash_e8b3f51d8f75870a___dot_LBB224_30:
		  debug loc 95 1236 2;
		  debug insn "lw x1, 76(x2)";
		  x1, tmp1 <== mload(x2 + 76);
		  debug insn "lw x8, 72(x2)";
		  x8, tmp1 <== mload(x2 + 72);
		  debug insn "lw x9, 68(x2)";
		  x9, tmp1 <== mload(x2 + 68);
		  debug insn "lw x18, 64(x2)";
		  x18, tmp1 <== mload(x2 + 64);
		  debug insn "lw x19, 60(x2)";
		  x19, tmp1 <== mload(x2 + 60);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6708:
		  debug insn "lw x20, 56(x2)";
		  x20, tmp1 <== mload(x2 + 56);
		  debug insn "lw x21, 52(x2)";
		  x21, tmp1 <== mload(x2 + 52);
		  debug insn "lw x22, 48(x2)";
		  x22, tmp1 <== mload(x2 + 48);
		  debug insn "addi x2, x2, 80";
		  x2 <== wrap(x2 + 80);
		  debug insn "ret ";
		  ret;
		_ZN4core3fmt9Formatter12pad_integral17h0f5bfd9b3d6cd343E:
		core_dash_e8b3f51d8f75870a___dot_Lfunc_begin225:
		  debug loc 95 1360 0;
		  debug insn "addi x2, x2, -64";
		  x2 <== wrap(x2 + 4294967232);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6710:
		  debug loc 95 0 0;
		  debug insn "sw x1, 60(x2)";
		  mstore x2 + 60, x1;
		  debug insn "sw x8, 56(x2)";
		  mstore x2 + 56, x8;
		  debug insn "sw x9, 52(x2)";
		  mstore x2 + 52, x9;
		  debug insn "sw x18, 48(x2)";
		  mstore x2 + 48, x18;
		  debug insn "sw x19, 44(x2)";
		  mstore x2 + 44, x19;
		  debug insn "sw x20, 40(x2)";
		  mstore x2 + 40, x20;
		  debug insn "sw x21, 36(x2)";
		  mstore x2 + 36, x21;
		  debug insn "sw x22, 32(x2)";
		  mstore x2 + 32, x22;
		  debug insn "sw x23, 28(x2)";
		  mstore x2 + 28, x23;
		  debug insn "sw x24, 24(x2)";
		  mstore x2 + 24, x24;
		  debug insn "sw x25, 20(x2)";
		  mstore x2 + 20, x25;
		  debug insn "sw x26, 16(x2)";
		  mstore x2 + 16, x26;
		  debug insn "sw x27, 12(x2)";
		  mstore x2 + 12, x27;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6711:
		  debug insn "mv x19, x15";
		  x19 <=X= x15;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6712:
		  debug insn "mv x18, x14";
		  x18 <=X= x14;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6713:
		  debug insn "mv x22, x13";
		  x22 <=X= x13;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6714:
		  debug insn "mv x20, x12";
		  x20 <=X= x12;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6715:
		  debug insn "mv x24, x10";
		  x24 <=X= x10;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6716:
		  debug loc 95 1364 12;
		  debug insn "beqz x11, core_dash_e8b3f51d8f75870a__.LBB225_8";
		  branch_if_zero x11, core_dash_e8b3f51d8f75870a___dot_LBB225_8;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6717:
		  debug loc 95 1842 9;
		  debug insn "lw x8, 24(x24)";
		  x8, tmp1 <== mload(x24 + 24);
		  debug insn "andi x10, x8, 1";
		  x10 <== and(x8, 1);
		  debug insn "lui x21, 272";
		  x21 <=X= 1114112;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6718:
		  debug loc 95 1367 19;
		  debug insn "beqz x10, core_dash_e8b3f51d8f75870a__.LBB225_3";
		  branch_if_zero x10, core_dash_e8b3f51d8f75870a___dot_LBB225_3;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6719:
		  debug loc 95 0 19;
		  debug insn "li x21, 43";
		  x21 <=X= 43;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6720:
		core_dash_e8b3f51d8f75870a___dot_LBB225_3:
		  debug loc 95 1367 19;
		  debug insn "add x25, x10, x19";
		  x25 <== wrap(x10 + x19);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6721:
		  debug loc 95 1899 9;
		  debug insn "andi x10, x8, 4";
		  x10 <== and(x8, 4);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6722:
		  debug loc 95 1372 25;
		  debug insn "beqz x10, core_dash_e8b3f51d8f75870a__.LBB225_9";
		  branch_if_zero x10, core_dash_e8b3f51d8f75870a___dot_LBB225_9;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6723:
		core_dash_e8b3f51d8f75870a___dot_LBB225_4:
		  debug loc 95 0 25;
		  debug insn "li x10, 16";
		  x10 <=X= 16;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6724:
		  debug loc 169 27 8;
		  debug insn "bgeu x22, x10, core_dash_e8b3f51d8f75870a__.LBB225_10";
		  branch_if_positive x22 - x10 + 1, core_dash_e8b3f51d8f75870a___dot_LBB225_10;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6725:
		  debug loc 169 0 8;
		  debug insn "li x10, 0";
		  x10 <=X= 0;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6726:
		  debug loc 108 146 24;
		  debug insn "beqz x22, core_dash_e8b3f51d8f75870a__.LBB225_11";
		  branch_if_zero x22, core_dash_e8b3f51d8f75870a___dot_LBB225_11;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6727:
		  debug loc 108 0 24;
		  debug insn "mv x11, x22";
		  x11 <=X= x22;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6728:
		  debug insn "mv x12, x20";
		  x12 <=X= x20;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6729:
		core_dash_e8b3f51d8f75870a___dot_LBB225_7:
		  debug loc 123 2421 21;
		  debug insn "lb x13, 0(x12)";
		  x13, tmp2 <== mload(x12 + 0);
		  x13 <== shr(x13, 8 * tmp2);
		  x13 <== sign_extend_byte(x13);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6730:
		  debug loc 116 499 18;
		  debug insn "addi x12, x12, 1";
		  x12 <== wrap(x12 + 1);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6731:
		  debug loc 170 25 5;
		  debug insn "slti x13, x13, -64";
		  tmp1 <== to_signed(x13);
		  x13 <=Y= is_positive(-64 - tmp1);
		  debug insn "xori x13, x13, 1";
		  x13 <== xor(x13, 1);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6732:
		  debug loc 108 146 24;
		  debug insn "addi x11, x11, -1";
		  x11 <== wrap(x11 + 4294967295);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6733:
		  debug loc 171 53 28;
		  debug insn "add x10, x10, x13";
		  x10 <== wrap(x10 + x13);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6734:
		  debug loc 108 146 24;
		  debug insn "bnez x11, core_dash_e8b3f51d8f75870a__.LBB225_7";
		  branch_if_nonzero x11, core_dash_e8b3f51d8f75870a___dot_LBB225_7;
		  debug insn "j core_dash_e8b3f51d8f75870a__.LBB225_11";
		  jump core_dash_e8b3f51d8f75870a___dot_LBB225_11;
		core_dash_e8b3f51d8f75870a___dot_LBB225_8:
		  debug loc 95 1899 9;
		  debug insn "lw x8, 24(x24)";
		  x8, tmp1 <== mload(x24 + 24);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6736:
		  debug loc 95 1366 13;
		  debug insn "addi x25, x19, 1";
		  x25 <== wrap(x19 + 1);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6737:
		  debug loc 95 0 13;
		  debug insn "li x21, 45";
		  x21 <=X= 45;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6738:
		  debug loc 95 1899 9;
		  debug insn "andi x10, x8, 4";
		  x10 <== and(x8, 4);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6739:
		  debug loc 95 1372 25;
		  debug insn "bnez x10, core_dash_e8b3f51d8f75870a__.LBB225_4";
		  branch_if_nonzero x10, core_dash_e8b3f51d8f75870a___dot_LBB225_4;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6740:
		core_dash_e8b3f51d8f75870a___dot_LBB225_9:
		  debug loc 95 0 25;
		  debug insn "li x20, 0";
		  x20 <=X= 0;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6741:
		  debug loc 95 1389 15;
		  debug insn "lw x10, 8(x24)";
		  x10, tmp1 <== mload(x24 + 8);
		  debug loc 95 1389 9;
		  debug insn "bnez x10, core_dash_e8b3f51d8f75870a__.LBB225_12";
		  branch_if_nonzero x10, core_dash_e8b3f51d8f75870a___dot_LBB225_12;
		  debug insn "j core_dash_e8b3f51d8f75870a__.LBB225_19";
		  jump core_dash_e8b3f51d8f75870a___dot_LBB225_19;
		core_dash_e8b3f51d8f75870a___dot_LBB225_10:
		  debug loc 169 34 9;
		  debug insn "mv x10, x20";
		  x10 <=X= x20;
		  debug insn "mv x11, x22";
		  x11 <=X= x22;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6743:
		  debug insn "call _ZN4core3str5count14do_count_chars17h97c55cd17370d3d0E";
		  call _ZN4core3str5count14do_count_chars17h97c55cd17370d3d0E;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6744:
		core_dash_e8b3f51d8f75870a___dot_LBB225_11:
		  debug loc 95 1373 13;
		  debug insn "add x25, x25, x10";
		  x25 <== wrap(x25 + x10);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6745:
		  debug loc 95 1389 15;
		  debug insn "lw x10, 8(x24)";
		  x10, tmp1 <== mload(x24 + 8);
		  debug loc 95 1389 9;
		  debug insn "beqz x10, core_dash_e8b3f51d8f75870a__.LBB225_19";
		  branch_if_zero x10, core_dash_e8b3f51d8f75870a___dot_LBB225_19;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6746:
		core_dash_e8b3f51d8f75870a___dot_LBB225_12:
		  debug loc 95 1398 35;
		  debug insn "lw x26, 12(x24)";
		  x26, tmp1 <== mload(x24 + 12);
		  debug loc 95 1398 26;
		  debug insn "bgeu x25, x26, core_dash_e8b3f51d8f75870a__.LBB225_19";
		  branch_if_positive x25 - x26 + 1, core_dash_e8b3f51d8f75870a___dot_LBB225_19;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6747:
		  debug loc 95 1925 9;
		  debug insn "andi x10, x8, 8";
		  x10 <== and(x8, 8);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6748:
		  debug loc 95 1404 26;
		  debug insn "bnez x10, core_dash_e8b3f51d8f75870a__.LBB225_22";
		  branch_if_nonzero x10, core_dash_e8b3f51d8f75870a___dot_LBB225_22;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6749:
		  debug loc 95 1509 27;
		  debug insn "lbu x11, 32(x24)";
		  x11, tmp2 <== mload(x24 + 32);
		  x11 <== shr(x11, 8 * tmp2);
		  x11 <== and(x11, 0xff);
		  debug insn "li x12, 3";
		  x12 <=X= 3;
		  debug insn "li x10, 1";
		  x10 <=X= 1;
		  debug insn "beq x11, x12, core_dash_e8b3f51d8f75870a__.LBB225_16";
		  branch_if_zero x11 - x12, core_dash_e8b3f51d8f75870a___dot_LBB225_16;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6750:
		  debug loc 95 0 27;
		  debug insn "mv x10, x11";
		  x10 <=X= x11;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6751:
		core_dash_e8b3f51d8f75870a___dot_LBB225_16:
		  debug loc 95 1514 35;
		  debug insn "andi x11, x10, 3";
		  x11 <== and(x10, 3);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6752:
		  debug loc 95 0 0;
		  debug insn "sub x10, x26, x25";
		  x10 <== wrap_signed(x26 - x25);
		  debug insn "beqz x11, core_dash_e8b3f51d8f75870a__.LBB225_26";
		  branch_if_zero x11, core_dash_e8b3f51d8f75870a___dot_LBB225_26;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6753:
		  debug insn "li x12, 1";
		  x12 <=X= 1;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6754:
		  debug loc 95 1514 35;
		  debug insn "bne x11, x12, core_dash_e8b3f51d8f75870a__.LBB225_27";
		  branch_if_nonzero x11 - x12, core_dash_e8b3f51d8f75870a___dot_LBB225_27;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6755:
		  debug loc 95 0 35;
		  debug insn "li x26, 0";
		  x26 <=X= 0;
		  debug insn "j core_dash_e8b3f51d8f75870a__.LBB225_28";
		  jump core_dash_e8b3f51d8f75870a___dot_LBB225_28;
		core_dash_e8b3f51d8f75870a___dot_LBB225_19:
		  debug insn "lw x8, 0(x24)";
		  x8, tmp1 <== mload(x24 + 0);
		  debug insn "lw x9, 4(x24)";
		  x9, tmp1 <== mload(x24 + 4);
		  debug insn "mv x10, x8";
		  x10 <=X= x8;
		  debug insn "mv x11, x9";
		  x11 <=X= x9;
		  debug insn "mv x12, x21";
		  x12 <=X= x21;
		  debug insn "mv x13, x20";
		  x13 <=X= x20;
		  debug insn "mv x14, x22";
		  x14 <=X= x22;
		  debug insn "call core_dash_e8b3f51d8f75870a___ZN4core3fmt9Formatter12pad_integral12write_prefix17hb01a11c664695966E";
		  call core_dash_e8b3f51d8f75870a___ZN4core3fmt9Formatter12pad_integral12write_prefix17hb01a11c664695966E;
		  debug insn "li x23, 1";
		  x23 <=X= 1;
		  debug insn "beqz x10, core_dash_e8b3f51d8f75870a__.LBB225_21";
		  branch_if_zero x10, core_dash_e8b3f51d8f75870a___dot_LBB225_21;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6757:
		core_dash_e8b3f51d8f75870a___dot_LBB225_20:
		  debug loc 95 1423 6;
		  debug insn "mv x10, x23";
		  x10 <=X= x23;
		  debug insn "lw x1, 60(x2)";
		  x1, tmp1 <== mload(x2 + 60);
		  debug insn "lw x8, 56(x2)";
		  x8, tmp1 <== mload(x2 + 56);
		  debug insn "lw x9, 52(x2)";
		  x9, tmp1 <== mload(x2 + 52);
		  debug insn "lw x18, 48(x2)";
		  x18, tmp1 <== mload(x2 + 48);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6758:
		  debug insn "lw x19, 44(x2)";
		  x19, tmp1 <== mload(x2 + 44);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6759:
		  debug insn "lw x20, 40(x2)";
		  x20, tmp1 <== mload(x2 + 40);
		  debug insn "lw x21, 36(x2)";
		  x21, tmp1 <== mload(x2 + 36);
		  debug insn "lw x22, 32(x2)";
		  x22, tmp1 <== mload(x2 + 32);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6760:
		  debug insn "lw x23, 28(x2)";
		  x23, tmp1 <== mload(x2 + 28);
		  debug insn "lw x24, 24(x2)";
		  x24, tmp1 <== mload(x2 + 24);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6761:
		  debug insn "lw x25, 20(x2)";
		  x25, tmp1 <== mload(x2 + 20);
		  debug insn "lw x26, 16(x2)";
		  x26, tmp1 <== mload(x2 + 16);
		  debug insn "lw x27, 12(x2)";
		  x27, tmp1 <== mload(x2 + 12);
		  debug insn "addi x2, x2, 64";
		  x2 <== wrap(x2 + 64);
		  debug insn "ret ";
		  ret;
		core_dash_e8b3f51d8f75870a___dot_LBB225_21:
		core_dash_e8b3f51d8f75870a___dot_Ltmp6762:
		  debug loc 95 0 0;
		  debug insn "lw x15, 12(x9)";
		  x15, tmp1 <== mload(x9 + 12);
		  debug insn "mv x10, x8";
		  x10 <=X= x8;
		  debug insn "mv x11, x18";
		  x11 <=X= x18;
		  debug insn "mv x12, x19";
		  x12 <=X= x19;
		  debug insn "lw x1, 60(x2)";
		  x1, tmp1 <== mload(x2 + 60);
		  debug insn "lw x8, 56(x2)";
		  x8, tmp1 <== mload(x2 + 56);
		  debug insn "lw x9, 52(x2)";
		  x9, tmp1 <== mload(x2 + 52);
		  debug insn "lw x18, 48(x2)";
		  x18, tmp1 <== mload(x2 + 48);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6763:
		  debug insn "lw x19, 44(x2)";
		  x19, tmp1 <== mload(x2 + 44);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6764:
		  debug insn "lw x20, 40(x2)";
		  x20, tmp1 <== mload(x2 + 40);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6765:
		  debug insn "lw x21, 36(x2)";
		  x21, tmp1 <== mload(x2 + 36);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6766:
		  debug insn "lw x22, 32(x2)";
		  x22, tmp1 <== mload(x2 + 32);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6767:
		  debug insn "lw x23, 28(x2)";
		  x23, tmp1 <== mload(x2 + 28);
		  debug insn "lw x24, 24(x2)";
		  x24, tmp1 <== mload(x2 + 24);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6768:
		  debug insn "lw x25, 20(x2)";
		  x25, tmp1 <== mload(x2 + 20);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6769:
		  debug insn "lw x26, 16(x2)";
		  x26, tmp1 <== mload(x2 + 16);
		  debug insn "lw x27, 12(x2)";
		  x27, tmp1 <== mload(x2 + 12);
		  debug insn "addi x2, x2, 64";
		  x2 <== wrap(x2 + 64);
		  debug insn "jr x15";
		  jump_dyn x15;
		core_dash_e8b3f51d8f75870a___dot_LBB225_22:
		core_dash_e8b3f51d8f75870a___dot_Ltmp6770:
		  debug loc 99 1157 9;
		  debug insn "lw x8, 28(x24)";
		  x8, tmp1 <== mload(x24 + 28);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6771:
		  debug loc 99 0 9;
		  debug insn "li x10, 48";
		  x10 <=X= 48;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6772:
		  debug loc 99 1157 9;
		  debug insn "lbu x11, 32(x24)";
		  x11, tmp2 <== mload(x24 + 32);
		  x11 <== shr(x11, 8 * tmp2);
		  x11 <== and(x11, 0xff);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6773:
		  debug loc 95 1407 17;
		  debug insn "sw x11, 8(x2)";
		  mstore x2 + 8, x11;
		  debug insn "lw x27, 0(x24)";
		  x27, tmp1 <== mload(x24 + 0);
		  debug insn "lw x9, 4(x24)";
		  x9, tmp1 <== mload(x24 + 4);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6774:
		  debug loc 99 1354 9;
		  debug insn "sw x10, 28(x24)";
		  mstore x24 + 28, x10;
		  debug insn "li x23, 1";
		  x23 <=X= 1;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6775:
		  debug loc 99 1354 9;
		  debug insn "sb x23, 32(x24)";
		  tmp1, tmp2 <== mload(x24 + 32);
		  tmp3 <== shl(0xff, 8 * tmp2);
		  tmp3 <== xor(tmp3, 0xffffffff);
		  tmp1 <== and(tmp1, tmp3);
		  tmp3 <== and(x23, 0xff);
		  tmp3 <== shl(tmp3, 8 * tmp2);
		  tmp1 <== or(tmp1, tmp3);
		  mstore x24 + 32 - tmp2, tmp1;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6776:
		  debug loc 95 1407 17;
		  debug insn "mv x10, x27";
		  x10 <=X= x27;
		  debug insn "mv x11, x9";
		  x11 <=X= x9;
		  debug insn "mv x12, x21";
		  x12 <=X= x21;
		  debug insn "mv x13, x20";
		  x13 <=X= x20;
		  debug insn "mv x14, x22";
		  x14 <=X= x22;
		  debug insn "call core_dash_e8b3f51d8f75870a___ZN4core3fmt9Formatter12pad_integral12write_prefix17hb01a11c664695966E";
		  call core_dash_e8b3f51d8f75870a___ZN4core3fmt9Formatter12pad_integral12write_prefix17hb01a11c664695966E;
		  debug insn "bnez x10, core_dash_e8b3f51d8f75870a__.LBB225_20";
		  branch_if_nonzero x10, core_dash_e8b3f51d8f75870a___dot_LBB225_20;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6777:
		  debug loc 95 0 17;
		  debug insn "mv x20, x8";
		  x20 <=X= x8;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6778:
		  debug loc 95 1520 9;
		  debug insn "sub x10, x26, x25";
		  x10 <== wrap_signed(x26 - x25);
		  debug insn "addi x8, x10, 1";
		  x8 <== wrap(x10 + 1);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6779:
		core_dash_e8b3f51d8f75870a___dot_LBB225_24:
		  debug loc 112 1435 52;
		  debug insn "addi x8, x8, -1";
		  x8 <== wrap(x8 + 4294967295);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6780:
		  debug loc 104 621 12;
		  debug insn "beqz x8, core_dash_e8b3f51d8f75870a__.LBB225_39";
		  branch_if_zero x8, core_dash_e8b3f51d8f75870a___dot_LBB225_39;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6781:
		  debug loc 95 1521 13;
		  debug insn "lw x12, 16(x9)";
		  x12, tmp1 <== mload(x9 + 16);
		  debug insn "li x11, 48";
		  x11 <=X= 48;
		  debug insn "mv x10, x27";
		  x10 <=X= x27;
		  debug insn "jalr x12";
		  jump_and_link_dyn x12;
		  debug insn "beqz x10, core_dash_e8b3f51d8f75870a__.LBB225_24";
		  branch_if_zero x10, core_dash_e8b3f51d8f75870a___dot_LBB225_24;
		  debug insn "j core_dash_e8b3f51d8f75870a__.LBB225_20";
		  jump core_dash_e8b3f51d8f75870a___dot_LBB225_20;
		core_dash_e8b3f51d8f75870a___dot_LBB225_26:
		  debug loc 95 0 13;
		  debug insn "mv x26, x10";
		  x26 <=X= x10;
		  debug insn "mv x10, x11";
		  x10 <=X= x11;
		  debug insn "j core_dash_e8b3f51d8f75870a__.LBB225_28";
		  jump core_dash_e8b3f51d8f75870a___dot_LBB225_28;
		core_dash_e8b3f51d8f75870a___dot_LBB225_27:
		  debug loc 95 1517 56;
		  debug insn "addi x11, x10, 1";
		  x11 <== wrap(x10 + 1);
		  debug loc 95 1517 43;
		  debug insn "srli x10, x10, 1";
		  x10 <== shr(x10, 1);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6784:
		  debug loc 95 1517 56;
		  debug insn "srli x26, x11, 1";
		  x26 <== shr(x11, 1);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6785:
		core_dash_e8b3f51d8f75870a___dot_LBB225_28:
		  debug loc 95 0 56;
		  debug insn "lw x25, 0(x24)";
		  x25, tmp1 <== mload(x24 + 0);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6786:
		  debug insn "lw x27, 4(x24)";
		  x27, tmp1 <== mload(x24 + 4);
		  debug insn "lw x8, 28(x24)";
		  x8, tmp1 <== mload(x24 + 28);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6787:
		  debug loc 95 1520 9;
		  debug insn "addi x9, x10, 1";
		  x9 <== wrap(x10 + 1);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6788:
		core_dash_e8b3f51d8f75870a___dot_LBB225_29:
		  debug loc 112 1435 52;
		  debug insn "addi x9, x9, -1";
		  x9 <== wrap(x9 + 4294967295);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6789:
		  debug loc 104 621 12;
		  debug insn "beqz x9, core_dash_e8b3f51d8f75870a__.LBB225_32";
		  branch_if_zero x9, core_dash_e8b3f51d8f75870a___dot_LBB225_32;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6790:
		  debug loc 95 1521 13;
		  debug insn "lw x12, 16(x27)";
		  x12, tmp1 <== mload(x27 + 16);
		  debug insn "mv x10, x25";
		  x10 <=X= x25;
		  debug insn "mv x11, x8";
		  x11 <=X= x8;
		  debug insn "jalr x12";
		  jump_and_link_dyn x12;
		  debug insn "beqz x10, core_dash_e8b3f51d8f75870a__.LBB225_29";
		  branch_if_zero x10, core_dash_e8b3f51d8f75870a___dot_LBB225_29;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6791:
		  debug loc 95 0 13;
		  debug insn "li x23, 1";
		  x23 <=X= 1;
		  debug insn "j core_dash_e8b3f51d8f75870a__.LBB225_20";
		  jump core_dash_e8b3f51d8f75870a___dot_LBB225_20;
		core_dash_e8b3f51d8f75870a___dot_LBB225_32:
		  debug insn "lui x10, 272";
		  x10 <=X= 1114112;
		  debug insn "li x23, 1";
		  x23 <=X= 1;
		  debug loc 95 1417 36;
		  debug insn "beq x8, x10, core_dash_e8b3f51d8f75870a__.LBB225_20";
		  branch_if_zero x8 - x10, core_dash_e8b3f51d8f75870a___dot_LBB225_20;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6793:
		  debug loc 95 1418 17;
		  debug insn "mv x10, x25";
		  x10 <=X= x25;
		  debug insn "mv x11, x27";
		  x11 <=X= x27;
		  debug insn "mv x12, x21";
		  x12 <=X= x21;
		  debug insn "mv x13, x20";
		  x13 <=X= x20;
		  debug insn "mv x14, x22";
		  x14 <=X= x22;
		  debug insn "call core_dash_e8b3f51d8f75870a___ZN4core3fmt9Formatter12pad_integral12write_prefix17hb01a11c664695966E";
		  call core_dash_e8b3f51d8f75870a___ZN4core3fmt9Formatter12pad_integral12write_prefix17hb01a11c664695966E;
		  debug insn "bnez x10, core_dash_e8b3f51d8f75870a__.LBB225_20";
		  branch_if_nonzero x10, core_dash_e8b3f51d8f75870a___dot_LBB225_20;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6794:
		  debug loc 95 1419 17;
		  debug insn "lw x13, 12(x27)";
		  x13, tmp1 <== mload(x27 + 12);
		  debug insn "mv x10, x25";
		  x10 <=X= x25;
		  debug insn "mv x11, x18";
		  x11 <=X= x18;
		  debug insn "mv x12, x19";
		  x12 <=X= x19;
		  debug insn "jalr x13";
		  jump_and_link_dyn x13;
		  debug insn "bnez x10, core_dash_e8b3f51d8f75870a__.LBB225_20";
		  branch_if_nonzero x10, core_dash_e8b3f51d8f75870a___dot_LBB225_20;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6795:
		  debug loc 95 0 17;
		  debug insn "li x9, 0";
		  x9 <=X= 0;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6796:
		core_dash_e8b3f51d8f75870a___dot_LBB225_36:
		  debug insn "beq x26, x9, core_dash_e8b3f51d8f75870a__.LBB225_41";
		  branch_if_zero x26 - x9, core_dash_e8b3f51d8f75870a___dot_LBB225_41;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6797:
		  debug loc 95 1287 13;
		  debug insn "lw x12, 16(x27)";
		  x12, tmp1 <== mload(x27 + 16);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6798:
		  debug loc 114 470 22;
		  debug insn "addi x9, x9, 1";
		  x9 <== wrap(x9 + 1);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6799:
		  debug loc 95 1287 13;
		  debug insn "mv x10, x25";
		  x10 <=X= x25;
		  debug insn "mv x11, x8";
		  x11 <=X= x8;
		  debug insn "jalr x12";
		  jump_and_link_dyn x12;
		  debug insn "beqz x10, core_dash_e8b3f51d8f75870a__.LBB225_36";
		  branch_if_zero x10, core_dash_e8b3f51d8f75870a___dot_LBB225_36;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6800:
		  debug insn "addi x10, x9, -1";
		  x10 <== wrap(x9 + 4294967295);
		  debug insn "j core_dash_e8b3f51d8f75870a__.LBB225_42";
		  jump core_dash_e8b3f51d8f75870a___dot_LBB225_42;
		core_dash_e8b3f51d8f75870a___dot_LBB225_39:
		  debug loc 95 1409 17;
		  debug insn "lw x13, 12(x9)";
		  x13, tmp1 <== mload(x9 + 12);
		  debug insn "mv x10, x27";
		  x10 <=X= x27;
		  debug insn "mv x11, x18";
		  x11 <=X= x18;
		  debug insn "mv x12, x19";
		  x12 <=X= x19;
		  debug insn "jalr x13";
		  jump_and_link_dyn x13;
		  debug insn "bnez x10, core_dash_e8b3f51d8f75870a__.LBB225_20";
		  branch_if_nonzero x10, core_dash_e8b3f51d8f75870a___dot_LBB225_20;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6802:
		  debug loc 95 0 17;
		  debug insn "li x23, 0";
		  x23 <=X= 0;
		  debug loc 95 1411 17;
		  debug insn "sw x20, 28(x24)";
		  mstore x24 + 28, x20;
		  debug loc 95 1412 17;
		  debug insn "lw x10, 8(x2)";
		  x10, tmp1 <== mload(x2 + 8);
		  debug insn "sb x10, 32(x24)";
		  tmp1, tmp2 <== mload(x24 + 32);
		  tmp3 <== shl(0xff, 8 * tmp2);
		  tmp3 <== xor(tmp3, 0xffffffff);
		  tmp1 <== and(tmp1, tmp3);
		  tmp3 <== and(x10, 0xff);
		  tmp3 <== shl(tmp3, 8 * tmp2);
		  tmp1 <== or(tmp1, tmp3);
		  mstore x24 + 32 - tmp2, tmp1;
		  debug insn "j core_dash_e8b3f51d8f75870a__.LBB225_20";
		  jump core_dash_e8b3f51d8f75870a___dot_LBB225_20;
		core_dash_e8b3f51d8f75870a___dot_LBB225_41:
		  debug loc 95 0 17;
		  debug insn "mv x10, x26";
		  x10 <=X= x26;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6804:
		core_dash_e8b3f51d8f75870a___dot_LBB225_42:
		  debug loc 112 1435 52;
		  debug insn "sltu x23, x10, x26";
		  x23 <=Y= is_positive(x26 - x10);
		  debug insn "j core_dash_e8b3f51d8f75870a__.LBB225_20";
		  jump core_dash_e8b3f51d8f75870a___dot_LBB225_20;
		core_dash_e8b3f51d8f75870a___ZN4core3fmt9Formatter12pad_integral12write_prefix17hb01a11c664695966E:
		core_dash_e8b3f51d8f75870a___dot_Lfunc_begin226:
		  debug loc 95 1381 0;
		  debug insn "addi x2, x2, -32";
		  x2 <== wrap(x2 + 4294967264);
		  debug insn "sw x1, 28(x2)";
		  mstore x2 + 28, x1;
		  debug insn "sw x8, 24(x2)";
		  mstore x2 + 24, x8;
		  debug insn "sw x9, 20(x2)";
		  mstore x2 + 20, x9;
		  debug insn "sw x18, 16(x2)";
		  mstore x2 + 16, x18;
		  debug insn "sw x19, 12(x2)";
		  mstore x2 + 12, x19;
		  debug insn "lui x15, 272";
		  x15 <=X= 1114112;
		  debug insn "mv x18, x14";
		  x18 <=X= x14;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6806:
		  debug insn "mv x9, x13";
		  x9 <=X= x13;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6807:
		  debug insn "mv x8, x11";
		  x8 <=X= x11;
		  debug insn "mv x19, x10";
		  x19 <=X= x10;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6808:
		  debug loc 95 1382 20;
		  debug insn "beq x12, x15, core_dash_e8b3f51d8f75870a__.LBB226_2";
		  branch_if_zero x12 - x15, core_dash_e8b3f51d8f75870a___dot_LBB226_2;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6809:
		  debug loc 95 1383 17;
		  debug insn "lw x13, 16(x8)";
		  x13, tmp1 <== mload(x8 + 16);
		  debug insn "mv x10, x19";
		  x10 <=X= x19;
		  debug insn "mv x11, x12";
		  x11 <=X= x12;
		  debug insn "jalr x13";
		  jump_and_link_dyn x13;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6810:
		  debug insn "mv x11, x10";
		  x11 <=X= x10;
		  debug insn "li x10, 1";
		  x10 <=X= 1;
		  debug insn "bnez x11, core_dash_e8b3f51d8f75870a__.LBB226_5";
		  branch_if_nonzero x11, core_dash_e8b3f51d8f75870a___dot_LBB226_5;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6811:
		core_dash_e8b3f51d8f75870a___dot_LBB226_2:
		  debug loc 95 1385 20;
		  debug insn "beqz x9, core_dash_e8b3f51d8f75870a__.LBB226_4";
		  branch_if_zero x9, core_dash_e8b3f51d8f75870a___dot_LBB226_4;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6812:
		  debug loc 95 1385 44;
		  debug insn "lw x15, 12(x8)";
		  x15, tmp1 <== mload(x8 + 12);
		  debug insn "mv x10, x19";
		  x10 <=X= x19;
		  debug insn "mv x11, x9";
		  x11 <=X= x9;
		  debug insn "mv x12, x18";
		  x12 <=X= x18;
		  debug insn "lw x1, 28(x2)";
		  x1, tmp1 <== mload(x2 + 28);
		  debug insn "lw x8, 24(x2)";
		  x8, tmp1 <== mload(x2 + 24);
		  debug insn "lw x9, 20(x2)";
		  x9, tmp1 <== mload(x2 + 20);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6813:
		  debug insn "lw x18, 16(x2)";
		  x18, tmp1 <== mload(x2 + 16);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6814:
		  debug insn "lw x19, 12(x2)";
		  x19, tmp1 <== mload(x2 + 12);
		  debug insn "addi x2, x2, 32";
		  x2 <== wrap(x2 + 32);
		  debug insn "jr x15";
		  jump_dyn x15;
		core_dash_e8b3f51d8f75870a___dot_LBB226_4:
		  debug loc 95 0 44;
		  debug insn "li x10, 0";
		  x10 <=X= 0;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6816:
		core_dash_e8b3f51d8f75870a___dot_LBB226_5:
		  debug loc 95 1386 10;
		  debug insn "lw x1, 28(x2)";
		  x1, tmp1 <== mload(x2 + 28);
		  debug insn "lw x8, 24(x2)";
		  x8, tmp1 <== mload(x2 + 24);
		  debug insn "lw x9, 20(x2)";
		  x9, tmp1 <== mload(x2 + 20);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6817:
		  debug insn "lw x18, 16(x2)";
		  x18, tmp1 <== mload(x2 + 16);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6818:
		  debug insn "lw x19, 12(x2)";
		  x19, tmp1 <== mload(x2 + 12);
		  debug insn "addi x2, x2, 32";
		  x2 <== wrap(x2 + 32);
		  debug insn "ret ";
		  ret;
		_ZN4core3fmt9Formatter3pad17h10049b44c877e9aaE:
		core_dash_e8b3f51d8f75870a___dot_Lfunc_begin227:
		  debug loc 95 1454 0;
		  debug insn "addi x2, x2, -48";
		  x2 <== wrap(x2 + 4294967248);
		  debug insn "sw x1, 44(x2)";
		  mstore x2 + 44, x1;
		  debug insn "sw x8, 40(x2)";
		  mstore x2 + 40, x8;
		  debug insn "sw x9, 36(x2)";
		  mstore x2 + 36, x9;
		  debug insn "sw x18, 32(x2)";
		  mstore x2 + 32, x18;
		  debug insn "sw x19, 28(x2)";
		  mstore x2 + 28, x19;
		  debug insn "sw x20, 24(x2)";
		  mstore x2 + 24, x20;
		  debug insn "sw x21, 20(x2)";
		  mstore x2 + 20, x21;
		  debug insn "sw x22, 16(x2)";
		  mstore x2 + 16, x22;
		  debug insn "sw x23, 12(x2)";
		  mstore x2 + 12, x23;
		  debug insn "mv x20, x10";
		  x20 <=X= x10;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6820:
		  debug loc 111 598 18;
		  debug insn "lw x5, 8(x10)";
		  x5, tmp1 <== mload(x10 + 8);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6821:
		  debug loc 95 1456 12;
		  debug insn "lw x10, 16(x10)";
		  x10, tmp1 <== mload(x10 + 16);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6822:
		  debug loc 131 344 9;
		  debug insn "addi x13, x5, -1";
		  x13 <== wrap(x5 + 4294967295);
		  debug insn "snez x13, x13";
		  x13 <=Y= is_not_equal_zero(x13);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6823:
		  debug loc 95 1456 12;
		  debug insn "addi x14, x10, -1";
		  x14 <== wrap(x10 + 4294967295);
		  debug insn "snez x14, x14";
		  x14 <=Y= is_not_equal_zero(x14);
		  debug insn "and x13, x13, x14";
		  x13 <== and(x13, x14);
		  debug insn "mv x19, x12";
		  x19 <=X= x12;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6824:
		  debug loc 95 0 12;
		  debug insn "mv x18, x11";
		  x18 <=X= x11;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6825:
		  debug loc 95 1456 12;
		  debug insn "bnez x13, core_dash_e8b3f51d8f75870a__.LBB227_38";
		  branch_if_nonzero x13, core_dash_e8b3f51d8f75870a___dot_LBB227_38;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6826:
		  debug loc 95 0 12;
		  debug insn "li x11, 1";
		  x11 <=X= 1;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6827:
		  debug loc 95 1461 24;
		  debug insn "bne x10, x11, core_dash_e8b3f51d8f75870a__.LBB227_26";
		  branch_if_nonzero x10 - x11, core_dash_e8b3f51d8f75870a___dot_LBB227_26;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6828:
		  debug loc 95 1461 29;
		  debug insn "lw x10, 20(x20)";
		  x10, tmp1 <== mload(x20 + 20);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6829:
		  debug loc 95 0 29;
		  debug insn "li x11, 0";
		  x11 <=X= 0;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6830:
		  debug loc 106 485 18;
		  debug insn "add x13, x18, x19";
		  x13 <== wrap(x18 + x19);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6831:
		  debug loc 123 330 9;
		  debug insn "addi x14, x10, 1";
		  x14 <== wrap(x10 + 1);
		  debug insn "lui x6, 272";
		  x6 <=X= 1114112;
		  debug insn "li x17, 223";
		  x17 <=X= 223;
		  debug insn "li x16, 240";
		  x16 <=X= 240;
		  debug insn "mv x12, x18";
		  x12 <=X= x18;
		  debug insn "j core_dash_e8b3f51d8f75870a__.LBB227_5";
		  jump core_dash_e8b3f51d8f75870a___dot_LBB227_5;
		core_dash_e8b3f51d8f75870a___dot_LBB227_3:
		  debug loc 123 0 9;
		  debug insn "addi x10, x12, 1";
		  x10 <== wrap(x12 + 1);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6833:
		core_dash_e8b3f51d8f75870a___dot_LBB227_4:
		  debug loc 165 145 38;
		  debug insn "sub x11, x11, x12";
		  x11 <== wrap_signed(x11 - x12);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6834:
		  debug loc 165 145 17;
		  debug insn "add x11, x11, x10";
		  x11 <== wrap(x11 + x10);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6835:
		  debug loc 165 0 17;
		  debug insn "mv x12, x10";
		  x12 <=X= x10;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6836:
		  debug loc 111 1095 9;
		  debug insn "beq x8, x6, core_dash_e8b3f51d8f75870a__.LBB227_26";
		  branch_if_zero x8 - x6, core_dash_e8b3f51d8f75870a___dot_LBB227_26;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6837:
		core_dash_e8b3f51d8f75870a___dot_LBB227_5:
		  debug loc 112 1435 52;
		  debug insn "addi x14, x14, -1";
		  x14 <== wrap(x14 + 4294967295);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6838:
		  debug loc 104 621 12;
		  debug insn "beqz x14, core_dash_e8b3f51d8f75870a__.LBB227_14";
		  branch_if_zero x14, core_dash_e8b3f51d8f75870a___dot_LBB227_14;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6839:
		  debug loc 108 146 24;
		  debug insn "beq x12, x13, core_dash_e8b3f51d8f75870a__.LBB227_26";
		  branch_if_zero x12 - x13, core_dash_e8b3f51d8f75870a___dot_LBB227_26;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6840:
		  debug loc 170 38 13;
		  debug insn "lb x10, 0(x12)";
		  x10, tmp2 <== mload(x12 + 0);
		  x10 <== shr(x10, 8 * tmp2);
		  x10 <== sign_extend_byte(x10);
		  debug insn "andi x8, x10, 255";
		  x8 <== and(x10, 255);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6841:
		  debug loc 170 39 8;
		  debug insn "bgez x10, core_dash_e8b3f51d8f75870a__.LBB227_3";
		  tmp1 <== to_signed(x10);
		  branch_if_positive tmp1 + 1, core_dash_e8b3f51d8f75870a___dot_LBB227_3;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6842:
		  debug loc 170 49 22;
		  debug insn "lbu x10, 1(x12)";
		  x10, tmp2 <== mload(x12 + 1);
		  x10 <== shr(x10, 8 * tmp2);
		  x10 <== and(x10, 0xff);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6843:
		  debug loc 170 12 5;
		  debug insn "andi x15, x8, 31";
		  x15 <== and(x8, 31);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6844:
		  debug loc 170 18 17;
		  debug insn "andi x9, x10, 63";
		  x9 <== and(x10, 63);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6845:
		  debug loc 170 51 8;
		  debug insn "bgeu x17, x8, core_dash_e8b3f51d8f75870a__.LBB227_12";
		  branch_if_positive x17 - x8 + 1, core_dash_e8b3f51d8f75870a___dot_LBB227_12;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6846:
		  debug loc 170 56 26;
		  debug insn "lbu x10, 2(x12)";
		  x10, tmp2 <== mload(x12 + 2);
		  x10 <== shr(x10, 8 * tmp2);
		  x10 <== and(x10, 0xff);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6847:
		  debug loc 170 18 5;
		  debug insn "slli x9, x9, 6";
		  x9 <== wrap16(x9 * 64);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6848:
		  debug loc 170 18 17;
		  debug insn "andi x10, x10, 63";
		  x10 <== and(x10, 63);
		  debug loc 170 18 5;
		  debug insn "or x9, x9, x10";
		  x9 <== or(x9, x10);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6849:
		  debug loc 170 59 12;
		  debug insn "bltu x8, x16, core_dash_e8b3f51d8f75870a__.LBB227_13";
		  branch_if_positive x16 - x8, core_dash_e8b3f51d8f75870a___dot_LBB227_13;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6850:
		  debug loc 170 64 30;
		  debug insn "lbu x10, 3(x12)";
		  x10, tmp2 <== mload(x12 + 3);
		  x10 <== shr(x10, 8 * tmp2);
		  x10 <== and(x10, 0xff);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6851:
		  debug loc 170 65 18;
		  debug insn "slli x15, x15, 29";
		  tmp1 <== wrap16(x15 * 65536);
		  x15 <== wrap16(tmp1 * 8192);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6852:
		  debug insn "srli x15, x15, 11";
		  x15 <== shr(x15, 11);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6853:
		  debug loc 170 18 5;
		  debug insn "slli x9, x9, 6";
		  x9 <== wrap16(x9 * 64);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6854:
		  debug loc 170 18 17;
		  debug insn "andi x10, x10, 63";
		  x10 <== and(x10, 63);
		  debug loc 170 18 5;
		  debug insn "or x10, x10, x9";
		  x10 <== or(x10, x9);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6855:
		  debug loc 170 65 13;
		  debug insn "or x8, x10, x15";
		  x8 <== or(x10, x15);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6856:
		  debug loc 165 140 9;
		  debug insn "beq x8, x6, core_dash_e8b3f51d8f75870a__.LBB227_26";
		  branch_if_zero x8 - x6, core_dash_e8b3f51d8f75870a___dot_LBB227_26;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6857:
		  debug loc 165 0 9;
		  debug insn "addi x10, x12, 4";
		  x10 <== wrap(x12 + 4);
		  debug insn "j core_dash_e8b3f51d8f75870a__.LBB227_4";
		  jump core_dash_e8b3f51d8f75870a___dot_LBB227_4;
		core_dash_e8b3f51d8f75870a___dot_LBB227_12:
		  debug insn "addi x10, x12, 2";
		  x10 <== wrap(x12 + 2);
		  debug insn "slli x15, x15, 6";
		  x15 <== wrap16(x15 * 64);
		  debug insn "or x8, x15, x9";
		  x8 <== or(x15, x9);
		  debug insn "j core_dash_e8b3f51d8f75870a__.LBB227_4";
		  jump core_dash_e8b3f51d8f75870a___dot_LBB227_4;
		core_dash_e8b3f51d8f75870a___dot_LBB227_13:
		  debug insn "addi x10, x12, 3";
		  x10 <== wrap(x12 + 3);
		  debug insn "slli x15, x15, 12";
		  x15 <== wrap16(x15 * 4096);
		  debug insn "or x8, x9, x15";
		  x8 <== or(x9, x15);
		  debug insn "j core_dash_e8b3f51d8f75870a__.LBB227_4";
		  jump core_dash_e8b3f51d8f75870a___dot_LBB227_4;
		core_dash_e8b3f51d8f75870a___dot_LBB227_14:
		  debug loc 108 146 24;
		  debug insn "beq x12, x13, core_dash_e8b3f51d8f75870a__.LBB227_26";
		  branch_if_zero x12 - x13, core_dash_e8b3f51d8f75870a___dot_LBB227_26;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6861:
		  debug loc 170 38 13;
		  debug insn "lb x10, 0(x12)";
		  x10, tmp2 <== mload(x12 + 0);
		  x10 <== shr(x10, 8 * tmp2);
		  x10 <== sign_extend_byte(x10);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6862:
		  debug loc 170 39 8;
		  debug insn "bgez x10, core_dash_e8b3f51d8f75870a__.LBB227_19";
		  tmp1 <== to_signed(x10);
		  branch_if_positive tmp1 + 1, core_dash_e8b3f51d8f75870a___dot_LBB227_19;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6863:
		  debug loc 170 0 0;
		  debug insn "andi x10, x10, 255";
		  x10 <== and(x10, 255);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6864:
		  debug insn "li x13, 224";
		  x13 <=X= 224;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6865:
		  debug loc 170 51 8;
		  debug insn "bltu x10, x13, core_dash_e8b3f51d8f75870a__.LBB227_19";
		  branch_if_positive x13 - x10, core_dash_e8b3f51d8f75870a___dot_LBB227_19;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6866:
		  debug loc 170 0 8;
		  debug insn "li x13, 240";
		  x13 <=X= 240;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6867:
		  debug loc 170 59 12;
		  debug insn "bltu x10, x13, core_dash_e8b3f51d8f75870a__.LBB227_19";
		  branch_if_positive x13 - x10, core_dash_e8b3f51d8f75870a___dot_LBB227_19;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6868:
		  debug loc 170 0 0;
		  debug insn "lbu x13, 1(x12)";
		  x13, tmp2 <== mload(x12 + 1);
		  x13 <== shr(x13, 8 * tmp2);
		  x13 <== and(x13, 0xff);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6869:
		  debug loc 170 56 26;
		  debug insn "lbu x14, 2(x12)";
		  x14, tmp2 <== mload(x12 + 2);
		  x14 <== shr(x14, 8 * tmp2);
		  x14 <== and(x14, 0xff);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6870:
		  debug loc 170 18 17;
		  debug insn "andi x13, x13, 63";
		  x13 <== and(x13, 63);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6871:
		  debug loc 170 18 17;
		  debug insn "andi x14, x14, 63";
		  x14 <== and(x14, 63);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6872:
		  debug loc 170 64 30;
		  debug insn "lbu x12, 3(x12)";
		  x12, tmp2 <== mload(x12 + 3);
		  x12 <== shr(x12, 8 * tmp2);
		  x12 <== and(x12, 0xff);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6873:
		  debug loc 170 65 18;
		  debug insn "slli x10, x10, 29";
		  tmp1 <== wrap16(x10 * 65536);
		  x10 <== wrap16(tmp1 * 8192);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6874:
		  debug insn "srli x10, x10, 11";
		  x10 <== shr(x10, 11);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6875:
		  debug loc 170 18 5;
		  debug insn "slli x13, x13, 12";
		  x13 <== wrap16(x13 * 4096);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6876:
		  debug insn "slli x14, x14, 6";
		  x14 <== wrap16(x14 * 64);
		  debug insn "or x13, x13, x14";
		  x13 <== or(x13, x14);
		  debug loc 170 18 17;
		  debug insn "andi x12, x12, 63";
		  x12 <== and(x12, 63);
		  debug loc 170 18 5;
		  debug insn "or x12, x12, x13";
		  x12 <== or(x12, x13);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6877:
		  debug loc 170 65 13;
		  debug insn "or x10, x10, x12";
		  x10 <== or(x10, x12);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6878:
		  debug loc 170 0 13;
		  debug insn "lui x12, 272";
		  x12 <=X= 1114112;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6879:
		  debug loc 165 140 9;
		  debug insn "beq x10, x12, core_dash_e8b3f51d8f75870a__.LBB227_26";
		  branch_if_zero x10 - x12, core_dash_e8b3f51d8f75870a___dot_LBB227_26;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6880:
		core_dash_e8b3f51d8f75870a___dot_LBB227_19:
		  debug loc 167 216 12;
		  debug insn "beqz x11, core_dash_e8b3f51d8f75870a__.LBB227_24";
		  branch_if_zero x11, core_dash_e8b3f51d8f75870a___dot_LBB227_24;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6881:
		  debug loc 167 220 9;
		  debug insn "bgeu x11, x19, core_dash_e8b3f51d8f75870a__.LBB227_23";
		  branch_if_positive x11 - x19 + 1, core_dash_e8b3f51d8f75870a___dot_LBB227_23;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6882:
		  debug loc 105 219 12;
		  debug insn "add x10, x18, x11";
		  x10 <== wrap(x18 + x11);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6883:
		  debug loc 167 232 19;
		  debug insn "lb x10, 0(x10)";
		  x10, tmp2 <== mload(x10 + 0);
		  x10 <== shr(x10, 8 * tmp2);
		  x10 <== sign_extend_byte(x10);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6884:
		  debug loc 167 0 19;
		  debug insn "li x12, -64";
		  x12 <=X= 4294967232;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6885:
		  debug loc 174 259 9;
		  debug insn "bge x10, x12, core_dash_e8b3f51d8f75870a__.LBB227_24";
		  tmp1 <== to_signed(x10);
		  tmp2 <== to_signed(x12);
		  branch_if_positive tmp1 - tmp2 + 1, core_dash_e8b3f51d8f75870a___dot_LBB227_24;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6886:
		core_dash_e8b3f51d8f75870a___dot_LBB227_22:
		  debug loc 174 0 9;
		  debug insn "li x10, 0";
		  x10 <=X= 0;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6887:
		  debug loc 111 847 9;
		  debug insn "bnez x10, core_dash_e8b3f51d8f75870a__.LBB227_25";
		  branch_if_nonzero x10, core_dash_e8b3f51d8f75870a___dot_LBB227_25;
		  debug insn "j core_dash_e8b3f51d8f75870a__.LBB227_26";
		  jump core_dash_e8b3f51d8f75870a___dot_LBB227_26;
		core_dash_e8b3f51d8f75870a___dot_LBB227_23:
		  debug loc 174 259 9;
		  debug insn "bne x11, x19, core_dash_e8b3f51d8f75870a__.LBB227_22";
		  branch_if_nonzero x11 - x19, core_dash_e8b3f51d8f75870a___dot_LBB227_22;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6889:
		core_dash_e8b3f51d8f75870a___dot_LBB227_24:
		  debug loc 174 0 9;
		  debug insn "mv x10, x18";
		  x10 <=X= x18;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6890:
		  debug loc 111 847 9;
		  debug insn "beqz x10, core_dash_e8b3f51d8f75870a__.LBB227_26";
		  branch_if_zero x10, core_dash_e8b3f51d8f75870a___dot_LBB227_26;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6891:
		core_dash_e8b3f51d8f75870a___dot_LBB227_25:
		  debug loc 111 0 9;
		  debug insn "mv x19, x11";
		  x19 <=X= x11;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6892:
		  debug insn "mv x18, x10";
		  x18 <=X= x10;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6893:
		core_dash_e8b3f51d8f75870a___dot_LBB227_26:
		  debug loc 95 1478 9;
		  debug insn "beqz x5, core_dash_e8b3f51d8f75870a__.LBB227_38";
		  branch_if_zero x5, core_dash_e8b3f51d8f75870a___dot_LBB227_38;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6894:
		  debug loc 95 1482 18;
		  debug insn "lw x8, 12(x20)";
		  x8, tmp1 <== mload(x20 + 12);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6895:
		  debug loc 95 0 18;
		  debug insn "li x10, 16";
		  x10 <=X= 16;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6896:
		  debug loc 169 27 8;
		  debug insn "bgeu x19, x10, core_dash_e8b3f51d8f75870a__.LBB227_37";
		  branch_if_positive x19 - x10 + 1, core_dash_e8b3f51d8f75870a___dot_LBB227_37;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6897:
		  debug loc 169 0 8;
		  debug insn "li x10, 0";
		  x10 <=X= 0;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6898:
		  debug loc 108 146 24;
		  debug insn "beqz x19, core_dash_e8b3f51d8f75870a__.LBB227_31";
		  branch_if_zero x19, core_dash_e8b3f51d8f75870a___dot_LBB227_31;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6899:
		  debug loc 108 0 24;
		  debug insn "mv x11, x19";
		  x11 <=X= x19;
		  debug insn "mv x12, x18";
		  x12 <=X= x18;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6900:
		core_dash_e8b3f51d8f75870a___dot_LBB227_30:
		  debug loc 123 2421 21;
		  debug insn "lb x13, 0(x12)";
		  x13, tmp2 <== mload(x12 + 0);
		  x13 <== shr(x13, 8 * tmp2);
		  x13 <== sign_extend_byte(x13);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6901:
		  debug loc 116 499 18;
		  debug insn "addi x12, x12, 1";
		  x12 <== wrap(x12 + 1);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6902:
		  debug loc 170 25 5;
		  debug insn "slti x13, x13, -64";
		  tmp1 <== to_signed(x13);
		  x13 <=Y= is_positive(-64 - tmp1);
		  debug insn "xori x13, x13, 1";
		  x13 <== xor(x13, 1);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6903:
		  debug loc 108 146 24;
		  debug insn "addi x11, x11, -1";
		  x11 <== wrap(x11 + 4294967295);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6904:
		  debug loc 171 53 28;
		  debug insn "add x10, x10, x13";
		  x10 <== wrap(x10 + x13);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6905:
		  debug loc 108 146 24;
		  debug insn "bnez x11, core_dash_e8b3f51d8f75870a__.LBB227_30";
		  branch_if_nonzero x11, core_dash_e8b3f51d8f75870a___dot_LBB227_30;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6906:
		core_dash_e8b3f51d8f75870a___dot_LBB227_31:
		  debug loc 95 1486 20;
		  debug insn "bgeu x10, x8, core_dash_e8b3f51d8f75870a__.LBB227_38";
		  branch_if_positive x10 - x8 + 1, core_dash_e8b3f51d8f75870a___dot_LBB227_38;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6907:
		core_dash_e8b3f51d8f75870a___dot_LBB227_32:
		  debug loc 95 1509 27;
		  debug insn "lbu x11, 32(x20)";
		  x11, tmp2 <== mload(x20 + 32);
		  x11 <== shr(x11, 8 * tmp2);
		  x11 <== and(x11, 0xff);
		  debug insn "li x13, 3";
		  x13 <=X= 3;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6908:
		  debug loc 95 0 27;
		  debug insn "li x12, 0";
		  x12 <=X= 0;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6909:
		  debug loc 95 1514 35;
		  debug insn "beq x11, x13, core_dash_e8b3f51d8f75870a__.LBB227_34";
		  branch_if_zero x11 - x13, core_dash_e8b3f51d8f75870a___dot_LBB227_34;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6910:
		  debug loc 95 0 35;
		  debug insn "mv x12, x11";
		  x12 <=X= x11;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6911:
		core_dash_e8b3f51d8f75870a___dot_LBB227_34:
		  debug loc 95 1514 35;
		  debug insn "andi x11, x12, 3";
		  x11 <== and(x12, 3);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6912:
		  debug loc 95 0 0;
		  debug insn "sub x10, x8, x10";
		  x10 <== wrap_signed(x8 - x10);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6913:
		  debug loc 95 1514 35;
		  debug insn "beqz x11, core_dash_e8b3f51d8f75870a__.LBB227_39";
		  branch_if_zero x11, core_dash_e8b3f51d8f75870a___dot_LBB227_39;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6914:
		  debug loc 95 0 35;
		  debug insn "li x12, 1";
		  x12 <=X= 1;
		  debug loc 95 1514 35;
		  debug insn "bne x11, x12, core_dash_e8b3f51d8f75870a__.LBB227_40";
		  branch_if_nonzero x11 - x12, core_dash_e8b3f51d8f75870a___dot_LBB227_40;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6915:
		  debug loc 95 0 35;
		  debug insn "li x21, 0";
		  x21 <=X= 0;
		  debug insn "j core_dash_e8b3f51d8f75870a__.LBB227_41";
		  jump core_dash_e8b3f51d8f75870a___dot_LBB227_41;
		core_dash_e8b3f51d8f75870a___dot_LBB227_37:
		  debug loc 169 34 9;
		  debug insn "mv x10, x18";
		  x10 <=X= x18;
		  debug insn "mv x11, x19";
		  x11 <=X= x19;
		  debug insn "call _ZN4core3str5count14do_count_chars17h97c55cd17370d3d0E";
		  call _ZN4core3str5count14do_count_chars17h97c55cd17370d3d0E;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6917:
		  debug loc 95 1486 20;
		  debug insn "bltu x10, x8, core_dash_e8b3f51d8f75870a__.LBB227_32";
		  branch_if_positive x8 - x10, core_dash_e8b3f51d8f75870a___dot_LBB227_32;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6918:
		core_dash_e8b3f51d8f75870a___dot_LBB227_38:
		  debug loc 95 0 0;
		  debug insn "lw x11, 4(x20)";
		  x11, tmp1 <== mload(x20 + 4);
		  debug insn "lw x10, 0(x20)";
		  x10, tmp1 <== mload(x20 + 0);
		  debug insn "lw x15, 12(x11)";
		  x15, tmp1 <== mload(x11 + 12);
		  debug insn "mv x11, x18";
		  x11 <=X= x18;
		  debug insn "mv x12, x19";
		  x12 <=X= x19;
		  debug insn "lw x1, 44(x2)";
		  x1, tmp1 <== mload(x2 + 44);
		  debug insn "lw x8, 40(x2)";
		  x8, tmp1 <== mload(x2 + 40);
		  debug insn "lw x9, 36(x2)";
		  x9, tmp1 <== mload(x2 + 36);
		  debug insn "lw x18, 32(x2)";
		  x18, tmp1 <== mload(x2 + 32);
		  debug insn "lw x19, 28(x2)";
		  x19, tmp1 <== mload(x2 + 28);
		  debug insn "lw x20, 24(x2)";
		  x20, tmp1 <== mload(x2 + 24);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6919:
		  debug insn "lw x21, 20(x2)";
		  x21, tmp1 <== mload(x2 + 20);
		  debug insn "lw x22, 16(x2)";
		  x22, tmp1 <== mload(x2 + 16);
		  debug insn "lw x23, 12(x2)";
		  x23, tmp1 <== mload(x2 + 12);
		  debug insn "addi x2, x2, 48";
		  x2 <== wrap(x2 + 48);
		  debug insn "jr x15";
		  jump_dyn x15;
		core_dash_e8b3f51d8f75870a___dot_LBB227_39:
		core_dash_e8b3f51d8f75870a___dot_Ltmp6920:
		  debug insn "mv x21, x10";
		  x21 <=X= x10;
		  debug insn "mv x10, x11";
		  x10 <=X= x11;
		  debug insn "j core_dash_e8b3f51d8f75870a__.LBB227_41";
		  jump core_dash_e8b3f51d8f75870a___dot_LBB227_41;
		core_dash_e8b3f51d8f75870a___dot_LBB227_40:
		  debug loc 95 1517 56;
		  debug insn "addi x11, x10, 1";
		  x11 <== wrap(x10 + 1);
		  debug loc 95 1517 43;
		  debug insn "srli x10, x10, 1";
		  x10 <== shr(x10, 1);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6922:
		  debug loc 95 1517 56;
		  debug insn "srli x21, x11, 1";
		  x21 <== shr(x11, 1);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6923:
		core_dash_e8b3f51d8f75870a___dot_LBB227_41:
		  debug loc 95 0 56;
		  debug insn "lw x22, 0(x20)";
		  x22, tmp1 <== mload(x20 + 0);
		  debug insn "lw x23, 4(x20)";
		  x23, tmp1 <== mload(x20 + 4);
		  debug insn "lw x9, 28(x20)";
		  x9, tmp1 <== mload(x20 + 28);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6924:
		  debug loc 95 1520 9;
		  debug insn "addi x8, x10, 1";
		  x8 <== wrap(x10 + 1);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6925:
		core_dash_e8b3f51d8f75870a___dot_LBB227_42:
		  debug loc 112 1435 52;
		  debug insn "addi x8, x8, -1";
		  x8 <== wrap(x8 + 4294967295);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6926:
		  debug loc 104 621 12;
		  debug insn "beqz x8, core_dash_e8b3f51d8f75870a__.LBB227_45";
		  branch_if_zero x8, core_dash_e8b3f51d8f75870a___dot_LBB227_45;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6927:
		  debug loc 95 1521 13;
		  debug insn "lw x12, 16(x23)";
		  x12, tmp1 <== mload(x23 + 16);
		  debug insn "mv x10, x22";
		  x10 <=X= x22;
		  debug insn "mv x11, x9";
		  x11 <=X= x9;
		  debug insn "jalr x12";
		  jump_and_link_dyn x12;
		  debug insn "beqz x10, core_dash_e8b3f51d8f75870a__.LBB227_42";
		  branch_if_zero x10, core_dash_e8b3f51d8f75870a___dot_LBB227_42;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6928:
		  debug loc 95 0 13;
		  debug insn "li x20, 1";
		  x20 <=X= 1;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6929:
		  debug insn "j core_dash_e8b3f51d8f75870a__.LBB227_53";
		  jump core_dash_e8b3f51d8f75870a___dot_LBB227_53;
		core_dash_e8b3f51d8f75870a___dot_LBB227_45:
		  debug insn "lui x10, 272";
		  x10 <=X= 1114112;
		  debug insn "li x20, 1";
		  x20 <=X= 1;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6931:
		  debug loc 95 1493 40;
		  debug insn "beq x9, x10, core_dash_e8b3f51d8f75870a__.LBB227_53";
		  branch_if_zero x9 - x10, core_dash_e8b3f51d8f75870a___dot_LBB227_53;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6932:
		  debug loc 95 1494 21;
		  debug insn "lw x13, 12(x23)";
		  x13, tmp1 <== mload(x23 + 12);
		  debug insn "mv x10, x22";
		  x10 <=X= x22;
		  debug insn "mv x11, x18";
		  x11 <=X= x18;
		  debug insn "mv x12, x19";
		  x12 <=X= x19;
		  debug insn "jalr x13";
		  jump_and_link_dyn x13;
		  debug insn "bnez x10, core_dash_e8b3f51d8f75870a__.LBB227_53";
		  branch_if_nonzero x10, core_dash_e8b3f51d8f75870a___dot_LBB227_53;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6933:
		  debug loc 95 0 21;
		  debug insn "li x8, 0";
		  x8 <=X= 0;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6934:
		core_dash_e8b3f51d8f75870a___dot_LBB227_48:
		  debug loc 104 621 12;
		  debug insn "beq x21, x8, core_dash_e8b3f51d8f75870a__.LBB227_51";
		  branch_if_zero x21 - x8, core_dash_e8b3f51d8f75870a___dot_LBB227_51;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6935:
		  debug loc 95 1287 13;
		  debug insn "lw x12, 16(x23)";
		  x12, tmp1 <== mload(x23 + 16);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6936:
		  debug loc 114 470 22;
		  debug insn "addi x8, x8, 1";
		  x8 <== wrap(x8 + 1);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6937:
		  debug loc 95 1287 13;
		  debug insn "mv x10, x22";
		  x10 <=X= x22;
		  debug insn "mv x11, x9";
		  x11 <=X= x9;
		  debug insn "jalr x12";
		  jump_and_link_dyn x12;
		  debug insn "beqz x10, core_dash_e8b3f51d8f75870a__.LBB227_48";
		  branch_if_zero x10, core_dash_e8b3f51d8f75870a___dot_LBB227_48;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6938:
		  debug insn "addi x10, x8, -1";
		  x10 <== wrap(x8 + 4294967295);
		  debug insn "j core_dash_e8b3f51d8f75870a__.LBB227_52";
		  jump core_dash_e8b3f51d8f75870a___dot_LBB227_52;
		core_dash_e8b3f51d8f75870a___dot_LBB227_51:
		  debug loc 95 0 13;
		  debug insn "mv x10, x21";
		  x10 <=X= x21;
		core_dash_e8b3f51d8f75870a___dot_Ltmp6940:
		core_dash_e8b3f51d8f75870a___dot_LBB227_52:
		  debug loc 112 1435 52;
		  debug insn "sltu x20, x10, x21";
		  x20 <=Y= is_positive(x21 - x10);
		core_dash_e8b3f51d8f75870a___dot_Ltmp6941:
		core_dash_e8b3f51d8f75870a___dot_LBB227_53:
		  debug loc 95 1499 6;
		  debug insn "mv x10, x20";
		  x10 <=X= x20;
		  debug insn "lw x1, 44(x2)";
		  x1, tmp1 <== mload(x2 + 44);
		  debug insn "lw x8, 40(x2)";
		  x8, tmp1 <== mload(x2 + 40);
		  debug insn "lw x9, 36(x2)";
		  x9, tmp1 <== mload(x2 + 36);
		  debug insn "lw x18, 32(x2)";
		  x18, tmp1 <== mload(x2 + 32);
		  debug insn "lw x19, 28(x2)";
		  x19, tmp1 <== mload(x2 + 28);
		  debug insn "lw x20, 24(x2)";
		  x20, tmp1 <== mload(x2 + 24);
		  debug insn "lw x21, 20(x2)";
		  x21, tmp1 <== mload(x2 + 20);
		  debug insn "lw x22, 16(x2)";
		  x22, tmp1 <== mload(x2 + 16);
		  debug insn "lw x23, 12(x2)";
		  x23, tmp1 <== mload(x2 + 12);
		  debug insn "addi x2, x2, 48";
		  x2 <== wrap(x2 + 48);
		  debug insn "ret ";
		  ret;
		_ZN4core3str5count14do_count_chars17h97c55cd17370d3d0E:
		core_dash_e8b3f51d8f75870a___dot_Lfunc_begin296:
		  debug loc 169 38 0;
		  debug insn "mv x12, x10";
		  x12 <=X= x10;
		core_dash_e8b3f51d8f75870a___dot_Ltmp8463:
		  debug loc 99 1718 35;
		  debug insn "addi x10, x10, 3";
		  x10 <== wrap(x10 + 3);
		  debug insn "andi x15, x10, -4";
		  x15 <== and(x10, 4294967292);
		core_dash_e8b3f51d8f75870a___dot_Ltmp8464:
		  debug loc 99 1719 31;
		  debug insn "sub x17, x15, x12";
		  x17 <== wrap_signed(x15 - x12);
		core_dash_e8b3f51d8f75870a___dot_Ltmp8465:
		  debug loc 113 3523 12;
		  debug insn "bltu x11, x17, core_dash_e8b3f51d8f75870a__.LBB296_2";
		  branch_if_positive x17 - x11, core_dash_e8b3f51d8f75870a___dot_LBB296_2;
		core_dash_e8b3f51d8f75870a___dot_Ltmp8466:
		  debug loc 113 1660 74;
		  debug insn "sub x16, x11, x17";
		  x16 <== wrap_signed(x11 - x17);
		core_dash_e8b3f51d8f75870a___dot_Ltmp8467:
		  debug loc 169 67 17;
		  debug insn "sltiu x10, x16, 4";
		  x10 <=Y= is_positive(4 - x16);
		  debug insn "sltiu x13, x17, 5";
		  x13 <=Y= is_positive(5 - x17);
		  debug insn "xori x13, x13, 1";
		  x13 <== xor(x13, 1);
		  debug insn "or x10, x10, x13";
		  x10 <== or(x10, x13);
		  debug insn "beqz x10, core_dash_e8b3f51d8f75870a__.LBB296_5";
		  branch_if_zero x10, core_dash_e8b3f51d8f75870a___dot_LBB296_5;
		core_dash_e8b3f51d8f75870a___dot_Ltmp8468:
		core_dash_e8b3f51d8f75870a___dot_LBB296_2:
		  debug loc 169 0 17;
		  debug insn "li x10, 0";
		  x10 <=X= 0;
		core_dash_e8b3f51d8f75870a___dot_Ltmp8469:
		  debug loc 108 146 24;
		  debug insn "beqz x11, core_dash_e8b3f51d8f75870a__.LBB296_4";
		  branch_if_zero x11, core_dash_e8b3f51d8f75870a___dot_LBB296_4;
		core_dash_e8b3f51d8f75870a___dot_Ltmp8470:
		core_dash_e8b3f51d8f75870a___dot_LBB296_3:
		  debug loc 123 2421 21;
		  debug insn "lb x13, 0(x12)";
		  x13, tmp2 <== mload(x12 + 0);
		  x13 <== shr(x13, 8 * tmp2);
		  x13 <== sign_extend_byte(x13);
		core_dash_e8b3f51d8f75870a___dot_Ltmp8471:
		  debug loc 116 499 18;
		  debug insn "addi x12, x12, 1";
		  x12 <== wrap(x12 + 1);
		core_dash_e8b3f51d8f75870a___dot_Ltmp8472:
		  debug loc 170 25 5;
		  debug insn "slti x13, x13, -64";
		  tmp1 <== to_signed(x13);
		  x13 <=Y= is_positive(-64 - tmp1);
		  debug insn "xori x13, x13, 1";
		  x13 <== xor(x13, 1);
		core_dash_e8b3f51d8f75870a___dot_Ltmp8473:
		  debug loc 108 146 24;
		  debug insn "addi x11, x11, -1";
		  x11 <== wrap(x11 + 4294967295);
		core_dash_e8b3f51d8f75870a___dot_Ltmp8474:
		  debug loc 171 53 28;
		  debug insn "add x10, x10, x13";
		  x10 <== wrap(x10 + x13);
		core_dash_e8b3f51d8f75870a___dot_Ltmp8475:
		  debug loc 108 146 24;
		  debug insn "bnez x11, core_dash_e8b3f51d8f75870a__.LBB296_3";
		  branch_if_nonzero x11, core_dash_e8b3f51d8f75870a___dot_LBB296_3;
		core_dash_e8b3f51d8f75870a___dot_Ltmp8476:
		core_dash_e8b3f51d8f75870a___dot_LBB296_4:
		  debug loc 169 107 2;
		  debug insn "ret ";
		  ret;
		core_dash_e8b3f51d8f75870a___dot_LBB296_5:
		core_dash_e8b3f51d8f75870a___dot_Ltmp8477:
		  debug loc 169 0 0;
		  debug insn "andi x13, x16, 3";
		  x13 <== and(x16, 3);
		core_dash_e8b3f51d8f75870a___dot_Ltmp8478:
		  debug insn "li x11, 0";
		  x11 <=X= 0;
		core_dash_e8b3f51d8f75870a___dot_Ltmp8479:
		  debug loc 108 146 24;
		  debug insn "beq x15, x12, core_dash_e8b3f51d8f75870a__.LBB296_8";
		  branch_if_zero x15 - x12, core_dash_e8b3f51d8f75870a___dot_LBB296_8;
		core_dash_e8b3f51d8f75870a___dot_Ltmp8480:
		  debug insn "sub x15, x12, x15";
		  x15 <== wrap_signed(x12 - x15);
		  debug insn "mv x10, x12";
		  x10 <=X= x12;
		core_dash_e8b3f51d8f75870a___dot_Ltmp8481:
		core_dash_e8b3f51d8f75870a___dot_LBB296_7:
		  debug loc 123 2421 21;
		  debug insn "lb x14, 0(x10)";
		  x14, tmp2 <== mload(x10 + 0);
		  x14 <== shr(x14, 8 * tmp2);
		  x14 <== sign_extend_byte(x14);
		core_dash_e8b3f51d8f75870a___dot_Ltmp8482:
		  debug loc 116 499 18;
		  debug insn "addi x10, x10, 1";
		  x10 <== wrap(x10 + 1);
		core_dash_e8b3f51d8f75870a___dot_Ltmp8483:
		  debug loc 170 25 5;
		  debug insn "slti x14, x14, -64";
		  tmp1 <== to_signed(x14);
		  x14 <=Y= is_positive(-64 - tmp1);
		  debug insn "xori x14, x14, 1";
		  x14 <== xor(x14, 1);
		core_dash_e8b3f51d8f75870a___dot_Ltmp8484:
		  debug loc 108 146 24;
		  debug insn "addi x15, x15, 1";
		  x15 <== wrap(x15 + 1);
		core_dash_e8b3f51d8f75870a___dot_Ltmp8485:
		  debug loc 171 53 28;
		  debug insn "add x11, x11, x14";
		  x11 <== wrap(x11 + x14);
		core_dash_e8b3f51d8f75870a___dot_Ltmp8486:
		  debug loc 108 146 24;
		  debug insn "bnez x15, core_dash_e8b3f51d8f75870a__.LBB296_7";
		  branch_if_nonzero x15, core_dash_e8b3f51d8f75870a___dot_LBB296_7;
		core_dash_e8b3f51d8f75870a___dot_Ltmp8487:
		core_dash_e8b3f51d8f75870a___dot_LBB296_8:
		  debug loc 169 0 0;
		  debug insn "add x5, x12, x17";
		  x5 <== wrap(x12 + x17);
		core_dash_e8b3f51d8f75870a___dot_Ltmp8488:
		  debug insn "li x12, 0";
		  x12 <=X= 0;
		core_dash_e8b3f51d8f75870a___dot_Ltmp8489:
		  debug loc 108 146 24;
		  debug insn "beqz x13, core_dash_e8b3f51d8f75870a__.LBB296_11";
		  branch_if_zero x13, core_dash_e8b3f51d8f75870a___dot_LBB296_11;
		core_dash_e8b3f51d8f75870a___dot_Ltmp8490:
		  debug loc 108 0 24;
		  debug insn "andi x10, x16, -4";
		  x10 <== and(x16, 4294967292);
		core_dash_e8b3f51d8f75870a___dot_Ltmp8491:
		  debug insn "add x14, x5, x10";
		  x14 <== wrap(x5 + x10);
		core_dash_e8b3f51d8f75870a___dot_Ltmp8492:
		core_dash_e8b3f51d8f75870a___dot_LBB296_10:
		  debug loc 123 2421 21;
		  debug insn "lb x10, 0(x14)";
		  x10, tmp2 <== mload(x14 + 0);
		  x10 <== shr(x10, 8 * tmp2);
		  x10 <== sign_extend_byte(x10);
		core_dash_e8b3f51d8f75870a___dot_Ltmp8493:
		  debug loc 116 499 18;
		  debug insn "addi x14, x14, 1";
		  x14 <== wrap(x14 + 1);
		core_dash_e8b3f51d8f75870a___dot_Ltmp8494:
		  debug loc 170 25 5;
		  debug insn "slti x10, x10, -64";
		  tmp1 <== to_signed(x10);
		  x10 <=Y= is_positive(-64 - tmp1);
		  debug insn "xori x10, x10, 1";
		  x10 <== xor(x10, 1);
		core_dash_e8b3f51d8f75870a___dot_Ltmp8495:
		  debug loc 108 146 24;
		  debug insn "addi x13, x13, -1";
		  x13 <== wrap(x13 + 4294967295);
		core_dash_e8b3f51d8f75870a___dot_Ltmp8496:
		  debug loc 171 53 28;
		  debug insn "add x12, x12, x10";
		  x12 <== wrap(x12 + x10);
		core_dash_e8b3f51d8f75870a___dot_Ltmp8497:
		  debug loc 108 146 24;
		  debug insn "bnez x13, core_dash_e8b3f51d8f75870a__.LBB296_10";
		  branch_if_nonzero x13, core_dash_e8b3f51d8f75870a___dot_LBB296_10;
		core_dash_e8b3f51d8f75870a___dot_Ltmp8498:
		core_dash_e8b3f51d8f75870a___dot_LBB296_11:
		  debug loc 108 0 24;
		  debug insn "srli x14, x16, 2";
		  x14 <== shr(x16, 2);
		core_dash_e8b3f51d8f75870a___dot_Ltmp8499:
		  debug insn "lui x10, 4112";
		  x10 <=X= 16842752;
		  debug insn "addi x30, x10, 257";
		  x30 <== wrap(x10 + 257);
		  debug insn "lui x10, 4080";
		  x10 <=X= 16711680;
		  debug insn "addi x17, x10, 255";
		  x17 <== wrap(x10 + 255);
		core_dash_e8b3f51d8f75870a___dot_Ltmp8500:
		  debug insn "lui x10, 16";
		  x10 <=X= 65536;
		  debug insn "addi x16, x10, 1";
		  x16 <== wrap(x10 + 1);
		  debug loc 169 71 21;
		  debug insn "add x10, x12, x11";
		  x10 <== wrap(x12 + x11);
		  debug insn "j core_dash_e8b3f51d8f75870a__.LBB296_13";
		  jump core_dash_e8b3f51d8f75870a___dot_LBB296_13;
		core_dash_e8b3f51d8f75870a___dot_LBB296_12:
		  debug loc 169 0 0;
		  debug insn "slli x12, x28, 2";
		  x12 <== wrap16(x28 * 4);
		  debug insn "add x5, x6, x12";
		  x5 <== wrap(x6 + x12);
		core_dash_e8b3f51d8f75870a___dot_Ltmp8502:
		  debug insn "sub x14, x7, x28";
		  x14 <== wrap_signed(x7 - x28);
		core_dash_e8b3f51d8f75870a___dot_Ltmp8503:
		  debug insn "andi x12, x28, 3";
		  x12 <== and(x28, 3);
		core_dash_e8b3f51d8f75870a___dot_Ltmp8504:
		  debug loc 169 126 27;
		  debug insn "and x13, x11, x17";
		  x13 <== and(x11, x17);
		  debug loc 169 126 52;
		  debug insn "srli x11, x11, 8";
		  x11 <== shr(x11, 8);
		core_dash_e8b3f51d8f75870a___dot_Ltmp8505:
		  debug loc 169 126 51;
		  debug insn "and x11, x11, x17";
		  x11 <== and(x11, x17);
		  debug loc 169 126 27;
		  debug insn "add x11, x11, x13";
		  x11 <== wrap(x11 + x13);
		core_dash_e8b3f51d8f75870a___dot_Ltmp8506:
		  debug loc 114 1226 13;
		  debug insn "mul x11, x11, x16";
		  x11, tmp1 <== mul(x11, x16);
		core_dash_e8b3f51d8f75870a___dot_Ltmp8507:
		  debug loc 169 127 5;
		  debug insn "srli x11, x11, 16";
		  x11 <== shr(x11, 16);
		core_dash_e8b3f51d8f75870a___dot_Ltmp8508:
		  debug loc 169 90 9;
		  debug insn "add x10, x10, x11";
		  x10 <== wrap(x10 + x11);
		core_dash_e8b3f51d8f75870a___dot_Ltmp8509:
		  debug loc 169 0 9;
		  debug insn "bnez x12, core_dash_e8b3f51d8f75870a__.LBB296_20";
		  branch_if_nonzero x12, core_dash_e8b3f51d8f75870a___dot_LBB296_20;
		core_dash_e8b3f51d8f75870a___dot_Ltmp8510:
		core_dash_e8b3f51d8f75870a___dot_LBB296_13:
		  debug insn "beqz x14, core_dash_e8b3f51d8f75870a__.LBB296_4";
		  branch_if_zero x14, core_dash_e8b3f51d8f75870a___dot_LBB296_4;
		core_dash_e8b3f51d8f75870a___dot_Ltmp8511:
		  debug insn "mv x7, x14";
		  x7 <=X= x14;
		core_dash_e8b3f51d8f75870a___dot_Ltmp8512:
		  debug insn "mv x6, x5";
		  x6 <=X= x5;
		core_dash_e8b3f51d8f75870a___dot_Ltmp8513:
		  debug insn "li x11, 192";
		  x11 <=X= 192;
		  debug insn "mv x28, x14";
		  x28 <=X= x14;
		core_dash_e8b3f51d8f75870a___dot_Ltmp8514:
		  debug insn "bltu x14, x11, core_dash_e8b3f51d8f75870a__.LBB296_16";
		  branch_if_positive x11 - x14, core_dash_e8b3f51d8f75870a___dot_LBB296_16;
		core_dash_e8b3f51d8f75870a___dot_Ltmp8515:
		  debug insn "li x28, 192";
		  x28 <=X= 192;
		core_dash_e8b3f51d8f75870a___dot_Ltmp8516:
		core_dash_e8b3f51d8f75870a___dot_LBB296_16:
		  debug loc 113 1022 56;
		  debug insn "andi x11, x28, 252";
		  x11 <== and(x28, 252);
		core_dash_e8b3f51d8f75870a___dot_Ltmp8517:
		  debug loc 106 485 18;
		  debug insn "slli x12, x11, 2";
		  x12 <== wrap16(x11 * 4);
		  debug insn "add x29, x6, x12";
		  x29 <== wrap(x6 + x12);
		  debug insn "beqz x11, core_dash_e8b3f51d8f75870a__.LBB296_12";
		  branch_if_zero x11, core_dash_e8b3f51d8f75870a___dot_LBB296_12;
		core_dash_e8b3f51d8f75870a___dot_Ltmp8518:
		  debug loc 106 0 18;
		  debug insn "li x11, 0";
		  x11 <=X= 0;
		core_dash_e8b3f51d8f75870a___dot_Ltmp8519:
		  debug insn "mv x12, x6";
		  x12 <=X= x6;
		core_dash_e8b3f51d8f75870a___dot_Ltmp8520:
		core_dash_e8b3f51d8f75870a___dot_LBB296_18:
		  debug insn "beqz x12, core_dash_e8b3f51d8f75870a__.LBB296_12";
		  branch_if_zero x12, core_dash_e8b3f51d8f75870a___dot_LBB296_12;
		core_dash_e8b3f51d8f75870a___dot_Ltmp8521:
		  debug loc 169 81 18;
		  debug insn "lw x14, 0(x12)";
		  x14, tmp1 <== mload(x12 + 0);
		core_dash_e8b3f51d8f75870a___dot_Ltmp8522:
		  debug loc 169 116 7;
		  debug insn "not x15, x14";
		  x15 <== wrap_signed(-x14 - 1);
		  debug loc 169 116 6;
		  debug insn "srli x15, x15, 7";
		  x15 <== shr(x15, 7);
		  debug loc 169 116 18;
		  debug insn "srli x14, x14, 6";
		  x14 <== shr(x14, 6);
		core_dash_e8b3f51d8f75870a___dot_Ltmp8523:
		  debug loc 169 81 18;
		  debug insn "lw x13, 4(x12)";
		  x13, tmp1 <== mload(x12 + 4);
		core_dash_e8b3f51d8f75870a___dot_Ltmp8524:
		  debug loc 169 116 5;
		  debug insn "or x14, x14, x15";
		  x14 <== or(x14, x15);
		  debug insn "and x14, x14, x30";
		  x14 <== and(x14, x30);
		core_dash_e8b3f51d8f75870a___dot_Ltmp8525:
		  debug loc 169 84 17;
		  debug insn "add x11, x11, x14";
		  x11 <== wrap(x11 + x14);
		core_dash_e8b3f51d8f75870a___dot_Ltmp8526:
		  debug loc 169 116 7;
		  debug insn "not x14, x13";
		  x14 <== wrap_signed(-x13 - 1);
		  debug loc 169 116 6;
		  debug insn "srli x14, x14, 7";
		  x14 <== shr(x14, 7);
		  debug loc 169 116 18;
		  debug insn "srli x13, x13, 6";
		  x13 <== shr(x13, 6);
		core_dash_e8b3f51d8f75870a___dot_Ltmp8527:
		  debug loc 169 81 18;
		  debug insn "lw x15, 8(x12)";
		  x15, tmp1 <== mload(x12 + 8);
		core_dash_e8b3f51d8f75870a___dot_Ltmp8528:
		  debug loc 169 116 5;
		  debug insn "or x13, x13, x14";
		  x13 <== or(x13, x14);
		  debug insn "and x13, x13, x30";
		  x13 <== and(x13, x30);
		core_dash_e8b3f51d8f75870a___dot_Ltmp8529:
		  debug loc 169 84 17;
		  debug insn "add x11, x11, x13";
		  x11 <== wrap(x11 + x13);
		core_dash_e8b3f51d8f75870a___dot_Ltmp8530:
		  debug loc 169 116 7;
		  debug insn "not x13, x15";
		  x13 <== wrap_signed(-x15 - 1);
		  debug loc 169 116 6;
		  debug insn "srli x13, x13, 7";
		  x13 <== shr(x13, 7);
		  debug loc 169 116 18;
		  debug insn "srli x14, x15, 6";
		  x14 <== shr(x15, 6);
		core_dash_e8b3f51d8f75870a___dot_Ltmp8531:
		  debug loc 169 81 18;
		  debug insn "lw x15, 12(x12)";
		  x15, tmp1 <== mload(x12 + 12);
		core_dash_e8b3f51d8f75870a___dot_Ltmp8532:
		  debug loc 169 116 5;
		  debug insn "or x13, x13, x14";
		  x13 <== or(x13, x14);
		  debug insn "and x13, x13, x30";
		  x13 <== and(x13, x30);
		core_dash_e8b3f51d8f75870a___dot_Ltmp8533:
		  debug loc 169 84 17;
		  debug insn "add x11, x11, x13";
		  x11 <== wrap(x11 + x13);
		core_dash_e8b3f51d8f75870a___dot_Ltmp8534:
		  debug loc 169 116 7;
		  debug insn "not x13, x15";
		  x13 <== wrap_signed(-x15 - 1);
		  debug loc 169 116 6;
		  debug insn "srli x13, x13, 7";
		  x13 <== shr(x13, 7);
		  debug loc 169 116 18;
		  debug insn "srli x14, x15, 6";
		  x14 <== shr(x15, 6);
		  debug loc 169 116 5;
		  debug insn "or x13, x13, x14";
		  x13 <== or(x13, x14);
		  debug insn "and x13, x13, x30";
		  x13 <== and(x13, x30);
		core_dash_e8b3f51d8f75870a___dot_Ltmp8535:
		  debug loc 108 146 24;
		  debug insn "addi x12, x12, 16";
		  x12 <== wrap(x12 + 16);
		core_dash_e8b3f51d8f75870a___dot_Ltmp8536:
		  debug loc 169 84 17;
		  debug insn "add x11, x11, x13";
		  x11 <== wrap(x11 + x13);
		core_dash_e8b3f51d8f75870a___dot_Ltmp8537:
		  debug loc 169 0 17;
		  debug insn "bne x12, x29, core_dash_e8b3f51d8f75870a__.LBB296_18";
		  branch_if_nonzero x12 - x29, core_dash_e8b3f51d8f75870a___dot_LBB296_18;
		  debug insn "j core_dash_e8b3f51d8f75870a__.LBB296_12";
		  jump core_dash_e8b3f51d8f75870a___dot_LBB296_12;
		core_dash_e8b3f51d8f75870a___dot_LBB296_20:
		  debug insn "beqz x6, core_dash_e8b3f51d8f75870a__.LBB296_25";
		  branch_if_zero x6, core_dash_e8b3f51d8f75870a___dot_LBB296_25;
		core_dash_e8b3f51d8f75870a___dot_Ltmp8539:
		  debug insn "li x11, 192";
		  x11 <=X= 192;
		core_dash_e8b3f51d8f75870a___dot_Ltmp8540:
		  debug loc 108 146 24;
		  debug insn "bltu x7, x11, core_dash_e8b3f51d8f75870a__.LBB296_23";
		  branch_if_positive x11 - x7, core_dash_e8b3f51d8f75870a___dot_LBB296_23;
		core_dash_e8b3f51d8f75870a___dot_Ltmp8541:
		  debug loc 108 0 24;
		  debug insn "li x7, 192";
		  x7 <=X= 192;
		core_dash_e8b3f51d8f75870a___dot_Ltmp8542:
		core_dash_e8b3f51d8f75870a___dot_LBB296_23:
		  debug insn "li x11, 0";
		  x11 <=X= 0;
		  debug loc 108 146 24;
		  debug insn "andi x12, x7, 3";
		  x12 <== and(x7, 3);
		core_dash_e8b3f51d8f75870a___dot_Ltmp8543:
		  debug insn "slli x12, x12, 2";
		  x12 <== wrap16(x12 * 4);
		core_dash_e8b3f51d8f75870a___dot_Ltmp8544:
		core_dash_e8b3f51d8f75870a___dot_LBB296_24:
		  debug loc 169 99 18;
		  debug insn "lw x13, 0(x29)";
		  x13, tmp1 <== mload(x29 + 0);
		core_dash_e8b3f51d8f75870a___dot_Ltmp8545:
		  debug loc 116 499 18;
		  debug insn "addi x29, x29, 4";
		  x29 <== wrap(x29 + 4);
		core_dash_e8b3f51d8f75870a___dot_Ltmp8546:
		  debug loc 169 116 7;
		  debug insn "not x14, x13";
		  x14 <== wrap_signed(-x13 - 1);
		  debug loc 169 116 6;
		  debug insn "srli x14, x14, 7";
		  x14 <== shr(x14, 7);
		  debug loc 169 116 18;
		  debug insn "srli x13, x13, 6";
		  x13 <== shr(x13, 6);
		core_dash_e8b3f51d8f75870a___dot_Ltmp8547:
		  debug loc 169 116 5;
		  debug insn "or x13, x13, x14";
		  x13 <== or(x13, x14);
		  debug insn "and x13, x13, x30";
		  x13 <== and(x13, x30);
		core_dash_e8b3f51d8f75870a___dot_Ltmp8548:
		  debug loc 108 146 24;
		  debug insn "addi x12, x12, -4";
		  x12 <== wrap(x12 + 4294967292);
		core_dash_e8b3f51d8f75870a___dot_Ltmp8549:
		  debug loc 169 100 17;
		  debug insn "add x11, x11, x13";
		  x11 <== wrap(x11 + x13);
		core_dash_e8b3f51d8f75870a___dot_Ltmp8550:
		  debug loc 108 146 24;
		  debug insn "bnez x12, core_dash_e8b3f51d8f75870a__.LBB296_24";
		  branch_if_nonzero x12, core_dash_e8b3f51d8f75870a___dot_LBB296_24;
		  debug insn "j core_dash_e8b3f51d8f75870a__.LBB296_26";
		  jump core_dash_e8b3f51d8f75870a___dot_LBB296_26;
		core_dash_e8b3f51d8f75870a___dot_LBB296_25:
		  debug loc 108 0 24;
		  debug insn "li x11, 0";
		  x11 <=X= 0;
		core_dash_e8b3f51d8f75870a___dot_Ltmp8552:
		core_dash_e8b3f51d8f75870a___dot_LBB296_26:
		  debug loc 169 126 27;
		  debug insn "and x12, x11, x17";
		  x12 <== and(x11, x17);
		  debug loc 169 126 52;
		  debug insn "srli x11, x11, 8";
		  x11 <== shr(x11, 8);
		core_dash_e8b3f51d8f75870a___dot_Ltmp8553:
		  debug loc 169 126 51;
		  debug insn "and x11, x11, x17";
		  x11 <== and(x11, x17);
		  debug loc 169 126 27;
		  debug insn "add x11, x11, x12";
		  x11 <== wrap(x11 + x12);
		core_dash_e8b3f51d8f75870a___dot_Ltmp8554:
		  debug loc 114 1226 13;
		  debug insn "mul x11, x11, x16";
		  x11, tmp1 <== mul(x11, x16);
		core_dash_e8b3f51d8f75870a___dot_Ltmp8555:
		  debug loc 169 127 5;
		  debug insn "srli x11, x11, 16";
		  x11 <== shr(x11, 16);
		core_dash_e8b3f51d8f75870a___dot_Ltmp8556:
		  debug loc 169 102 13;
		  debug insn "add x10, x10, x11";
		  x10 <== wrap(x10 + x11);
		core_dash_e8b3f51d8f75870a___dot_Ltmp8557:
		  debug loc 169 107 2;
		  debug insn "ret ";
		  ret;
		core_dash_e8b3f51d8f75870a___ZN4core3fmt3num3imp7fmt_u3217h2c2f4e152b70db5aE:
		core_dash_e8b3f51d8f75870a___dot_Lfunc_begin571:
		  debug loc 94 211 0;
		  debug insn "addi x2, x2, -64";
		  x2 <== wrap(x2 + 4294967232);
		  debug insn "sw x1, 60(x2)";
		  mstore x2 + 60, x1;
		  debug insn "sw x8, 56(x2)";
		  mstore x2 + 56, x8;
		  debug insn "sw x9, 52(x2)";
		  mstore x2 + 52, x9;
		  debug insn "mv x16, x12";
		  x16 <=X= x12;
		core_dash_e8b3f51d8f75870a___dot_Ltmp14014:
		  debug loc 94 230 23;
		  debug insn "srli x13, x10, 4";
		  x13 <== shr(x10, 4);
		  debug insn "li x14, 625";
		  x14 <=X= 625;
		core_dash_e8b3f51d8f75870a___dot_Ltmp14015:
		  debug loc 94 0 23;
		  debug insn "li x12, 39";
		  x12 <=X= 39;
		  debug loc 94 230 23;
		  debug insn "bgeu x13, x14, core_dash_e8b3f51d8f75870a__.LBB571_4";
		  branch_if_positive x13 - x14 + 1, core_dash_e8b3f51d8f75870a___dot_LBB571_4;
		core_dash_e8b3f51d8f75870a___dot_Ltmp14016:
		  debug loc 94 0 23;
		  debug insn "li x13, 99";
		  x13 <=X= 99;
		core_dash_e8b3f51d8f75870a___dot_Ltmp14017:
		  debug loc 94 249 20;
		  debug insn "bltu x13, x10, core_dash_e8b3f51d8f75870a__.LBB571_7";
		  branch_if_positive x10 - x13, core_dash_e8b3f51d8f75870a___dot_LBB571_7;
		core_dash_e8b3f51d8f75870a___dot_Ltmp14018:
		core_dash_e8b3f51d8f75870a___dot_LBB571_2:
		  debug loc 94 0 20;
		  debug insn "li x13, 10";
		  x13 <=X= 10;
		  debug loc 94 257 20;
		  debug insn "bgeu x10, x13, core_dash_e8b3f51d8f75870a__.LBB571_8";
		  branch_if_positive x10 - x13 + 1, core_dash_e8b3f51d8f75870a___dot_LBB571_8;
		core_dash_e8b3f51d8f75870a___dot_Ltmp14019:
		core_dash_e8b3f51d8f75870a___dot_LBB571_3:
		  debug loc 94 258 21;
		  debug insn "addi x12, x12, -1";
		  x12 <== wrap(x12 + 4294967295);
		core_dash_e8b3f51d8f75870a___dot_Ltmp14020:
		  debug loc 94 0 21;
		  debug insn "addi x13, x2, 13";
		  x13 <== wrap(x2 + 13);
		core_dash_e8b3f51d8f75870a___dot_Ltmp14021:
		  debug loc 116 499 18;
		  debug insn "add x13, x13, x12";
		  x13 <== wrap(x13 + x12);
		core_dash_e8b3f51d8f75870a___dot_Ltmp14022:
		  debug loc 94 259 21;
		  debug insn "addi x10, x10, 48";
		  x10 <== wrap(x10 + 48);
		core_dash_e8b3f51d8f75870a___dot_Ltmp14023:
		  debug insn "sb x10, 0(x13)";
		  tmp1, tmp2 <== mload(x13 + 0);
		  tmp3 <== shl(0xff, 8 * tmp2);
		  tmp3 <== xor(tmp3, 0xffffffff);
		  tmp1 <== and(tmp1, tmp3);
		  tmp3 <== and(x10, 0xff);
		  tmp3 <== shl(tmp3, 8 * tmp2);
		  tmp1 <== or(tmp1, tmp3);
		  mstore x13 + 0 - tmp2, tmp1;
		  debug insn "j core_dash_e8b3f51d8f75870a__.LBB571_9";
		  jump core_dash_e8b3f51d8f75870a___dot_LBB571_9;
		core_dash_e8b3f51d8f75870a___dot_LBB571_4:
		  debug loc 94 0 21;
		  debug insn "li x12, 0";
		  x12 <=X= 0;
		  debug insn "lui x13, 858993";
		  x13 <=X= 3518435328;
		  debug insn "addi x5, x13, 1881";
		  x5 <== wrap(x13 + 1881);
		  debug insn "lui x13, 2";
		  x13 <=X= 8192;
		  debug insn "addi x7, x13, 1808";
		  x7 <== wrap(x13 + 1808);
		  debug insn "lui x13, 1";
		  x13 <=X= 4096;
		  debug insn "addi x28, x13, 1147";
		  x28 <== wrap(x13 + 1147);
		  debug insn "li x17, 100";
		  x17 <=X= 100;
		  debug insn "lui x14, 16";
		  x14 <=X= 65536;
		  debug insn "addi x30, x14, 524";
		  x30 <== wrap(x14 + 524);
		  debug insn "addi x6, x2, 13";
		  x6 <== wrap(x2 + 13);
		  debug insn "lui x15, 24414";
		  x15 <=X= 99999744;
		  debug insn "addi x29, x15, 255";
		  x29 <== wrap(x15 + 255);
		core_dash_e8b3f51d8f75870a___dot_Ltmp14025:
		core_dash_e8b3f51d8f75870a___dot_LBB571_5:
		  debug insn "mv x13, x10";
		  x13 <=X= x10;
		core_dash_e8b3f51d8f75870a___dot_Ltmp14026:
		  debug loc 94 232 21;
		  debug insn "mulhu x10, x10, x5";
		  tmp1, x10 <== mul(x10, x5);
		  debug insn "srli x10, x10, 13";
		  x10 <== shr(x10, 13);
		core_dash_e8b3f51d8f75870a___dot_Ltmp14027:
		  debug loc 94 0 21;
		  debug insn "mul x15, x10, x7";
		  x15, tmp1 <== mul(x10, x7);
		  debug insn "sub x15, x13, x15";
		  x15 <== wrap_signed(x13 - x15);
		core_dash_e8b3f51d8f75870a___dot_Ltmp14028:
		  debug loc 94 234 30;
		  debug insn "slli x14, x15, 16";
		  x14 <== wrap16(x15 * 65536);
		  debug insn "srli x14, x14, 18";
		  x14 <== shr(x14, 18);
		  debug insn "mul x14, x14, x28";
		  x14, tmp1 <== mul(x14, x28);
		  debug insn "srli x31, x14, 17";
		  x31 <== shr(x14, 17);
		  debug insn "srli x14, x14, 16";
		  x14 <== shr(x14, 16);
		  debug insn "andi x8, x14, 2046";
		  x8 <== and(x14, 2046);
		core_dash_e8b3f51d8f75870a___dot_Ltmp14029:
		  debug loc 94 0 30;
		  debug insn "mul x14, x31, x17";
		  x14, tmp1 <== mul(x31, x17);
		  debug insn "sub x14, x15, x14";
		  x14 <== wrap_signed(x15 - x14);
		core_dash_e8b3f51d8f75870a___dot_Ltmp14030:
		  debug loc 94 235 30;
		  debug insn "slli x14, x14, 17";
		  tmp1 <== wrap16(x14 * 65536);
		  x14 <== wrap16(tmp1 * 2);
		  debug insn "srli x14, x14, 16";
		  x14 <== shr(x14, 16);
		core_dash_e8b3f51d8f75870a___dot_Ltmp14031:
		  debug loc 106 485 18;
		  debug insn "add x15, x30, x8";
		  x15 <== wrap(x30 + x8);
		core_dash_e8b3f51d8f75870a___dot_Ltmp14032:
		  debug loc 115 2372 9;
		  debug insn "add x8, x6, x12";
		  x8 <== wrap(x6 + x12);
		core_dash_e8b3f51d8f75870a___dot_Ltmp14033:
		  debug insn "lbu x31, 0(x15)";
		  x31, tmp2 <== mload(x15 + 0);
		  x31 <== shr(x31, 8 * tmp2);
		  x31 <== and(x31, 0xff);
		  debug insn "lb x15, 1(x15)";
		  x15, tmp2 <== mload(x15 + 1);
		  x15 <== shr(x15, 8 * tmp2);
		  x15 <== sign_extend_byte(x15);
		core_dash_e8b3f51d8f75870a___dot_Ltmp14034:
		  debug loc 106 485 18;
		  debug insn "add x14, x14, x30";
		  x14 <== wrap(x14 + x30);
		core_dash_e8b3f51d8f75870a___dot_Ltmp14035:
		  debug loc 115 2372 9;
		  debug insn "lb x9, 1(x14)";
		  x9, tmp2 <== mload(x14 + 1);
		  x9 <== shr(x9, 8 * tmp2);
		  x9 <== sign_extend_byte(x9);
		  debug insn "lbu x14, 0(x14)";
		  x14, tmp2 <== mload(x14 + 0);
		  x14 <== shr(x14, 8 * tmp2);
		  x14 <== and(x14, 0xff);
		core_dash_e8b3f51d8f75870a___dot_Ltmp14036:
		  debug loc 115 2372 9;
		  debug insn "sb x15, 36(x8)";
		  tmp1, tmp2 <== mload(x8 + 36);
		  tmp3 <== shl(0xff, 8 * tmp2);
		  tmp3 <== xor(tmp3, 0xffffffff);
		  tmp1 <== and(tmp1, tmp3);
		  tmp3 <== and(x15, 0xff);
		  tmp3 <== shl(tmp3, 8 * tmp2);
		  tmp1 <== or(tmp1, tmp3);
		  mstore x8 + 36 - tmp2, tmp1;
		  debug insn "sb x31, 35(x8)";
		  tmp1, tmp2 <== mload(x8 + 35);
		  tmp3 <== shl(0xff, 8 * tmp2);
		  tmp3 <== xor(tmp3, 0xffffffff);
		  tmp1 <== and(tmp1, tmp3);
		  tmp3 <== and(x31, 0xff);
		  tmp3 <== shl(tmp3, 8 * tmp2);
		  tmp1 <== or(tmp1, tmp3);
		  mstore x8 + 35 - tmp2, tmp1;
		core_dash_e8b3f51d8f75870a___dot_Ltmp14037:
		  debug loc 115 2372 9;
		  debug insn "sb x9, 38(x8)";
		  tmp1, tmp2 <== mload(x8 + 38);
		  tmp3 <== shl(0xff, 8 * tmp2);
		  tmp3 <== xor(tmp3, 0xffffffff);
		  tmp1 <== and(tmp1, tmp3);
		  tmp3 <== and(x9, 0xff);
		  tmp3 <== shl(tmp3, 8 * tmp2);
		  tmp1 <== or(tmp1, tmp3);
		  mstore x8 + 38 - tmp2, tmp1;
		  debug insn "sb x14, 37(x8)";
		  tmp1, tmp2 <== mload(x8 + 37);
		  tmp3 <== shl(0xff, 8 * tmp2);
		  tmp3 <== xor(tmp3, 0xffffffff);
		  tmp1 <== and(tmp1, tmp3);
		  tmp3 <== and(x14, 0xff);
		  tmp3 <== shl(tmp3, 8 * tmp2);
		  tmp1 <== or(tmp1, tmp3);
		  mstore x8 + 37 - tmp2, tmp1;
		core_dash_e8b3f51d8f75870a___dot_Ltmp14038:
		  debug loc 94 230 23;
		  debug insn "addi x12, x12, -4";
		  x12 <== wrap(x12 + 4294967292);
		core_dash_e8b3f51d8f75870a___dot_Ltmp14039:
		  debug insn "bltu x29, x13, core_dash_e8b3f51d8f75870a__.LBB571_5";
		  branch_if_positive x13 - x29, core_dash_e8b3f51d8f75870a___dot_LBB571_5;
		core_dash_e8b3f51d8f75870a___dot_Ltmp14040:
		  debug loc 94 249 20;
		  debug insn "addi x12, x12, 39";
		  x12 <== wrap(x12 + 39);
		core_dash_e8b3f51d8f75870a___dot_Ltmp14041:
		  debug loc 94 0 20;
		  debug insn "li x13, 99";
		  x13 <=X= 99;
		  debug loc 94 249 20;
		  debug insn "bgeu x13, x10, core_dash_e8b3f51d8f75870a__.LBB571_2";
		  branch_if_positive x13 - x10 + 1, core_dash_e8b3f51d8f75870a___dot_LBB571_2;
		core_dash_e8b3f51d8f75870a___dot_Ltmp14042:
		core_dash_e8b3f51d8f75870a___dot_LBB571_7:
		  debug loc 94 251 21;
		  debug insn "slli x13, x10, 16";
		  x13 <== wrap16(x10 * 65536);
		  debug insn "srli x13, x13, 18";
		  x13 <== shr(x13, 18);
		  debug insn "lui x14, 1";
		  x14 <=X= 4096;
		  debug insn "addi x14, x14, 1147";
		  x14 <== wrap(x14 + 1147);
		  debug insn "mul x13, x13, x14";
		  x13, tmp1 <== mul(x13, x14);
		  debug insn "srli x13, x13, 17";
		  x13 <== shr(x13, 17);
		core_dash_e8b3f51d8f75870a___dot_Ltmp14043:
		  debug loc 94 0 21;
		  debug insn "li x14, 100";
		  x14 <=X= 100;
		  debug insn "mul x14, x13, x14";
		  x14, tmp1 <== mul(x13, x14);
		  debug insn "sub x10, x10, x14";
		  x10 <== wrap_signed(x10 - x14);
		core_dash_e8b3f51d8f75870a___dot_Ltmp14044:
		  debug loc 94 250 30;
		  debug insn "slli x10, x10, 17";
		  tmp1 <== wrap16(x10 * 65536);
		  x10 <== wrap16(tmp1 * 2);
		  debug insn "srli x10, x10, 16";
		  x10 <== shr(x10, 16);
		core_dash_e8b3f51d8f75870a___dot_Ltmp14045:
		  debug loc 94 252 21;
		  debug insn "addi x12, x12, -2";
		  x12 <== wrap(x12 + 4294967294);
		core_dash_e8b3f51d8f75870a___dot_Ltmp14046:
		  debug loc 106 485 18;
		  debug insn "lui x14, 16";
		  x14 <=X= 65536;
		  debug insn "addi x14, x14, 524";
		  x14 <== wrap(x14 + 524);
		core_dash_e8b3f51d8f75870a___dot_Ltmp14047:
		  debug insn "add x10, x10, x14";
		  x10 <== wrap(x10 + x14);
		core_dash_e8b3f51d8f75870a___dot_Ltmp14048:
		  debug loc 115 2372 9;
		  debug insn "lb x14, 1(x10)";
		  x14, tmp2 <== mload(x10 + 1);
		  x14 <== shr(x14, 8 * tmp2);
		  x14 <== sign_extend_byte(x14);
		core_dash_e8b3f51d8f75870a___dot_Ltmp14049:
		  debug insn "lbu x10, 0(x10)";
		  x10, tmp2 <== mload(x10 + 0);
		  x10 <== shr(x10, 8 * tmp2);
		  x10 <== and(x10, 0xff);
		core_dash_e8b3f51d8f75870a___dot_Ltmp14050:
		  debug loc 115 0 9;
		  debug insn "addi x15, x2, 13";
		  x15 <== wrap(x2 + 13);
		core_dash_e8b3f51d8f75870a___dot_Ltmp14051:
		  debug loc 116 499 18;
		  debug insn "add x15, x15, x12";
		  x15 <== wrap(x15 + x12);
		core_dash_e8b3f51d8f75870a___dot_Ltmp14052:
		  debug loc 115 2372 9;
		  debug insn "sb x14, 1(x15)";
		  tmp1, tmp2 <== mload(x15 + 1);
		  tmp3 <== shl(0xff, 8 * tmp2);
		  tmp3 <== xor(tmp3, 0xffffffff);
		  tmp1 <== and(tmp1, tmp3);
		  tmp3 <== and(x14, 0xff);
		  tmp3 <== shl(tmp3, 8 * tmp2);
		  tmp1 <== or(tmp1, tmp3);
		  mstore x15 + 1 - tmp2, tmp1;
		  debug insn "sb x10, 0(x15)";
		  tmp1, tmp2 <== mload(x15 + 0);
		  tmp3 <== shl(0xff, 8 * tmp2);
		  tmp3 <== xor(tmp3, 0xffffffff);
		  tmp1 <== and(tmp1, tmp3);
		  tmp3 <== and(x10, 0xff);
		  tmp3 <== shl(tmp3, 8 * tmp2);
		  tmp1 <== or(tmp1, tmp3);
		  mstore x15 + 0 - tmp2, tmp1;
		  debug insn "mv x10, x13";
		  x10 <=X= x13;
		core_dash_e8b3f51d8f75870a___dot_Ltmp14053:
		  debug loc 115 0 9;
		  debug insn "li x13, 10";
		  x13 <=X= 10;
		core_dash_e8b3f51d8f75870a___dot_Ltmp14054:
		  debug loc 94 257 20;
		  debug insn "bltu x10, x13, core_dash_e8b3f51d8f75870a__.LBB571_3";
		  branch_if_positive x13 - x10, core_dash_e8b3f51d8f75870a___dot_LBB571_3;
		core_dash_e8b3f51d8f75870a___dot_Ltmp14055:
		core_dash_e8b3f51d8f75870a___dot_LBB571_8:
		  debug loc 94 261 30;
		  debug insn "slli x10, x10, 1";
		  x10 <== wrap16(x10 * 2);
		core_dash_e8b3f51d8f75870a___dot_Ltmp14056:
		  debug loc 94 262 21;
		  debug insn "addi x12, x12, -2";
		  x12 <== wrap(x12 + 4294967294);
		core_dash_e8b3f51d8f75870a___dot_Ltmp14057:
		  debug loc 106 485 18;
		  debug insn "lui x13, 16";
		  x13 <=X= 65536;
		  debug insn "addi x13, x13, 524";
		  x13 <== wrap(x13 + 524);
		core_dash_e8b3f51d8f75870a___dot_Ltmp14058:
		  debug insn "add x10, x10, x13";
		  x10 <== wrap(x10 + x13);
		core_dash_e8b3f51d8f75870a___dot_Ltmp14059:
		  debug loc 115 2372 9;
		  debug insn "lb x13, 1(x10)";
		  x13, tmp2 <== mload(x10 + 1);
		  x13 <== shr(x13, 8 * tmp2);
		  x13 <== sign_extend_byte(x13);
		core_dash_e8b3f51d8f75870a___dot_Ltmp14060:
		  debug insn "lbu x10, 0(x10)";
		  x10, tmp2 <== mload(x10 + 0);
		  x10 <== shr(x10, 8 * tmp2);
		  x10 <== and(x10, 0xff);
		core_dash_e8b3f51d8f75870a___dot_Ltmp14061:
		  debug loc 115 0 9;
		  debug insn "addi x14, x2, 13";
		  x14 <== wrap(x2 + 13);
		core_dash_e8b3f51d8f75870a___dot_Ltmp14062:
		  debug loc 116 499 18;
		  debug insn "add x14, x14, x12";
		  x14 <== wrap(x14 + x12);
		core_dash_e8b3f51d8f75870a___dot_Ltmp14063:
		  debug loc 115 2372 9;
		  debug insn "sb x13, 1(x14)";
		  tmp1, tmp2 <== mload(x14 + 1);
		  tmp3 <== shl(0xff, 8 * tmp2);
		  tmp3 <== xor(tmp3, 0xffffffff);
		  tmp1 <== and(tmp1, tmp3);
		  tmp3 <== and(x13, 0xff);
		  tmp3 <== shl(tmp3, 8 * tmp2);
		  tmp1 <== or(tmp1, tmp3);
		  mstore x14 + 1 - tmp2, tmp1;
		  debug insn "sb x10, 0(x14)";
		  tmp1, tmp2 <== mload(x14 + 0);
		  tmp3 <== shl(0xff, 8 * tmp2);
		  tmp3 <== xor(tmp3, 0xffffffff);
		  tmp1 <== and(tmp1, tmp3);
		  tmp3 <== and(x10, 0xff);
		  tmp3 <== shl(tmp3, 8 * tmp2);
		  tmp1 <== or(tmp1, tmp3);
		  mstore x14 + 0 - tmp2, tmp1;
		core_dash_e8b3f51d8f75870a___dot_Ltmp14064:
		core_dash_e8b3f51d8f75870a___dot_LBB571_9:
		  debug loc 115 0 9;
		  debug insn "addi x10, x2, 13";
		  x10 <== wrap(x2 + 13);
		core_dash_e8b3f51d8f75870a___dot_Ltmp14065:
		  debug loc 116 499 18;
		  debug insn "add x14, x10, x12";
		  x14 <== wrap(x10 + x12);
		  debug insn "li x10, 39";
		  x10 <=X= 39;
		core_dash_e8b3f51d8f75870a___dot_Ltmp14066:
		  debug loc 94 271 62;
		  debug insn "sub x15, x10, x12";
		  x15 <== wrap_signed(x10 - x12);
		core_dash_e8b3f51d8f75870a___dot_Ltmp14067:
		  debug loc 94 273 13;
		  debug insn "lui x10, 16";
		  x10 <=X= 65536;
		  debug insn "addi x12, x10, 424";
		  x12 <== wrap(x10 + 424);
		core_dash_e8b3f51d8f75870a___dot_Ltmp14068:
		  debug insn "mv x10, x16";
		  x10 <=X= x16;
		  debug insn "li x13, 0";
		  x13 <=X= 0;
		  debug insn "call _ZN4core3fmt9Formatter12pad_integral17h0f5bfd9b3d6cd343E";
		  call _ZN4core3fmt9Formatter12pad_integral17h0f5bfd9b3d6cd343E;
		core_dash_e8b3f51d8f75870a___dot_Ltmp14069:
		  debug loc 94 274 10;
		  debug insn "lw x1, 60(x2)";
		  x1, tmp1 <== mload(x2 + 60);
		  debug insn "lw x8, 56(x2)";
		  x8, tmp1 <== mload(x2 + 56);
		  debug insn "lw x9, 52(x2)";
		  x9, tmp1 <== mload(x2 + 52);
		  debug insn "addi x2, x2, 64";
		  x2 <== wrap(x2 + 64);
		  debug insn "ret ";
		  ret;
		_ZN4core3fmt3num3imp52_$LT$impl$u20$core_dot__dot_fmt_dot__dot_Display$u20$for$u20$u32$GT$3fmt17h361d4a177e5aa0a5E:
		core_dash_e8b3f51d8f75870a___dot_Lfunc_begin577:
		  debug loc 94 279 0;
		  debug loc 94 44 37;
		  debug insn "lw x10, 0(x10)";
		  x10, tmp1 <== mload(x10 + 0);
		core_dash_e8b3f51d8f75870a___dot_Ltmp14095:
		  debug loc 94 0 37;
		  debug insn "mv x12, x11";
		  x12 <=X= x11;
		core_dash_e8b3f51d8f75870a___dot_Ltmp14096:
		  debug loc 94 287 17;
		  debug insn "li x11, 1";
		  x11 <=X= 1;
		  debug insn "tail core_dash_e8b3f51d8f75870a___ZN4core3fmt3num3imp7fmt_u3217h2c2f4e152b70db5aE";
		  jump core_dash_e8b3f51d8f75870a___ZN4core3fmt3num3imp7fmt_u3217h2c2f4e152b70db5aE;
		_ZN53_$LT$core_dot__dot_fmt_dot__dot_Error$u20$as$u20$core_dot__dot_fmt_dot__dot_Debug$GT$3fmt17h8ce97bea8e305373E:
		core_dash_e8b3f51d8f75870a___dot_Lfunc_begin605:
		  debug loc 95 98 0;
		  debug loc 95 1639 9;
		  debug insn "lw x12, 4(x11)";
		  x12, tmp1 <== mload(x11 + 4);
		  debug insn "lw x10, 0(x11)";
		  x10, tmp1 <== mload(x11 + 0);
		core_dash_e8b3f51d8f75870a___dot_Ltmp14798:
		  debug insn "lw x15, 12(x12)";
		  x15, tmp1 <== mload(x12 + 12);
		  debug insn "lui x11, 16";
		  x11 <=X= 65536;
		core_dash_e8b3f51d8f75870a___dot_Ltmp14799:
		  debug insn "addi x11, x11, 724";
		  x11 <== wrap(x11 + 724);
		core_dash_e8b3f51d8f75870a___dot_Ltmp14800:
		  debug insn "li x12, 5";
		  x12 <=X= 5;
		  debug insn "jr x15";
		  jump_dyn x15;
		core_dash_e8b3f51d8f75870a___ZN42_$LT$$RF$T$u20$as$u20$core_dot__dot_fmt_dot__dot_Debug$GT$3fmt17h1906125bc95027d2E:
		core_dash_e8b3f51d8f75870a___dot_Lfunc_begin617:
		  debug loc 95 2372 0;
		  debug loc 95 2372 71;
		  debug insn "lw x12, 4(x10)";
		  x12, tmp1 <== mload(x10 + 4);
		  debug insn "lw x10, 0(x10)";
		  x10, tmp1 <== mload(x10 + 0);
		core_dash_e8b3f51d8f75870a___dot_Ltmp14936:
		  debug loc 95 2372 62;
		  debug insn "lw x15, 12(x12)";
		  x15, tmp1 <== mload(x12 + 12);
		  debug insn "jr x15";
		  jump_dyn x15;
		core_dash_e8b3f51d8f75870a___ZN44_$LT$$RF$T$u20$as$u20$core_dot__dot_fmt_dot__dot_Display$GT$3fmt17h1e2de74ca37ee72aE:
		core_dash_e8b3f51d8f75870a___dot_Lfunc_begin692:
		  debug loc 95 2372 0;
		  debug loc 95 2372 71;
		  debug insn "lw x10, 0(x10)";
		  x10, tmp1 <== mload(x10 + 0);
		core_dash_e8b3f51d8f75870a___dot_Ltmp16344:
		  debug loc 95 2372 71;
		  debug insn "lw x13, 0(x10)";
		  x13, tmp1 <== mload(x10 + 0);
		  debug insn "lw x12, 4(x10)";
		  x12, tmp1 <== mload(x10 + 4);
		core_dash_e8b3f51d8f75870a___dot_Ltmp16345:
		  debug loc 95 0 71;
		  debug insn "mv x10, x11";
		  x10 <=X= x11;
		core_dash_e8b3f51d8f75870a___dot_Ltmp16346:
		  debug loc 95 2441 9;
		  debug insn "mv x11, x13";
		  x11 <=X= x13;
		  debug insn "tail _ZN4core3fmt9Formatter3pad17h10049b44c877e9aaE";
		  jump _ZN4core3fmt9Formatter3pad17h10049b44c877e9aaE;
		core_dash_e8b3f51d8f75870a___ZN44_$LT$$RF$T$u20$as$u20$core_dot__dot_fmt_dot__dot_Display$GT$3fmt17hbb08b750573e0112E:
		core_dash_e8b3f51d8f75870a___dot_Lfunc_begin693:
		  debug loc 95 2372 0;
		  debug loc 95 2372 71;
		  debug insn "lw x13, 0(x10)";
		  x13, tmp1 <== mload(x10 + 0);
		  debug insn "lw x12, 4(x10)";
		  x12, tmp1 <== mload(x10 + 4);
		core_dash_e8b3f51d8f75870a___dot_Ltmp16348:
		  debug loc 95 0 71;
		  debug insn "mv x10, x11";
		  x10 <=X= x11;
		core_dash_e8b3f51d8f75870a___dot_Ltmp16349:
		  debug loc 95 2441 9;
		  debug insn "mv x11, x13";
		  x11 <=X= x13;
		  debug insn "tail _ZN4core3fmt9Formatter3pad17h10049b44c877e9aaE";
		  jump _ZN4core3fmt9Formatter3pad17h10049b44c877e9aaE;
		core_dash_e8b3f51d8f75870a___ZN44_$LT$$RF$T$u20$as$u20$core_dot__dot_fmt_dot__dot_Display$GT$3fmt17hd10a92ec7570104aE:
		core_dash_e8b3f51d8f75870a___dot_Lfunc_begin694:
		  debug loc 95 2372 0;
		  debug insn "addi x2, x2, -32";
		  x2 <== wrap(x2 + 4294967264);
		core_dash_e8b3f51d8f75870a___dot_Ltmp16351:
		  debug loc 95 2372 71;
		  debug insn "sw x1, 28(x2)";
		  mstore x2 + 28, x1;
		  debug insn "lw x12, 0(x10)";
		  x12, tmp1 <== mload(x10 + 0);
		core_dash_e8b3f51d8f75870a___dot_Ltmp16352:
		  debug loc 95 535 24;
		  debug insn "lw x10, 20(x12)";
		  x10, tmp1 <== mload(x12 + 20);
		core_dash_e8b3f51d8f75870a___dot_Ltmp16353:
		  debug insn "sw x10, 20(x2)";
		  mstore x2 + 20, x10;
		  debug insn "lw x10, 16(x12)";
		  x10, tmp1 <== mload(x12 + 16);
		  debug insn "sw x10, 16(x2)";
		  mstore x2 + 16, x10;
		  debug insn "lw x10, 12(x12)";
		  x10, tmp1 <== mload(x12 + 12);
		  debug insn "sw x10, 12(x2)";
		  mstore x2 + 12, x10;
		  debug insn "lw x10, 8(x12)";
		  x10, tmp1 <== mload(x12 + 8);
		  debug insn "sw x10, 8(x2)";
		  mstore x2 + 8, x10;
		  debug insn "lw x13, 4(x12)";
		  x13, tmp1 <== mload(x12 + 4);
		  debug loc 95 535 15;
		  debug insn "lw x10, 0(x11)";
		  x10, tmp1 <== mload(x11 + 0);
		  debug loc 95 535 24;
		  debug insn "sw x13, 4(x2)";
		  mstore x2 + 4, x13;
		  debug insn "lw x12, 0(x12)";
		  x12, tmp1 <== mload(x12 + 0);
		core_dash_e8b3f51d8f75870a___dot_Ltmp16354:
		  debug loc 95 535 15;
		  debug insn "lw x11, 4(x11)";
		  x11, tmp1 <== mload(x11 + 4);
		core_dash_e8b3f51d8f75870a___dot_Ltmp16355:
		  debug loc 95 535 24;
		  debug insn "sw x12, 0(x2)";
		  mstore x2 + 0, x12;
		  debug loc 95 535 9;
		  debug insn "mv x12, x2";
		  x12 <=X= x2;
		  debug insn "call _ZN4core3fmt5write17h87a75934cc3b911aE";
		  call _ZN4core3fmt5write17h87a75934cc3b911aE;
		core_dash_e8b3f51d8f75870a___dot_Ltmp16356:
		  debug loc 95 2372 84;
		  debug insn "lw x1, 28(x2)";
		  x1, tmp1 <== mload(x2 + 28);
		  debug insn "addi x2, x2, 32";
		  x2 <== wrap(x2 + 32);
		  debug insn "ret ";
		  ret;
		many_chunks_dash_1054733ad03afc41___ZN5alloc7raw_vec11finish_grow17h862016746d2f359dE:
		many_chunks_dash_1054733ad03afc41___dot_Lfunc_begin0:
		  debug loc 202 448 0;
		  debug insn "addi x2, x2, -16";
		  x2 <== wrap(x2 + 4294967280);
		  debug insn "sw x1, 12(x2)";
		  mstore x2 + 12, x1;
		  debug insn "sw x8, 8(x2)";
		  mstore x2 + 8, x8;
		  debug insn "sw x9, 4(x2)";
		  mstore x2 + 4, x9;
		  debug insn "sw x18, 0(x2)";
		  mstore x2 + 0, x18;
		  debug insn "mv x9, x11";
		  x9 <=X= x11;
		many_chunks_dash_1054733ad03afc41___dot_Ltmp0:
		  debug insn "mv x8, x10";
		  x8 <=X= x10;
		many_chunks_dash_1054733ad03afc41___dot_Ltmp1:
		  debug loc 203 858 9;
		  debug insn "beqz x12, many_chunks_dash_1054733ad03afc41__.LBB0_6";
		  branch_if_zero x12, many_chunks_dash_1054733ad03afc41___dot_LBB0_6;
		many_chunks_dash_1054733ad03afc41___dot_Ltmp2:
		  debug loc 202 506 8;
		  debug insn "bltz x9, many_chunks_dash_1054733ad03afc41__.LBB0_7";
		  branch_if_positive x9 - 2**31 + 1, many_chunks_dash_1054733ad03afc41___dot_LBB0_7;
		many_chunks_dash_1054733ad03afc41___dot_Ltmp3:
		  debug loc 202 0 8;
		  debug insn "mv x18, x12";
		  x18 <=X= x12;
		many_chunks_dash_1054733ad03afc41___dot_Ltmp4:
		  debug loc 202 461 25;
		  debug insn "lw x10, 8(x13)";
		  x10, tmp1 <== mload(x13 + 8);
		  debug insn "beqz x10, many_chunks_dash_1054733ad03afc41__.LBB0_9";
		  branch_if_zero x10, many_chunks_dash_1054733ad03afc41___dot_LBB0_9;
		many_chunks_dash_1054733ad03afc41___dot_Ltmp5:
		  debug loc 202 461 36;
		  debug insn "lw x11, 4(x13)";
		  x11, tmp1 <== mload(x13 + 4);
		many_chunks_dash_1054733ad03afc41___dot_Ltmp6:
		  debug loc 202 0 36;
		  debug insn "beqz x11, many_chunks_dash_1054733ad03afc41__.LBB0_9";
		  branch_if_zero x11, many_chunks_dash_1054733ad03afc41___dot_LBB0_9;
		many_chunks_dash_1054733ad03afc41___dot_Ltmp7:
		  debug insn "lw x10, 0(x13)";
		  x10, tmp1 <== mload(x13 + 0);
		many_chunks_dash_1054733ad03afc41___dot_Ltmp8:
		  debug loc 204 132 14;
		  debug insn "mv x12, x18";
		  x12 <=X= x18;
		  debug insn "mv x13, x9";
		  x13 <=X= x9;
		many_chunks_dash_1054733ad03afc41___dot_Ltmp9:
		  debug insn "call __rg_realloc";
		  call __rg_realloc;
		many_chunks_dash_1054733ad03afc41___dot_Ltmp10:
		  debug loc 203 858 9;
		  debug insn "bnez x10, many_chunks_dash_1054733ad03afc41__.LBB0_11";
		  branch_if_nonzero x10, many_chunks_dash_1054733ad03afc41___dot_LBB0_11;
		many_chunks_dash_1054733ad03afc41___dot_Ltmp11:
		many_chunks_dash_1054733ad03afc41___dot_LBB0_5:
		  debug loc 203 860 23;
		  debug insn "sw x9, 4(x8)";
		  mstore x8 + 4, x9;
		  debug insn "sw x18, 8(x8)";
		  mstore x8 + 8, x18;
		  debug insn "j many_chunks_dash_1054733ad03afc41__.LBB0_8";
		  jump many_chunks_dash_1054733ad03afc41___dot_LBB0_8;
		many_chunks_dash_1054733ad03afc41___dot_LBB0_6:
		  debug loc 203 2107 23;
		  debug insn "sw x9, 4(x8)";
		  mstore x8 + 4, x9;
		many_chunks_dash_1054733ad03afc41___dot_Ltmp13:
		many_chunks_dash_1054733ad03afc41___dot_LBB0_7:
		  debug loc 202 0 0;
		  debug insn "sw x0, 8(x8)";
		  mstore x8 + 8, x0;
		many_chunks_dash_1054733ad03afc41___dot_Ltmp14:
		many_chunks_dash_1054733ad03afc41___dot_LBB0_8:
		  debug insn "li x11, 1";
		  x11 <=X= 1;
		  debug insn "j many_chunks_dash_1054733ad03afc41__.LBB0_12";
		  jump many_chunks_dash_1054733ad03afc41___dot_LBB0_12;
		many_chunks_dash_1054733ad03afc41___dot_LBB0_9:
		  debug insn "beqz x9, many_chunks_dash_1054733ad03afc41__.LBB0_13";
		  branch_if_zero x9, many_chunks_dash_1054733ad03afc41___dot_LBB0_13;
		many_chunks_dash_1054733ad03afc41___dot_Ltmp16:
		  debug insn "mv x10, x9";
		  x10 <=X= x9;
		  debug insn "mv x11, x18";
		  x11 <=X= x18;
		  debug insn "call __rg_alloc";
		  call __rg_alloc;
		many_chunks_dash_1054733ad03afc41___dot_Ltmp17:
		  debug loc 203 858 9;
		  debug insn "beqz x10, many_chunks_dash_1054733ad03afc41__.LBB0_5";
		  branch_if_zero x10, many_chunks_dash_1054733ad03afc41___dot_LBB0_5;
		many_chunks_dash_1054733ad03afc41___dot_Ltmp18:
		many_chunks_dash_1054733ad03afc41___dot_LBB0_11:
		  debug loc 203 0 9;
		  debug insn "li x11, 0";
		  x11 <=X= 0;
		many_chunks_dash_1054733ad03afc41___dot_Ltmp19:
		  debug loc 203 859 22;
		  debug insn "sw x10, 4(x8)";
		  mstore x8 + 4, x10;
		  debug insn "sw x9, 8(x8)";
		  mstore x8 + 8, x9;
		many_chunks_dash_1054733ad03afc41___dot_Ltmp20:
		many_chunks_dash_1054733ad03afc41___dot_LBB0_12:
		  debug loc 202 0 0;
		  debug insn "sw x11, 0(x8)";
		  mstore x8 + 0, x11;
		  debug loc 202 473 2;
		  debug insn "lw x1, 12(x2)";
		  x1, tmp1 <== mload(x2 + 12);
		  debug insn "lw x8, 8(x2)";
		  x8, tmp1 <== mload(x2 + 8);
		  debug insn "lw x9, 4(x2)";
		  x9, tmp1 <== mload(x2 + 4);
		many_chunks_dash_1054733ad03afc41___dot_Ltmp21:
		  debug insn "lw x18, 0(x2)";
		  x18, tmp1 <== mload(x2 + 0);
		  debug insn "addi x2, x2, 16";
		  x2 <== wrap(x2 + 16);
		  debug insn "ret ";
		  ret;
		many_chunks_dash_1054733ad03afc41___dot_LBB0_13:
		  debug loc 202 0 2;
		  debug insn "mv x10, x18";
		  x10 <=X= x18;
		  debug insn "bnez x10, many_chunks_dash_1054733ad03afc41__.LBB0_11";
		  branch_if_nonzero x10, many_chunks_dash_1054733ad03afc41___dot_LBB0_11;
		  debug insn "j many_chunks_dash_1054733ad03afc41__.LBB0_5";
		  jump many_chunks_dash_1054733ad03afc41___dot_LBB0_5;
		many_chunks_dash_1054733ad03afc41___ZN5alloc7raw_vec19RawVec$LT$T$C$A$GT$16reserve_for_push17hda58fa1bff2773ddE:
		many_chunks_dash_1054733ad03afc41___dot_Lfunc_begin1:
		  debug loc 202 297 0;
		  debug insn "addi x2, x2, -48";
		  x2 <== wrap(x2 + 4294967248);
		many_chunks_dash_1054733ad03afc41___dot_Ltmp23:
		  debug loc 205 1479 26;
		  debug insn "sw x1, 44(x2)";
		  mstore x2 + 44, x1;
		  debug insn "sw x8, 40(x2)";
		  mstore x2 + 40, x8;
		  debug insn "sw x9, 36(x2)";
		  mstore x2 + 36, x9;
		  debug insn "addi x11, x11, 1";
		  x11 <== wrap(x11 + 1);
		many_chunks_dash_1054733ad03afc41___dot_Ltmp24:
		  debug loc 202 390 28;
		  debug insn "beqz x11, many_chunks_dash_1054733ad03afc41__.LBB1_11";
		  branch_if_zero x11, many_chunks_dash_1054733ad03afc41___dot_LBB1_11;
		many_chunks_dash_1054733ad03afc41___dot_Ltmp25:
		  debug loc 202 0 28;
		  debug insn "mv x8, x10";
		  x8 <=X= x10;
		many_chunks_dash_1054733ad03afc41___dot_Ltmp26:
		  debug loc 202 394 28;
		  debug insn "lw x10, 0(x10)";
		  x10, tmp1 <== mload(x10 + 0);
		  debug insn "slli x9, x10, 1";
		  x9 <== wrap16(x10 * 2);
		many_chunks_dash_1054733ad03afc41___dot_Ltmp27:
		  debug loc 206 0 0;
		  debug insn "bltu x11, x9, many_chunks_dash_1054733ad03afc41__.LBB1_3";
		  branch_if_positive x9 - x11, many_chunks_dash_1054733ad03afc41___dot_LBB1_3;
		many_chunks_dash_1054733ad03afc41___dot_Ltmp28:
		  debug insn "mv x9, x11";
		  x9 <=X= x11;
		many_chunks_dash_1054733ad03afc41___dot_Ltmp29:
		many_chunks_dash_1054733ad03afc41___dot_LBB1_3:
		  debug insn "li x11, 4";
		  x11 <=X= 4;
		many_chunks_dash_1054733ad03afc41___dot_Ltmp30:
		  debug insn "bltu x11, x9, many_chunks_dash_1054733ad03afc41__.LBB1_5";
		  branch_if_positive x9 - x11, many_chunks_dash_1054733ad03afc41___dot_LBB1_5;
		many_chunks_dash_1054733ad03afc41___dot_Ltmp31:
		  debug insn "li x9, 4";
		  x9 <=X= 4;
		many_chunks_dash_1054733ad03afc41___dot_Ltmp32:
		many_chunks_dash_1054733ad03afc41___dot_LBB1_5:
		  debug insn "srli x11, x9, 29";
		  x11 <== shr(x9, 29);
		  debug insn "seqz x12, x11";
		  x12 <=Y= is_equal_zero(x11);
		many_chunks_dash_1054733ad03afc41___dot_Ltmp33:
		  debug loc 207 452 16;
		  debug insn "slli x11, x9, 2";
		  x11 <== wrap16(x9 * 4);
		  debug insn "slli x12, x12, 2";
		  x12 <== wrap16(x12 * 4);
		many_chunks_dash_1054733ad03afc41___dot_Ltmp34:
		  debug loc 202 241 12;
		  debug insn "beqz x10, many_chunks_dash_1054733ad03afc41__.LBB1_7";
		  branch_if_zero x10, many_chunks_dash_1054733ad03afc41___dot_LBB1_7;
		many_chunks_dash_1054733ad03afc41___dot_Ltmp35:
		  debug loc 202 400 43;
		  debug insn "lw x13, 4(x8)";
		  x13, tmp1 <== mload(x8 + 4);
		many_chunks_dash_1054733ad03afc41___dot_Ltmp36:
		  debug loc 207 452 16;
		  debug insn "slli x10, x10, 2";
		  x10 <== wrap16(x10 * 4);
		many_chunks_dash_1054733ad03afc41___dot_Ltmp37:
		  debug loc 202 248 17;
		  debug insn "sw x13, 24(x2)";
		  mstore x2 + 24, x13;
		  debug insn "sw x10, 28(x2)";
		  mstore x2 + 28, x10;
		  debug insn "li x10, 4";
		  x10 <=X= 4;
		  debug insn "sw x10, 32(x2)";
		  mstore x2 + 32, x10;
		  debug insn "j many_chunks_dash_1054733ad03afc41__.LBB1_8";
		  jump many_chunks_dash_1054733ad03afc41___dot_LBB1_8;
		many_chunks_dash_1054733ad03afc41___dot_LBB1_7:
		  debug loc 202 242 13;
		  debug insn "sw x0, 32(x2)";
		  mstore x2 + 32, x0;
		many_chunks_dash_1054733ad03afc41___dot_Ltmp39:
		many_chunks_dash_1054733ad03afc41___dot_LBB1_8:
		  debug loc 202 400 19;
		  debug insn "addi x10, x2, 8";
		  x10 <== wrap(x2 + 8);
		  debug insn "addi x13, x2, 24";
		  x13 <== wrap(x2 + 24);
		  debug insn "call many_chunks_dash_1054733ad03afc41___ZN5alloc7raw_vec11finish_grow17h862016746d2f359dE";
		  call many_chunks_dash_1054733ad03afc41___ZN5alloc7raw_vec11finish_grow17h862016746d2f359dE;
		many_chunks_dash_1054733ad03afc41___dot_Ltmp40:
		  debug loc 203 2091 15;
		  debug insn "lw x11, 8(x2)";
		  x11, tmp1 <== mload(x2 + 8);
		  debug loc 203 2091 9;
		  debug insn "lw x10, 12(x2)";
		  x10, tmp1 <== mload(x2 + 12);
		many_chunks_dash_1054733ad03afc41___dot_Ltmp41:
		  debug loc 202 400 19;
		  debug insn "beqz x11, many_chunks_dash_1054733ad03afc41__.LBB1_12";
		  branch_if_zero x11, many_chunks_dash_1054733ad03afc41___dot_LBB1_12;
		many_chunks_dash_1054733ad03afc41___dot_Ltmp42:
		  debug loc 202 0 0;
		  debug insn "lw x11, 16(x2)";
		  x11, tmp1 <== mload(x2 + 16);
		  debug insn "lui x12, 524288";
		  x12 <=X= 2147483648;
		  debug insn "addi x12, x12, 1";
		  x12 <== wrap(x12 + 1);
		many_chunks_dash_1054733ad03afc41___dot_Ltmp43:
		  debug loc 202 488 5;
		  debug insn "beq x11, x12, many_chunks_dash_1054733ad03afc41__.LBB1_13";
		  branch_if_zero x11 - x12, many_chunks_dash_1054733ad03afc41___dot_LBB1_13;
		many_chunks_dash_1054733ad03afc41___dot_Ltmp44:
		  debug insn "bnez x11, many_chunks_dash_1054733ad03afc41__.LBB1_14";
		  branch_if_nonzero x11, many_chunks_dash_1054733ad03afc41___dot_LBB1_14;
		many_chunks_dash_1054733ad03afc41___dot_Ltmp45:
		many_chunks_dash_1054733ad03afc41___dot_LBB1_11:
		  debug loc 202 489 34;
		  debug insn "call _ZN5alloc7raw_vec17capacity_overflow17h2c1c04a797021fabE";
		  call _ZN5alloc7raw_vec17capacity_overflow17h2c1c04a797021fabE;
		  debug insn "unimp ";
		  fail;
		many_chunks_dash_1054733ad03afc41___dot_LBB1_12:
		  debug loc 202 368 9;
		  debug insn "sw x10, 4(x8)";
		  mstore x8 + 4, x10;
		  debug loc 202 369 9;
		  debug insn "sw x9, 0(x8)";
		  mstore x8 + 0, x9;
		many_chunks_dash_1054733ad03afc41___dot_Ltmp47:
		many_chunks_dash_1054733ad03afc41___dot_LBB1_13:
		  debug loc 202 299 6;
		  debug insn "lw x1, 44(x2)";
		  x1, tmp1 <== mload(x2 + 44);
		  debug insn "lw x8, 40(x2)";
		  x8, tmp1 <== mload(x2 + 40);
		many_chunks_dash_1054733ad03afc41___dot_Ltmp48:
		  debug insn "lw x9, 36(x2)";
		  x9, tmp1 <== mload(x2 + 36);
		  debug insn "addi x2, x2, 48";
		  x2 <== wrap(x2 + 48);
		  debug insn "ret ";
		  ret;
		many_chunks_dash_1054733ad03afc41___dot_LBB1_14:
		many_chunks_dash_1054733ad03afc41___dot_Ltmp49:
		  debug loc 202 490 43;
		  debug insn "call _ZN5alloc5alloc18handle_alloc_error17hd9333cd203b4ffb7E";
		  call _ZN5alloc5alloc18handle_alloc_error17hd9333cd203b4ffb7E;
		many_chunks_dash_1054733ad03afc41___dot_Ltmp50:
		  debug insn "unimp ";
		  fail;
		main:
		many_chunks_dash_1054733ad03afc41___dot_Lfunc_begin2:
		  debug loc 208 7 0;
		  debug insn "addi x2, x2, -16";
		  x2 <== wrap(x2 + 4294967280);
		many_chunks_dash_1054733ad03afc41___dot_Ltmp52:
		  debug loc 209 425 9;
		  debug insn "sw x1, 12(x2)";
		  mstore x2 + 12, x1;
		  debug insn "sw x0, 0(x2)";
		  mstore x2 + 0, x0;
		  debug insn "li x10, 4";
		  x10 <=X= 4;
		  debug insn "sw x10, 4(x2)";
		  mstore x2 + 4, x10;
		  debug insn "sw x0, 8(x2)";
		  mstore x2 + 8, x0;
		many_chunks_dash_1054733ad03afc41___dot_Ltmp53:
		  debug loc 209 1835 13;
		  debug insn "mv x10, x2";
		  x10 <=X= x2;
		  debug insn "li x11, 0";
		  x11 <=X= 0;
		  debug insn "call many_chunks_dash_1054733ad03afc41___ZN5alloc7raw_vec19RawVec$LT$T$C$A$GT$16reserve_for_push17hda58fa1bff2773ddE";
		  call many_chunks_dash_1054733ad03afc41___ZN5alloc7raw_vec19RawVec$LT$T$C$A$GT$16reserve_for_push17hda58fa1bff2773ddE;
		  debug loc 209 1838 45;
		  debug insn "lw x11, 8(x2)";
		  x11, tmp1 <== mload(x2 + 8);
		many_chunks_dash_1054733ad03afc41___dot_Ltmp54:
		  debug loc 202 224 9;
		  debug insn "lw x10, 4(x2)";
		  x10, tmp1 <== mload(x2 + 4);
		many_chunks_dash_1054733ad03afc41___dot_Ltmp55:
		  debug loc 210 499 18;
		  debug insn "slli x11, x11, 2";
		  x11 <== wrap16(x11 * 4);
		many_chunks_dash_1054733ad03afc41___dot_Ltmp56:
		  debug insn "add x12, x10, x11";
		  x12 <== wrap(x10 + x11);
		  debug insn "li x11, 1";
		  x11 <=X= 1;
		many_chunks_dash_1054733ad03afc41___dot_Ltmp57:
		  debug loc 211 1354 9;
		  debug insn "sw x11, 0(x12)";
		  mstore x12 + 0, x11;
		  debug insn "lui x12, 24";
		  x12 <=X= 98304;
		many_chunks_dash_1054733ad03afc41___dot_Ltmp58:
		  debug loc 211 0 9;
		  debug insn "addi x12, x12, 1696";
		  x12 <== wrap(x12 + 1696);
		  debug insn "li x14, 1";
		  x14 <=X= 1;
		many_chunks_dash_1054733ad03afc41___dot_Ltmp59:
		many_chunks_dash_1054733ad03afc41___dot_LBB2_1:
		  debug insn "mv x13, x14";
		  x13 <=X= x14;
		many_chunks_dash_1054733ad03afc41___dot_Ltmp60:
		  debug loc 206 1435 52;
		  debug insn "addi x12, x12, -1";
		  x12 <== wrap(x12 + 4294967295);
		many_chunks_dash_1054733ad03afc41___dot_Ltmp61:
		  debug loc 208 16 19;
		  debug insn "add x14, x14, x11";
		  x14 <== wrap(x14 + x11);
		many_chunks_dash_1054733ad03afc41___dot_Ltmp62:
		  debug loc 208 0 19;
		  debug insn "mv x11, x13";
		  x11 <=X= x13;
		many_chunks_dash_1054733ad03afc41___dot_Ltmp63:
		  debug loc 212 621 12;
		  debug insn "bnez x12, many_chunks_dash_1054733ad03afc41__.LBB2_1";
		  branch_if_nonzero x12, many_chunks_dash_1054733ad03afc41___dot_LBB2_1;
		many_chunks_dash_1054733ad03afc41___dot_Ltmp64:
		  debug loc 208 21 5;
		  debug insn "blez x13, many_chunks_dash_1054733ad03afc41__.LBB2_6";
		  tmp1 <== to_signed(x13);
		  branch_if_positive -tmp1 + 1, many_chunks_dash_1054733ad03afc41___dot_LBB2_6;
		many_chunks_dash_1054733ad03afc41___dot_Ltmp65:
		  debug loc 208 22 1;
		  debug insn "lw x11, 0(x2)";
		  x11, tmp1 <== mload(x2 + 0);
		many_chunks_dash_1054733ad03afc41___dot_Ltmp66:
		  debug loc 202 241 12;
		  debug insn "beqz x11, many_chunks_dash_1054733ad03afc41__.LBB2_5";
		  branch_if_zero x11, many_chunks_dash_1054733ad03afc41___dot_LBB2_5;
		many_chunks_dash_1054733ad03afc41___dot_Ltmp67:
		  debug loc 207 452 16;
		  debug insn "slli x11, x11, 2";
		  x11 <== wrap16(x11 * 4);
		many_chunks_dash_1054733ad03afc41___dot_Ltmp68:
		  debug loc 204 113 14;
		  debug insn "li x12, 4";
		  x12 <=X= 4;
		  debug insn "lw x1, 12(x2)";
		  x1, tmp1 <== mload(x2 + 12);
		  debug insn "addi x2, x2, 16";
		  x2 <== wrap(x2 + 16);
		  debug insn "tail __rg_dealloc";
		  jump __rg_dealloc;
		many_chunks_dash_1054733ad03afc41___dot_LBB2_5:
		  debug loc 208 22 2;
		  debug insn "lw x1, 12(x2)";
		  x1, tmp1 <== mload(x2 + 12);
		  debug insn "addi x2, x2, 16";
		  x2 <== wrap(x2 + 16);
		  debug insn "ret ";
		  ret;
		many_chunks_dash_1054733ad03afc41___dot_LBB2_6:
		many_chunks_dash_1054733ad03afc41___dot_Ltmp70:
		  debug loc 208 21 5;
		  debug insn "lui x10, 16";
		  x10 <=X= 65536;
		  debug insn "addi x10, x10, 732";
		  x10 <== wrap(x10 + 732);
		  debug insn "lui x11, 16";
		  x11 <=X= 65536;
		  debug insn "addi x12, x11, 768";
		  x12 <== wrap(x11 + 768);
		  debug insn "li x11, 23";
		  x11 <=X= 23;
		  debug insn "call _ZN4core9panicking5panic17h83c2c4098b58b628E";
		  call _ZN4core9panicking5panic17h83c2c4098b58b628E;
		many_chunks_dash_1054733ad03afc41___dot_Ltmp71:
		  debug insn "unimp ";
		  fail;
		runtime_dash_b33a6d6e449ccd5c___ZN44_$LT$$RF$T$u20$as$u20$core_dot__dot_fmt_dot__dot_Display$GT$3fmt17ha66a21a5de81b0beE:
		runtime_dash_b33a6d6e449ccd5c___dot_Lfunc_begin0:
		  debug loc 215 2372 0;
		  debug loc 215 2372 71;
		  debug insn "lw x10, 0(x10)";
		  x10, tmp1 <== mload(x10 + 0);
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp0:
		  debug loc 215 2372 62;
		  debug insn "tail _ZN73_$LT$core..panic..panic_info..PanicInfo$u20$as$u20$core..fmt..Display$GT$3fmt17h0360d72aed724b29E";
		  jump _ZN73_$LT$core_dot__dot_panic_dot__dot_panic_info_dot__dot_PanicInfo$u20$as$u20$core_dot__dot_fmt_dot__dot_Display$GT$3fmt17h0360d72aed724b29E;
		runtime_dash_b33a6d6e449ccd5c___ZN4core3fmt5Write10write_char17hcb7f8f8c82f6d9bfE:
		runtime_dash_b33a6d6e449ccd5c___dot_Lfunc_begin1:
		  debug loc 215 168 0;
		  debug insn "addi x2, x2, -16";
		  x2 <== wrap(x2 + 4294967280);
		  debug insn "li x10, 128";
		  x10 <=X= 128;
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp2:
		  debug loc 215 169 43;
		  debug insn "sw x0, 12(x2)";
		  mstore x2 + 12, x0;
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp3:
		  debug loc 216 1702 8;
		  debug insn "bgeu x11, x10, runtime_dash_b33a6d6e449ccd5c__.LBB1_2";
		  branch_if_positive x11 - x10 + 1, runtime_dash_b33a6d6e449ccd5c___dot_LBB1_2;
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp4:
		  debug loc 216 1733 13;
		  debug insn "sb x11, 12(x2)";
		  tmp1, tmp2 <== mload(x2 + 12);
		  tmp3 <== shl(0xff, 8 * tmp2);
		  tmp3 <== xor(tmp3, 0xffffffff);
		  tmp1 <== and(tmp1, tmp3);
		  tmp3 <== and(x11, 0xff);
		  tmp3 <== shl(tmp3, 8 * tmp2);
		  tmp1 <== or(tmp1, tmp3);
		  mstore x2 + 12 - tmp2, tmp1;
		  debug insn "li x11, 1";
		  x11 <=X= 1;
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp5:
		  debug loc 216 0 13;
		  debug insn "j runtime_dash_b33a6d6e449ccd5c__.LBB1_7";
		  jump runtime_dash_b33a6d6e449ccd5c___dot_LBB1_7;
		runtime_dash_b33a6d6e449ccd5c___dot_LBB1_2:
		  debug loc 216 1704 15;
		  debug insn "srli x10, x11, 11";
		  x10 <== shr(x11, 11);
		  debug insn "bnez x10, runtime_dash_b33a6d6e449ccd5c__.LBB1_4";
		  branch_if_nonzero x10, runtime_dash_b33a6d6e449ccd5c___dot_LBB1_4;
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp7:
		  debug loc 216 1736 19;
		  debug insn "srli x10, x11, 6";
		  x10 <== shr(x11, 6);
		  debug loc 216 1736 13;
		  debug insn "ori x10, x10, 192";
		  x10 <== or(x10, 192);
		  debug insn "sb x10, 12(x2)";
		  tmp1, tmp2 <== mload(x2 + 12);
		  tmp3 <== shl(0xff, 8 * tmp2);
		  tmp3 <== xor(tmp3, 0xffffffff);
		  tmp1 <== and(tmp1, tmp3);
		  tmp3 <== and(x10, 0xff);
		  tmp3 <== shl(tmp3, 8 * tmp2);
		  tmp1 <== or(tmp1, tmp3);
		  mstore x2 + 12 - tmp2, tmp1;
		  debug loc 216 1737 18;
		  debug insn "andi x10, x11, 63";
		  x10 <== and(x11, 63);
		  debug loc 216 1737 13;
		  debug insn "ori x10, x10, 128";
		  x10 <== or(x10, 128);
		  debug insn "sb x10, 13(x2)";
		  tmp1, tmp2 <== mload(x2 + 13);
		  tmp3 <== shl(0xff, 8 * tmp2);
		  tmp3 <== xor(tmp3, 0xffffffff);
		  tmp1 <== and(tmp1, tmp3);
		  tmp3 <== and(x10, 0xff);
		  tmp3 <== shl(tmp3, 8 * tmp2);
		  tmp1 <== or(tmp1, tmp3);
		  mstore x2 + 13 - tmp2, tmp1;
		  debug insn "li x11, 2";
		  x11 <=X= 2;
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp8:
		  debug loc 216 0 13;
		  debug insn "j runtime_dash_b33a6d6e449ccd5c__.LBB1_7";
		  jump runtime_dash_b33a6d6e449ccd5c___dot_LBB1_7;
		runtime_dash_b33a6d6e449ccd5c___dot_LBB1_4:
		  debug loc 216 1706 15;
		  debug insn "srli x10, x11, 16";
		  x10 <== shr(x11, 16);
		  debug loc 216 1706 12;
		  debug insn "bnez x10, runtime_dash_b33a6d6e449ccd5c__.LBB1_6";
		  branch_if_nonzero x10, runtime_dash_b33a6d6e449ccd5c___dot_LBB1_6;
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp10:
		  debug loc 216 1740 19;
		  debug insn "srli x10, x11, 12";
		  x10 <== shr(x11, 12);
		  debug loc 216 1740 13;
		  debug insn "ori x10, x10, 224";
		  x10 <== or(x10, 224);
		  debug insn "sb x10, 12(x2)";
		  tmp1, tmp2 <== mload(x2 + 12);
		  tmp3 <== shl(0xff, 8 * tmp2);
		  tmp3 <== xor(tmp3, 0xffffffff);
		  tmp1 <== and(tmp1, tmp3);
		  tmp3 <== and(x10, 0xff);
		  tmp3 <== shl(tmp3, 8 * tmp2);
		  tmp1 <== or(tmp1, tmp3);
		  mstore x2 + 12 - tmp2, tmp1;
		  debug loc 216 1741 18;
		  debug insn "slli x10, x11, 20";
		  tmp1 <== wrap16(x11 * 65536);
		  x10 <== wrap16(tmp1 * 16);
		  debug insn "srli x10, x10, 26";
		  x10 <== shr(x10, 26);
		  debug loc 216 1741 13;
		  debug insn "ori x10, x10, 128";
		  x10 <== or(x10, 128);
		  debug insn "sb x10, 13(x2)";
		  tmp1, tmp2 <== mload(x2 + 13);
		  tmp3 <== shl(0xff, 8 * tmp2);
		  tmp3 <== xor(tmp3, 0xffffffff);
		  tmp1 <== and(tmp1, tmp3);
		  tmp3 <== and(x10, 0xff);
		  tmp3 <== shl(tmp3, 8 * tmp2);
		  tmp1 <== or(tmp1, tmp3);
		  mstore x2 + 13 - tmp2, tmp1;
		  debug loc 216 1742 18;
		  debug insn "andi x10, x11, 63";
		  x10 <== and(x11, 63);
		  debug loc 216 1742 13;
		  debug insn "ori x10, x10, 128";
		  x10 <== or(x10, 128);
		  debug insn "sb x10, 14(x2)";
		  tmp1, tmp2 <== mload(x2 + 14);
		  tmp3 <== shl(0xff, 8 * tmp2);
		  tmp3 <== xor(tmp3, 0xffffffff);
		  tmp1 <== and(tmp1, tmp3);
		  tmp3 <== and(x10, 0xff);
		  tmp3 <== shl(tmp3, 8 * tmp2);
		  tmp1 <== or(tmp1, tmp3);
		  mstore x2 + 14 - tmp2, tmp1;
		  debug insn "li x11, 3";
		  x11 <=X= 3;
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp11:
		  debug loc 216 0 13;
		  debug insn "j runtime_dash_b33a6d6e449ccd5c__.LBB1_7";
		  jump runtime_dash_b33a6d6e449ccd5c___dot_LBB1_7;
		runtime_dash_b33a6d6e449ccd5c___dot_LBB1_6:
		  debug loc 216 1745 18;
		  debug insn "slli x10, x11, 11";
		  x10 <== wrap16(x11 * 2048);
		  debug insn "srli x10, x10, 29";
		  x10 <== shr(x10, 29);
		  debug loc 216 1745 13;
		  debug insn "ori x10, x10, 240";
		  x10 <== or(x10, 240);
		  debug insn "sb x10, 12(x2)";
		  tmp1, tmp2 <== mload(x2 + 12);
		  tmp3 <== shl(0xff, 8 * tmp2);
		  tmp3 <== xor(tmp3, 0xffffffff);
		  tmp1 <== and(tmp1, tmp3);
		  tmp3 <== and(x10, 0xff);
		  tmp3 <== shl(tmp3, 8 * tmp2);
		  tmp1 <== or(tmp1, tmp3);
		  mstore x2 + 12 - tmp2, tmp1;
		  debug loc 216 1746 18;
		  debug insn "slli x10, x11, 14";
		  x10 <== wrap16(x11 * 16384);
		  debug insn "srli x10, x10, 26";
		  x10 <== shr(x10, 26);
		  debug loc 216 1746 13;
		  debug insn "ori x10, x10, 128";
		  x10 <== or(x10, 128);
		  debug insn "sb x10, 13(x2)";
		  tmp1, tmp2 <== mload(x2 + 13);
		  tmp3 <== shl(0xff, 8 * tmp2);
		  tmp3 <== xor(tmp3, 0xffffffff);
		  tmp1 <== and(tmp1, tmp3);
		  tmp3 <== and(x10, 0xff);
		  tmp3 <== shl(tmp3, 8 * tmp2);
		  tmp1 <== or(tmp1, tmp3);
		  mstore x2 + 13 - tmp2, tmp1;
		  debug loc 216 1747 18;
		  debug insn "slli x10, x11, 20";
		  tmp1 <== wrap16(x11 * 65536);
		  x10 <== wrap16(tmp1 * 16);
		  debug insn "srli x10, x10, 26";
		  x10 <== shr(x10, 26);
		  debug loc 216 1747 13;
		  debug insn "ori x10, x10, 128";
		  x10 <== or(x10, 128);
		  debug insn "sb x10, 14(x2)";
		  tmp1, tmp2 <== mload(x2 + 14);
		  tmp3 <== shl(0xff, 8 * tmp2);
		  tmp3 <== xor(tmp3, 0xffffffff);
		  tmp1 <== and(tmp1, tmp3);
		  tmp3 <== and(x10, 0xff);
		  tmp3 <== shl(tmp3, 8 * tmp2);
		  tmp1 <== or(tmp1, tmp3);
		  mstore x2 + 14 - tmp2, tmp1;
		  debug loc 216 1748 18;
		  debug insn "andi x10, x11, 63";
		  x10 <== and(x11, 63);
		  debug loc 216 1748 13;
		  debug insn "ori x10, x10, 128";
		  x10 <== or(x10, 128);
		  debug insn "sb x10, 15(x2)";
		  tmp1, tmp2 <== mload(x2 + 15);
		  tmp3 <== shl(0xff, 8 * tmp2);
		  tmp3 <== xor(tmp3, 0xffffffff);
		  tmp1 <== and(tmp1, tmp3);
		  tmp3 <== and(x10, 0xff);
		  tmp3 <== shl(tmp3, 8 * tmp2);
		  tmp1 <== or(tmp1, tmp3);
		  mstore x2 + 15 - tmp2, tmp1;
		  debug insn "li x11, 4";
		  x11 <=X= 4;
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp13:
		runtime_dash_b33a6d6e449ccd5c___dot_LBB1_7:
		  debug loc 216 0 13;
		  debug insn "li x12, 0";
		  x12 <=X= 0;
		  debug insn "addi x13, x2, 12";
		  x13 <== wrap(x2 + 12);
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp14:
		runtime_dash_b33a6d6e449ccd5c___dot_LBB1_8:
		  debug loc 217 499 18;
		  debug insn "add x10, x13, x12";
		  x10 <== wrap(x13 + x12);
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp15:
		  debug loc 218 1804 19;
		  debug insn "lbu x10, 0(x10)";
		  x10, tmp2 <== mload(x10 + 0);
		  x10 <== shr(x10, 8 * tmp2);
		  x10 <== and(x10, 0xff);
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp16:
		  debug loc 219 38 9;
		  debug insn "ebreak ";
		  x0 <=X= ${ ("print_char", x10) };

		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp17:
		  debug loc 220 146 24;
		  debug insn "addi x12, x12, 1";
		  x12 <== wrap(x12 + 1);
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp18:
		  debug insn "bne x11, x12, runtime_dash_b33a6d6e449ccd5c__.LBB1_8";
		  branch_if_nonzero x11 - x12, runtime_dash_b33a6d6e449ccd5c___dot_LBB1_8;
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp19:
		  debug loc 215 170 6;
		  debug insn "li x10, 0";
		  x10 <=X= 0;
		  debug insn "addi x2, x2, 16";
		  x2 <== wrap(x2 + 16);
		  debug insn "ret ";
		  ret;
		runtime_dash_b33a6d6e449ccd5c___ZN4core3fmt5Write9write_fmt17he9cd6fc9cbb882bfE:
		runtime_dash_b33a6d6e449ccd5c___dot_Lfunc_begin2:
		  debug loc 215 191 0;
		  debug insn "addi x2, x2, -48";
		  x2 <== wrap(x2 + 4294967248);
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp21:
		  debug loc 215 192 26;
		  debug insn "sw x1, 44(x2)";
		  mstore x2 + 44, x1;
		  debug insn "lw x12, 20(x11)";
		  x12, tmp1 <== mload(x11 + 20);
		  debug insn "lw x13, 16(x11)";
		  x13, tmp1 <== mload(x11 + 16);
		  debug insn "sw x10, 12(x2)";
		  mstore x2 + 12, x10;
		  debug insn "sw x12, 36(x2)";
		  mstore x2 + 36, x12;
		  debug insn "sw x13, 32(x2)";
		  mstore x2 + 32, x13;
		  debug insn "lw x10, 12(x11)";
		  x10, tmp1 <== mload(x11 + 12);
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp22:
		  debug insn "lw x12, 8(x11)";
		  x12, tmp1 <== mload(x11 + 8);
		  debug insn "lw x13, 4(x11)";
		  x13, tmp1 <== mload(x11 + 4);
		  debug insn "lw x11, 0(x11)";
		  x11, tmp1 <== mload(x11 + 0);
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp23:
		  debug insn "sw x10, 28(x2)";
		  mstore x2 + 28, x10;
		  debug insn "sw x12, 24(x2)";
		  mstore x2 + 24, x12;
		  debug insn "sw x13, 20(x2)";
		  mstore x2 + 20, x13;
		  debug insn "sw x11, 16(x2)";
		  mstore x2 + 16, x11;
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp24:
		  debug loc 215 192 9;
		  debug insn "lui x10, 16";
		  x10 <=X= 65536;
		  debug insn "addi x11, x10, 784";
		  x11 <== wrap(x10 + 784);
		  debug insn "addi x10, x2, 12";
		  x10 <== wrap(x2 + 12);
		  debug insn "addi x12, x2, 16";
		  x12 <== wrap(x2 + 16);
		  debug insn "call _ZN4core3fmt5write17h87a75934cc3b911aE";
		  call _ZN4core3fmt5write17h87a75934cc3b911aE;
		  debug loc 215 193 6;
		  debug insn "lw x1, 44(x2)";
		  x1, tmp1 <== mload(x2 + 44);
		  debug insn "addi x2, x2, 48";
		  x2 <== wrap(x2 + 48);
		  debug insn "ret ";
		  ret;
		runtime_dash_b33a6d6e449ccd5c___ZN4core3ptr37drop_in_place$LT$core_dot__dot_fmt_dot__dot_Error$GT$17hac39e9c214b967d0E:
		runtime_dash_b33a6d6e449ccd5c___dot_Lfunc_begin3:
		  debug loc 223 490 0;
		  debug loc 223 490 1;
		  debug insn "ret ";
		  ret;
		runtime_dash_b33a6d6e449ccd5c___ZN50_$LT$$RF$mut$u20$W$u20$as$u20$core_dot__dot_fmt_dot__dot_Write$GT$10write_char17hbac5473a26c1dbe9E:
		runtime_dash_b33a6d6e449ccd5c___dot_Lfunc_begin4:
		  debug loc 215 202 0;
		  debug insn "addi x2, x2, -16";
		  x2 <== wrap(x2 + 4294967280);
		  debug insn "li x10, 128";
		  x10 <=X= 128;
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp27:
		  debug loc 215 169 43;
		  debug insn "sw x0, 12(x2)";
		  mstore x2 + 12, x0;
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp28:
		  debug loc 216 1702 8;
		  debug insn "bgeu x11, x10, runtime_dash_b33a6d6e449ccd5c__.LBB4_2";
		  branch_if_positive x11 - x10 + 1, runtime_dash_b33a6d6e449ccd5c___dot_LBB4_2;
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp29:
		  debug loc 216 1733 13;
		  debug insn "sb x11, 12(x2)";
		  tmp1, tmp2 <== mload(x2 + 12);
		  tmp3 <== shl(0xff, 8 * tmp2);
		  tmp3 <== xor(tmp3, 0xffffffff);
		  tmp1 <== and(tmp1, tmp3);
		  tmp3 <== and(x11, 0xff);
		  tmp3 <== shl(tmp3, 8 * tmp2);
		  tmp1 <== or(tmp1, tmp3);
		  mstore x2 + 12 - tmp2, tmp1;
		  debug insn "li x11, 1";
		  x11 <=X= 1;
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp30:
		  debug loc 216 0 13;
		  debug insn "j runtime_dash_b33a6d6e449ccd5c__.LBB4_7";
		  jump runtime_dash_b33a6d6e449ccd5c___dot_LBB4_7;
		runtime_dash_b33a6d6e449ccd5c___dot_LBB4_2:
		  debug loc 216 1704 15;
		  debug insn "srli x10, x11, 11";
		  x10 <== shr(x11, 11);
		  debug insn "bnez x10, runtime_dash_b33a6d6e449ccd5c__.LBB4_4";
		  branch_if_nonzero x10, runtime_dash_b33a6d6e449ccd5c___dot_LBB4_4;
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp32:
		  debug loc 216 1736 19;
		  debug insn "srli x10, x11, 6";
		  x10 <== shr(x11, 6);
		  debug loc 216 1736 13;
		  debug insn "ori x10, x10, 192";
		  x10 <== or(x10, 192);
		  debug insn "sb x10, 12(x2)";
		  tmp1, tmp2 <== mload(x2 + 12);
		  tmp3 <== shl(0xff, 8 * tmp2);
		  tmp3 <== xor(tmp3, 0xffffffff);
		  tmp1 <== and(tmp1, tmp3);
		  tmp3 <== and(x10, 0xff);
		  tmp3 <== shl(tmp3, 8 * tmp2);
		  tmp1 <== or(tmp1, tmp3);
		  mstore x2 + 12 - tmp2, tmp1;
		  debug loc 216 1737 18;
		  debug insn "andi x10, x11, 63";
		  x10 <== and(x11, 63);
		  debug loc 216 1737 13;
		  debug insn "ori x10, x10, 128";
		  x10 <== or(x10, 128);
		  debug insn "sb x10, 13(x2)";
		  tmp1, tmp2 <== mload(x2 + 13);
		  tmp3 <== shl(0xff, 8 * tmp2);
		  tmp3 <== xor(tmp3, 0xffffffff);
		  tmp1 <== and(tmp1, tmp3);
		  tmp3 <== and(x10, 0xff);
		  tmp3 <== shl(tmp3, 8 * tmp2);
		  tmp1 <== or(tmp1, tmp3);
		  mstore x2 + 13 - tmp2, tmp1;
		  debug insn "li x11, 2";
		  x11 <=X= 2;
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp33:
		  debug loc 216 0 13;
		  debug insn "j runtime_dash_b33a6d6e449ccd5c__.LBB4_7";
		  jump runtime_dash_b33a6d6e449ccd5c___dot_LBB4_7;
		runtime_dash_b33a6d6e449ccd5c___dot_LBB4_4:
		  debug loc 216 1706 15;
		  debug insn "srli x10, x11, 16";
		  x10 <== shr(x11, 16);
		  debug loc 216 1706 12;
		  debug insn "bnez x10, runtime_dash_b33a6d6e449ccd5c__.LBB4_6";
		  branch_if_nonzero x10, runtime_dash_b33a6d6e449ccd5c___dot_LBB4_6;
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp35:
		  debug loc 216 1740 19;
		  debug insn "srli x10, x11, 12";
		  x10 <== shr(x11, 12);
		  debug loc 216 1740 13;
		  debug insn "ori x10, x10, 224";
		  x10 <== or(x10, 224);
		  debug insn "sb x10, 12(x2)";
		  tmp1, tmp2 <== mload(x2 + 12);
		  tmp3 <== shl(0xff, 8 * tmp2);
		  tmp3 <== xor(tmp3, 0xffffffff);
		  tmp1 <== and(tmp1, tmp3);
		  tmp3 <== and(x10, 0xff);
		  tmp3 <== shl(tmp3, 8 * tmp2);
		  tmp1 <== or(tmp1, tmp3);
		  mstore x2 + 12 - tmp2, tmp1;
		  debug loc 216 1741 18;
		  debug insn "slli x10, x11, 20";
		  tmp1 <== wrap16(x11 * 65536);
		  x10 <== wrap16(tmp1 * 16);
		  debug insn "srli x10, x10, 26";
		  x10 <== shr(x10, 26);
		  debug loc 216 1741 13;
		  debug insn "ori x10, x10, 128";
		  x10 <== or(x10, 128);
		  debug insn "sb x10, 13(x2)";
		  tmp1, tmp2 <== mload(x2 + 13);
		  tmp3 <== shl(0xff, 8 * tmp2);
		  tmp3 <== xor(tmp3, 0xffffffff);
		  tmp1 <== and(tmp1, tmp3);
		  tmp3 <== and(x10, 0xff);
		  tmp3 <== shl(tmp3, 8 * tmp2);
		  tmp1 <== or(tmp1, tmp3);
		  mstore x2 + 13 - tmp2, tmp1;
		  debug loc 216 1742 18;
		  debug insn "andi x10, x11, 63";
		  x10 <== and(x11, 63);
		  debug loc 216 1742 13;
		  debug insn "ori x10, x10, 128";
		  x10 <== or(x10, 128);
		  debug insn "sb x10, 14(x2)";
		  tmp1, tmp2 <== mload(x2 + 14);
		  tmp3 <== shl(0xff, 8 * tmp2);
		  tmp3 <== xor(tmp3, 0xffffffff);
		  tmp1 <== and(tmp1, tmp3);
		  tmp3 <== and(x10, 0xff);
		  tmp3 <== shl(tmp3, 8 * tmp2);
		  tmp1 <== or(tmp1, tmp3);
		  mstore x2 + 14 - tmp2, tmp1;
		  debug insn "li x11, 3";
		  x11 <=X= 3;
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp36:
		  debug loc 216 0 13;
		  debug insn "j runtime_dash_b33a6d6e449ccd5c__.LBB4_7";
		  jump runtime_dash_b33a6d6e449ccd5c___dot_LBB4_7;
		runtime_dash_b33a6d6e449ccd5c___dot_LBB4_6:
		  debug loc 216 1745 18;
		  debug insn "slli x10, x11, 11";
		  x10 <== wrap16(x11 * 2048);
		  debug insn "srli x10, x10, 29";
		  x10 <== shr(x10, 29);
		  debug loc 216 1745 13;
		  debug insn "ori x10, x10, 240";
		  x10 <== or(x10, 240);
		  debug insn "sb x10, 12(x2)";
		  tmp1, tmp2 <== mload(x2 + 12);
		  tmp3 <== shl(0xff, 8 * tmp2);
		  tmp3 <== xor(tmp3, 0xffffffff);
		  tmp1 <== and(tmp1, tmp3);
		  tmp3 <== and(x10, 0xff);
		  tmp3 <== shl(tmp3, 8 * tmp2);
		  tmp1 <== or(tmp1, tmp3);
		  mstore x2 + 12 - tmp2, tmp1;
		  debug loc 216 1746 18;
		  debug insn "slli x10, x11, 14";
		  x10 <== wrap16(x11 * 16384);
		  debug insn "srli x10, x10, 26";
		  x10 <== shr(x10, 26);
		  debug loc 216 1746 13;
		  debug insn "ori x10, x10, 128";
		  x10 <== or(x10, 128);
		  debug insn "sb x10, 13(x2)";
		  tmp1, tmp2 <== mload(x2 + 13);
		  tmp3 <== shl(0xff, 8 * tmp2);
		  tmp3 <== xor(tmp3, 0xffffffff);
		  tmp1 <== and(tmp1, tmp3);
		  tmp3 <== and(x10, 0xff);
		  tmp3 <== shl(tmp3, 8 * tmp2);
		  tmp1 <== or(tmp1, tmp3);
		  mstore x2 + 13 - tmp2, tmp1;
		  debug loc 216 1747 18;
		  debug insn "slli x10, x11, 20";
		  tmp1 <== wrap16(x11 * 65536);
		  x10 <== wrap16(tmp1 * 16);
		  debug insn "srli x10, x10, 26";
		  x10 <== shr(x10, 26);
		  debug loc 216 1747 13;
		  debug insn "ori x10, x10, 128";
		  x10 <== or(x10, 128);
		  debug insn "sb x10, 14(x2)";
		  tmp1, tmp2 <== mload(x2 + 14);
		  tmp3 <== shl(0xff, 8 * tmp2);
		  tmp3 <== xor(tmp3, 0xffffffff);
		  tmp1 <== and(tmp1, tmp3);
		  tmp3 <== and(x10, 0xff);
		  tmp3 <== shl(tmp3, 8 * tmp2);
		  tmp1 <== or(tmp1, tmp3);
		  mstore x2 + 14 - tmp2, tmp1;
		  debug loc 216 1748 18;
		  debug insn "andi x10, x11, 63";
		  x10 <== and(x11, 63);
		  debug loc 216 1748 13;
		  debug insn "ori x10, x10, 128";
		  x10 <== or(x10, 128);
		  debug insn "sb x10, 15(x2)";
		  tmp1, tmp2 <== mload(x2 + 15);
		  tmp3 <== shl(0xff, 8 * tmp2);
		  tmp3 <== xor(tmp3, 0xffffffff);
		  tmp1 <== and(tmp1, tmp3);
		  tmp3 <== and(x10, 0xff);
		  tmp3 <== shl(tmp3, 8 * tmp2);
		  tmp1 <== or(tmp1, tmp3);
		  mstore x2 + 15 - tmp2, tmp1;
		  debug insn "li x11, 4";
		  x11 <=X= 4;
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp38:
		runtime_dash_b33a6d6e449ccd5c___dot_LBB4_7:
		  debug loc 216 0 13;
		  debug insn "li x12, 0";
		  x12 <=X= 0;
		  debug insn "addi x13, x2, 12";
		  x13 <== wrap(x2 + 12);
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp39:
		runtime_dash_b33a6d6e449ccd5c___dot_LBB4_8:
		  debug loc 217 499 18;
		  debug insn "add x10, x13, x12";
		  x10 <== wrap(x13 + x12);
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp40:
		  debug loc 218 1804 19;
		  debug insn "lbu x10, 0(x10)";
		  x10, tmp2 <== mload(x10 + 0);
		  x10 <== shr(x10, 8 * tmp2);
		  x10 <== and(x10, 0xff);
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp41:
		  debug loc 219 38 9;
		  debug insn "ebreak ";
		  x0 <=X= ${ ("print_char", x10) };

		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp42:
		  debug loc 220 146 24;
		  debug insn "addi x12, x12, 1";
		  x12 <== wrap(x12 + 1);
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp43:
		  debug insn "bne x11, x12, runtime_dash_b33a6d6e449ccd5c__.LBB4_8";
		  branch_if_nonzero x11 - x12, runtime_dash_b33a6d6e449ccd5c___dot_LBB4_8;
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp44:
		  debug loc 215 204 6;
		  debug insn "li x10, 0";
		  x10 <=X= 0;
		  debug insn "addi x2, x2, 16";
		  x2 <== wrap(x2 + 16);
		  debug insn "ret ";
		  ret;
		runtime_dash_b33a6d6e449ccd5c___ZN50_$LT$$RF$mut$u20$W$u20$as$u20$core_dot__dot_fmt_dot__dot_Write$GT$9write_fmt17h9360d1a9672f9147E:
		runtime_dash_b33a6d6e449ccd5c___dot_Lfunc_begin5:
		  debug loc 215 206 0;
		  debug insn "addi x2, x2, -48";
		  x2 <== wrap(x2 + 4294967248);
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp46:
		  debug loc 215 207 9;
		  debug insn "sw x1, 44(x2)";
		  mstore x2 + 44, x1;
		  debug insn "lw x10, 0(x10)";
		  x10, tmp1 <== mload(x10 + 0);
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp47:
		  debug loc 215 192 26;
		  debug insn "lw x12, 20(x11)";
		  x12, tmp1 <== mload(x11 + 20);
		  debug insn "lw x13, 16(x11)";
		  x13, tmp1 <== mload(x11 + 16);
		  debug insn "sw x10, 12(x2)";
		  mstore x2 + 12, x10;
		  debug insn "sw x12, 36(x2)";
		  mstore x2 + 36, x12;
		  debug insn "sw x13, 32(x2)";
		  mstore x2 + 32, x13;
		  debug insn "lw x10, 12(x11)";
		  x10, tmp1 <== mload(x11 + 12);
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp48:
		  debug insn "lw x12, 8(x11)";
		  x12, tmp1 <== mload(x11 + 8);
		  debug insn "lw x13, 4(x11)";
		  x13, tmp1 <== mload(x11 + 4);
		  debug insn "lw x11, 0(x11)";
		  x11, tmp1 <== mload(x11 + 0);
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp49:
		  debug insn "sw x10, 28(x2)";
		  mstore x2 + 28, x10;
		  debug insn "sw x12, 24(x2)";
		  mstore x2 + 24, x12;
		  debug insn "sw x13, 20(x2)";
		  mstore x2 + 20, x13;
		  debug insn "sw x11, 16(x2)";
		  mstore x2 + 16, x11;
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp50:
		  debug loc 215 192 9;
		  debug insn "lui x10, 16";
		  x10 <=X= 65536;
		  debug insn "addi x11, x10, 784";
		  x11 <== wrap(x10 + 784);
		  debug insn "addi x10, x2, 12";
		  x10 <== wrap(x2 + 12);
		  debug insn "addi x12, x2, 16";
		  x12 <== wrap(x2 + 16);
		  debug insn "call _ZN4core3fmt5write17h87a75934cc3b911aE";
		  call _ZN4core3fmt5write17h87a75934cc3b911aE;
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp51:
		  debug loc 215 208 6;
		  debug insn "lw x1, 44(x2)";
		  x1, tmp1 <== mload(x2 + 44);
		  debug insn "addi x2, x2, 48";
		  x2 <== wrap(x2 + 48);
		  debug insn "ret ";
		  ret;
		runtime_dash_b33a6d6e449ccd5c___ZN50_$LT$$RF$mut$u20$W$u20$as$u20$core_dot__dot_fmt_dot__dot_Write$GT$9write_str17h87da48f65744beb5E:
		runtime_dash_b33a6d6e449ccd5c___dot_Lfunc_begin6:
		  debug loc 215 198 0;
		  debug loc 220 146 24;
		  debug insn "beqz x12, runtime_dash_b33a6d6e449ccd5c__.LBB6_2";
		  branch_if_zero x12, runtime_dash_b33a6d6e449ccd5c___dot_LBB6_2;
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp53:
		runtime_dash_b33a6d6e449ccd5c___dot_LBB6_1:
		  debug loc 218 1804 19;
		  debug insn "lbu x10, 0(x11)";
		  x10, tmp2 <== mload(x11 + 0);
		  x10 <== shr(x10, 8 * tmp2);
		  x10 <== and(x10, 0xff);
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp54:
		  debug loc 219 38 9;
		  debug insn "ebreak ";
		  x0 <=X= ${ ("print_char", x10) };

		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp55:
		  debug loc 217 499 18;
		  debug insn "addi x11, x11, 1";
		  x11 <== wrap(x11 + 1);
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp56:
		  debug loc 220 146 24;
		  debug insn "addi x12, x12, -1";
		  x12 <== wrap(x12 + 4294967295);
		  debug insn "bnez x12, runtime_dash_b33a6d6e449ccd5c__.LBB6_1";
		  branch_if_nonzero x12, runtime_dash_b33a6d6e449ccd5c___dot_LBB6_1;
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp57:
		runtime_dash_b33a6d6e449ccd5c___dot_LBB6_2:
		  debug loc 215 200 6;
		  debug insn "li x10, 0";
		  x10 <=X= 0;
		  debug insn "ret ";
		  ret;
		_ZN63_$LT$runtime_dot__dot_fmt_dot__dot_ProverWriter$u20$as$u20$core_dot__dot_fmt_dot__dot_Write$GT$9write_str17h56ede652d1afdb54E:
		runtime_dash_b33a6d6e449ccd5c___dot_Lfunc_begin12:
		  debug loc 219 18 0;
		  debug loc 220 146 24;
		  debug insn "beqz x12, runtime_dash_b33a6d6e449ccd5c__.LBB12_2";
		  branch_if_zero x12, runtime_dash_b33a6d6e449ccd5c___dot_LBB12_2;
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp184:
		runtime_dash_b33a6d6e449ccd5c___dot_LBB12_1:
		  debug loc 218 1804 19;
		  debug insn "lbu x10, 0(x11)";
		  x10, tmp2 <== mload(x11 + 0);
		  x10 <== shr(x10, 8 * tmp2);
		  x10 <== and(x10, 0xff);
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp185:
		  debug loc 219 38 9;
		  debug insn "ebreak ";
		  x0 <=X= ${ ("print_char", x10) };

		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp186:
		  debug loc 217 499 18;
		  debug insn "addi x11, x11, 1";
		  x11 <== wrap(x11 + 1);
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp187:
		  debug loc 220 146 24;
		  debug insn "addi x12, x12, -1";
		  x12 <== wrap(x12 + 4294967295);
		  debug insn "bnez x12, runtime_dash_b33a6d6e449ccd5c__.LBB12_1";
		  branch_if_nonzero x12, runtime_dash_b33a6d6e449ccd5c___dot_LBB12_1;
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp188:
		runtime_dash_b33a6d6e449ccd5c___dot_LBB12_2:
		  debug loc 219 21 6;
		  debug insn "li x10, 0";
		  x10 <=X= 0;
		  debug insn "ret ";
		  ret;
		rust_begin_unwind:
		runtime_dash_b33a6d6e449ccd5c___dot_Lfunc_begin14:
		  debug loc 213 19 0;
		  debug insn "addi x2, x2, -48";
		  x2 <== wrap(x2 + 4294967248);
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp197:
		  debug loc 213 22 9;
		  debug insn "sw x1, 44(x2)";
		  mstore x2 + 44, x1;
		  debug insn "lui x11, 16";
		  x11 <=X= 65536;
		  debug insn "lbu x12, 948(x11)";
		  x12, tmp2 <== mload(x11 + 948);
		  x12 <== shr(x12, 8 * tmp2);
		  x12 <== and(x12, 0xff);
		  debug insn "sw x10, 4(x2)";
		  mstore x2 + 4, x10;
		  debug loc 213 22 8;
		  debug insn "beqz x12, runtime_dash_b33a6d6e449ccd5c__.LBB14_2";
		  branch_if_zero x12, runtime_dash_b33a6d6e449ccd5c___dot_LBB14_2;
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp198:
		  debug loc 219 38 9;
		  debug insn "li x10, 80";
		  x10 <=X= 80;
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp199:
		  debug insn "ebreak ";
		  x0 <=X= ${ ("print_char", x10) };

		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp200:
		  debug insn "li x10, 97";
		  x10 <=X= 97;
		  debug insn "ebreak ";
		  x0 <=X= ${ ("print_char", x10) };

		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp201:
		  debug insn "li x10, 110";
		  x10 <=X= 110;
		  debug insn "ebreak ";
		  x0 <=X= ${ ("print_char", x10) };

		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp202:
		  debug insn "li x10, 105";
		  x10 <=X= 105;
		  debug insn "ebreak ";
		  x0 <=X= ${ ("print_char", x10) };

		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp203:
		  debug insn "li x10, 99";
		  x10 <=X= 99;
		  debug insn "ebreak ";
		  x0 <=X= ${ ("print_char", x10) };

		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp204:
		  debug insn "li x10, 32";
		  x10 <=X= 32;
		  debug insn "ebreak ";
		  x0 <=X= ${ ("print_char", x10) };

		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp205:
		  debug insn "li x10, 104";
		  x10 <=X= 104;
		  debug insn "ebreak ";
		  x0 <=X= ${ ("print_char", x10) };

		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp206:
		  debug insn "li x10, 97";
		  x10 <=X= 97;
		  debug insn "ebreak ";
		  x0 <=X= ${ ("print_char", x10) };

		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp207:
		  debug insn "li x10, 110";
		  x10 <=X= 110;
		  debug insn "ebreak ";
		  x0 <=X= ${ ("print_char", x10) };

		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp208:
		  debug insn "li x10, 100";
		  x10 <=X= 100;
		  debug insn "ebreak ";
		  x0 <=X= ${ ("print_char", x10) };

		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp209:
		  debug insn "li x10, 108";
		  x10 <=X= 108;
		  debug insn "ebreak ";
		  x0 <=X= ${ ("print_char", x10) };

		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp210:
		  debug insn "li x10, 101";
		  x10 <=X= 101;
		  debug insn "ebreak ";
		  x0 <=X= ${ ("print_char", x10) };

		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp211:
		  debug insn "li x10, 114";
		  x10 <=X= 114;
		  debug insn "ebreak ";
		  x0 <=X= ${ ("print_char", x10) };

		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp212:
		  debug insn "li x10, 32";
		  x10 <=X= 32;
		  debug insn "ebreak ";
		  x0 <=X= ${ ("print_char", x10) };

		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp213:
		  debug insn "li x10, 104";
		  x10 <=X= 104;
		  debug insn "ebreak ";
		  x0 <=X= ${ ("print_char", x10) };

		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp214:
		  debug insn "li x10, 97";
		  x10 <=X= 97;
		  debug insn "ebreak ";
		  x0 <=X= ${ ("print_char", x10) };

		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp215:
		  debug insn "li x10, 115";
		  x10 <=X= 115;
		  debug insn "ebreak ";
		  x0 <=X= ${ ("print_char", x10) };

		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp216:
		  debug insn "li x10, 32";
		  x10 <=X= 32;
		  debug insn "ebreak ";
		  x0 <=X= ${ ("print_char", x10) };

		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp217:
		  debug insn "li x10, 112";
		  x10 <=X= 112;
		  debug insn "ebreak ";
		  x0 <=X= ${ ("print_char", x10) };

		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp218:
		  debug insn "li x10, 97";
		  x10 <=X= 97;
		  debug insn "ebreak ";
		  x0 <=X= ${ ("print_char", x10) };

		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp219:
		  debug insn "li x10, 110";
		  x10 <=X= 110;
		  debug insn "ebreak ";
		  x0 <=X= ${ ("print_char", x10) };

		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp220:
		  debug insn "li x10, 105";
		  x10 <=X= 105;
		  debug insn "ebreak ";
		  x0 <=X= ${ ("print_char", x10) };

		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp221:
		  debug insn "li x10, 99";
		  x10 <=X= 99;
		  debug insn "ebreak ";
		  x0 <=X= ${ ("print_char", x10) };

		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp222:
		  debug insn "li x10, 107";
		  x10 <=X= 107;
		  debug insn "ebreak ";
		  x0 <=X= ${ ("print_char", x10) };

		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp223:
		  debug insn "li x10, 101";
		  x10 <=X= 101;
		  debug insn "ebreak ";
		  x0 <=X= ${ ("print_char", x10) };

		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp224:
		  debug insn "li x10, 100";
		  x10 <=X= 100;
		  debug insn "ebreak ";
		  x0 <=X= ${ ("print_char", x10) };

		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp225:
		  debug insn "li x10, 33";
		  x10 <=X= 33;
		  debug insn "ebreak ";
		  x0 <=X= ${ ("print_char", x10) };

		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp226:
		  debug insn "li x10, 32";
		  x10 <=X= 32;
		  debug insn "ebreak ";
		  x0 <=X= ${ ("print_char", x10) };

		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp227:
		  debug insn "li x10, 84";
		  x10 <=X= 84;
		  debug insn "ebreak ";
		  x0 <=X= ${ ("print_char", x10) };

		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp228:
		  debug insn "li x10, 104";
		  x10 <=X= 104;
		  debug insn "ebreak ";
		  x0 <=X= ${ ("print_char", x10) };

		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp229:
		  debug insn "li x10, 105";
		  x10 <=X= 105;
		  debug insn "ebreak ";
		  x0 <=X= ${ ("print_char", x10) };

		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp230:
		  debug insn "li x10, 110";
		  x10 <=X= 110;
		  debug insn "ebreak ";
		  x0 <=X= ${ ("print_char", x10) };

		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp231:
		  debug insn "li x10, 103";
		  x10 <=X= 103;
		  debug insn "ebreak ";
		  x0 <=X= ${ ("print_char", x10) };

		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp232:
		  debug insn "li x10, 115";
		  x10 <=X= 115;
		  debug insn "ebreak ";
		  x0 <=X= ${ ("print_char", x10) };

		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp233:
		  debug insn "li x10, 32";
		  x10 <=X= 32;
		  debug insn "ebreak ";
		  x0 <=X= ${ ("print_char", x10) };

		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp234:
		  debug insn "li x10, 97";
		  x10 <=X= 97;
		  debug insn "ebreak ";
		  x0 <=X= ${ ("print_char", x10) };

		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp235:
		  debug insn "li x10, 114";
		  x10 <=X= 114;
		  debug insn "ebreak ";
		  x0 <=X= ${ ("print_char", x10) };

		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp236:
		  debug insn "li x10, 101";
		  x10 <=X= 101;
		  debug insn "ebreak ";
		  x0 <=X= ${ ("print_char", x10) };

		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp237:
		  debug insn "li x10, 32";
		  x10 <=X= 32;
		  debug insn "ebreak ";
		  x0 <=X= ${ ("print_char", x10) };

		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp238:
		  debug insn "li x10, 118";
		  x10 <=X= 118;
		  debug insn "ebreak ";
		  x0 <=X= ${ ("print_char", x10) };

		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp239:
		  debug insn "li x10, 101";
		  x10 <=X= 101;
		  debug insn "ebreak ";
		  x0 <=X= ${ ("print_char", x10) };

		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp240:
		  debug insn "li x10, 114";
		  x10 <=X= 114;
		  debug insn "ebreak ";
		  x0 <=X= ${ ("print_char", x10) };

		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp241:
		  debug insn "li x10, 121";
		  x10 <=X= 121;
		  debug insn "ebreak ";
		  x0 <=X= ${ ("print_char", x10) };

		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp242:
		  debug insn "li x10, 32";
		  x10 <=X= 32;
		  debug insn "ebreak ";
		  x0 <=X= ${ ("print_char", x10) };

		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp243:
		  debug insn "li x10, 100";
		  x10 <=X= 100;
		  debug insn "ebreak ";
		  x0 <=X= ${ ("print_char", x10) };

		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp244:
		  debug insn "li x10, 105";
		  x10 <=X= 105;
		  debug insn "ebreak ";
		  x0 <=X= ${ ("print_char", x10) };

		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp245:
		  debug insn "li x10, 114";
		  x10 <=X= 114;
		  debug insn "ebreak ";
		  x0 <=X= ${ ("print_char", x10) };

		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp246:
		  debug insn "li x10, 101";
		  x10 <=X= 101;
		  debug insn "ebreak ";
		  x0 <=X= ${ ("print_char", x10) };

		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp247:
		  debug insn "li x10, 32";
		  x10 <=X= 32;
		  debug insn "ebreak ";
		  x0 <=X= ${ ("print_char", x10) };

		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp248:
		  debug insn "li x10, 105";
		  x10 <=X= 105;
		  debug insn "ebreak ";
		  x0 <=X= ${ ("print_char", x10) };

		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp249:
		  debug insn "li x10, 110";
		  x10 <=X= 110;
		  debug insn "ebreak ";
		  x0 <=X= ${ ("print_char", x10) };

		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp250:
		  debug insn "li x10, 100";
		  x10 <=X= 100;
		  debug insn "ebreak ";
		  x0 <=X= ${ ("print_char", x10) };

		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp251:
		  debug insn "li x10, 101";
		  x10 <=X= 101;
		  debug insn "ebreak ";
		  x0 <=X= ${ ("print_char", x10) };

		  debug insn "li x10, 101";
		  x10 <=X= 101;
		  debug insn "ebreak ";
		  x0 <=X= ${ ("print_char", x10) };

		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp252:
		  debug insn "li x10, 100";
		  x10 <=X= 100;
		  debug insn "ebreak ";
		  x0 <=X= ${ ("print_char", x10) };

		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp253:
		  debug insn "li x10, 46";
		  x10 <=X= 46;
		  debug insn "ebreak ";
		  x0 <=X= ${ ("print_char", x10) };

		  debug insn "li x10, 46";
		  x10 <=X= 46;
		  debug insn "ebreak ";
		  x0 <=X= ${ ("print_char", x10) };

		  debug insn "li x10, 46";
		  x10 <=X= 46;
		  debug insn "ebreak ";
		  x0 <=X= ${ ("print_char", x10) };

		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp254:
		  debug insn "li x10, 10";
		  x10 <=X= 10;
		  debug insn "ebreak ";
		  x0 <=X= ${ ("print_char", x10) };

		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp255:
		  debug loc 219 0 9;
		  debug insn "j runtime_dash_b33a6d6e449ccd5c__.LBB14_3";
		  jump runtime_dash_b33a6d6e449ccd5c___dot_LBB14_3;
		runtime_dash_b33a6d6e449ccd5c___dot_LBB14_2:
		  debug insn "li x10, 1";
		  x10 <=X= 1;
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp257:
		  debug loc 213 23 9;
		  debug insn "sb x10, 948(x11)";
		  tmp1, tmp2 <== mload(x11 + 948);
		  tmp3 <== shl(0xff, 8 * tmp2);
		  tmp3 <== xor(tmp3, 0xffffffff);
		  tmp1 <== and(tmp1, tmp3);
		  tmp3 <== and(x10, 0xff);
		  tmp3 <== shl(tmp3, 8 * tmp2);
		  tmp1 <== or(tmp1, tmp3);
		  mstore x11 + 948 - tmp2, tmp1;
		  debug insn "addi x11, x2, 4";
		  x11 <== wrap(x2 + 4);
		  debug loc 213 25 9;
		  debug insn "sw x11, 8(x2)";
		  mstore x2 + 8, x11;
		  debug insn "load_dynamic x11, runtime_dash_b33a6d6e449ccd5c___ZN44_$LT$$RF$T$u20$as$u20$core..fmt..Display$GT$3fmt17ha66a21a5de81b0beE";
		  x11 <== load_label(runtime_dash_b33a6d6e449ccd5c___ZN44_$LT$$RF$T$u20$as$u20$core_dot__dot_fmt_dot__dot_Display$GT$3fmt17ha66a21a5de81b0beE);
		  debug insn "sw x11, 12(x2)";
		  mstore x2 + 12, x11;
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp258:
		  debug loc 219 12 38;
		  debug insn "sw x0, 16(x2)";
		  mstore x2 + 16, x0;
		  debug insn "lui x11, 16";
		  x11 <=X= 65536;
		  debug insn "addi x11, x11, 932";
		  x11 <== wrap(x11 + 932);
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp259:
		  debug insn "sw x11, 24(x2)";
		  mstore x2 + 24, x11;
		  debug insn "li x11, 2";
		  x11 <=X= 2;
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp260:
		  debug insn "sw x11, 28(x2)";
		  mstore x2 + 28, x11;
		  debug insn "addi x11, x2, 8";
		  x11 <== wrap(x2 + 8);
		  debug insn "sw x11, 32(x2)";
		  mstore x2 + 32, x11;
		  debug insn "sw x10, 36(x2)";
		  mstore x2 + 36, x10;
		  debug loc 219 12 5;
		  debug insn "lui x10, 16";
		  x10 <=X= 65536;
		  debug insn "addi x11, x10, 808";
		  x11 <== wrap(x10 + 808);
		  debug insn "addi x10, x2, 40";
		  x10 <== wrap(x2 + 40);
		  debug insn "addi x12, x2, 16";
		  x12 <== wrap(x2 + 16);
		  debug insn "call _ZN4core3fmt5write17h87a75934cc3b911aE";
		  call _ZN4core3fmt5write17h87a75934cc3b911aE;
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp261:
		  debug loc 227 1110 9;
		  debug insn "bnez x10, runtime_dash_b33a6d6e449ccd5c__.LBB14_5";
		  branch_if_nonzero x10, runtime_dash_b33a6d6e449ccd5c___dot_LBB14_5;
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp262:
		runtime_dash_b33a6d6e449ccd5c___dot_LBB14_3:
		  debug loc 213 30 5;
		  debug insn "unimp ";
		  fail;
		runtime_dash_b33a6d6e449ccd5c___dot_LBB14_5:
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp263:
		  debug loc 227 1112 23;
		  debug insn "lui x10, 16";
		  x10 <=X= 65536;
		  debug insn "addi x10, x10, 832";
		  x10 <== wrap(x10 + 832);
		  debug insn "lui x11, 16";
		  x11 <=X= 65536;
		  debug insn "addi x13, x11, 876";
		  x13 <== wrap(x11 + 876);
		  debug insn "lui x11, 16";
		  x11 <=X= 65536;
		  debug insn "addi x14, x11, 912";
		  x14 <== wrap(x11 + 912);
		  debug insn "li x11, 43";
		  x11 <=X= 43;
		  debug insn "addi x12, x2, 40";
		  x12 <== wrap(x2 + 40);
		  debug insn "call _ZN4core6result13unwrap_failed17h3809f1cd94940bfeE";
		  call _ZN4core6result13unwrap_failed17h3809f1cd94940bfeE;
		  debug insn "unimp ";
		  fail;
		__runtime_start:
		runtime_dash_b33a6d6e449ccd5c___dot_Lfunc_begin15:
		  debug loc 213 48 0;
		  debug loc 213 50 9;
		  debug insn "tail main";
		  jump main;
		__rg_alloc:
		runtime_dash_b33a6d6e449ccd5c___dot_Lfunc_begin16:
		  debug loc 214 63 0;
		  debug loc 228 452 18;
		  debug insn "lui x12, 16";
		  x12 <=X= 65536;
		  debug insn "lw x13, 1072(x12)";
		  x13, tmp1 <== mload(x12 + 1072);
		  debug insn "addi x14, x12, 1072";
		  x14 <== wrap(x12 + 1072);
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp266:
		  debug loc 214 41 28;
		  debug insn "addi x14, x14, 4";
		  x14 <== wrap(x14 + 4);
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp267:
		  debug insn "add x15, x14, x11";
		  x15 <== wrap(x14 + x11);
		  debug insn "add x13, x13, x15";
		  x13 <== wrap(x13 + x15);
		  debug loc 214 41 27;
		  debug insn "addi x13, x13, -1";
		  x13 <== wrap(x13 + 4294967295);
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp268:
		  debug loc 214 41 61;
		  debug insn "neg x11, x11";
		  x11 <== wrap_signed(0 - x11);
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp269:
		  debug loc 214 41 27;
		  debug insn "and x11, x11, x13";
		  x11 <== and(x11, x13);
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp270:
		  debug loc 214 44 37;
		  debug insn "sub x10, x10, x14";
		  x10 <== wrap_signed(x10 - x14);
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp271:
		  debug loc 214 47 34;
		  debug insn "add x13, x11, x10";
		  x13 <== wrap(x11 + x10);
		  debug insn "lui x14, 262144";
		  x14 <=X= 1073741824;
		  debug insn "li x10, 0";
		  x10 <=X= 0;
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp272:
		  debug loc 214 49 12;
		  debug insn "bltu x14, x13, runtime_dash_b33a6d6e449ccd5c__.LBB16_2";
		  branch_if_positive x13 - x14, runtime_dash_b33a6d6e449ccd5c___dot_LBB16_2;
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp273:
		  debug loc 223 1354 9;
		  debug insn "sw x13, 1072(x12)";
		  mstore x12 + 1072, x13;
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp274:
		  debug loc 223 0 9;
		  debug insn "mv x10, x11";
		  x10 <=X= x11;
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp275:
		runtime_dash_b33a6d6e449ccd5c___dot_LBB16_2:
		  debug loc 214 63 95;
		  debug insn "ret ";
		  ret;
		__rg_dealloc:
		runtime_dash_b33a6d6e449ccd5c___dot_Lfunc_begin17:
		  debug loc 214 63 0;
		  debug loc 214 63 95;
		  debug insn "ret ";
		  ret;
		__rg_realloc:
		runtime_dash_b33a6d6e449ccd5c___dot_Lfunc_begin18:
		  debug loc 214 63 0;
		  debug insn "addi x2, x2, -16";
		  x2 <== wrap(x2 + 4294967280);
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp278:
		  debug loc 230 0 0;
		  debug insn "sw x1, 12(x2)";
		  mstore x2 + 12, x1;
		  debug insn "sw x8, 8(x2)";
		  mstore x2 + 8, x8;
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp279:
		  debug loc 228 452 18;
		  debug insn "lui x16, 16";
		  x16 <=X= 65536;
		  debug insn "lw x15, 1072(x16)";
		  x15, tmp1 <== mload(x16 + 1072);
		  debug insn "addi x8, x16, 1072";
		  x8 <== wrap(x16 + 1072);
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp280:
		  debug loc 214 41 28;
		  debug insn "addi x14, x8, 4";
		  x14 <== wrap(x8 + 4);
		  debug insn "add x8, x14, x12";
		  x8 <== wrap(x14 + x12);
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp281:
		  debug insn "add x15, x15, x8";
		  x15 <== wrap(x15 + x8);
		  debug loc 214 41 27;
		  debug insn "addi x15, x15, -1";
		  x15 <== wrap(x15 + 4294967295);
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp282:
		  debug loc 214 41 61;
		  debug insn "neg x12, x12";
		  x12 <== wrap_signed(0 - x12);
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp283:
		  debug loc 214 41 27;
		  debug insn "and x8, x15, x12";
		  x8 <== and(x15, x12);
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp284:
		  debug loc 214 44 37;
		  debug insn "sub x12, x13, x14";
		  x12 <== wrap_signed(x13 - x14);
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp285:
		  debug loc 214 47 34;
		  debug insn "add x15, x8, x12";
		  x15 <== wrap(x8 + x12);
		  debug insn "lui x14, 262144";
		  x14 <=X= 1073741824;
		  debug insn "mv x12, x11";
		  x12 <=X= x11;
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp286:
		  debug loc 214 0 34;
		  debug insn "mv x11, x10";
		  x11 <=X= x10;
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp287:
		  debug insn "li x10, 0";
		  x10 <=X= 0;
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp288:
		  debug loc 214 49 12;
		  debug insn "bltu x14, x15, runtime_dash_b33a6d6e449ccd5c__.LBB18_5";
		  branch_if_positive x15 - x14, runtime_dash_b33a6d6e449ccd5c___dot_LBB18_5;
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp289:
		  debug loc 223 1354 9;
		  debug insn "sw x15, 1072(x16)";
		  mstore x16 + 1072, x15;
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp290:
		  debug loc 230 267 12;
		  debug insn "beqz x8, runtime_dash_b33a6d6e449ccd5c__.LBB18_6";
		  branch_if_zero x8, runtime_dash_b33a6d6e449ccd5c___dot_LBB18_6;
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp291:
		  debug loc 231 0 0;
		  debug insn "bltu x12, x13, runtime_dash_b33a6d6e449ccd5c__.LBB18_4";
		  branch_if_positive x13 - x12, runtime_dash_b33a6d6e449ccd5c___dot_LBB18_4;
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp292:
		  debug insn "mv x12, x13";
		  x12 <=X= x13;
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp293:
		runtime_dash_b33a6d6e449ccd5c___dot_LBB18_4:
		  debug loc 232 2372 9;
		  debug insn "mv x10, x8";
		  x10 <=X= x8;
		  debug insn "call memcpy";
		  call memcpy;
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp294:
		  debug loc 232 0 9;
		  debug insn "mv x10, x8";
		  x10 <=X= x8;
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp295:
		runtime_dash_b33a6d6e449ccd5c___dot_LBB18_5:
		  debug loc 214 63 95;
		  debug insn "lw x1, 12(x2)";
		  x1, tmp1 <== mload(x2 + 12);
		  debug insn "lw x8, 8(x2)";
		  x8, tmp1 <== mload(x2 + 8);
		  debug insn "addi x2, x2, 16";
		  x2 <== wrap(x2 + 16);
		  debug insn "ret ";
		  ret;
		runtime_dash_b33a6d6e449ccd5c___dot_LBB18_6:
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp296:
		  debug loc 214 0 95;
		  debug insn "li x10, 0";
		  x10 <=X= 0;
		  debug loc 214 63 95;
		  debug insn "lw x1, 12(x2)";
		  x1, tmp1 <== mload(x2 + 12);
		  debug insn "lw x8, 8(x2)";
		  x8, tmp1 <== mload(x2 + 8);
		  debug insn "addi x2, x2, 16";
		  x2 <== wrap(x2 + 16);
		  debug insn "ret ";
		  ret;
		runtime_dash_b33a6d6e449ccd5c___ZN7runtime9allocator11alloc_error17hbc749fffa2ad4e7eE:
		runtime_dash_b33a6d6e449ccd5c___dot_Lfunc_begin20:
		  debug loc 214 66 0;
		  debug insn "addi x2, x2, -48";
		  x2 <== wrap(x2 + 4294967248);
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp309:
		  debug loc 233 129 9;
		  debug insn "sw x10, 40(x2)";
		  mstore x2 + 40, x10;
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp310:
		  debug loc 234 97 9;
		  debug insn "sw x11, 44(x2)";
		  mstore x2 + 44, x11;
		  debug insn "addi x10, x2, 40";
		  x10 <== wrap(x2 + 40);
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp311:
		  debug loc 214 67 5;
		  debug insn "sw x10, 24(x2)";
		  mstore x2 + 24, x10;
		  debug insn "load_dynamic x10, _ZN4core3fmt3num3imp52_$LT$impl$u20$core..fmt..Display$u20$for$u20$u32$GT$3fmt17h361d4a177e5aa0a5E";
		  x10 <== load_label(_ZN4core3fmt3num3imp52_$LT$impl$u20$core_dot__dot_fmt_dot__dot_Display$u20$for$u20$u32$GT$3fmt17h361d4a177e5aa0a5E);
		  debug insn "sw x10, 28(x2)";
		  mstore x2 + 28, x10;
		  debug insn "addi x11, x2, 44";
		  x11 <== wrap(x2 + 44);
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp312:
		  debug insn "sw x11, 32(x2)";
		  mstore x2 + 32, x11;
		  debug insn "sw x10, 36(x2)";
		  mstore x2 + 36, x10;
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp313:
		  debug loc 215 398 9;
		  debug insn "lui x10, 16";
		  x10 <=X= 65536;
		  debug insn "addi x10, x10, 1008";
		  x10 <== wrap(x10 + 1008);
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp314:
		  debug insn "sw x10, 8(x2)";
		  mstore x2 + 8, x10;
		  debug insn "li x10, 3";
		  x10 <=X= 3;
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp315:
		  debug insn "sw x10, 12(x2)";
		  mstore x2 + 12, x10;
		  debug insn "sw x0, 0(x2)";
		  mstore x2 + 0, x0;
		  debug insn "addi x10, x2, 24";
		  x10 <== wrap(x2 + 24);
		  debug insn "sw x10, 16(x2)";
		  mstore x2 + 16, x10;
		  debug insn "li x10, 2";
		  x10 <=X= 2;
		  debug insn "sw x10, 20(x2)";
		  mstore x2 + 20, x10;
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp316:
		  debug loc 214 67 5;
		  debug insn "lui x10, 16";
		  x10 <=X= 65536;
		  debug insn "addi x11, x10, 1056";
		  x11 <== wrap(x10 + 1056);
		  debug insn "mv x10, x2";
		  x10 <=X= x2;
		  debug insn "call _ZN4core9panicking9panic_fmt17h38a84a0a59f0bde7E";
		  call _ZN4core9panicking9panic_fmt17h38a84a0a59f0bde7E;
		  debug insn "unimp ";
		  fail;
		__rg_oom:
		runtime_dash_b33a6d6e449ccd5c___dot_Lfunc_begin21:
		  debug loc 214 66 0;
		  debug loc 214 66 1;
		  debug insn "call runtime_dash_b33a6d6e449ccd5c___ZN7runtime9allocator11alloc_error17hbc749fffa2ad4e7eE";
		  call runtime_dash_b33a6d6e449ccd5c___ZN7runtime9allocator11alloc_error17hbc749fffa2ad4e7eE;
		runtime_dash_b33a6d6e449ccd5c___dot_Ltmp318:
		  debug insn "unimp ";
		  fail;
		// This is the data initialization routine.
__data_init:
		// data alloc_dash_0c41f8373e14af19__.L__unnamed_73
mstore 0x10100, 0x6573552f;
		mstore 0x10104, 0x672f7372;
		mstore 0x10108, 0x67726f65;
		mstore 0x1010c, 0x75722e2f;
		mstore 0x10110, 0x70757473;
		mstore 0x10114, 0x6f6f742f;
		mstore 0x10118, 0x6168636c;
		mstore 0x1011c, 0x2f736e69;
		mstore 0x10120, 0x6867696e;
		mstore 0x10124, 0x2d796c74;
		mstore 0x10128, 0x33323032;
		mstore 0x1012c, 0x2d31302d;
		mstore 0x10130, 0x612d3330;
		mstore 0x10134, 0x68637261;
		mstore 0x10138, 0x612d3436;
		mstore 0x1013c, 0x656c7070;
		mstore 0x10140, 0x7261642d;
		mstore 0x10144, 0x2f6e6977;
		mstore 0x10148, 0x2f62696c;
		mstore 0x1014c, 0x74737572;
		mstore 0x10150, 0x2f62696c;
		mstore 0x10154, 0x2f637273;
		mstore 0x10158, 0x74737572;
		mstore 0x1015c, 0x62696c2f;
		mstore 0x10160, 0x79726172;
		mstore 0x10164, 0x6c6c612f;
		mstore 0x10168, 0x732f636f;
		mstore 0x1016c, 0x722f6372;
		mstore 0x10170, 0x765f7761;
		mstore 0x10174, 0x722e6365;
		mstore 0x10178, 0x73;
		// data alloc_dash_0c41f8373e14af19__.L__unnamed_46
mstore 0x1017c, 0x61706163;
		mstore 0x10180, 0x79746963;
		mstore 0x10184, 0x65766f20;
		mstore 0x10188, 0x6f6c6672;
		mstore 0x1018c, 0x77;
		// data alloc_dash_0c41f8373e14af19__.L__unnamed_23
mstore 0x10190, 0x1017c;
		mstore 0x10194, 0x11;
		// data alloc_dash_0c41f8373e14af19__.L__unnamed_25
mstore 0x10198, 0x10100;
		mstore 0x1019c, 0x79;
		mstore 0x101a0, 0x206;
		mstore 0x101a4, 0x5;
		// data core_dash_e8b3f51d8f75870a__.L__unnamed_738
mstore 0x101a8, 0x3a;
		// data core_dash_e8b3f51d8f75870a__.L__unnamed_269
mstore 0x101ac, 0x101a8;
		mstore 0x101b4, 0x101a8;
		mstore 0x101b8, 0x1;
		mstore 0x101bc, 0x101a8;
		mstore 0x101c0, 0x1;
		// data core_dash_e8b3f51d8f75870a__.L__unnamed_270
mstore 0x101c4, 0x696e6170;
		mstore 0x101c8, 0x64656b63;
		mstore 0x101cc, 0x20746120;
		// data core_dash_e8b3f51d8f75870a__.L__unnamed_739
mstore 0x101d0, 0x27;
		// data core_dash_e8b3f51d8f75870a__.L__unnamed_740
mstore 0x101d4, 0x202c27;
		// data core_dash_e8b3f51d8f75870a__.L__unnamed_271
mstore 0x101d8, 0x101d0;
		mstore 0x101dc, 0x1;
		mstore 0x101e0, 0x101d4;
		mstore 0x101e4, 0x3;
		// data core_dash_e8b3f51d8f75870a__.L__unnamed_10
tmp1 <== load_label(core_dash_e8b3f51d8f75870a___ZN4core3ptr102drop_in_place$LT$$RF$core_dot__dot_iter_dot__dot_adapters_dot__dot_copied_dot__dot_Copied$LT$core_dot__dot_slice_dot__dot_iter_dot__dot_Iter$LT$u8$GT$$GT$$GT$17hc185e3e6ab652ffaE);
		mstore 0x101e8, tmp1;
		mstore 0x101f0, 0x1;
		tmp1 <== load_label(core_dash_e8b3f51d8f75870a___ZN36_$LT$T$u20$as$u20$core_dot__dot_any_dot__dot_Any$GT$7type_id17h8092b8f856eee998E);
		mstore 0x101f4, tmp1;
		// data core_dash_e8b3f51d8f75870a__.L__unnamed_289
mstore 0x101f8, 0x203a;
		// data core_dash_e8b3f51d8f75870a__.L__unnamed_283
mstore 0x101fc, 0x101a8;
		mstore 0x10204, 0x101f8;
		mstore 0x10208, 0x2;
		// data core_dash_e8b3f51d8f75870a__.L__unnamed_320
mstore 0x1020c, 0x31303030;
		mstore 0x10210, 0x33303230;
		mstore 0x10214, 0x35303430;
		mstore 0x10218, 0x37303630;
		mstore 0x1021c, 0x39303830;
		mstore 0x10220, 0x31313031;
		mstore 0x10224, 0x33313231;
		mstore 0x10228, 0x35313431;
		mstore 0x1022c, 0x37313631;
		mstore 0x10230, 0x39313831;
		mstore 0x10234, 0x31323032;
		mstore 0x10238, 0x33323232;
		mstore 0x1023c, 0x35323432;
		mstore 0x10240, 0x37323632;
		mstore 0x10244, 0x39323832;
		mstore 0x10248, 0x31333033;
		mstore 0x1024c, 0x33333233;
		mstore 0x10250, 0x35333433;
		mstore 0x10254, 0x37333633;
		mstore 0x10258, 0x39333833;
		mstore 0x1025c, 0x31343034;
		mstore 0x10260, 0x33343234;
		mstore 0x10264, 0x35343434;
		mstore 0x10268, 0x37343634;
		mstore 0x1026c, 0x39343834;
		mstore 0x10270, 0x31353035;
		mstore 0x10274, 0x33353235;
		mstore 0x10278, 0x35353435;
		mstore 0x1027c, 0x37353635;
		mstore 0x10280, 0x39353835;
		mstore 0x10284, 0x31363036;
		mstore 0x10288, 0x33363236;
		mstore 0x1028c, 0x35363436;
		mstore 0x10290, 0x37363636;
		mstore 0x10294, 0x39363836;
		mstore 0x10298, 0x31373037;
		mstore 0x1029c, 0x33373237;
		mstore 0x102a0, 0x35373437;
		mstore 0x102a4, 0x37373637;
		mstore 0x102a8, 0x39373837;
		mstore 0x102ac, 0x31383038;
		mstore 0x102b0, 0x33383238;
		mstore 0x102b4, 0x35383438;
		mstore 0x102b8, 0x37383638;
		mstore 0x102bc, 0x39383838;
		mstore 0x102c0, 0x31393039;
		mstore 0x102c4, 0x33393239;
		mstore 0x102c8, 0x35393439;
		mstore 0x102cc, 0x37393639;
		mstore 0x102d0, 0x39393839;
		// data core_dash_e8b3f51d8f75870a__.L__unnamed_553
mstore 0x102d4, 0x6f727245;
		mstore 0x102d8, 0x72;
		// data many_chunks_dash_1054733ad03afc41__.L__unnamed_1
mstore 0x102dc, 0x65737361;
		mstore 0x102e0, 0x6f697472;
		mstore 0x102e4, 0x6166206e;
		mstore 0x102e8, 0x64656c69;
		mstore 0x102ec, 0x2061203a;
		mstore 0x102f0, 0x30203e;
		// data many_chunks_dash_1054733ad03afc41__.L__unnamed_3
mstore 0x102f4, 0x2f637273;
		mstore 0x102f8, 0x2e62696c;
		mstore 0x102fc, 0x7372;
		// data many_chunks_dash_1054733ad03afc41__.L__unnamed_2
mstore 0x10300, 0x102f4;
		mstore 0x10304, 0xa;
		mstore 0x10308, 0x15;
		mstore 0x1030c, 0x5;
		// data runtime_dash_b33a6d6e449ccd5c__.L__unnamed_1
tmp1 <== load_label(runtime_dash_b33a6d6e449ccd5c___ZN4core3ptr37drop_in_place$LT$core_dot__dot_fmt_dot__dot_Error$GT$17hac39e9c214b967d0E);
		mstore 0x10310, tmp1;
		mstore 0x10314, 0x4;
		mstore 0x10318, 0x4;
		tmp1 <== load_label(runtime_dash_b33a6d6e449ccd5c___ZN50_$LT$$RF$mut$u20$W$u20$as$u20$core_dot__dot_fmt_dot__dot_Write$GT$9write_str17h87da48f65744beb5E);
		mstore 0x1031c, tmp1;
		tmp1 <== load_label(runtime_dash_b33a6d6e449ccd5c___ZN50_$LT$$RF$mut$u20$W$u20$as$u20$core_dot__dot_fmt_dot__dot_Write$GT$10write_char17hbac5473a26c1dbe9E);
		mstore 0x10320, tmp1;
		tmp1 <== load_label(runtime_dash_b33a6d6e449ccd5c___ZN50_$LT$$RF$mut$u20$W$u20$as$u20$core_dot__dot_fmt_dot__dot_Write$GT$9write_fmt17h9360d1a9672f9147E);
		mstore 0x10324, tmp1;
		// data runtime_dash_b33a6d6e449ccd5c__.L__unnamed_2
tmp1 <== load_label(runtime_dash_b33a6d6e449ccd5c___ZN4core3ptr37drop_in_place$LT$core_dot__dot_fmt_dot__dot_Error$GT$17hac39e9c214b967d0E);
		mstore 0x10328, tmp1;
		mstore 0x10330, 0x1;
		tmp1 <== load_label(_ZN63_$LT$runtime_dot__dot_fmt_dot__dot_ProverWriter$u20$as$u20$core_dot__dot_fmt_dot__dot_Write$GT$9write_str17h56ede652d1afdb54E);
		mstore 0x10334, tmp1;
		tmp1 <== load_label(runtime_dash_b33a6d6e449ccd5c___ZN4core3fmt5Write10write_char17hcb7f8f8c82f6d9bfE);
		mstore 0x10338, tmp1;
		tmp1 <== load_label(runtime_dash_b33a6d6e449ccd5c___ZN4core3fmt5Write9write_fmt17he9cd6fc9cbb882bfE);
		mstore 0x1033c, tmp1;
		// data runtime_dash_b33a6d6e449ccd5c__.L__unnamed_6
mstore 0x10340, 0x6c6c6163;
		mstore 0x10344, 0x60206465;
		mstore 0x10348, 0x75736552;
		mstore 0x1034c, 0x3a3a746c;
		mstore 0x10350, 0x72776e75;
		mstore 0x10354, 0x29287061;
		mstore 0x10358, 0x6e6f2060;
		mstore 0x1035c, 0x206e6120;
		mstore 0x10360, 0x72724560;
		mstore 0x10364, 0x61762060;
		mstore 0x10368, 0x65756c;
		// data runtime_dash_b33a6d6e449ccd5c__.L__unnamed_3
tmp1 <== load_label(runtime_dash_b33a6d6e449ccd5c___ZN4core3ptr37drop_in_place$LT$core_dot__dot_fmt_dot__dot_Error$GT$17hac39e9c214b967d0E);
		mstore 0x1036c, tmp1;
		mstore 0x10374, 0x1;
		tmp1 <== load_label(_ZN53_$LT$core_dot__dot_fmt_dot__dot_Error$u20$as$u20$core_dot__dot_fmt_dot__dot_Debug$GT$3fmt17h8ce97bea8e305373E);
		mstore 0x10378, tmp1;
		// data runtime_dash_b33a6d6e449ccd5c__.L__unnamed_13
mstore 0x1037c, 0x746e7572;
		mstore 0x10380, 0x2f656d69;
		mstore 0x10384, 0x2f637273;
		mstore 0x10388, 0x2e746d66;
		mstore 0x1038c, 0x7372;
		// data runtime_dash_b33a6d6e449ccd5c__.L__unnamed_7
mstore 0x10390, 0x1037c;
		mstore 0x10394, 0x12;
		mstore 0x10398, 0xc;
		mstore 0x1039c, 0x2c;
		// data runtime_dash_b33a6d6e449ccd5c__.L__unnamed_14
mstore 0x103a0, 0xa;
		// data runtime_dash_b33a6d6e449ccd5c__.L__unnamed_8
mstore 0x103a4, 0x10328;
		mstore 0x103ac, 0x103a0;
		mstore 0x103b0, 0x1;
		// data runtime_dash_b33a6d6e449ccd5c__.L__unnamed_15
mstore 0x103b8, 0x6f6d656d;
		mstore 0x103bc, 0x61207972;
		mstore 0x103c0, 0x636f6c6c;
		mstore 0x103c4, 0x6f697461;
		mstore 0x103c8, 0x666f206e;
		mstore 0x103cc, 0x20;
		// data runtime_dash_b33a6d6e449ccd5c__.L__unnamed_16
mstore 0x103d0, 0x74796220;
		mstore 0x103d4, 0x77207365;
		mstore 0x103d8, 0x20687469;
		mstore 0x103dc, 0x67696c61;
		mstore 0x103e0, 0x6e656d6e;
		mstore 0x103e4, 0x2074;
		// data runtime_dash_b33a6d6e449ccd5c__.L__unnamed_17
mstore 0x103e8, 0x69616620;
		mstore 0x103ec, 0x64656c;
		// data runtime_dash_b33a6d6e449ccd5c__.L__unnamed_9
mstore 0x103f0, 0x103b8;
		mstore 0x103f4, 0x15;
		mstore 0x103f8, 0x103d0;
		mstore 0x103fc, 0x16;
		mstore 0x10400, 0x103e8;
		mstore 0x10404, 0x7;
		// data runtime_dash_b33a6d6e449ccd5c__.L__unnamed_18
mstore 0x10408, 0x746e7572;
		mstore 0x1040c, 0x2f656d69;
		mstore 0x10410, 0x2f637273;
		mstore 0x10414, 0x6f6c6c61;
		mstore 0x10418, 0x6f746163;
		mstore 0x1041c, 0x73722e72;
		// data runtime_dash_b33a6d6e449ccd5c__.L__unnamed_10
mstore 0x10420, 0x10408;
		mstore 0x10424, 0x18;
		mstore 0x10428, 0x43;
		mstore 0x1042c, 0x5;
		// This is the end of the data initialization routine.
ret;
    }
}    
