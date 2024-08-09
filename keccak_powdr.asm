
use std::machines::binary::Binary;
use std::machines::shift::Shift;
use std::machines::split::split_gl::SplitGL;
machine Main with degree: 262144 {
Binary binary;
Shift shift;
SplitGL split_gl;


    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg Z[<=];
    reg W[<=];
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


    // =============== read-write memory =======================
    // Read-write memory. Columns are sorted by m_addr and
    // then by m_step. m_change is 1 if and only if m_addr changes
    // in the next row.
    col witness m_addr;
    col witness m_step;
    col witness m_change;
    col witness m_value;

    // Memory operation flags: If none is active, it's a read.
    col witness m_is_write;
    std::utils::force_bool(m_is_write);

    // Selectors
    col witness m_selector_read;
    col witness m_selector_write;
    std::utils::force_bool(m_selector_read);
    std::utils::force_bool(m_selector_write);

    // No selector active -> no write
    (1 - m_selector_read - m_selector_write) * m_is_write = 0;
    
    col operation_id = m_is_write;

    // If the next line is a not a write and we have an address change,
    // then the value is zero.
    (1 - m_is_write') * m_change * m_value' = 0;

    // m_change has to be 1 in the last row, so that a first read on row zero is constrained to return 0
    (1 - m_change) * LAST = 0;

    // If the next line is a read and we stay at the same address, then the
    // value cannot change.
    (1 - m_is_write') * (1 - m_change) * (m_value' - m_value) = 0;

    col witness m_diff_lower;
    col witness m_diff_upper;

    col fixed FIRST = [1] + [0]*;
    let LAST = FIRST';
    col fixed STEP(i) { i };
    col fixed BIT16(i) { i & 0xffff };

    {m_diff_lower} in {BIT16};
    {m_diff_upper} in {BIT16};

    std::utils::force_bool(m_change);

    // if m_change is zero, m_addr has to stay the same.
    (m_addr' - m_addr) * (1 - m_change) = 0;

    // Except for the last row, if m_change is 1, then m_addr has to increase,
    // if it is zero, m_step has to increase.
    // `m_diff_upper * 2**16 + m_diff_lower` has to be equal to the difference **minus one**.
    // Since we know that both m_addr and m_step can only be 32-Bit, this enforces that
    // the values are strictly increasing.
    col diff = (m_change * (m_addr' - m_addr) + (1 - m_change) * (m_step' - m_step));
    (1 - LAST) * (diff - 1 - m_diff_upper * 2**16 - m_diff_lower) = 0;

    // ============== memory instructions ==============

    let up_to_three: col = |i| i % 4;
    let six_bits: col = |i| i % 2**6;
    /// Loads one word from an address Y, where Y can be between 0 and 2**33 (sic!),
    /// wraps the address to 32 bits and rounds it down to the next multiple of 4.
    /// Returns the loaded word and the remainder of the division by 4.
    instr mload Y -> X, Z {
        // Z * (Z - 1) * (Z - 2) * (Z - 3) = 0,
        { Z } in { up_to_three },
        Y = wrap_bit * 2**32 + X_b4 * 0x1000000 + X_b3 * 0x10000 + X_b2 * 0x100 + X_b1 * 4 + Z,
        { X_b1 } in { six_bits },
        {
            0,
            X_b4 * 0x1000000 + X_b3 * 0x10000 + X_b2 * 0x100 + X_b1 * 4,
            STEP,
            X
        } is m_selector_read { operation_id, m_addr, m_step, m_value }
        // If we could access the shift machine here, we
        // could even do the following to complete the mload:
        // { W, X, Z} in { shr.value, shr.amount, shr.amount}
    }

    /// Stores Z at address Y % 2**32. Y can be between 0 and 2**33.
    /// Y should be a multiple of 4, but this instruction does not enforce it.
    instr mstore Y, Z {
        { 1, X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000, STEP, Z } is m_selector_write { operation_id, m_addr, m_step, m_value },
        // Wrap the addr value
        Y = (X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000) + wrap_bit * 2**32
    }
    
    // ============== Constraint on x0 =======================

    x0 = 0;

    // ============== iszero check for X =======================
    let XIsZero = std::utils::is_zero(X);

    // ============== control-flow instructions ==============

    instr load_label l: label -> X { X = l }

    instr jump l: label -> Y { pc' = l, Y = pc + 1}
    instr jump_dyn X -> Y { pc' = X, Y = pc + 1}

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

    // ================= submachine instructions =================
    instr and Y, Z -> X ~ binary.and ;
    instr or Y, Z -> X ~ binary.or ;
    instr xor Y, Z -> X ~ binary.xor ;
    instr shl Y, Z -> X ~ shift.shl ;
    instr shr Y, Z -> X ~ shift.shr ;
    instr split_gl Z -> X, Y ~ split_gl.split ;
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
    instr mul Z, W -> X, Y ~ split_gl.split Z * W -> X, Y;


let initial_memory: (fe, fe)[] = [

];

    function main {
		.debug file 1 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/fmt" "mod.rs";
		.debug file 2 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/ptr" "mod.rs";
		.debug file 3 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/alloc" "src/raw_vec.rs";
		.debug file 4 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/alloc" "src/alloc.rs";
		.debug file 5 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src" "error.rs";
		.debug file 6 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/alloc" "layout.rs";
		.debug file 7 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/num" "mod.rs";
		.debug file 8 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src" "cmp.rs";
		.debug file 9 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src" "result.rs";
		.debug file 10 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/num" "uint_macros.rs";
		.debug file 11 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src" "panic.rs";
		.debug file 12 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/alloc" "src/boxed.rs";
		.debug file 13 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src" "intrinsics.rs";
		.debug file 14 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/convert" "mod.rs";
		.debug file 15 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/alloc" "src/vec/mod.rs";
		.debug file 16 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/alloc" "src/string.rs";
		.debug file 17 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/alloc" "src/slice.rs";
		.debug file 18 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/alloc" "src/str.rs";
		.debug file 19 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/ptr" "const_ptr.rs";
		.debug file 20 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/alloc" "src/borrow.rs";
		.debug file 21 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/ptr" "mut_ptr.rs";
		.debug file 22 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/alloc" "src/vec/spec_extend.rs";
		.debug file 23 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/alloc" "src/collections/btree/map.rs";
		.debug file 24 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/macros" "mod.rs";
		.debug file 25 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/alloc" "src/collections/btree/mem.rs";
		.debug file 26 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/alloc" "src/collections/mod.rs";
		.debug file 27 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/alloc" "src/ffi/c_str.rs";
		.debug file 28 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src" "option.rs";
		.debug file 29 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/slice" "memchr.rs";
		.debug file 30 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/mem" "mod.rs";
		.debug file 31 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/ffi" "c_str.rs";
		.debug file 32 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/alloc" "src/fmt.rs";
		.debug file 33 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/slice/iter" "macros.rs";
		.debug file 34 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/iter/traits" "accum.rs";
		.debug file 35 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/iter/adapters" "map.rs";
		.debug file 36 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/iter/traits" "iterator.rs";
		.debug file 37 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/alloc" "src/rc.rs";
		.debug file 38 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/mem" "maybe_uninit.rs";
		.debug file 39 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/str" "validations.rs";
		.debug file 40 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/str" "iter.rs";
		.debug file 41 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/str" "mod.rs";
		.debug file 42 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/str" "traits.rs";
		.debug file 43 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/iter/traits" "double_ended.rs";
		.debug file 44 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/ptr" "non_null.rs";
		.debug file 45 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/iter/adapters" "skip_while.rs";
		.debug file 46 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/iter/adapters" "rev.rs";
		.debug file 47 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/slice" "iter.rs";
		.debug file 48 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/slice" "mod.rs";
		.debug file 49 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/str" "lossy.rs";
		.debug file 50 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/char" "decode.rs";
		.debug file 51 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/iter/adapters" "cloned.rs";
		.debug file 52 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/iter/adapters" "mod.rs";
		.debug file 53 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/iter/adapters" "copied.rs";
		.debug file 54 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/iter" "mod.rs";
		.debug file 55 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/ops" "try_trait.rs";
		.debug file 56 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/char" "methods.rs";
		.debug file 57 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/ops" "function.rs";
		.debug file 58 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/iter/traits" "collect.rs";
		.debug file 59 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/str" "pattern.rs";
		.debug file 60 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/alloc" "src/vec/drain.rs";
		.debug file 61 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/alloc" "src/sync.rs";
		.debug file 62 "/Users/steve/.cargo/registry/src/index.crates.io-6f17d22bba15001f/compiler_builtins-0.1.105" "src/../libm/src/math/exp2.rs";
		.debug file 63 "/Users/steve/.cargo/registry/src/index.crates.io-6f17d22bba15001f/compiler_builtins-0.1.105" "src/../libm/src/math/exp2f.rs";
		.debug file 64 "/Users/steve/.cargo/registry/src/index.crates.io-6f17d22bba15001f/compiler_builtins-0.1.105" "src/../libm/src/math/k_tan.rs";
		.debug file 65 "/Users/steve/.cargo/registry/src/index.crates.io-6f17d22bba15001f/compiler_builtins-0.1.105" "src/macros.rs";
		.debug file 66 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/ops" "bit.rs";
		.debug file 67 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src" "cmp.rs";
		.debug file 68 "/Users/steve/.cargo/registry/src/index.crates.io-6f17d22bba15001f/compiler_builtins-0.1.105" "src/float/add.rs";
		.debug file 69 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/num" "mod.rs";
		.debug file 70 "/Users/steve/.cargo/registry/src/index.crates.io-6f17d22bba15001f/compiler_builtins-0.1.105" "src/float/mod.rs";
		.debug file 71 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/ops" "arith.rs";
		.debug file 72 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/num" "uint_macros.rs";
		.debug file 73 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/num" "int_macros.rs";
		.debug file 74 "/Users/steve/.cargo/registry/src/index.crates.io-6f17d22bba15001f/compiler_builtins-0.1.105" "src/int/mod.rs";
		.debug file 75 "/Users/steve/.cargo/registry/src/index.crates.io-6f17d22bba15001f/compiler_builtins-0.1.105" "src/float/cmp.rs";
		.debug file 76 "/Users/steve/.cargo/registry/src/index.crates.io-6f17d22bba15001f/compiler_builtins-0.1.105" "src/float/conv.rs";
		.debug file 77 "/Users/steve/.cargo/registry/src/index.crates.io-6f17d22bba15001f/compiler_builtins-0.1.105" "src/float/div.rs";
		.debug file 78 "/Users/steve/.cargo/registry/src/index.crates.io-6f17d22bba15001f/compiler_builtins-0.1.105" "src/float/extend.rs";
		.debug file 79 "/Users/steve/.cargo/registry/src/index.crates.io-6f17d22bba15001f/compiler_builtins-0.1.105" "src/float/mul.rs";
		.debug file 80 "/Users/steve/.cargo/registry/src/index.crates.io-6f17d22bba15001f/compiler_builtins-0.1.105" "src/float/pow.rs";
		.debug file 81 "/Users/steve/.cargo/registry/src/index.crates.io-6f17d22bba15001f/compiler_builtins-0.1.105" "src/float/sub.rs";
		.debug file 82 "/Users/steve/.cargo/registry/src/index.crates.io-6f17d22bba15001f/compiler_builtins-0.1.105" "src/float/trunc.rs";
		.debug file 83 "/Users/steve/.cargo/registry/src/index.crates.io-6f17d22bba15001f/compiler_builtins-0.1.105" "src/int/specialized_div_rem/delegate.rs";
		.debug file 84 "/Users/steve/.cargo/registry/src/index.crates.io-6f17d22bba15001f/compiler_builtins-0.1.105" "src/int/specialized_div_rem/mod.rs";
		.debug file 85 "/Users/steve/.cargo/registry/src/index.crates.io-6f17d22bba15001f/compiler_builtins-0.1.105" "src/int/specialized_div_rem/norm_shift.rs";
		.debug file 86 "/Users/steve/.cargo/registry/src/index.crates.io-6f17d22bba15001f/compiler_builtins-0.1.105" "src/int/specialized_div_rem/binary_long.rs";
		.debug file 87 "/Users/steve/.cargo/registry/src/index.crates.io-6f17d22bba15001f/compiler_builtins-0.1.105" "src/int/addsub.rs";
		.debug file 88 "/Users/steve/.cargo/registry/src/index.crates.io-6f17d22bba15001f/compiler_builtins-0.1.105" "src/int/leading_zeros.rs";
		.debug file 89 "/Users/steve/.cargo/registry/src/index.crates.io-6f17d22bba15001f/compiler_builtins-0.1.105" "src/int/mul.rs";
		.debug file 90 "/Users/steve/.cargo/registry/src/index.crates.io-6f17d22bba15001f/compiler_builtins-0.1.105" "src/int/sdiv.rs";
		.debug file 91 "/Users/steve/.cargo/registry/src/index.crates.io-6f17d22bba15001f/compiler_builtins-0.1.105" "src/int/udiv.rs";
		.debug file 92 "/Users/steve/.cargo/registry/src/index.crates.io-6f17d22bba15001f/compiler_builtins-0.1.105" "src/int/shift.rs";
		.debug file 93 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/num" "f64.rs";
		.debug file 94 "/Users/steve/.cargo/registry/src/index.crates.io-6f17d22bba15001f/compiler_builtins-0.1.105" "src/../libm/src/math/fmin.rs";
		.debug file 95 "/Users/steve/.cargo/registry/src/index.crates.io-6f17d22bba15001f/compiler_builtins-0.1.105" "src/math.rs";
		.debug file 96 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/num" "f32.rs";
		.debug file 97 "/Users/steve/.cargo/registry/src/index.crates.io-6f17d22bba15001f/compiler_builtins-0.1.105" "src/../libm/src/math/fminf.rs";
		.debug file 98 "/Users/steve/.cargo/registry/src/index.crates.io-6f17d22bba15001f/compiler_builtins-0.1.105" "src/../libm/src/math/fmax.rs";
		.debug file 99 "/Users/steve/.cargo/registry/src/index.crates.io-6f17d22bba15001f/compiler_builtins-0.1.105" "src/../libm/src/math/fmaxf.rs";
		.debug file 100 "/Users/steve/.cargo/registry/src/index.crates.io-6f17d22bba15001f/compiler_builtins-0.1.105" "src/../libm/src/math/fmod.rs";
		.debug file 101 "/Users/steve/.cargo/registry/src/index.crates.io-6f17d22bba15001f/compiler_builtins-0.1.105" "src/../libm/src/math/fmodf.rs";
		.debug file 102 "/Users/steve/.cargo/registry/src/index.crates.io-6f17d22bba15001f/compiler_builtins-0.1.105" "src/mem/impls.rs";
		.debug file 103 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/ptr" "mut_ptr.rs";
		.debug file 104 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/ptr" "const_ptr.rs";
		.debug file 105 "/Users/steve/.cargo/registry/src/index.crates.io-6f17d22bba15001f/compiler_builtins-0.1.105" "src/mem/mod.rs";
		.debug file 106 "/Users/steve/.cargo/registry/src/index.crates.io-6f17d22bba15001f/compiler_builtins-0.1.105" "src/riscv.rs";
		.debug file 107 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/num/flt2dec/strategy/dragon.rs";
		.debug file 108 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/num/flt2dec/strategy/grisu.rs";
		.debug file 109 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/fmt/num.rs";
		.debug file 110 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/fmt/rt.rs";
		.debug file 111 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/num/dec2flt/table.rs";
		.debug file 112 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/unicode/unicode_data.rs";
		.debug file 113 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/ptr/mod.rs";
		.debug file 114 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/ops/function.rs";
		.debug file 115 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/num/f32.rs";
		.debug file 116 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/fmt/mod.rs";
		.debug file 117 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/panic.rs";
		.debug file 118 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/num/f64.rs";
		.debug file 119 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/num/dec2flt/decimal.rs";
		.debug file 120 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/iter/range.rs";
		.debug file 121 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/slice/index.rs";
		.debug file 122 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/ptr/const_ptr.rs";
		.debug file 123 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/iter/adapters/take.rs";
		.debug file 124 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/slice/iter/macros.rs";
		.debug file 125 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/array/mod.rs";
		.debug file 126 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/iter/adapters/enumerate.rs";
		.debug file 127 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/slice/mod.rs";
		.debug file 128 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/num/mod.rs";
		.debug file 129 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/num/dec2flt/common.rs";
		.debug file 130 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/intrinsics.rs";
		.debug file 131 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/ptr/mut_ptr.rs";
		.debug file 132 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/ptr/non_null.rs";
		.debug file 133 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/num/uint_macros.rs";
		.debug file 134 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/iter/adapters/rev.rs";
		.debug file 135 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/num/dec2flt/float.rs";
		.debug file 136 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/num/dec2flt/lemire.rs";
		.debug file 137 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/num/int_macros.rs";
		.debug file 138 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/num/dec2flt/parse.rs";
		.debug file 139 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/num/dec2flt/mod.rs";
		.debug file 140 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/num/diy_float.rs";
		.debug file 141 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/num/flt2dec/estimator.rs";
		.debug file 142 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/num/bignum.rs";
		.debug file 143 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/slice/iter.rs";
		.debug file 144 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/cmp.rs";
		.debug file 145 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/iter/adapters/zip.rs";
		.debug file 146 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/iter/traits/double_ended.rs";
		.debug file 147 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/option.rs";
		.debug file 148 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/iter/traits/iterator.rs";
		.debug file 149 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/num/flt2dec/mod.rs";
		.debug file 150 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/iter/adapters/cloned.rs";
		.debug file 151 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/num/fmt.rs";
		.debug file 152 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/num/error.rs";
		.debug file 153 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/num/int_log10.rs";
		.debug file 154 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/num/overflow_panic.rs";
		.debug file 155 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/ops/range.rs";
		.debug file 156 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/char/methods.rs";
		.debug file 157 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/ops/arith.rs";
		.debug file 158 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/result.rs";
		.debug file 159 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/mem/transmutability.rs";
		.debug file 160 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/ptr/alignment.rs";
		.debug file 161 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/num/nonzero.rs";
		.debug file 162 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/error.rs";
		.debug file 163 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/fmt/builders.rs";
		.debug file 164 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/macros/mod.rs";
		.debug file 165 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/any.rs";
		.debug file 166 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/ascii/ascii_char.rs";
		.debug file 167 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/convert/num.rs";
		.debug file 168 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/escape.rs";
		.debug file 169 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/ascii.rs";
		.debug file 170 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/cell.rs";
		.debug file 171 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/char/convert.rs";
		.debug file 172 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/char/decode.rs";
		.debug file 173 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/char/mod.rs";
		.debug file 174 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/ffi/c_str.rs";
		.debug file 175 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/slice/ascii.rs";
		.debug file 176 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/slice/memchr.rs";
		.debug file 177 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/ffi/mod.rs";
		.debug file 178 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/io/borrowed_buf.rs";
		.debug file 179 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/net/display_buffer.rs";
		.debug file 180 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/mem/maybe_uninit.rs";
		.debug file 181 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/net/ip_addr.rs";
		.debug file 182 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/net/parser.rs";
		.debug file 183 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/convert/mod.rs";
		.debug file 184 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/net/socket_addr.rs";
		.debug file 185 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/panicking.rs";
		.debug file 186 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/panic/location.rs";
		.debug file 187 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/panic/panic_info.rs";
		.debug file 188 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/sync/atomic.rs";
		.debug file 189 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/str/iter.rs";
		.debug file 190 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/slice/cmp.rs";
		.debug file 191 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/str/pattern.rs";
		.debug file 192 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/str/mod.rs";
		.debug file 193 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/fmt/float.rs";
		.debug file 194 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/num/flt2dec/decoder.rs";
		.debug file 195 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/str/count.rs";
		.debug file 196 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/str/validations.rs";
		.debug file 197 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/iter/traits/accum.rs";
		.debug file 198 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/iter/adapters/map.rs";
		.debug file 199 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/iter/adapters/filter.rs";
		.debug file 200 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/mem/mod.rs";
		.debug file 201 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/str/traits.rs";
		.debug file 202 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/clone.rs";
		.debug file 203 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/iter/adapters/fuse.rs";
		.debug file 204 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/iter/adapters/flatten.rs";
		.debug file 205 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/internal_macros.rs";
		.debug file 206 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/iter/adapters/take_while.rs";
		.debug file 207 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/iter/mod.rs";
		.debug file 208 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/ops/try_trait.rs";
		.debug file 209 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/str/converts.rs";
		.debug file 210 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/str/error.rs";
		.debug file 211 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/str/lossy.rs";
		.debug file 212 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/time.rs";
		.debug file 213 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/unicode/printable.rs";
		.debug file 214 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/task/wake.rs";
		.debug file 215 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/alloc/layout.rs";
		.debug file 216 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/alloc/mod.rs";
		.debug file 217 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/num/dec2flt/number.rs";
		.debug file 218 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/num/dec2flt/slow.rs";
		.debug file 219 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/ops/bit.rs";
		.debug file 220 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/array/iter.rs";
		.debug file 221 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core" "src/iter/adapters/chain.rs";
		.debug file 222 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/fmt" "mod.rs";
		.debug file 223 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/fmt" "num.rs";
		.debug file 224 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/ptr" "mod.rs";
		.debug file 225 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src" "panicking.rs";
		.debug file 226 "/Users/steve/.cargo/registry/src/index.crates.io-6f17d22bba15001f/half-1.8.3" "src/bfloat/convert.rs";
		.debug file 227 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/num" "mod.rs";
		.debug file 228 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/num" "uint_macros.rs";
		.debug file 229 "/Users/steve/.cargo/registry/src/index.crates.io-6f17d22bba15001f/half-1.8.3" "src/bfloat.rs";
		.debug file 230 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src" "result.rs";
		.debug file 231 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/ops" "function.rs";
		.debug file 232 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/num" "f32.rs";
		.debug file 233 "/Users/steve/.cargo/registry/src/index.crates.io-6f17d22bba15001f/half-1.8.3" "src/binary16/convert.rs";
		.debug file 234 "/Users/steve/.cargo/registry/src/index.crates.io-6f17d22bba15001f/half-1.8.3" "src/binary16.rs";
		.debug file 235 "/Users/steve/.cargo/registry/src/index.crates.io-6f17d22bba15001f/half-1.8.3" "src/slice.rs";
		.debug file 236 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/slice" "iter.rs";
		.debug file 237 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/slice" "index.rs";
		.debug file 238 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src" "intrinsics.rs";
		.debug file 239 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/ptr" "mut_ptr.rs";
		.debug file 240 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/slice" "mod.rs";
		.debug file 241 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/iter/traits" "iterator.rs";
		.debug file 242 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/slice/iter" "macros.rs";
		.debug file 243 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/iter/adapters" "enumerate.rs";
		.debug file 244 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/ptr" "non_null.rs";
		.debug file 245 "/Users/steve/Documents/repo/powdr-5_6_24/powdr/riscv/tests/riscv_data/keccak_powdr" "src/lib.rs";
		.debug file 246 "/Users/steve/Documents/repo/powdr-5_6_24/powdr/riscv-runtime" "src/lib.rs";
		.debug file 247 "/Users/steve/Documents/repo/powdr-5_6_24/powdr/riscv-runtime" "src/allocator.rs";
		.debug file 248 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/fmt" "mod.rs";
		.debug file 249 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/char" "methods.rs";
		.debug file 250 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src" "option.rs";
		.debug file 251 "/Users/steve/Documents/repo/powdr-5_6_24/powdr/riscv-runtime" "src/fmt.rs";
		.debug file 252 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/ptr" "non_null.rs";
		.debug file 253 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/slice/iter" "macros.rs";
		.debug file 254 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/iter/adapters" "copied.rs";
		.debug file 255 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/str" "iter.rs";
		.debug file 256 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/ptr" "mod.rs";
		.debug file 257 "/Users/steve/Documents/repo/powdr-5_6_24/powdr/riscv-runtime" "src/arith.rs";
		.debug file 258 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src" "result.rs";
		.debug file 259 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/num" "mod.rs";
		.debug file 260 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src" "intrinsics.rs";
		.debug file 261 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/array" "mod.rs";
		.debug file 262 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/convert" "mod.rs";
		.debug file 263 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/num" "uint_macros.rs";
		.debug file 264 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/slice" "mod.rs";
		.debug file 265 "/Users/steve/Documents/repo/powdr-5_6_24/powdr/riscv-runtime" "src/ec.rs";
		.debug file 266 "/Users/steve/Documents/repo/powdr-5_6_24/powdr/riscv-runtime" "src/hash.rs";
		.debug file 267 "/Users/steve/Documents/repo/powdr-5_6_24/powdr/riscv-runtime" "src/io.rs";
		.debug file 268 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/iter/adapters" "enumerate.rs";
		.debug file 269 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src" "cell.rs";
		.debug file 270 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/mem" "mod.rs";
		.debug file 271 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/alloc" "global.rs";
		.debug file 272 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src" "cmp.rs";
		.debug file 273 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/alloc" "layout.rs";
		.debug file 274 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/ptr" "alignment.rs";
		.debug file 275 "/Users/steve/Documents/repo/powdr-5_6_24/powdr/riscv-syscalls" "src/lib.rs";
		.debug file 276 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/fmt" "mod.rs";
		.debug file 277 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/slice" "cmp.rs";
		.debug file 278 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src" "cmp.rs";
		.debug file 279 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/str" "traits.rs";
		.debug file 280 "/Users/steve/.cargo/registry/src/index.crates.io-6f17d22bba15001f/serde-1.0.205" "src/de/impls.rs";
		.debug file 281 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/fmt" "mod.rs";
		.debug file 282 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/ptr" "mod.rs";
		.debug file 283 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/alloc/src" "boxed.rs";
		.debug file 284 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/alloc/src" "alloc.rs";
		.debug file 285 "/Users/steve/.cargo/registry/src/index.crates.io-6f17d22bba15001f/serde-1.0.205" "src/de/value.rs";
		.debug file 286 "/Users/steve/.cargo/registry/src/index.crates.io-6f17d22bba15001f/serde-1.0.205" "src/de/format.rs";
		.debug file 287 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/slice" "index.rs";
		.debug file 288 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/ptr" "mut_ptr.rs";
		.debug file 289 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src" "intrinsics.rs";
		.debug file 290 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/slice" "mod.rs";
		.debug file 291 "/Users/steve/.cargo/registry/src/index.crates.io-6f17d22bba15001f/serde-1.0.205" "src/de/ignored_any.rs";
		.debug file 292 "/Users/steve/.cargo/registry/src/index.crates.io-6f17d22bba15001f/serde-1.0.205" "src/de/mod.rs";
		.debug file 293 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/ptr" "non_null.rs";
		.debug file 294 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/slice/iter" "macros.rs";
		.debug file 295 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/iter/adapters" "enumerate.rs";
		.debug file 296 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/num" "f64.rs";
		.debug file 297 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/macros" "mod.rs";
		.debug file 298 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/slice" "memchr.rs";
		.debug file 299 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src" "option.rs";
		.debug file 300 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/slice" "cmp.rs";
		.debug file 301 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/str" "pattern.rs";
		.debug file 302 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/str" "mod.rs";
		.debug file 303 "/Users/steve/.cargo/registry/src/index.crates.io-6f17d22bba15001f/serde-1.0.205" "src/ser/fmt.rs";
		.debug file 304 "/Users/steve/.cargo/registry/src/index.crates.io-6f17d22bba15001f/serde-1.0.205" "src/private/de.rs";
		.debug file 305 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/alloc/src" "raw_vec.rs";
		.debug file 306 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/alloc/src/vec" "mod.rs";
		.debug file 307 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src" "result.rs";
		.debug file 308 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/alloc/src" "string.rs";
		.debug file 309 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/mem" "mod.rs";
		.debug file 310 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src" "cmp.rs";
		.debug file 311 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/str" "traits.rs";
		.debug file 312 "/Users/steve/.cargo/registry/src/index.crates.io-6f17d22bba15001f/serde-1.0.205" "src/private/ser.rs";
		.debug file 313 "/Users/steve/.cargo/registry/src/index.crates.io-6f17d22bba15001f/serde-1.0.205" "src/private/doc.rs";
		.debug file 314 "/Users/steve/.cargo/registry/src/index.crates.io-6f17d22bba15001f/serde-1.0.205" "src/private/mod.rs";
		.debug file 315 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/fmt" "mod.rs";
		.debug file 316 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/fmt" "num.rs";
		.debug file 317 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/ptr" "mod.rs";
		.debug file 318 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/alloc/src" "raw_vec.rs";
		.debug file 319 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src" "result.rs";
		.debug file 320 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/alloc/src" "alloc.rs";
		.debug file 321 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/num" "mod.rs";
		.debug file 322 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src" "cmp.rs";
		.debug file 323 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/alloc" "layout.rs";
		.debug file 324 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/num" "uint_macros.rs";
		.debug file 325 "/Users/steve/.cargo/registry/src/index.crates.io-6f17d22bba15001f/serde_cbor-0.11.2" "src/error.rs";
		.debug file 326 "/Users/steve/.cargo/registry/src/index.crates.io-6f17d22bba15001f/serde_cbor-0.11.2" "src/read.rs";
		.debug file 327 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/alloc/src/vec" "mod.rs";
		.debug file 328 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/slice" "index.rs";
		.debug file 329 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/ptr" "const_ptr.rs";
		.debug file 330 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/ptr" "mut_ptr.rs";
		.debug file 331 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src" "intrinsics.rs";
		.debug file 332 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/alloc/src/vec" "spec_extend.rs";
		.debug file 333 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/slice" "mod.rs";
		.debug file 334 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/mem" "mod.rs";
		.debug file 335 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/slice" "rotate.rs";
		.debug file 336 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/iter" "range.rs";
		.debug file 337 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src" "panic.rs";
		.debug file 338 "/Users/steve/.cargo/registry/src/index.crates.io-6f17d22bba15001f/serde_cbor-0.11.2" "src/write.rs";
		x10 <== and(x10, x10);
		x10 <== shl(x10, x10);
		x10, x11 <== split_gl(x10);
		x10 <=X= 0;
		x11 <=X= 0;
		// Set stack pointer
x2 <=X= 65536;
		x1 <== jump(__runtime_start);
		return;
		main:
		keccak_powdr_dash_11eac361efd62b6f___dot_Lfunc_begin0:
		  .debug loc 245 33 2;
		  .debug insn "ret ";
		  tmp1 <== jump_dyn(x1);
		__runtime_start:
		powdr_riscv_runtime_dash_663c9b00bfeeda72___dot_Lfunc_begin18:
		  .debug loc 246 44 9;
		  .debug insn "tail main";
		  tmp1 <== jump(main);
		// ecall handler
		__ecall_handler:
		branch_if_zero x5 - 0, __ecall_handler_input;
		branch_if_zero x5 - 1, __ecall_handler_data_identifier;
		branch_if_zero x5 - 2, __ecall_handler_output;
		__invalid_syscall:
		fail;
		__ecall_handler_input:
		x10 <=X= ${ std::prover::Query::Input(std::convert::int(std::prover::eval(x10))) };
		tmp1 <== jump_dyn(x1);
		__ecall_handler_data_identifier:
		x10 <=X= ${ std::prover::Query::DataIdentifier(std::convert::int(std::prover::eval(x11)), std::convert::int(std::prover::eval(x10))) };
		tmp1 <== jump_dyn(x1);
		__ecall_handler_output:
		x0 <=X= ${ std::prover::Query::Output(std::convert::int(std::prover::eval(x10)), std::convert::int(std::prover::eval(x11))) };
		tmp1 <== jump_dyn(x1);
		// end of ecall handler
    }
}    
