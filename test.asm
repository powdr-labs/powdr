use std::machines::binary_bb::Binary16;
use std::machines::range::Bit2;
use std::machines::range::Bit6;
use std::machines::range::Bit7;
use std::machines::range::Byte;
use std::machines::range::Byte2;
use std::machines::binary::ByteBinary;
use std::machines::split::ByteCompare;
use std::machines::shift::ByteShift;
use std::machines::arith_bb::ArithBB;
use std::machines::arith_bb_mul::Arith16Mul;
machine Main with min_degree: 32, max_degree: 1048576 {
ArithBB arith_bb(byte2);
Arith16Mul arith_bb_mul;
Binary16 binary(byte_binary);
Bit2 bit2;
Bit6 bit6;
Bit7 bit7;
Byte byte;
Byte2 byte2;
ByteBinary byte_binary;
ByteCompare byte_compare;
ByteShift byte_shift;


    reg pc[@pc];
    reg XL[<=];
    reg XH[<=];
    reg YL[<=];
    reg YH[<=];
    reg ZL[<=];
    reg ZH[<=];
    reg WL[<=];
    reg WH[<=];

    std::machines::memory_bb::Memory memory(byte2);


    // Increased by 4 in each step, because we do up to 4 register memory accesses per step
    col fixed STEP(i) { 4 * i };

    // ============== memory instructions ==============

    /// Loads one word from an address V = val(X) + Y, where V can be between 0 and 2**33 (sic!),
    /// wraps the address to 32 bits and rounds it down to the next multiple of 4.
    /// Writes the loaded word and the remainder of the division by 4 to registers Z and W,
    /// respectively.
    instr mload XL, YH, YL, ZL, WL
        link ~> (tmp1_h, tmp1_l) = regs.mload(XL, STEP)

        link ~> (tmp2_h, tmp2_l) = arith_bb.add(tmp1_h, tmp1_l, YH, YL)

        link ~> (tmp3_h, tmp3_l) = memory.mload(tmp2_h * 2**16 + X_b2 * 0x100 + X_b1 * 4, STEP + 1)
        link ~> regs.mstore(ZL, STEP + 2, tmp3_h, tmp3_l)
        link ~> regs.mstore(WL, STEP + 3, 0, tmp4_l)
        link => bit2.check(tmp4_l)
        link => bit6.check(X_b1)
    {
        tmp2_l = X_b2 * 0x100 + X_b1 * 4 + tmp4_l
    }

    // Stores val(W) at address (V = val(X) - val(Y) + Z) % 2**32.
    // V can be between 0 and 2**33.
    // V should be a multiple of 4, but this instruction does not enforce it.
    instr mstore XL, YL, ZH, ZL, WL
        link ~> (tmp1_h, tmp1_l) = regs.mload(XL, STEP)
        link ~> (tmp2_h, tmp2_l) = regs.mload(YL, STEP + 1)
        link ~> (tmp3_h, tmp3_l) = regs.mload(WL, STEP + 2)

        link ~> (tmp4_h, tmp4_l) = arith_bb.sub(tmp1_h, tmp1_l, tmp2_h, tmp2_l)
        link ~> (tmp5_h, tmp5_l) = arith_bb.add(tmp4_h, tmp4_l, ZH, ZL)

        link ~> memory.mstore(tmp5_h * 2**16 + tmp5_l, STEP + 3, tmp3_h, tmp3_l);


    // =============== Register memory =======================
std::machines::memory_bb::Memory regs(byte2);
    // Get the value in register Y.
    instr get_reg YL -> XH, XL link ~> (XH, XL) = regs.mload(YL, STEP);

    // Set the value in register X to the value in register Y.
    instr set_reg XL, YH, YL -> link ~> regs.mstore(XL, STEP, YH, YL);

    // We still need these registers prover inputs.
    reg query_arg_1_h;
    reg query_arg_1_l;
    reg query_arg_2_h;
    reg query_arg_2_l;

    // Witness columns used in instuctions for intermediate values inside instructions.
    col witness tmp1_h;
    col witness tmp1_l;
    col witness tmp2_h;
    col witness tmp2_l;
    col witness tmp3_h;
    col witness tmp3_l;
    col witness tmp4_h;
    col witness tmp4_l;
    col witness tmp5_h;
    col witness tmp5_l;

    link => byte2.check(tmp1_h);
    link => byte2.check(tmp1_l);
    link => byte2.check(tmp2_h);
    link => byte2.check(tmp2_l);
    link => byte2.check(tmp3_h);
    link => byte2.check(tmp3_l);
    link => byte2.check(tmp4_h);
    link => byte2.check(tmp4_l);
    link => byte2.check(tmp5_h);
    link => byte2.check(tmp5_l);

    // We need to add these inline instead of using std::utils::is_zero
    // because when XX is not constrained, witgen will try to set XX,
    // XX_inv and XXIsZero to zero, which fails this constraint.
    // Therefore, we have to activate constrained whenever XXIsZero is used.
    // XXIsZero = 1 - XX * XX_inv
    col witness XX, XX_inv, XXIsZero;
    std::utils::force_bool(XXIsZero);
    XXIsZero * XX = 0;

    // ============== control-flow instructions ==============

    // Load the value of label `l` into register X.
    instr load_label XL, l: label
        link ~> regs.mstore(XL, STEP, tmp1_h, tmp1_l)
    {
        tmp1_h * 2**16 + tmp1_l = l
    }

    // Jump to `l` and store the return program counter in register W.
    instr jump l: label, WL
        link ~> regs.mstore(WL, STEP, tmp1_h, tmp1_l)
        link => byte.check(tmp1_h)
    {
        (tmp1_h * 2**16) + tmp1_l = pc + 1,
        pc' = l
    }
    
    // Jump to the address in register X and store the return program counter in register W.
    instr jump_dyn XL, WL
        link ~> (tmp1_h, tmp1_l) = regs.mload(XL, STEP)
        link ~> regs.mstore(WL, STEP, tmp2_h, tmp2_l)
        link => byte.check(tmp2_h)
    {
        tmp5_h = (tmp1_h * 2**16) + tmp1_l,
        pc' = tmp5_h,
        pc + 1 = tmp2_h * 2**16 + tmp2_l
    }

    // Jump to `l` if val(X) - val(Y) is nonzero, where X and Y are register ids.
    instr branch_if_diff_nonzero XL, YL, l: label
        link ~> (tmp1_h, tmp1_l) = regs.mload(XL, STEP)
        link ~> (tmp2_h, tmp2_l) = regs.mload(YL, STEP + 1)
        link ~> (tmp3_h, tmp3_l) = arith_bb.sub(tmp1_h, tmp1_l, tmp2_h, tmp2_l)
    {
        XXIsZero = 1 - XX * XX_inv,
        XX = (tmp3_h + tmp3_l),
        pc' = (1 - XXIsZero) * l + XXIsZero * (pc + 1)
    }

    // Jump to `l` if (val(X) - val(Y)) == Z, where X and Y are register ids and Z is a number.
    instr branch_if_diff_equal XL, YL, ZH, ZL, l: label
        link ~> (tmp1_h, tmp1_l) = regs.mload(XL, STEP)
        link ~> (tmp2_h, tmp2_l) = regs.mload(YL, STEP + 1)
        link ~> (tmp3_h, tmp3_l) = arith_bb.sub(tmp1_h, tmp1_l, tmp2_h, tmp2_l)
        link ~> (tmp4_h, tmp4_l) = arith_bb.sub(tmp3_h, tmp3_l, ZH, ZL)
    {
        XXIsZero = 1 - XX * XX_inv,
        // TODO is this correct?
        XX = tmp5_h + tmp5_l,
        pc' = XXIsZero * l + (1 - XXIsZero) * (pc + 1)
    }

    // Skips W instructions if val(X) - val(Y) + Z is zero, where X and Y are register ids and Z is a
    // constant offset.
    instr skip_if_equal XL, YL, ZH, ZL, WL
        link ~> (tmp1_h, tmp1_l) = regs.mload(XL, STEP)
        link ~> (tmp2_h, tmp2_l) = regs.mload(YL, STEP + 1)
        link ~> (tmp3_h, tmp3_l) = arith_bb.sub(tmp1_h, tmp1_l, tmp2_h, tmp2_l)
        link ~> (tmp4_h, tmp4_l) = arith_bb.add(tmp3_h, tmp3_l, ZH, ZL)
    {
        XXIsZero = 1 - XX * XX_inv,
        XX = tmp4_h + tmp4_l,
        pc' = pc + 1 + (XXIsZero * WL)
    }

    // Branches to `l` if V = val(X) - val(Y) - Z is positive, i.e. val(X) - val(Y) > Z,
    // where X and Y are register ids and Z is a constant.
    // V is required to be the difference of two 32-bit unsigned values.
    // i.e. -2**32 < V < 2**32.
    instr branch_if_diff_greater_than XL, YL, ZH, ZL, l: label
        link ~> (tmp1_h, tmp1_l) = regs.mload(XL, STEP)
        link ~> (tmp2_h, tmp2_l) = regs.mload(YL, STEP + 1)
        link ~> (tmp3_h, tmp3_l) = arith_bb.sub(tmp1_h, tmp1_l, tmp2_h, tmp2_l)
        link ~> (tmp4_h, tmp4_l) = arith_bb.sub(tmp3_h, tmp3_l, ZH, ZL)
    {
        tmp4_h = X_b1 + Y_7bit * 0x100 + wrap_bit * 0x8000,
        pc' = wrap_bit * l + (1 - wrap_bit) * (pc + 1)
    }

    // Stores 1 in register W if V = val(X) - val(Y) - Z is positive,
    // i.e. val(X) - val(Y) > Z, where X and Y are register ids and Z is a constant.
    // V is required to be the difference of two 32-bit unsigend values.
    // i.e. -2**32 < V < 2**32
    instr is_diff_greater_than XL, YL, ZH, ZL, WL
        link ~> (tmp1_h, tmp1_l) = regs.mload(XL, STEP)
        link ~> (tmp2_h, tmp2_l) = regs.mload(YL, STEP + 1)
        link ~> (tmp3_h, tmp3_l) = arith_bb.sub(tmp1_h, tmp1_l, tmp2_h, tmp2_l)
        link ~> (tmp4_h, tmp4_l) = arith_bb.sub(tmp3_h, tmp3_l, ZH, ZL)
        link ~> regs.mstore(WL, STEP + 2, 0, wrap_bit)
    {
        tmp4_h = X_b1 + Y_7bit * 0x100 + wrap_bit * 0x8000
    }

    // Stores val(X) * Z + W in register Y.
    instr affine XL, YL, ZH, ZL, WH, WL
        link ~> (tmp1_h, tmp1_l) = regs.mload(XL, STEP)
        // the mul machine is currently implemented as big endian, should change to match the rest
        link ~> (tmp3_l, tmp3_h, tmp2_l, tmp2_h) = arith_bb_mul.mul(tmp1_l, tmp1_h, ZL, ZH)
        // we ignore tmp3 because that's the high 32 bits of the 64 bits multiplication result
        link ~> (tmp4_h, tmp4_l) = arith_bb.add(tmp2_h, tmp2_l, WH, WL)
        link ~> regs.mstore(YL, STEP + 1, tmp4_h, tmp4_l);

    // ================= wrapping instructions =================

    // Computes V = val(X) + val(Y) + Z, wraps it in 32 bits, and stores the result in register W.
    // Requires 0 <= V < 2**33.
    instr add_wrap XL, YL, ZH, ZL, WL
        link ~> (tmp1_h, tmp1_l) = regs.mload(XL, STEP)
        link ~> (tmp2_h, tmp2_l) = regs.mload(YL, STEP + 1)
        link ~> (tmp3_h, tmp3_l) = arith_bb.add(tmp2_h, tmp2_l, ZH, ZL)
        link ~> (tmp4_h, tmp4_l) = arith_bb.add(tmp1_h, tmp1_l, tmp3_h, tmp3_l)
        link ~> regs.mstore(WL, STEP + 2, tmp4_h, tmp4_l)
    {
        // TODO is this correct?
        //tmp1_col + tmp2_col + Z = tmp3_col + wrap_bit * 2**32,
        //tmp3_col = X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000
    }

    /*
    // Computes V = val(X) - val(Y) + Z, wraps it in 32 bits, and stores the result in register W.
    // Requires -2**32 <= V < 2**32.
    instr sub_wrap_with_offset X, Y, Z, W
        link ~> tmp1_col = regs.mload(X, STEP)
        link ~> tmp2_col = regs.mload(Y, STEP + 1)
        link ~> regs.mstore(W, STEP + 2, tmp3_col)
    {
        (tmp1_col - tmp2_col + Z) + 2**32 = tmp3_col + wrap_bit * 2**32,
        tmp3_col = X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000
    }

    // ================= logical instructions =================

    // Stores 1 in register W if the value in register X is zero,
    // otherwise stores 0.
    instr is_equal_zero X, W
        link ~> tmp1_col = regs.mload(X, STEP)
        link ~> regs.mstore(W, STEP + 2, XXIsZero)
    {
        XXIsZero = 1 - XX * XX_inv,
        XX = tmp1_col
    }

    // Stores 1 in register W if val(X) == val(Y), otherwise stores 0.
    instr is_not_equal X, Y, W
        link ~> tmp1_col = regs.mload(X, STEP)
        link ~> tmp2_col = regs.mload(Y, STEP + 1)
        link ~> regs.mstore(W, STEP + 2, tmp3_col)
    {
        XXIsZero = 1 - XX * XX_inv,
        XX = tmp1_col - tmp2_col,
        tmp3_col = 1 - XXIsZero
    }
    */

    // ================= submachine instructions =================
    instr and XL, YL, ZH, ZL, WL link ~> (tmp1_h, tmp1_l) = regs.mload(XL, STEP) link ~> (tmp2_h, tmp2_l) = regs.mload(YL, STEP + 1) link ~> (tmp3_h, tmp3_l) = arith_bb.add(tmp2_h, tmp2_l, ZH, ZL) link ~> (tmp4_h, tmp4_l) = binary.and(tmp1_h, tmp1_l, tmp3_h, tmp3_l) link ~> regs.mstore(WL, STEP + 3, tmp4_h, tmp4_l){  }
    instr or XL, YL, ZH, ZL, WL link ~> (tmp1_h, tmp1_l) = regs.mload(XL, STEP) link ~> (tmp2_h, tmp2_l) = regs.mload(YL, STEP + 1) link ~> (tmp3_h, tmp3_l) = arith_bb.add(tmp2_h, tmp2_l, ZH, ZL) link ~> (tmp4_h, tmp4_l) = binary.or(tmp1_h, tmp1_l, tmp3_h, tmp3_l) link ~> regs.mstore(WL, STEP + 3, tmp4_h, tmp4_l){  }
    instr xor XL, YL, ZH, ZL, WL link ~> (tmp1_h, tmp1_l) = regs.mload(XL, STEP) link ~> (tmp2_h, tmp2_l) = regs.mload(YL, STEP + 1) link ~> (tmp3_h, tmp3_l) = arith_bb.add(tmp2_h, tmp2_l, ZH, ZL) link ~> (tmp4_h, tmp4_l) = binary.xor(tmp1_h, tmp1_l, tmp3_h, tmp3_l) link ~> regs.mstore(WL, STEP + 3, tmp4_h, tmp4_l){  }
    col witness X_b1;
    col witness X_b2;
    /*
    col witness X_b3;
    col witness X_b4;
    link => byte.check(X_b1);
    link => byte.check(X_b2);
    link => byte.check(X_b3);
    link => byte.check(X_b4);
    */
    col witness wrap_bit;
    wrap_bit * (1 - wrap_bit) = 0;

    /*
    // Sign extends the value in register X and stores it in register Y.
    // Input is a 32 bit unsigned number. We check bit 7 and set all higher bits to that value.
    instr sign_extend_byte X, Y
        link ~> tmp1_col = regs.mload(X, STEP)
        link ~> regs.mstore(Y, STEP + 3, tmp3_col)
    {
        // wrap_bit is used as sign_bit here.
        tmp1_col = Y_7bit + wrap_bit * 0x80 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000,
        tmp3_col = Y_7bit + wrap_bit * 0xffffff80
    }
    */
    col witness Y_7bit;
    link => bit7.check(Y_7bit);
    /*

    // Sign extends the value in register X and stores it in register Y.
    // Input is a 32 bit unsigned number. We check bit 15 and set all higher bits to that value.
    instr sign_extend_16_bits X, Y
        link ~> tmp1_col = regs.mload(X, STEP)
        link ~> regs.mstore(Y, STEP + 3, tmp3_col)
    {
        Y_15bit = X_b1 + Y_7bit * 0x100,

        // wrap_bit is used as sign_bit here.
        tmp1_col = Y_15bit + wrap_bit * 0x8000 + X_b3 * 0x10000 + X_b4 * 0x1000000,
        tmp3_col = Y_15bit + wrap_bit * 0xffff8000
    }
    col witness Y_15bit;
    */

    /*
    // Converts the value in register X to a signed number and stores it in register Y.
    // Input is a 32 bit unsigned number (0 <= val(X) < 2**32) interpreted as a two's complement numbers.
    // Returns a signed number (-2**31 <= val(Y) < 2**31).
    instr to_signed X, Y
        link ~> tmp1_col = regs.mload(X, STEP)
        link ~> regs.mstore(Y, STEP + 1, tmp3_col)
    {
        // wrap_bit is used as sign_bit here.
        tmp1_col = X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + Y_7bit * 0x1000000 + wrap_bit * 0x80000000,
        tmp3_col = tmp1_col - wrap_bit * 0x100000000
    }

    */
    // ======================= assertions =========================

    instr fail { 1 = 0 }

    /*
    // Wraps V = val(X) * Y and stores it in register Z,
    // where X is a register and Y is a constant factor.
    // Removes up to 16 bits beyond 32
    // TODO is this really safe?
    instr wrap16 X, Y, Z
        link ~> tmp1_col = regs.mload(X, STEP)
        link ~> regs.mstore(Z, STEP + 3, tmp3_col)
    {
        (tmp1_col * Y) = Y_b5 * 2**32 + Y_b6 * 2**40 + tmp3_col,
        tmp3_col = X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000
    }

    col witness Y_b5;
    col witness Y_b6;
    col witness Y_b7;
    col witness Y_b8;
    link => byte.check(Y_b5);
    link => byte.check(Y_b6);
    link => byte.check(Y_b7);
    link => byte.check(Y_b8);

    col witness REM_b1;
    col witness REM_b2;
    col witness REM_b3;
    col witness REM_b4;
    link => byte.check(REM_b1);
    link => byte.check(REM_b2);
    link => byte.check(REM_b3);
    link => byte.check(REM_b4);

    // Computes Q = val(Y) / val(X) and R = val(Y) % val(X) and stores them in registers Z and W.
    instr divremu Y, X, Z, W
        link ~> tmp1_col = regs.mload(Y, STEP)
        link ~> tmp2_col = regs.mload(X, STEP + 1)
        link ~> regs.mstore(Z, STEP + 2, tmp3_col)
        link ~> regs.mstore(W, STEP + 3, tmp4_col)
    {
        XXIsZero = 1 - XX * XX_inv,
        XX = tmp2_col,

        // if X is zero, remainder is set to dividend, as per RISC-V specification:
        tmp2_col * tmp3_col + tmp4_col = tmp1_col,

        // remainder >= 0:
        tmp4_col = REM_b1 + REM_b2 * 0x100 + REM_b3 * 0x10000 + REM_b4 * 0x1000000,

        // remainder < divisor, conditioned to val(X) not being 0:
        (1 - XXIsZero) * (tmp2_col - tmp4_col - 1 - Y_b5 - Y_b6 * 0x100 - Y_b7 * 0x10000 - Y_b8 * 0x1000000) = 0,

        // in case X is zero, we set quotient according to RISC-V specification
        XXIsZero * (tmp3_col - 0xffffffff) = 0,

        // quotient is 32 bits:
        tmp3_col = X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000
    }
    */

    // Computes V = val(X) * val(Y) and
    // stores the lower 32 bits in register Z and the upper 32 bits in register W.
    /*
    instr mul X, Y, Z, W
        link ~> tmp1_col = regs.mload(X, STEP)
        link ~> tmp2_col = regs.mload(Y, STEP + 1)
        link ~> (tmp3_col, tmp4_col) = split_gl.split(tmp1_col * tmp2_col)
        link ~> regs.mstore(Z, STEP + 2, tmp3_col)
        link ~> regs.mstore(W, STEP + 3, tmp4_col);
    */


  instr assert_eq XL, YH, YL
        link ~> (tmp1_h, tmp1_l) = regs.mload(XL, STEP)
  {
    tmp1_h = YH,
    tmp1_l = YL
  }

let initial_memory: (fe, fe)[] = [

];

    function main {
		and 0, 0, 0, 0, 0;
		jump __data_init, 1;
		set_reg 0, 0, 0;
		jump ___dot_L000110f4, 1;
		return;
		___dot_L000110f4:
		  set_reg 10, 1, 8896;
		  mstore 10, 0, 0, 0, 1;
		  add_wrap 0, 0, 0, 0, 10;
		  add_wrap 0, 0, 0, 2, 10;
		  set_reg 1, 255, 3840;
		  xor 1, 0, 65535, 65295, 3;
		  set_reg 29, 65280, 61455;
		  add_wrap 0, 0, 0, 2, 28;
		  branch_if_diff_nonzero 3, 29, ___dot_L000112b0;
		  add_wrap 0, 0, 0, 3, 10;
		  set_reg 1, 4080, 4080;
		  xor 1, 0, 0, 240, 3;
		  set_reg 29, 4080, 3840;
		  add_wrap 0, 0, 0, 3, 28;
		  branch_if_diff_nonzero 3, 29, ___dot_L000112b0;
		  add_wrap 0, 0, 0, 4, 10;
		  set_reg 1, 255, 2303;
		  xor 1, 0, 0, 1807, 3;
		  set_reg 29, 255, 4080;
		  add_wrap 0, 0, 0, 4, 28;
		  branch_if_diff_nonzero 3, 29, ___dot_L000112b0;
		  add_wrap 0, 0, 0, 5, 10;
		  set_reg 1, 61455, 61455;
		  xor 1, 0, 0, 240, 3;
		  set_reg 29, 61455, 61695;
		  add_wrap 0, 0, 0, 5, 28;
		  branch_if_diff_nonzero 3, 29, ___dot_L000112b0;
		  add_wrap 0, 0, 0, 6, 10;
		  set_reg 1, 65280, 63232;
		  xor 1, 0, 0, 1807, 1;
		  set_reg 29, 65280, 61455;
		  add_wrap 0, 0, 0, 6, 28;
		  branch_if_diff_nonzero 1, 29, ___dot_L000112b0;
		  add_wrap 0, 0, 0, 7, 10;
		  add_wrap 0, 0, 0, 0, 4;
		___dot_L00011194:
		  set_reg 1, 4080, 4080;
		  xor 1, 0, 0, 240, 3;
		  add_wrap 0, 3, 0, 0, 6;
		  add_wrap 4, 0, 0, 1, 4;
		  add_wrap 0, 0, 0, 2, 5;
		  branch_if_diff_nonzero 4, 5, ___dot_L00011194;
		  set_reg 29, 4080, 3840;
		  add_wrap 0, 0, 0, 7, 28;
		  branch_if_diff_nonzero 6, 29, ___dot_L000112b0;
		  add_wrap 0, 0, 0, 8, 10;
		  add_wrap 0, 0, 0, 0, 4;
		___dot_L000111bc:
		  set_reg 1, 255, 2303;
		  xor 1, 0, 0, 1807, 3;
		  add_wrap 0, 3, 0, 0, 6;
		  add_wrap 4, 0, 0, 1, 4;
		  add_wrap 0, 0, 0, 2, 5;
		  branch_if_diff_nonzero 4, 5, ___dot_L000111bc;
		  set_reg 29, 255, 4080;
		  add_wrap 0, 0, 0, 8, 28;
		  branch_if_diff_nonzero 6, 29, ___dot_L000112b0;
		  add_wrap 0, 0, 0, 9, 10;
		  add_wrap 0, 0, 0, 0, 4;
		___dot_L000111e6:
		  set_reg 1, 61455, 61455;
		  xor 1, 0, 0, 240, 3;
		  add_wrap 0, 3, 0, 0, 6;
		  add_wrap 4, 0, 0, 1, 4;
		  add_wrap 0, 0, 0, 2, 5;
		  branch_if_diff_nonzero 4, 5, ___dot_L000111e6;
		  set_reg 29, 61455, 61695;
		  add_wrap 0, 0, 0, 9, 28;
		  branch_if_diff_nonzero 6, 29, ___dot_L000112b0;
		  add_wrap 0, 0, 0, 10, 10;
		  add_wrap 0, 0, 0, 0, 4;
		___dot_L00011212:
		  set_reg 1, 4080, 4080;
		  xor 1, 0, 0, 240, 3;
		  add_wrap 4, 0, 0, 1, 4;
		  add_wrap 0, 0, 0, 2, 5;
		  branch_if_diff_nonzero 4, 5, ___dot_L00011212;
		  set_reg 29, 4080, 3840;
		  add_wrap 0, 0, 0, 10, 28;
		  branch_if_diff_nonzero 3, 29, ___dot_L000112b0;
		  add_wrap 0, 0, 0, 11, 10;
		  add_wrap 0, 0, 0, 0, 4;
		___dot_L00011238:
		  set_reg 1, 255, 4095;
		  xor 1, 0, 0, 15, 3;
		  add_wrap 4, 0, 0, 1, 4;
		  add_wrap 0, 0, 0, 2, 5;
		  branch_if_diff_nonzero 4, 5, ___dot_L00011238;
		  set_reg 29, 255, 4080;
		  add_wrap 0, 0, 0, 11, 28;
		  branch_if_diff_nonzero 3, 29, ___dot_L000112b0;
		  add_wrap 0, 0, 0, 12, 10;
		  add_wrap 0, 0, 0, 0, 4;
		___dot_L0001125e:
		  set_reg 1, 61455, 61455;
		  xor 1, 0, 0, 240, 3;
		  add_wrap 4, 0, 0, 1, 4;
		  add_wrap 0, 0, 0, 2, 5;
		  branch_if_diff_nonzero 4, 5, ___dot_L0001125e;
		  set_reg 29, 61455, 61695;
		  add_wrap 0, 0, 0, 12, 28;
		  branch_if_diff_nonzero 3, 29, ___dot_L000112b0;
		  add_wrap 0, 0, 0, 13, 10;
		  xor 0, 0, 0, 240, 1;
		  add_wrap 0, 0, 0, 240, 29;
		  add_wrap 0, 0, 0, 13, 28;
		  branch_if_diff_nonzero 1, 29, ___dot_L000112b0;
		  add_wrap 0, 0, 0, 14, 10;
		  set_reg 1, 255, 255;
		  add_wrap 0, 0, 0, 0, 29;
		  add_wrap 0, 0, 0, 14, 28;
		  branch_if_diff_nonzero 0, 29, ___dot_L000112b0;
		  branch_if_diff_nonzero 0, 28, ___dot_L000112b2;
		___dot_L000112b0:
		  fail;
		___dot_L000112b2:
		  set_reg 10, 1, 8896;
		  mload 10, 0, 0, 1, 32;
		  jump_dyn 1, 32;
		// This is the data initialization routine.
		__data_init:
		set_reg 33, 0x464c, 0x457f;
		set_reg 32, 0x1, 0x0;
		mstore 32, 0, 0, 0, 33;
		set_reg 33, 0x1, 0x101;
		set_reg 32, 0x1, 0x4;
		mstore 32, 0, 0, 0, 33;
		set_reg 33, 0xf3, 0x2;
		set_reg 32, 0x1, 0x10;
		mstore 32, 0, 0, 0, 33;
		set_reg 33, 0x0, 0x1;
		set_reg 32, 0x1, 0x14;
		mstore 32, 0, 0, 0, 33;
		set_reg 33, 0x1, 0x10f4;
		set_reg 32, 0x1, 0x18;
		mstore 32, 0, 0, 0, 33;
		set_reg 33, 0x0, 0x34;
		set_reg 32, 0x1, 0x1c;
		mstore 32, 0, 0, 0, 33;
		set_reg 33, 0x0, 0x7b0;
		set_reg 32, 0x1, 0x20;
		mstore 32, 0, 0, 0, 33;
		set_reg 33, 0x0, 0x1;
		set_reg 32, 0x1, 0x24;
		mstore 32, 0, 0, 0, 33;
		set_reg 33, 0x20, 0x34;
		set_reg 32, 0x1, 0x28;
		mstore 32, 0, 0, 0, 33;
		set_reg 33, 0x28, 0x6;
		set_reg 32, 0x1, 0x2c;
		mstore 32, 0, 0, 0, 33;
		set_reg 33, 0x8, 0xa;
		set_reg 32, 0x1, 0x30;
		mstore 32, 0, 0, 0, 33;
		set_reg 33, 0x0, 0x6;
		set_reg 32, 0x1, 0x34;
		mstore 32, 0, 0, 0, 33;
		set_reg 33, 0x0, 0x34;
		set_reg 32, 0x1, 0x38;
		mstore 32, 0, 0, 0, 33;
		set_reg 33, 0x1, 0x34;
		set_reg 32, 0x1, 0x3c;
		mstore 32, 0, 0, 0, 33;
		set_reg 33, 0x1, 0x34;
		set_reg 32, 0x1, 0x40;
		mstore 32, 0, 0, 0, 33;
		set_reg 33, 0x0, 0xc0;
		set_reg 32, 0x1, 0x44;
		mstore 32, 0, 0, 0, 33;
		set_reg 33, 0x0, 0xc0;
		set_reg 32, 0x1, 0x48;
		mstore 32, 0, 0, 0, 33;
		set_reg 33, 0x0, 0x4;
		set_reg 32, 0x1, 0x4c;
		mstore 32, 0, 0, 0, 33;
		set_reg 33, 0x0, 0x4;
		set_reg 32, 0x1, 0x50;
		mstore 32, 0, 0, 0, 33;
		set_reg 33, 0x0, 0x1;
		set_reg 32, 0x1, 0x54;
		mstore 32, 0, 0, 0, 33;
		set_reg 33, 0x1, 0x0;
		set_reg 32, 0x1, 0x5c;
		mstore 32, 0, 0, 0, 33;
		set_reg 33, 0x1, 0x0;
		set_reg 32, 0x1, 0x60;
		mstore 32, 0, 0, 0, 33;
		set_reg 33, 0x0, 0xf4;
		set_reg 32, 0x1, 0x64;
		mstore 32, 0, 0, 0, 33;
		set_reg 33, 0x0, 0xf4;
		set_reg 32, 0x1, 0x68;
		mstore 32, 0, 0, 0, 33;
		set_reg 33, 0x0, 0x4;
		set_reg 32, 0x1, 0x6c;
		mstore 32, 0, 0, 0, 33;
		set_reg 33, 0x0, 0x1000;
		set_reg 32, 0x1, 0x70;
		mstore 32, 0, 0, 0, 33;
		set_reg 33, 0x0, 0x1;
		set_reg 32, 0x1, 0x74;
		mstore 32, 0, 0, 0, 33;
		set_reg 33, 0x0, 0xf4;
		set_reg 32, 0x1, 0x78;
		mstore 32, 0, 0, 0, 33;
		set_reg 33, 0x1, 0x10f4;
		set_reg 32, 0x1, 0x7c;
		mstore 32, 0, 0, 0, 33;
		set_reg 33, 0x1, 0x10f4;
		set_reg 32, 0x1, 0x80;
		mstore 32, 0, 0, 0, 33;
		set_reg 33, 0x0, 0x1cc;
		set_reg 32, 0x1, 0x84;
		mstore 32, 0, 0, 0, 33;
		set_reg 33, 0x0, 0x1cc;
		set_reg 32, 0x1, 0x88;
		mstore 32, 0, 0, 0, 33;
		set_reg 33, 0x0, 0x5;
		set_reg 32, 0x1, 0x8c;
		mstore 32, 0, 0, 0, 33;
		set_reg 33, 0x0, 0x1000;
		set_reg 32, 0x1, 0x90;
		mstore 32, 0, 0, 0, 33;
		set_reg 33, 0x0, 0x1;
		set_reg 32, 0x1, 0x94;
		mstore 32, 0, 0, 0, 33;
		set_reg 33, 0x0, 0x2c0;
		set_reg 32, 0x1, 0x98;
		mstore 32, 0, 0, 0, 33;
		set_reg 33, 0x1, 0x22c0;
		set_reg 32, 0x1, 0x9c;
		mstore 32, 0, 0, 0, 33;
		set_reg 33, 0x1, 0x22c0;
		set_reg 32, 0x1, 0xa0;
		mstore 32, 0, 0, 0, 33;
		set_reg 33, 0x0, 0x4;
		set_reg 32, 0x1, 0xa4;
		mstore 32, 0, 0, 0, 33;
		set_reg 33, 0x0, 0x4;
		set_reg 32, 0x1, 0xa8;
		mstore 32, 0, 0, 0, 33;
		set_reg 33, 0x0, 0x6;
		set_reg 32, 0x1, 0xac;
		mstore 32, 0, 0, 0, 33;
		set_reg 33, 0x0, 0x1000;
		set_reg 32, 0x1, 0xb0;
		mstore 32, 0, 0, 0, 33;
		set_reg 33, 0x6474, 0xe551;
		set_reg 32, 0x1, 0xb4;
		mstore 32, 0, 0, 0, 33;
		set_reg 33, 0x0, 0x6;
		set_reg 32, 0x1, 0xcc;
		mstore 32, 0, 0, 0, 33;
		set_reg 33, 0x7000, 0x3;
		set_reg 32, 0x1, 0xd4;
		mstore 32, 0, 0, 0, 33;
		set_reg 33, 0x0, 0x414;
		set_reg 32, 0x1, 0xd8;
		mstore 32, 0, 0, 0, 33;
		set_reg 33, 0x0, 0x32;
		set_reg 32, 0x1, 0xe4;
		mstore 32, 0, 0, 0, 33;
		set_reg 33, 0x0, 0x32;
		set_reg 32, 0x1, 0xe8;
		mstore 32, 0, 0, 0, 33;
		set_reg 33, 0x0, 0x4;
		set_reg 32, 0x1, 0xec;
		mstore 32, 0, 0, 0, 33;
		set_reg 33, 0x0, 0x1;
		set_reg 32, 0x1, 0xf0;
		mstore 32, 0, 0, 0, 33;
		// This is the end of the data initialization routine.
		jump_dyn 1, 32;
		// ecall handler
		__ecall_handler:
		branch_if_diff_equal 5, 0, 0, 0, __ecall_handler_input;
		branch_if_diff_equal 5, 0, 0, 1, __ecall_handler_data_identifier;
		branch_if_diff_equal 5, 0, 0, 2, __ecall_handler_output;
		branch_if_diff_equal 5, 0, 0, 9, __ecall_handler_halt;
		__invalid_syscall:
		fail;
		__ecall_handler_input:
		query_arg_1_h, query_arg_1_l <== get_reg(10);
		set_reg 10, 0, ${ std::prelude::Query::Input(std::convert::int(std::prover::eval(query_arg_1_l))) };
		jump_dyn 1, 32;
		__ecall_handler_data_identifier:
		query_arg_1_h, query_arg_1_l <== get_reg(10);
		query_arg_2_h, query_arg_2_l <== get_reg(11);
		set_reg 10, 0, ${ std::prelude::Query::DataIdentifier(std::convert::int(std::prover::eval(query_arg_2_l)), std::convert::int(std::prover::eval(query_arg_1_l))) };
		jump_dyn 1, 32;
		__ecall_handler_output:
		query_arg_1_h, query_arg_1_l <== get_reg(10);
		query_arg_2_h, query_arg_2_l <== get_reg(11);
		set_reg 0, 0, ${ std::prelude::Query::Output(std::convert::int(std::prover::eval(query_arg_1_l)), std::convert::int(std::prover::eval(query_arg_2_l))) };
		jump_dyn 1, 32;
		__ecall_handler_halt:
		return;
		jump_dyn 1, 32;
		// end of ecall handler
    }
}    
