# 1 "sources/sb.S"
# 1 "<built-in>"
# 1 "<command-line>"
# 31 "<command-line>"
# 1 "/usr/include/stdc-predef.h" 1 3 4
# 32 "<command-line>" 2
# 1 "sources/sb.S"
# See LICENSE for license details.

#*****************************************************************************
# sb.S
#-----------------------------------------------------------------------------

# Test sb instruction.


# 1 "sources/riscv_test.h" 1
# 11 "sources/sb.S" 2
# 1 "sources/test_macros.h" 1






#-----------------------------------------------------------------------
# Helper macros
#-----------------------------------------------------------------------
# 20 "sources/test_macros.h"
# We use a macro hack to simpify code generation for various numbers
# of bubble cycles.
# 36 "sources/test_macros.h"
#-----------------------------------------------------------------------
# RV64UI MACROS
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------
# Tests for instructions with immediate operand
#-----------------------------------------------------------------------
# 92 "sources/test_macros.h"
#-----------------------------------------------------------------------
# Tests for vector config instructions
#-----------------------------------------------------------------------
# 120 "sources/test_macros.h"
#-----------------------------------------------------------------------
# Tests for an instruction with register operands
#-----------------------------------------------------------------------
# 148 "sources/test_macros.h"
#-----------------------------------------------------------------------
# Tests for an instruction with register-register operands
#-----------------------------------------------------------------------
# 242 "sources/test_macros.h"
#-----------------------------------------------------------------------
# Test memory instructions
#-----------------------------------------------------------------------
# 319 "sources/test_macros.h"
#-----------------------------------------------------------------------
# Test branch instructions
#-----------------------------------------------------------------------
# 404 "sources/test_macros.h"
#-----------------------------------------------------------------------
# Test jump instructions
#-----------------------------------------------------------------------
# 433 "sources/test_macros.h"
#-----------------------------------------------------------------------
# RV64UF MACROS
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------
# Tests floating-point instructions
#-----------------------------------------------------------------------
# 569 "sources/test_macros.h"
#-----------------------------------------------------------------------
# Pass and fail code (assumes test num is in x28)
#-----------------------------------------------------------------------
# 581 "sources/test_macros.h"
#-----------------------------------------------------------------------
# Test data section
#-----------------------------------------------------------------------
# 12 "sources/sb.S" 2


.globl __runtime_start; __runtime_start:

  #-------------------------------------------------------------
  # Basic tests
  #-------------------------------------------------------------

  test_2: li x10, 2; ebreak; la x1, tdat; li x2, 0xffffffaa; sb x2, 0(x1); lb x3, 0(x1);; li x29, 0xffffffaa; li x28, 2; bne x3, x29, fail;;
  test_3: li x10, 3; ebreak; la x1, tdat; li x2, 0x00000000; sb x2, 1(x1); lb x3, 1(x1);; li x29, 0x00000000; li x28, 3; bne x3, x29, fail;;
  test_4: li x10, 4; ebreak; la x1, tdat; li x2, 0xffffefa0; sb x2, 2(x1); lh x3, 2(x1);; li x29, 0xffffefa0; li x28, 4; bne x3, x29, fail;;
  test_5: li x10, 5; ebreak; la x1, tdat; li x2, 0x0000000a; sb x2, 3(x1); lb x3, 3(x1);; li x29, 0x0000000a; li x28, 5; bne x3, x29, fail;;

  # Test with negative offset

  test_6: li x10, 6; ebreak; la x1, tdat8; li x2, 0xffffffaa; sb x2, -3(x1); lb x3, -3(x1);; li x29, 0xffffffaa; li x28, 6; bne x3, x29, fail;;
  test_7: li x10, 7; ebreak; la x1, tdat8; li x2, 0x00000000; sb x2, -2(x1); lb x3, -2(x1);; li x29, 0x00000000; li x28, 7; bne x3, x29, fail;;
  test_8: li x10, 8; ebreak; la x1, tdat8; li x2, 0xffffffa0; sb x2, -1(x1); lb x3, -1(x1);; li x29, 0xffffffa0; li x28, 8; bne x3, x29, fail;;
  test_9: li x10, 9; ebreak; la x1, tdat8; li x2, 0x0000000a; sb x2, 0(x1); lb x3, 0(x1);; li x29, 0x0000000a; li x28, 9; bne x3, x29, fail;;

  # Test with a negative base

  test_10: li x10, 10; ebreak; la x1, tdat9; li x2, 0x12345678; addi x4, x1, -32; sb x2, 32(x4); lb x3, 0(x1);; li x29, 0x78; li x28, 10; bne x3, x29, fail;







  # Test with unaligned base

  test_11: li x10, 11; ebreak; la x1, tdat9; li x2, 0x00003098; addi x1, x1, -6; sb x2, 7(x1); la x4, tdat10; lb x3, 0(x4);; li x29, 0xffffff98; li x28, 11; bne x3, x29, fail;
# 53 "sources/sb.S"
  #-------------------------------------------------------------
  # Bypassing tests
  #-------------------------------------------------------------

  test_12: li x28, 12; li x4, 0; test_12_l1: li x1, 0xffffffdd; la x2, tdat; sb x1, 0(x2); lb x3, 0(x2); li x29, 0xffffffdd; bne x3, x29, fail; addi x4, x4, 1; li x5, 2; bne x4, x5, test_12_l1;
  test_13: li x28, 13; li x4, 0; test_13_l1: li x1, 0xffffffcd; la x2, tdat; nop; sb x1, 1(x2); lb x3, 1(x2); li x29, 0xffffffcd; bne x3, x29, fail; addi x4, x4, 1; li x5, 2; bne x4, x5, test_13_l1;
  test_14: li x28, 14; li x4, 0; test_14_l1: li x1, 0xffffffcc; la x2, tdat; nop; nop; sb x1, 2(x2); lb x3, 2(x2); li x29, 0xffffffcc; bne x3, x29, fail; addi x4, x4, 1; li x5, 2; bne x4, x5, test_14_l1;
  test_15: li x28, 15; li x4, 0; test_15_l1: li x1, 0xffffffbc; nop; la x2, tdat; sb x1, 3(x2); lb x3, 3(x2); li x29, 0xffffffbc; bne x3, x29, fail; addi x4, x4, 1; li x5, 2; bne x4, x5, test_15_l1;
  test_16: li x28, 16; li x4, 0; test_16_l1: li x1, 0xffffffbb; nop; la x2, tdat; nop; sb x1, 4(x2); lb x3, 4(x2); li x29, 0xffffffbb; bne x3, x29, fail; addi x4, x4, 1; li x5, 2; bne x4, x5, test_16_l1;
  test_17: li x28, 17; li x4, 0; test_17_l1: li x1, 0xffffffab; nop; nop; la x2, tdat; sb x1, 5(x2); lb x3, 5(x2); li x29, 0xffffffab; bne x3, x29, fail; addi x4, x4, 1; li x5, 2; bne x4, x5, test_17_l1;

  test_18: li x28, 18; li x4, 0; test_18_l1: la x2, tdat; li x1, 0x33; sb x1, 0(x2); lb x3, 0(x2); li x29, 0x33; bne x3, x29, fail; addi x4, x4, 1; li x5, 2; bne x4, x5, test_18_l1;
  test_19: li x28, 19; li x4, 0; test_19_l1: la x2, tdat; li x1, 0x23; nop; sb x1, 1(x2); lb x3, 1(x2); li x29, 0x23; bne x3, x29, fail; addi x4, x4, 1; li x5, 2; bne x4, x5, test_19_l1;
  test_20: li x28, 20; li x4, 0; test_20_l1: la x2, tdat; li x1, 0x22; nop; nop; sb x1, 2(x2); lb x3, 2(x2); li x29, 0x22; bne x3, x29, fail; addi x4, x4, 1; li x5, 2; bne x4, x5, test_20_l1;
  test_21: li x28, 21; li x4, 0; test_21_l1: la x2, tdat; nop; li x1, 0x12; sb x1, 3(x2); lb x3, 3(x2); li x29, 0x12; bne x3, x29, fail; addi x4, x4, 1; li x5, 2; bne x4, x5, test_21_l1;
  test_22: li x28, 22; li x4, 0; test_22_l1: la x2, tdat; nop; li x1, 0x11; nop; sb x1, 4(x2); lb x3, 4(x2); li x29, 0x11; bne x3, x29, fail; addi x4, x4, 1; li x5, 2; bne x4, x5, test_22_l1;
  test_23: li x28, 23; li x4, 0; test_23_l1: la x2, tdat; nop; nop; li x1, 0x01; sb x1, 5(x2); lb x3, 5(x2); li x29, 0x01; bne x3, x29, fail; addi x4, x4, 1; li x5, 2; bne x4, x5, test_23_l1;

  li a0, 0xef
  la a1, tdat
  sb a0, 3(a1)

  bne x0, x28, pass; fail: unimp;; pass: ___pass: j ___pass;



  .data
.balign 4;

 

tdat:
tdat1: .byte 0xef
tdat2: .byte 0xef
tdat3: .byte 0xef
tdat4: .byte 0xef
tdat5: .byte 0xef
tdat6: .byte 0xef
tdat7: .byte 0xef
tdat8: .byte 0xef
tdat9: .byte 0xef
tdat10: .byte 0xef


