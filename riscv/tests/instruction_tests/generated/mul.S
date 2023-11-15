# 0 "sources/mul.S"
# 0 "<built-in>"
# 0 "<command-line>"
# 1 "/usr/include/stdc-predef.h" 1 3 4
# 0 "<command-line>" 2
# 1 "sources/mul.S"
# See LICENSE for license details.

#*****************************************************************************
# mul.S
#-----------------------------------------------------------------------------

# Test mul instruction.


# 1 "sources/riscv_test.h" 1
# 11 "sources/mul.S" 2
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
# 12 "sources/mul.S" 2


.globl __runtime_start; __runtime_start: la x10,__return_pointer; sw x1,0(x10); li x10,0

  #-------------------------------------------------------------
  # Arithmetic tests
  #-------------------------------------------------------------

  test_32: li x10, 32; ebreak; li x1, 0x00007e00; li x2, 0xb6db6db7; mul x3, x1, x2;; li x29, 0x00001200; li x28, 32; bne x3, x29, fail;;
  test_33: li x10, 33; ebreak; li x1, 0x00007fc0; li x2, 0xb6db6db7; mul x3, x1, x2;; li x29, 0x00001240; li x28, 33; bne x3, x29, fail;;

  test_2: li x10, 2; ebreak; li x1, 0x00000000; li x2, 0x00000000; mul x3, x1, x2;; li x29, 0x00000000; li x28, 2; bne x3, x29, fail;;
  test_3: li x10, 3; ebreak; li x1, 0x00000001; li x2, 0x00000001; mul x3, x1, x2;; li x29, 0x00000001; li x28, 3; bne x3, x29, fail;;
  test_4: li x10, 4; ebreak; li x1, 0x00000003; li x2, 0x00000007; mul x3, x1, x2;; li x29, 0x00000015; li x28, 4; bne x3, x29, fail;;

  test_5: li x10, 5; ebreak; li x1, 0x00000000; li x2, 0xffff8000; mul x3, x1, x2;; li x29, 0x00000000; li x28, 5; bne x3, x29, fail;;
  test_6: li x10, 6; ebreak; li x1, 0x80000000; li x2, 0x00000000; mul x3, x1, x2;; li x29, 0x00000000; li x28, 6; bne x3, x29, fail;;
  test_7: li x10, 7; ebreak; li x1, 0x80000000; li x2, 0xffff8000; mul x3, x1, x2;; li x29, 0x00000000; li x28, 7; bne x3, x29, fail;;

  test_30: li x10, 30; ebreak; li x1, 0xaaaaaaab; li x2, 0x0002fe7d; mul x3, x1, x2;; li x29, 0x0000ff7f; li x28, 30; bne x3, x29, fail;;
  test_31: li x10, 31; ebreak; li x1, 0x0002fe7d; li x2, 0xaaaaaaab; mul x3, x1, x2;; li x29, 0x0000ff7f; li x28, 31; bne x3, x29, fail;;

  test_34: li x10, 34; ebreak; li x1, 0xff000000; li x2, 0xff000000; mul x3, x1, x2;; li x29, 0x00000000; li x28, 34; bne x3, x29, fail;;

  test_35: li x10, 35; ebreak; li x1, 0xffffffff; li x2, 0xffffffff; mul x3, x1, x2;; li x29, 0x00000001; li x28, 35; bne x3, x29, fail;;
  test_36: li x10, 36; ebreak; li x1, 0xffffffff; li x2, 0x00000001; mul x3, x1, x2;; li x29, 0xffffffff; li x28, 36; bne x3, x29, fail;;
  test_37: li x10, 37; ebreak; li x1, 0x00000001; li x2, 0xffffffff; mul x3, x1, x2;; li x29, 0xffffffff; li x28, 37; bne x3, x29, fail;;

  #-------------------------------------------------------------
  # Source/Destination tests
  #-------------------------------------------------------------

  test_8: li x10, 8; ebreak; li x1, 13; li x2, 11; mul x1, x1, x2;; li x29, 143; li x28, 8; bne x1, x29, fail;;
  test_9: li x10, 9; ebreak; li x1, 14; li x2, 11; mul x2, x1, x2;; li x29, 154; li x28, 9; bne x2, x29, fail;;
  test_10: li x10, 10; ebreak; li x1, 13; mul x1, x1, x1;; li x29, 169; li x28, 10; bne x1, x29, fail;;

  #-------------------------------------------------------------
  # Bypassing tests
  #-------------------------------------------------------------

  test_11: li x10, 11; ebreak; li x4, 0; test_11_l1: li x1, 13; li x2, 11; mul x3, x1, x2; addi x6, x3, 0; addi x4, x4, 1; li x5, 2; bne x4, x5, test_11_l1; li x29, 143; li x28, 11; bne x6, x29, fail;;
  test_12: li x10, 12; ebreak; li x4, 0; test_12_l1: li x1, 14; li x2, 11; mul x3, x1, x2; nop; addi x6, x3, 0; addi x4, x4, 1; li x5, 2; bne x4, x5, test_12_l1; li x29, 154; li x28, 12; bne x6, x29, fail;;
  test_13: li x10, 13; ebreak; li x4, 0; test_13_l1: li x1, 15; li x2, 11; mul x3, x1, x2; nop; nop; addi x6, x3, 0; addi x4, x4, 1; li x5, 2; bne x4, x5, test_13_l1; li x29, 165; li x28, 13; bne x6, x29, fail;;

  test_14: li x10, 14; ebreak; li x4, 0; test_14_l1: li x1, 13; li x2, 11; mul x3, x1, x2; addi x4, x4, 1; li x5, 2; bne x4, x5, test_14_l1; li x29, 143; li x28, 14; bne x3, x29, fail;;
  test_15: li x10, 15; ebreak; li x4, 0; test_15_l1: li x1, 14; li x2, 11; nop; mul x3, x1, x2; addi x4, x4, 1; li x5, 2; bne x4, x5, test_15_l1; li x29, 154; li x28, 15; bne x3, x29, fail;;
  test_16: li x10, 16; ebreak; li x4, 0; test_16_l1: li x1, 15; li x2, 11; nop; nop; mul x3, x1, x2; addi x4, x4, 1; li x5, 2; bne x4, x5, test_16_l1; li x29, 165; li x28, 16; bne x3, x29, fail;;
  test_17: li x10, 17; ebreak; li x4, 0; test_17_l1: li x1, 13; nop; li x2, 11; mul x3, x1, x2; addi x4, x4, 1; li x5, 2; bne x4, x5, test_17_l1; li x29, 143; li x28, 17; bne x3, x29, fail;;
  test_18: li x10, 18; ebreak; li x4, 0; test_18_l1: li x1, 14; nop; li x2, 11; nop; mul x3, x1, x2; addi x4, x4, 1; li x5, 2; bne x4, x5, test_18_l1; li x29, 154; li x28, 18; bne x3, x29, fail;;
  test_19: li x10, 19; ebreak; li x4, 0; test_19_l1: li x1, 15; nop; nop; li x2, 11; mul x3, x1, x2; addi x4, x4, 1; li x5, 2; bne x4, x5, test_19_l1; li x29, 165; li x28, 19; bne x3, x29, fail;;

  test_20: li x10, 20; ebreak; li x4, 0; test_20_l1: li x2, 11; li x1, 13; mul x3, x1, x2; addi x4, x4, 1; li x5, 2; bne x4, x5, test_20_l1; li x29, 143; li x28, 20; bne x3, x29, fail;;
  test_21: li x10, 21; ebreak; li x4, 0; test_21_l1: li x2, 11; li x1, 14; nop; mul x3, x1, x2; addi x4, x4, 1; li x5, 2; bne x4, x5, test_21_l1; li x29, 154; li x28, 21; bne x3, x29, fail;;
  test_22: li x10, 22; ebreak; li x4, 0; test_22_l1: li x2, 11; li x1, 15; nop; nop; mul x3, x1, x2; addi x4, x4, 1; li x5, 2; bne x4, x5, test_22_l1; li x29, 165; li x28, 22; bne x3, x29, fail;;
  test_23: li x10, 23; ebreak; li x4, 0; test_23_l1: li x2, 11; nop; li x1, 13; mul x3, x1, x2; addi x4, x4, 1; li x5, 2; bne x4, x5, test_23_l1; li x29, 143; li x28, 23; bne x3, x29, fail;;
  test_24: li x10, 24; ebreak; li x4, 0; test_24_l1: li x2, 11; nop; li x1, 14; nop; mul x3, x1, x2; addi x4, x4, 1; li x5, 2; bne x4, x5, test_24_l1; li x29, 154; li x28, 24; bne x3, x29, fail;;
  test_25: li x10, 25; ebreak; li x4, 0; test_25_l1: li x2, 11; nop; nop; li x1, 15; mul x3, x1, x2; addi x4, x4, 1; li x5, 2; bne x4, x5, test_25_l1; li x29, 165; li x28, 25; bne x3, x29, fail;;

  test_26: li x10, 26; ebreak; li x1, 31; mul x2, x0, x1;; li x29, 0; li x28, 26; bne x2, x29, fail;;
  test_27: li x10, 27; ebreak; li x1, 32; mul x2, x1, x0;; li x29, 0; li x28, 27; bne x2, x29, fail;;
  test_28: li x10, 28; ebreak; mul x1, x0, x0;; li x29, 0; li x28, 28; bne x1, x29, fail;;
  test_29: li x10, 29; ebreak; li x1, 33; li x2, 34; mul x0, x1, x2;; li x29, 0; li x28, 29; bne x0, x29, fail;;

  bne x0, x28, pass; fail: unimp;; pass: la x10,__return_pointer; lw x1,0(x10); ret;



  .data
.balign 4; __return_pointer: .word 0;

 


