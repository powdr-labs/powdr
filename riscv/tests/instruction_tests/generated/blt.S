# 0 "sources/blt.S"
# 0 "<built-in>"
# 0 "<command-line>"
# 1 "/usr/include/stdc-predef.h" 1 3 4
# 0 "<command-line>" 2
# 1 "sources/blt.S"
# See LICENSE for license details.

#*****************************************************************************
# blt.S
#-----------------------------------------------------------------------------

# Test blt instruction.


# 1 "sources/riscv_test.h" 1
# 11 "sources/blt.S" 2
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
# 12 "sources/blt.S" 2


.globl __runtime_start; __runtime_start: la x10,__return_pointer; sw x1,0(x10); li x10,0

  #-------------------------------------------------------------
  # Branch tests
  #-------------------------------------------------------------

  # Each test checks both forward and backward branches

  test_2: li x28, 2; li x1, 0; li x2, 1; blt x1, x2, test_2_l2; bne x0, x28, fail; test_2_l1: bne x0, x28, test_2_l3; test_2_l2: blt x1, x2, test_2_l1; bne x0, x28, fail; test_2_l3:;
  test_3: li x28, 3; li x1, -1; li x2, 1; blt x1, x2, test_3_l2; bne x0, x28, fail; test_3_l1: bne x0, x28, test_3_l3; test_3_l2: blt x1, x2, test_3_l1; bne x0, x28, fail; test_3_l3:;
  test_4: li x28, 4; li x1, -2; li x2, -1; blt x1, x2, test_4_l2; bne x0, x28, fail; test_4_l1: bne x0, x28, test_4_l3; test_4_l2: blt x1, x2, test_4_l1; bne x0, x28, fail; test_4_l3:;

  test_5: li x28, 5; li x1, 1; li x2, 0; blt x1, x2, test_5_l1; bne x0, x28, test_5_l2; test_5_l1: bne x0, x28, fail; test_5_l2: blt x1, x2, test_5_l1; test_5_l3:;
  test_6: li x28, 6; li x1, 1; li x2, -1; blt x1, x2, test_6_l1; bne x0, x28, test_6_l2; test_6_l1: bne x0, x28, fail; test_6_l2: blt x1, x2, test_6_l1; test_6_l3:;
  test_7: li x28, 7; li x1, -1; li x2, -2; blt x1, x2, test_7_l1; bne x0, x28, test_7_l2; test_7_l1: bne x0, x28, fail; test_7_l2: blt x1, x2, test_7_l1; test_7_l3:;
  test_8: li x28, 8; li x1, 1; li x2, -2; blt x1, x2, test_8_l1; bne x0, x28, test_8_l2; test_8_l1: bne x0, x28, fail; test_8_l2: blt x1, x2, test_8_l1; test_8_l3:;

  #-------------------------------------------------------------
  # Bypassing tests
  #-------------------------------------------------------------

  test_9: li x28, 9; li x4, 0; test_9_l1: li x1, 0; li x2, -1; blt x1, x2, fail; addi x4, x4, 1; li x5, 2; bne x4, x5, test_9_l1;
  test_10: li x28, 10; li x4, 0; test_10_l1: li x1, 0; li x2, -1; nop; blt x1, x2, fail; addi x4, x4, 1; li x5, 2; bne x4, x5, test_10_l1;
  test_11: li x28, 11; li x4, 0; test_11_l1: li x1, 0; li x2, -1; nop; nop; blt x1, x2, fail; addi x4, x4, 1; li x5, 2; bne x4, x5, test_11_l1;
  test_12: li x28, 12; li x4, 0; test_12_l1: li x1, 0; nop; li x2, -1; blt x1, x2, fail; addi x4, x4, 1; li x5, 2; bne x4, x5, test_12_l1;
  test_13: li x28, 13; li x4, 0; test_13_l1: li x1, 0; nop; li x2, -1; nop; blt x1, x2, fail; addi x4, x4, 1; li x5, 2; bne x4, x5, test_13_l1;
  test_14: li x28, 14; li x4, 0; test_14_l1: li x1, 0; nop; nop; li x2, -1; blt x1, x2, fail; addi x4, x4, 1; li x5, 2; bne x4, x5, test_14_l1;

  test_15: li x28, 15; li x4, 0; test_15_l1: li x1, 0; li x2, -1; blt x1, x2, fail; addi x4, x4, 1; li x5, 2; bne x4, x5, test_15_l1;
  test_16: li x28, 16; li x4, 0; test_16_l1: li x1, 0; li x2, -1; nop; blt x1, x2, fail; addi x4, x4, 1; li x5, 2; bne x4, x5, test_16_l1;
  test_17: li x28, 17; li x4, 0; test_17_l1: li x1, 0; li x2, -1; nop; nop; blt x1, x2, fail; addi x4, x4, 1; li x5, 2; bne x4, x5, test_17_l1;
  test_18: li x28, 18; li x4, 0; test_18_l1: li x1, 0; nop; li x2, -1; blt x1, x2, fail; addi x4, x4, 1; li x5, 2; bne x4, x5, test_18_l1;
  test_19: li x28, 19; li x4, 0; test_19_l1: li x1, 0; nop; li x2, -1; nop; blt x1, x2, fail; addi x4, x4, 1; li x5, 2; bne x4, x5, test_19_l1;
  test_20: li x28, 20; li x4, 0; test_20_l1: li x1, 0; nop; nop; li x2, -1; blt x1, x2, fail; addi x4, x4, 1; li x5, 2; bne x4, x5, test_20_l1;

  #-------------------------------------------------------------
  # Test delay slot instructions not executed nor bypassed
  #-------------------------------------------------------------

  test_21: li x10, 21; ebreak; li x1, 1; blt x0, x1, test_21_l1; addi x1, x1, 1; addi x1, x1, 1; addi x1, x1, 1; addi x1, x1, 1; test_21_l1: addi x1, x1, 1; addi x1, x1, 1;; li x29, 3; li x28, 21; bne x1, x29, fail;
# 64 "sources/blt.S"
  bne x0, x28, pass; fail: unimp;; pass: la x10,__return_pointer; lw x1,0(x10); ret;



  .data
.balign 4; __return_pointer: .word 0;

 


