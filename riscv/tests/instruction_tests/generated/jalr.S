# 1 "sources/jalr.S"
# 1 "<built-in>"
# 1 "<command-line>"
# 31 "<command-line>"
# 1 "/usr/include/stdc-predef.h" 1 3 4
# 32 "<command-line>" 2
# 1 "sources/jalr.S"
# See LICENSE for license details.

#*****************************************************************************
# jalr.S
#-----------------------------------------------------------------------------

# Test jalr instruction.


# 1 "sources/riscv_test.h" 1
# 11 "sources/jalr.S" 2
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
# 12 "sources/jalr.S" 2

.option norvc


.globl __runtime_start; __runtime_start:

  #-------------------------------------------------------------
  # Test 2: Basic test
  #-------------------------------------------------------------

test_2:
  li x28, 2
  li x31, 0
  la x2, target_2

linkaddr_2:
  jalr x19, x2, 0
  nop
  nop

  j fail

target_2:
  la x1, linkaddr_2
  addi x1, x1, 4
  bne x1, x19, fail

  #-------------------------------------------------------------
  # Test 3: Check r0 target and that r31 is not modified
  #-------------------------------------------------------------

test_3:
  li x28, 3
  li x31, 0
  la x3, target_3

linkaddr_3:
  jalr x0, x3, 0
  nop

  j fail

target_3:
  bne x31, x0, fail

  #-------------------------------------------------------------
  # Bypassing tests
  #-------------------------------------------------------------

  test_4: li x28, 4; li x4, 0; test_4_l1: la x6, test_4_l2; jalr x19, x6, 0; bne x0, x28, fail; test_4_l2: addi x4, x4, 1; li x5, 2; bne x4, x5, test_4_l1;
  test_5: li x28, 5; li x4, 0; test_5_l1: la x6, test_5_l2; nop; jalr x19, x6, 0; bne x0, x28, fail; test_5_l2: addi x4, x4, 1; li x5, 2; bne x4, x5, test_5_l1;
  test_6: li x28, 6; li x4, 0; test_6_l1: la x6, test_6_l2; nop; nop; jalr x19, x6, 0; bne x0, x28, fail; test_6_l2: addi x4, x4, 1; li x5, 2; bne x4, x5, test_6_l1;

  #-------------------------------------------------------------
  # Test delay slot instructions not executed nor bypassed
  #-------------------------------------------------------------

  test_7: li x10, 7; ebreak; li x1, 1; la x2, test_7_l1; jalr x19, x2, -4; addi x1, x1, 1; addi x1, x1, 1; addi x1, x1, 1; addi x1, x1, 1; test_7_l1: addi x1, x1, 1; addi x1, x1, 1;; li x29, 4; li x28, 7; bne x1, x29, fail;
# 81 "sources/jalr.S"
  bne x0, x28, pass; fail: unimp;; pass: ___pass: j ___pass;



  .data
.balign 4;

 


