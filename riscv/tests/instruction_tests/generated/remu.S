# 0 "sources/remu.S"
# 0 "<built-in>"
# 0 "<command-line>"
# 1 "/usr/include/stdc-predef.h" 1 3 4
# 0 "<command-line>" 2
# 1 "sources/remu.S"
# See LICENSE for license details.

#*****************************************************************************
# remu.S
#-----------------------------------------------------------------------------

# Test remu instruction.


# 1 "sources/riscv_test.h" 1
# 11 "sources/remu.S" 2
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
# 12 "sources/remu.S" 2


.globl __runtime_start; __runtime_start: la x10,__return_pointer; sw x1,0(x10); li x10,0

  #-------------------------------------------------------------
  # Arithmetic tests
  #-------------------------------------------------------------

  test_2: li x10, 2; ebreak; li x1, 20; li x2, 6; remu x3, x1, x2;; li x29, 2; li x28, 2; bne x3, x29, fail;;
  test_3: li x10, 3; ebreak; li x1, -20; li x2, 6; remu x3, x1, x2;; li x29, 2; li x28, 3; bne x3, x29, fail;;
  test_4: li x10, 4; ebreak; li x1, 20; li x2, -6; remu x3, x1, x2;; li x29, 20; li x28, 4; bne x3, x29, fail;;
  test_5: li x10, 5; ebreak; li x1, -20; li x2, -6; remu x3, x1, x2;; li x29, -20; li x28, 5; bne x3, x29, fail;;

  test_6: li x10, 6; ebreak; li x1, -1<<31; li x2, 1; remu x3, x1, x2;; li x29, 0; li x28, 6; bne x3, x29, fail;;
  test_7: li x10, 7; ebreak; li x1, -1<<31; li x2, -1; remu x3, x1, x2;; li x29, -1<<31; li x28, 7; bne x3, x29, fail;;

  test_8: li x10, 8; ebreak; li x1, -1<<31; li x2, 0; remu x3, x1, x2;; li x29, -1<<31; li x28, 8; bne x3, x29, fail;;
  test_9: li x10, 9; ebreak; li x1, 1; li x2, 0; remu x3, x1, x2;; li x29, 1; li x28, 9; bne x3, x29, fail;;
  test_10: li x10, 10; ebreak; li x1, 0; li x2, 0; remu x3, x1, x2;; li x29, 0; li x28, 10; bne x3, x29, fail;;

  bne x0, x28, pass; fail: unimp;; pass: la x10,__return_pointer; lw x1,0(x10); ret;



  .data
.balign 4; __return_pointer: .word 0;

 


