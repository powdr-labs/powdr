# 0 "sources/lb.S"
# 0 "<built-in>"
# 0 "<command-line>"
# 1 "/usr/include/stdc-predef.h" 1 3 4
# 0 "<command-line>" 2
# 1 "sources/lb.S"
# See LICENSE for license details.

#*****************************************************************************
# lb.S
#-----------------------------------------------------------------------------

# Test lb instruction.


# 1 "sources/riscv_test.h" 1
# 11 "sources/lb.S" 2
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
# 12 "sources/lb.S" 2


.globl __runtime_start; __runtime_start: la x10,__return_pointer; sw x1,0(x10); li x10,0

  #-------------------------------------------------------------
  # Basic tests
  #-------------------------------------------------------------

  test_2: li x10, 2; ebreak; la x1, tdat1; lb x3, 0(x1);; li x29, 0xffffffff; li x28, 2; bne x3, x29, fail;;
  test_3: li x10, 3; ebreak; la x1, tdat2; lb x3, 0(x1);; li x29, 0x00000000; li x28, 3; bne x3, x29, fail;;
  test_4: li x10, 4; ebreak; la x1, tdat3; lb x3, 0(x1);; li x29, 0xfffffff0; li x28, 4; bne x3, x29, fail;;
  test_5: li x10, 5; ebreak; la x1, tdat4; lb x3, 0(x1);; li x29, 0x0000000f; li x28, 5; bne x3, x29, fail;;

  # Test with negative offset

  test_6: li x10, 6; ebreak; la x1, tdat1; lb x3, 0(x1);; li x29, 0xffffffff; li x28, 6; bne x3, x29, fail;;
  test_7: li x10, 7; ebreak; la x1, tdat2; lb x3, 0(x1);; li x29, 0x00000000; li x28, 7; bne x3, x29, fail;;
  test_8: li x10, 8; ebreak; la x1, tdat3; lb x3, 0(x1);; li x29, 0xfffffff0; li x28, 8; bne x3, x29, fail;;
  test_9: li x10, 9; ebreak; la x1, tdat4; lb x3, 0(x1);; li x29, 0x0000000f; li x28, 9; bne x3, x29, fail;;

  # Test with a negative base

  test_10: li x10, 10; ebreak; la x1, tdat1; addi x1, x1, -32; lb x3, 32(x1);; li x29, 0xffffffff; li x28, 10; bne x3, x29, fail;





  # Test with unaligned base

  test_11: li x10, 11; ebreak; la x1, tdat; addi x1, x1, -6; lb x3, 7(x1);; li x29, 0x00000000; li x28, 11; bne x3, x29, fail;





  #-------------------------------------------------------------
  # Bypassing tests
  #-------------------------------------------------------------

  test_12: li x28, 12; li x4, 0; test_12_l1: la x1, tdat3; lb x3, 0(x1); addi x6, x3, 0; li x29, 0xfffffff0; bne x6, x29, fail; addi x4, x4, 1; li x5, 2; bne x4, x5, test_12_l1;;
  test_13: li x28, 13; li x4, 0; test_13_l1: la x1, tdat4; lb x3, 0(x1); nop; addi x6, x3, 0; li x29, 0x0000000f; bne x6, x29, fail; addi x4, x4, 1; li x5, 2; bne x4, x5, test_13_l1;;
  test_14: li x28, 14; li x4, 0; test_14_l1: la x1, tdat2; lb x3, 0(x1); nop; nop; addi x6, x3, 0; li x29, 0x00000000; bne x6, x29, fail; addi x4, x4, 1; li x5, 2; bne x4, x5, test_14_l1;;

  test_15: li x28, 15; li x4, 0; test_15_l1: la x1, tdat3; lb x3, 0(x1); li x29, 0xfffffff0; bne x3, x29, fail; addi x4, x4, 1; li x5, 2; bne x4, x5, test_15_l1;
  test_16: li x28, 16; li x4, 0; test_16_l1: la x1, tdat4; nop; lb x3, 0(x1); li x29, 0x0000000f; bne x3, x29, fail; addi x4, x4, 1; li x5, 2; bne x4, x5, test_16_l1;
  test_17: li x28, 17; li x4, 0; test_17_l1: la x1, tdat2; nop; nop; lb x3, 0(x1); li x29, 0x00000000; bne x3, x29, fail; addi x4, x4, 1; li x5, 2; bne x4, x5, test_17_l1;

  #-------------------------------------------------------------
  # Test write-after-write hazard
  #-------------------------------------------------------------

  test_18: li x10, 18; ebreak; la x3, tdat1; lb x2, 0(x3); li x2, 2;; li x29, 2; li x28, 18; bne x2, x29, fail;





  test_19: li x10, 19; ebreak; la x3, tdat1; lb x2, 0(x3); nop; li x2, 2;; li x29, 2; li x28, 19; bne x2, x29, fail;






  bne x0, x28, pass; fail: unimp;; pass: la x10,__return_pointer; lw x1,0(x10); ret;



  .data
.balign 4; __return_pointer: .word 0;

 

.type tdat,@object
tdat:
.type tdat1,@object
tdat1: .byte 0xff
.type tdat2,@object
tdat2: .byte 0x00
.type tdat3,@object
tdat3: .byte 0xf0
.type tdat4,@object
tdat4: .byte 0x0f


