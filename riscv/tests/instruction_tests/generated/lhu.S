# 0 "sources/lhu.S"
# 0 "<built-in>"
# 0 "<command-line>"
# 1 "/usr/include/stdc-predef.h" 1 3 4
# 0 "<command-line>" 2
# 1 "sources/lhu.S"
# See LICENSE for license details.

#*****************************************************************************
# lhu.S
#-----------------------------------------------------------------------------

# Test lhu instruction.


# 1 "sources/riscv_test.h" 1
# 11 "sources/lhu.S" 2
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
# 12 "sources/lhu.S" 2


.globl __runtime_start; __runtime_start:

  #-------------------------------------------------------------
  # Basic tests
  #-------------------------------------------------------------

  test_2: li x10, 2; ebreak; la x1, tdat1; lhu x3, 0(x1);; li x29, 0x000000ff; li x28, 2; bne x3, x29, fail;;
  test_3: li x10, 3; ebreak; la x1, tdat2; lhu x3, 0(x1);; li x29, 0x0000ff00; li x28, 3; bne x3, x29, fail;;
  test_4: li x10, 4; ebreak; la x1, tdat3; lhu x3, 0(x1);; li x29, 0x00000ff0; li x28, 4; bne x3, x29, fail;;
  test_5: li x10, 5; ebreak; la x1, tdat4; lhu x3, 0(x1);; li x29, 0x0000f00f; li x28, 5; bne x3, x29, fail;;

  # Test with negative offset

  test_6: li x10, 6; ebreak; la x1, tdat1; lhu x3, 0(x1);; li x29, 0x000000ff; li x28, 6; bne x3, x29, fail;;
  test_7: li x10, 7; ebreak; la x1, tdat2; lhu x3, 0(x1);; li x29, 0x0000ff00; li x28, 7; bne x3, x29, fail;;
  test_8: li x10, 8; ebreak; la x1, tdat3; lhu x3, 0(x1);; li x29, 0x00000ff0; li x28, 8; bne x3, x29, fail;;
  test_9: li x10, 9; ebreak; la x1, tdat4; lhu x3, 0(x1);; li x29, 0x0000f00f; li x28, 9; bne x3, x29, fail;;

  # Test with a negative base

  test_10: li x10, 10; ebreak; la x1, tdat1; addi x1, x1, -32; lhu x3, 32(x1);; li x29, 0x000000ff; li x28, 10; bne x3, x29, fail;





  # Test with unaligned base

  # TODO once we support unalignmed memory access
  #test_11: li x10, 11; ebreak; la x1, tdat; addi x1, x1, -5; lhu x3, 7(x1);; li x29, 0x0000ff00; li x28, 11; bne x3, x29, fail;





  #-------------------------------------------------------------
  # Bypassing tests
  #-------------------------------------------------------------

  test_12: li x28, 12; li x4, 0; test_12_l1: la x1, tdat3; lhu x3, 0(x1); addi x6, x3, 0; li x29, 0x00000ff0; bne x6, x29, fail; addi x4, x4, 1; li x5, 2; bne x4, x5, test_12_l1;;
  test_13: li x28, 13; li x4, 0; test_13_l1: la x1, tdat4; lhu x3, 0(x1); nop; addi x6, x3, 0; li x29, 0x0000f00f; bne x6, x29, fail; addi x4, x4, 1; li x5, 2; bne x4, x5, test_13_l1;;
  test_14: li x28, 14; li x4, 0; test_14_l1: la x1, tdat2; lhu x3, 0(x1); nop; nop; addi x6, x3, 0; li x29, 0x0000ff00; bne x6, x29, fail; addi x4, x4, 1; li x5, 2; bne x4, x5, test_14_l1;;

  test_15: li x28, 15; li x4, 0; test_15_l1: la x1, tdat3; lhu x3, 0(x1); li x29, 0x00000ff0; bne x3, x29, fail; addi x4, x4, 1; li x5, 2; bne x4, x5, test_15_l1;
  test_16: li x28, 16; li x4, 0; test_16_l1: la x1, tdat4; nop; lhu x3, 0(x1); li x29, 0x0000f00f; bne x3, x29, fail; addi x4, x4, 1; li x5, 2; bne x4, x5, test_16_l1;
  test_17: li x28, 17; li x4, 0; test_17_l1: la x1, tdat2; nop; nop; lhu x3, 0(x1); li x29, 0x0000ff00; bne x3, x29, fail; addi x4, x4, 1; li x5, 2; bne x4, x5, test_17_l1;

  #-------------------------------------------------------------
  # Test write-after-write hazard
  #-------------------------------------------------------------

  test_18: li x10, 18; ebreak; la x3, tdat1; lhu x2, 0(x3); li x2, 2;; li x29, 2; li x28, 18; bne x2, x29, fail;





  test_19: li x10, 19; ebreak; la x3, tdat1; lhu x2, 0(x3); nop; li x2, 2;; li x29, 2; li x28, 19; bne x2, x29, fail;






  bne x0, x28, pass; fail: unimp;; pass: ___pass: j ___pass;



  .data
.balign 4;

 

.type tdat,@object
tdat:
.type tdat1,@object
# TODO
#tdat1: .half 0x00ff
tdat1: .word 0x00ff
.type tdat2,@object
# TODO
#tdat2: .half 0xff00
tdat2: .word 0xff00
.type tdat3,@object
# TODO
#tdat3: .half 0x0ff0
tdat3: .word 0x0ff0
.type tdat4,@object
# TODO
#tdat4: .half 0xf00f
tdat4: .word 0xf00f
