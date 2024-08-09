# 0 "sources/sw.S"
# 0 "<built-in>"
# 0 "<command-line>"
# 1 "/usr/include/stdc-predef.h" 1 3 4
# 0 "<command-line>" 2
# 1 "sources/sw.S"
# See LICENSE for license details.

#*****************************************************************************
# sw.S
#-----------------------------------------------------------------------------

# Test sw instruction.


# 1 "sources/riscv_test.h" 1
# 11 "sources/sw.S" 2
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
# 12 "sources/sw.S" 2


.globl _start; .globl __runtime_start; _start: __runtime_start: la x10,__return_pointer; sw x1,0(x10); li x10,0

  #-------------------------------------------------------------
  # Basic tests
  #-------------------------------------------------------------

  test_2: li x10, 2; ebreak; la x1, tdat; li x2, 0x0000000000aa00aa; sw x2, 0(x1); lw x3, 0(x1);; li x29, 0x0000000000aa00aa; li x28, 2; bne x3, x29, fail;;
  test_3: li x10, 3; ebreak; la x1, tdat; li x2, 0xffffffffaa00aa00; sw x2, 4(x1); lw x3, 4(x1);; li x29, 0xffffffffaa00aa00; li x28, 3; bne x3, x29, fail;;
  test_4: li x10, 4; ebreak; la x1, tdat; li x2, 0x000000000aa00aa0; sw x2, 8(x1); lw x3, 8(x1);; li x29, 0x000000000aa00aa0; li x28, 4; bne x3, x29, fail;;
  test_5: li x10, 5; ebreak; la x1, tdat; li x2, 0xffffffffa00aa00a; sw x2, 12(x1); lw x3, 12(x1);; li x29, 0xffffffffa00aa00a; li x28, 5; bne x3, x29, fail;;

  # Test with negative offset

  test_6: li x10, 6; ebreak; la x1, tdat8; li x2, 0x0000000000aa00aa; sw x2, -12(x1); lw x3, -12(x1);; li x29, 0x0000000000aa00aa; li x28, 6; bne x3, x29, fail;;
  test_7: li x10, 7; ebreak; la x1, tdat8; li x2, 0xffffffffaa00aa00; sw x2, -8(x1); lw x3, -8(x1);; li x29, 0xffffffffaa00aa00; li x28, 7; bne x3, x29, fail;;
  test_8: li x10, 8; ebreak; la x1, tdat8; li x2, 0x000000000aa00aa0; sw x2, -4(x1); lw x3, -4(x1);; li x29, 0x000000000aa00aa0; li x28, 8; bne x3, x29, fail;;
  test_9: li x10, 9; ebreak; la x1, tdat8; li x2, 0xffffffffa00aa00a; sw x2, 0(x1); lw x3, 0(x1);; li x29, 0xffffffffa00aa00a; li x28, 9; bne x3, x29, fail;;

  # Test with a negative base

  test_10: li x10, 10; ebreak; la x1, tdat9; li x2, 0x12345678; addi x4, x1, -32; sw x2, 32(x4); lw x5, 0(x1);; li x29, 0x12345678; li x28, 10; bne x5, x29, fail;







  # Test with unaligned base

  test_11: li x10, 11; ebreak; la x1, tdat9; li x2, 0x58213098; addi x1, x1, -3; sw x2, 7(x1); la x4, tdat10; lw x5, 0(x4);; li x29, 0x58213098; li x28, 11; bne x5, x29, fail;
# 53 "sources/sw.S"
  #-------------------------------------------------------------
  # Bypassing tests
  #-------------------------------------------------------------

  test_12: li x28, 12; li x4, 0; test_12_l1: li x1, 0xffffffffaabbccdd; la x2, tdat; sw x1, 0(x2); lw x3, 0(x2); li x29, 0xffffffffaabbccdd; bne x3, x29, fail; addi x4, x4, 1; li x5, 2; bne x4, x5, test_12_l1;
  test_13: li x28, 13; li x4, 0; test_13_l1: li x1, 0xffffffffdaabbccd; la x2, tdat; nop; sw x1, 4(x2); lw x3, 4(x2); li x29, 0xffffffffdaabbccd; bne x3, x29, fail; addi x4, x4, 1; li x5, 2; bne x4, x5, test_13_l1;
  test_14: li x28, 14; li x4, 0; test_14_l1: li x1, 0xffffffffddaabbcc; la x2, tdat; nop; nop; sw x1, 8(x2); lw x3, 8(x2); li x29, 0xffffffffddaabbcc; bne x3, x29, fail; addi x4, x4, 1; li x5, 2; bne x4, x5, test_14_l1;
  test_15: li x28, 15; li x4, 0; test_15_l1: li x1, 0xffffffffcddaabbc; nop; la x2, tdat; sw x1, 12(x2); lw x3, 12(x2); li x29, 0xffffffffcddaabbc; bne x3, x29, fail; addi x4, x4, 1; li x5, 2; bne x4, x5, test_15_l1;
  test_16: li x28, 16; li x4, 0; test_16_l1: li x1, 0xffffffffccddaabb; nop; la x2, tdat; nop; sw x1, 16(x2); lw x3, 16(x2); li x29, 0xffffffffccddaabb; bne x3, x29, fail; addi x4, x4, 1; li x5, 2; bne x4, x5, test_16_l1;
  test_17: li x28, 17; li x4, 0; test_17_l1: li x1, 0xffffffffbccddaab; nop; nop; la x2, tdat; sw x1, 20(x2); lw x3, 20(x2); li x29, 0xffffffffbccddaab; bne x3, x29, fail; addi x4, x4, 1; li x5, 2; bne x4, x5, test_17_l1;

  test_18: li x28, 18; li x4, 0; test_18_l1: la x2, tdat; li x1, 0x00112233; sw x1, 0(x2); lw x3, 0(x2); li x29, 0x00112233; bne x3, x29, fail; addi x4, x4, 1; li x5, 2; bne x4, x5, test_18_l1;
  test_19: li x28, 19; li x4, 0; test_19_l1: la x2, tdat; li x1, 0x30011223; nop; sw x1, 4(x2); lw x3, 4(x2); li x29, 0x30011223; bne x3, x29, fail; addi x4, x4, 1; li x5, 2; bne x4, x5, test_19_l1;
  test_20: li x28, 20; li x4, 0; test_20_l1: la x2, tdat; li x1, 0x33001122; nop; nop; sw x1, 8(x2); lw x3, 8(x2); li x29, 0x33001122; bne x3, x29, fail; addi x4, x4, 1; li x5, 2; bne x4, x5, test_20_l1;
  test_21: li x28, 21; li x4, 0; test_21_l1: la x2, tdat; nop; li x1, 0x23300112; sw x1, 12(x2); lw x3, 12(x2); li x29, 0x23300112; bne x3, x29, fail; addi x4, x4, 1; li x5, 2; bne x4, x5, test_21_l1;
  test_22: li x28, 22; li x4, 0; test_22_l1: la x2, tdat; nop; li x1, 0x22330011; nop; sw x1, 16(x2); lw x3, 16(x2); li x29, 0x22330011; bne x3, x29, fail; addi x4, x4, 1; li x5, 2; bne x4, x5, test_22_l1;
  test_23: li x28, 23; li x4, 0; test_23_l1: la x2, tdat; nop; nop; li x1, 0x12233001; sw x1, 20(x2); lw x3, 20(x2); li x29, 0x12233001; bne x3, x29, fail; addi x4, x4, 1; li x5, 2; bne x4, x5, test_23_l1;

  bne x0, x28, pass; fail: unimp;; pass: la x10,__return_pointer; lw x1,0(x10); ret;



  .data
.balign 4; __return_pointer: .word 0;

 

tdat:
tdat1: .word 0xdeadbeef
tdat2: .word 0xdeadbeef
tdat3: .word 0xdeadbeef
tdat4: .word 0xdeadbeef
tdat5: .word 0xdeadbeef
tdat6: .word 0xdeadbeef
tdat7: .word 0xdeadbeef
tdat8: .word 0xdeadbeef
tdat9: .word 0xdeadbeef
tdat10: .word 0xdeadbeef


