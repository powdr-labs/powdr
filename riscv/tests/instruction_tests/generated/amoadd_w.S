# 0 "sources/amoadd_w.S"
# 0 "<built-in>"
# 0 "<command-line>"
# 1 "/usr/include/stdc-predef.h" 1 3 4
# 0 "<command-line>" 2
# 1 "sources/amoadd_w.S"
# See LICENSE for license details.

#*****************************************************************************
# amoadd_w.S
#-----------------------------------------------------------------------------

# Test amoadd.w instruction.


# 1 "sources/riscv_test.h" 1
# 11 "sources/amoadd_w.S" 2
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
# 12 "sources/amoadd_w.S" 2


.globl __runtime_start; __runtime_start: la x10,__return_pointer; sw x1,0(x10); li x10,0

  test_2: li x10, 2; ebreak; li a0, 0xffffffff80000000; li a1, 0xfffffffffffff800; la a3, amo_operand; sw a0, 0(a3); amoadd.w a4, a1, 0(a3);; li x29, 0xffffffff80000000; li x28, 2; bne a4, x29, fail;







  test_3: li x10, 3; ebreak; lw a5, 0(a3); li x29, 0x000000007ffff800; li x28, 3; bne a5, x29, fail;

  # try again after a cache miss
  test_4: li x10, 4; ebreak; li a1, 0xffffffff80000000; amoadd.w a4, a1, 0(a3);; li x29, 0x000000007ffff800; li x28, 4; bne a4, x29, fail;




  test_5: li x10, 5; ebreak; lw a5, 0(a3); li x29, 0xfffffffffffff800; li x28, 5; bne a5, x29, fail;

  bne x0, x28, pass; fail: unimp;; pass: la x10,__return_pointer; lw x1,0(x10); ret;



  .data
.balign 4; __return_pointer: .word 0;

 



    .bss
    .align 3
amo_operand:
    .word 0
    .word 0
