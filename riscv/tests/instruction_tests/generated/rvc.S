# 0 "sources/rvc.S"
# 0 "<built-in>"
# 0 "<command-line>"
# 1 "/usr/include/stdc-predef.h" 1 3 4
# 0 "<command-line>" 2
# 1 "sources/rvc.S"
# See LICENSE for license details.

#*****************************************************************************
# rvc.S
#-----------------------------------------------------------------------------

# Test RVC corner cases.


# 1 "sources/riscv_test.h" 1
# 11 "sources/rvc.S" 2
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
# 12 "sources/rvc.S" 2


.globl _start; .globl __runtime_start; _start: __runtime_start: la x10,__return_pointer; sw x1,0(x10); li x10,0

  .align 2
  .option push
  .option norvc





# Powdr note: we don't support the following test because it inserts data in
# the middle of the code, which breaks our expected separation of code and
# data.

# li x28, 2
# li a1, 666
# test_2: li x10, 2; ebreak; # j 1f; # .align 3; # data: # .dword 0xfedcba9876543210; # .dword 0xfedcba9876543210; # .align 12; # .skip 4094; # 1: addi a1, a1, 1; li x29, 667; li x28, 2; bne a1, x29, fail;
# 40 "sources/rvc.S"
  li sp, 0x1234
  test_3: li x10, 3; ebreak; .option push; .option rvc; c.addi4spn a0, sp, 1020; .align 2; .option pop; li x29, 0x1234 + 1020; li x28, 3; bne a0, x29, fail;
  test_4: li x10, 4; ebreak; .option push; .option rvc; c.addi16sp sp, 496; .align 2; .option pop; li x29, 0x1234 + 496; li x28, 4; bne sp, x29, fail;
  test_5: li x10, 5; ebreak; .option push; .option rvc; c.addi16sp sp, -512; .align 2; .option pop; li x29, 0x1234 + 496 - 512; li x28, 5; bne sp, x29, fail;

  la a1, data
  test_6: li x10, 6; ebreak; .option push; .option rvc; c.lw a0, 4(a1); addi a0, a0, 1; c.sw a0, 4(a1); c.lw a2, 4(a1); .align 2; .option pop; li x29, 0xfffffffffedcba99; li x28, 6; bne a2, x29, fail;




  test_8: li x10, 8; ebreak; .option push; .option rvc; ori a0, x0, 1; c.addi a0, -16; .align 2; .option pop; li x29, -15; li x28, 8; bne a0, x29, fail;
  test_9: li x10, 9; ebreak; .option push; .option rvc; ori a5, x0, 1; c.li a5, -16; .align 2; .option pop; li x29, -16; li x28, 9; bne a5, x29, fail;




  test_11: li x10, 11; ebreak; .option push; .option rvc; c.lui s0, 0xfffe1; c.srai s0, 12; .align 2; .option pop; li x29, 0xffffffffffffffe1; li x28, 11; bne s0, x29, fail;



  test_12: li x10, 12; ebreak; .option push; .option rvc; c.lui s0, 0xfffe1; c.srli s0, 12; .align 2; .option pop; li x29, 0x000fffe1; li x28, 12; bne s0, x29, fail;

  test_14: li x10, 14; ebreak; .option push; .option rvc; c.li s0, -2; c.andi s0, ~0x10; .align 2; .option pop; li x29, ~0x11; li x28, 14; bne s0, x29, fail;
  test_15: li x10, 15; ebreak; .option push; .option rvc; li s1, 20; li a0, 6; c.sub s1, a0; .align 2; .option pop; li x29, 14; li x28, 15; bne s1, x29, fail;
  test_16: li x10, 16; ebreak; .option push; .option rvc; li s1, 20; li a0, 6; c.xor s1, a0; .align 2; .option pop; li x29, 18; li x28, 16; bne s1, x29, fail;
  test_17: li x10, 17; ebreak; .option push; .option rvc; li s1, 20; li a0, 6; c.or s1, a0; .align 2; .option pop; li x29, 22; li x28, 17; bne s1, x29, fail;
  test_18: li x10, 18; ebreak; .option push; .option rvc; li s1, 20; li a0, 6; c.and s1, a0; .align 2; .option pop; li x29, 4; li x28, 18; bne s1, x29, fail;




  test_21: li x10, 21; ebreak; .option push; .option rvc; li s0, 0x1234; c.slli s0, 4; .align 2; .option pop; li x29, 0x12340; li x28, 21; bne s0, x29, fail;

  test_30: li x10, 30; ebreak; .option push; .option rvc; li ra, 0; c.j 1f; c.j 2f; 1:c.j 1f; 2:j fail; 1:; .align 2; .option pop; li x29, 0; li x28, 30; bne ra, x29, fail;







  test_31: li x10, 31; ebreak; .option push; .option rvc; li a0, 0; c.beqz a0, 1f; c.j 2f; 1:c.j 1f; 2:j fail; 1:; .align 2; .option pop; li x29, 0; li x28, 31; bne x0, x29, fail;







  test_32: li x10, 32; ebreak; .option push; .option rvc; li a0, 1; c.bnez a0, 1f; c.j 2f; 1:c.j 1f; 2:j fail; 1:; .align 2; .option pop; li x29, 0; li x28, 32; bne x0, x29, fail;







  test_33: li x10, 33; ebreak; .option push; .option rvc; li a0, 1; c.beqz a0, 1f; c.j 2f; 1:c.j fail; 2:; .align 2; .option pop; li x29, 0; li x28, 33; bne x0, x29, fail;






  test_34: li x10, 34; ebreak; .option push; .option rvc; li a0, 0; c.bnez a0, 1f; c.j 2f; 1:c.j fail; 2:; .align 2; .option pop; li x29, 0; li x28, 34; bne x0, x29, fail;






  test_35: li x10, 35; ebreak; .option push; .option rvc; la t0, 1f; li ra, 0; c.jr t0; c.j 2f; 1:c.j 1f; 2:j fail; 1:; .align 2; .option pop; li x29, 0; li x28, 35; bne ra, x29, fail;
# 121 "sources/rvc.S"
## Powdr note: we don't support tests 36 and 37 because they do arithmetic on text addresses, which we don't support.
# test_36: li x10, 36; ebreak; .option push; .option rvc; # la t0, 1f; # li ra, 0; # c.jalr t0; # c.j 2f; # 1:c.j 1f; # 2:j fail; # 1:sub ra, ra, t0; .align 2; .option pop; li x29, -2; li x28, 36; bne ra, x29, fail;
# 131 "sources/rvc.S"
##if 32 == 32
# test_37: li x10, 37; ebreak; .option push; .option rvc; # la t0, 1f; # li ra, 0; # c.jal 1f; # c.j 2f; # 1:c.j 1f; # 2:j fail; # 1:sub ra, ra, t0; .align 2; .option pop; li x29, -2; li x28, 37; bne ra, x29, fail;







##endif

  la sp, data
  test_40: li x10, 40; ebreak; .option push; .option rvc; c.lwsp a0, 12(sp); addi a0, a0, 1; c.swsp a0, 12(sp); c.lwsp a2, 12(sp); .align 2; .option pop; li x29, 0xfffffffffedcba99; li x28, 40; bne a2, x29, fail;




  test_42: li x10, 42; ebreak; .option push; .option rvc; li a0, 0x123; c.mv t0, a0; c.add t0, a0; .align 2; .option pop; li x29, 0x246; li x28, 42; bne t0, x29, fail;

  .option pop

  bne x0, x28, pass; fail: unimp;; pass: la x10,__return_pointer; lw x1,0(x10); ret;



  .data
.balign 4; __return_pointer: .word 0;

 

data:
  .dword 0xfedcba9876543210;
  .dword 0xfedcba9876543210;


