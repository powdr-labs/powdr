# 0 "sources/lrsc.S"
# 0 "<built-in>"
# 0 "<command-line>"
# 1 "/usr/include/stdc-predef.h" 1 3 4
# 0 "<command-line>" 2
# 1 "sources/lrsc.S"
# See LICENSE for license details.

#*****************************************************************************
# lrsr.S
#-----------------------------------------------------------------------------

# Test LR/SC instructions.


# 1 "sources/riscv_test.h" 1
# 11 "sources/lrsc.S" 2
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
# 12 "sources/lrsc.S" 2


.globl __runtime_start; __runtime_start: la x10,__return_pointer; sw x1,0(x10); li x10,0

 # get a unique core id
la a0, coreid
li a1, 1
amoadd.w a2, a1, (a0)

# for now, only run this on core 0
aaa:li a3, 1
bgeu a2, a3, aaa

aab: lw a1, (a0)
bltu a1, a3, aab

# make sure that sc without a reservation fails.
test_2: li x10, 2; ebreak; la a0, foo; li a5, 0xdeadbeef; sc.w a4, a5, (a0);; li x29, 1; li x28, 2; bne a4, x29, fail;





 # make sure the failing sc did not commit into memory
test_3: li x10, 3; ebreak; lw a4, foo(x0);; li x29, 0; li x28, 3; bne a4, x29, fail;




 # Disable test case 4 for now. It assumes a <1K reservation granule, when
# in reality any size granule is valid. After discussion in issue #315,
# decided to simply disable the test for now.
# (See https:

## make sure that sc with the wrong reservation fails.
## TODO is this actually mandatory behavior?
#test_4: li x10, 4; ebreak; # la a0, foo; # la a1, fooTest3; # lr.w a1, (a1); # sc.w a4, a1, (a0); #; li x29, 1; li x28, 4; bne a4, x29, fail;
# 57 "sources/lrsc.S"
 # have each core add its coreid+1 to foo 1024 times
la a0, foo
li a1, 1<<10
addi a2, a2, 1
aac: lr.w a4, (a0)
add a4, a4, a2
sc.w a4, a4, (a0)
bnez a4, aac
addi a1, a1, -1
bnez a1, aac

# wait for all cores to finish
la a0, barrier
li a1, 1
amoadd.w x0, a1, (a0)
aad: lw a1, (a0)
blt a1, a3, aad
fence

# expected result is 512*ncores*(ncores+1)
test_5: li x10, 5; ebreak; lw a0, foo(x0); slli a1, a3, 10 - 1; aae:sub a0, a0, a1; addi a3, a3, -1; bgez a3, aae; li x29, 0; li x28, 5; bne a0, x29, fail;







 # make sure that sc-after-successful-sc fails.
test_6: li x10, 6; ebreak; la a0, foo; aaf:lr.w a1, (a0); sc.w a1, x0, (a0); bnez a1, aaf; sc.w a1, x0, (a0); sc.w a2, x0, (a0); add a1, a1, a2; li x29, 2; li x28, 6; bne a1, x29, fail;
# 97 "sources/lrsc.S"
bne x0, x28, pass; fail: unimp;; pass: la x10,__return_pointer; lw x1,0(x10); ret;



  .data
.balign 4; __return_pointer: .word 0;

 

coreid: .word 0
barrier: .word 0
foo: .word 0
.skip 1024
fooTest3: .word 0

