#ifndef _ENV_PICORV32_TEST_H
#define _ENV_PICORV32_TEST_H

#ifndef TEST_FUNC_NAME
#  define TEST_FUNC_NAME mytest
#  define TEST_FUNC_TXT "mytest"
#  define TEST_FUNC_RET mytest_ret
#endif

#define RVTEST_RV32U
#define TESTNUM x28

#define RVTEST_CODE_BEGIN		\
	.globl __runtime_start;		\
__runtime_start:				\

// 	lui	a0,%hi(.test_name);	\
// 	addi	a0,a0,%lo(.test_name);	\
// 	lui	a2,0x10000;	\
// .prname_next:				\
// 	lb	a1,0(a0);		\
// 	beq	a1,zero,.prname_done;	\
// 	sw	a1,0(a2);		\
// 	addi	a0,a0,1;		\
// 	jal	zero,.prname_next;	\
// .test_name:				\
// 	.ascii TEST_FUNC_TXT;		\
// 	.byte 0x00;			\
// 	.balign 4, 0;			\
// .prname_done:				\
// 	addi	a1,zero,46;		\
// 	sw	a1,0(a2);		\
// 	sw	a1,0(a2);

// TODO we could (and should?) also output something
#define RVTEST_PASS			\
    ___pass: \
	j ___pass;

// TODO we could (and should?) also output something
#define RVTEST_FAIL			\
	unimp;

#define RVTEST_CODE_END
#define RVTEST_DATA_BEGIN .balign 4;
#define RVTEST_DATA_END

#endif
