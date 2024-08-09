	.text
	.attribute	4, 16
	.attribute	5, "rv32i2p1_m2p0_a2p1_c2p0"
	.file	"powdr_riscv_syscalls.b0de478eb457f70-cgu.0"
	.file	1 "/Users/steve/Documents/repo/powdr-5_6_24/powdr/riscv-syscalls" "src/lib.rs"
	.section	".text._ZN44_$LT$$RF$T$u20$as$u20$core..fmt..Display$GT$3fmt17h7370650b9d1da708E","ax",@progbits
	.p2align	1
	.type	_ZN44_$LT$$RF$T$u20$as$u20$core..fmt..Display$GT$3fmt17h7370650b9d1da708E,@function
_ZN44_$LT$$RF$T$u20$as$u20$core..fmt..Display$GT$3fmt17h7370650b9d1da708E:
.Lfunc_begin0:
	.cfi_sections .debug_frame
	.cfi_startproc
	.file	2 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/fmt" "mod.rs"
	.loc	2 2294 71 prologue_end
	lw	a2, 0(a0)
	lw	a3, 4(a0)
	mv	a4, a1
.Ltmp0:
	.loc	2 2294 62 is_stmt 0
	mv	a0, a2
.Ltmp1:
	mv	a1, a3
	mv	a2, a4
.Ltmp2:
	tail	_ZN42_$LT$str$u20$as$u20$core..fmt..Display$GT$3fmt17h45c916f6fbc68766E
.Ltmp3:
.Lfunc_end0:
	.size	_ZN44_$LT$$RF$T$u20$as$u20$core..fmt..Display$GT$3fmt17h7370650b9d1da708E, .Lfunc_end0-_ZN44_$LT$$RF$T$u20$as$u20$core..fmt..Display$GT$3fmt17h7370650b9d1da708E
	.cfi_endproc

	.section	".text._ZN68_$LT$powdr_riscv_syscalls..Syscall$u20$as$u20$core..fmt..Display$GT$3fmt17hdf8850d239dac7f5E","ax",@progbits
	.globl	_ZN68_$LT$powdr_riscv_syscalls..Syscall$u20$as$u20$core..fmt..Display$GT$3fmt17hdf8850d239dac7f5E
	.p2align	1
	.type	_ZN68_$LT$powdr_riscv_syscalls..Syscall$u20$as$u20$core..fmt..Display$GT$3fmt17hdf8850d239dac7f5E,@function
_ZN68_$LT$powdr_riscv_syscalls..Syscall$u20$as$u20$core..fmt..Display$GT$3fmt17hdf8850d239dac7f5E:
.Lfunc_begin1:
	.loc	1 24 0 is_stmt 1
	.cfi_startproc
	addi	sp, sp, -48
	.cfi_def_cfa_offset 48
.Ltmp4:
	.loc	1 25 39 prologue_end
	sw	ra, 44(sp)
	.cfi_offset ra, -4
	lw	a0, 0(a0)
.Ltmp5:
	.loc	1 0 39 is_stmt 0
	slli	a0, a0, 2
	lui	a2, %hi(.LJTI1_0)
	addi	a2, a2, %lo(.LJTI1_0)
	add	a0, a0, a2
	lw	a2, 0(a0)
	mv	a0, a1
.Ltmp6:
	jr	a2
.Ltmp7:
.LBB1_1:
	.loc	1 61 16 is_stmt 1
	lui	a1, %hi(.L__unnamed_1)
	addi	a1, a1, %lo(.L__unnamed_1)
	sw	a1, 36(sp)
	li	a1, 5
	j	.LBB1_12
.Ltmp8:
.LBB1_2:
	.loc	1 62 25
	lui	a1, %hi(.L__unnamed_2)
	addi	a1, a1, %lo(.L__unnamed_2)
	sw	a1, 36(sp)
	li	a1, 15
	j	.LBB1_12
.Ltmp9:
.LBB1_3:
	.loc	1 63 17
	lui	a1, %hi(.L__unnamed_3)
	addi	a1, a1, %lo(.L__unnamed_3)
	j	.LBB1_7
.Ltmp10:
.LBB1_4:
	.loc	1 64 21
	lui	a1, %hi(.L__unnamed_4)
	addi	a1, a1, %lo(.L__unnamed_4)
	sw	a1, 36(sp)
	li	a1, 11
	j	.LBB1_12
.Ltmp11:
.LBB1_5:
	.loc	1 65 20
	lui	a1, %hi(.L__unnamed_5)
	addi	a1, a1, %lo(.L__unnamed_5)
	sw	a1, 36(sp)
	li	a1, 10
	j	.LBB1_12
.Ltmp12:
.LBB1_6:
	.loc	1 66 16
	lui	a1, %hi(.L__unnamed_6)
	addi	a1, a1, %lo(.L__unnamed_6)
.Ltmp13:
.LBB1_7:
	.loc	1 0 0 is_stmt 0
	sw	a1, 36(sp)
	li	a1, 6
	j	.LBB1_12
.Ltmp14:
.LBB1_8:
	.loc	1 67 19 is_stmt 1
	lui	a1, %hi(.L__unnamed_7)
	addi	a1, a1, %lo(.L__unnamed_7)
	sw	a1, 36(sp)
	li	a1, 9
	j	.LBB1_12
.Ltmp15:
.LBB1_9:
	.loc	1 69 18
	lui	a1, %hi(.L__unnamed_8)
	addi	a1, a1, %lo(.L__unnamed_8)
	j	.LBB1_11
.Ltmp16:
.LBB1_10:
	.loc	1 68 17
	lui	a1, %hi(.L__unnamed_9)
	addi	a1, a1, %lo(.L__unnamed_9)
.Ltmp17:
.LBB1_11:
	.loc	1 0 0 is_stmt 0
	sw	a1, 36(sp)
	li	a1, 7
.Ltmp18:
.LBB1_12:
	sw	a1, 40(sp)
	addi	a1, sp, 36
	.loc	1 25 17 is_stmt 1
	sw	a1, 28(sp)
	lui	a1, %hi(_ZN44_$LT$$RF$T$u20$as$u20$core..fmt..Display$GT$3fmt17h7370650b9d1da708E)
	addi	a1, a1, %lo(_ZN44_$LT$$RF$T$u20$as$u20$core..fmt..Display$GT$3fmt17h7370650b9d1da708E)
	sw	a1, 32(sp)
.Ltmp19:
	.loc	2 335 9
	lui	a1, %hi(.L__unnamed_10)
	addi	a1, a1, %lo(.L__unnamed_10)
.Ltmp20:
	sw	a1, 4(sp)
	li	a1, 1
.Ltmp21:
	sw	a1, 8(sp)
	sw	zero, 20(sp)
	addi	a2, sp, 28
	sw	a2, 12(sp)
	sw	a1, 16(sp)
.Ltmp22:
	.loc	1 25 17
	addi	a1, sp, 4
	call	_ZN4core3fmt9Formatter9write_fmt17hf1a4b1e0be961690E
.Ltmp23:
	.loc	1 28 14
	lw	ra, 44(sp)
	.loc	1 28 14 epilogue_begin is_stmt 0
	addi	sp, sp, 48
	ret
.Ltmp24:
.Lfunc_end1:
	.size	_ZN68_$LT$powdr_riscv_syscalls..Syscall$u20$as$u20$core..fmt..Display$GT$3fmt17hdf8850d239dac7f5E, .Lfunc_end1-_ZN68_$LT$powdr_riscv_syscalls..Syscall$u20$as$u20$core..fmt..Display$GT$3fmt17hdf8850d239dac7f5E
	.cfi_endproc
	.section	".rodata._ZN68_$LT$powdr_riscv_syscalls..Syscall$u20$as$u20$core..fmt..Display$GT$3fmt17hdf8850d239dac7f5E","a",@progbits
	.p2align	2, 0x0
.LJTI1_0:
	.word	.LBB1_1
	.word	.LBB1_2
	.word	.LBB1_3
	.word	.LBB1_4
	.word	.LBB1_5
	.word	.LBB1_6
	.word	.LBB1_8
	.word	.LBB1_9
	.word	.LBB1_10

	.section	".text._ZN76_$LT$powdr_riscv_syscalls..Syscall$u20$as$u20$core..str..traits..FromStr$GT$8from_str17h27673a40b24fea4aE","ax",@progbits
	.globl	_ZN76_$LT$powdr_riscv_syscalls..Syscall$u20$as$u20$core..str..traits..FromStr$GT$8from_str17h27673a40b24fea4aE
	.p2align	1
	.type	_ZN76_$LT$powdr_riscv_syscalls..Syscall$u20$as$u20$core..str..traits..FromStr$GT$8from_str17h27673a40b24fea4aE,@function
_ZN76_$LT$powdr_riscv_syscalls..Syscall$u20$as$u20$core..str..traits..FromStr$GT$8from_str17h27673a40b24fea4aE:
.Lfunc_begin2:
	.loc	1 33 0 is_stmt 1
	.cfi_startproc
	addi	sp, sp, -16
	.cfi_def_cfa_offset 16
	sw	ra, 12(sp)
	sw	s0, 8(sp)
	sw	s1, 4(sp)
	.cfi_offset ra, -4
	.cfi_offset s0, -8
	.cfi_offset s1, -12
	mv	s1, a1
.Ltmp25:
	.file	3 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/slice" "cmp.rs"
	.loc	3 84 12 prologue_end
	addi	a1, a1, -5
	li	a2, 10
.Ltmp26:
	bltu	a2, a1, .LBB2_14
.Ltmp27:
	.loc	3 0 12 is_stmt 0
	mv	s0, a0
.Ltmp28:
	slli	a1, a1, 2
	lui	a0, %hi(.LJTI2_0)
	addi	a0, a0, %lo(.LJTI2_0)
	add	a0, a0, a1
	lw	a0, 0(a0)
	jr	a0
.Ltmp29:
.LBB2_2:
	.loc	3 92 13 is_stmt 1
	lui	a0, %hi(.L__unnamed_1)
	addi	a1, a0, %lo(.L__unnamed_1)
	li	a2, 5
	mv	a0, s0
	call	memcmp@plt
.Ltmp30:
	.loc	1 35 23
	bnez	a0, .LBB2_14
	j	.LBB2_15
.Ltmp31:
.LBB2_3:
	.loc	3 92 13
	lui	a0, %hi(.L__unnamed_3)
	addi	a1, a0, %lo(.L__unnamed_3)
	li	a2, 6
	mv	a0, s0
	call	memcmp@plt
.Ltmp32:
	.loc	1 35 23
	beqz	a0, .LBB2_16
.Ltmp33:
	.loc	3 92 13
	lui	a0, %hi(.L__unnamed_6)
	addi	a1, a0, %lo(.L__unnamed_6)
.Ltmp34:
	li	a2, 6
	mv	a0, s0
	call	memcmp@plt
.Ltmp35:
	.loc	1 35 23
	bnez	a0, .LBB2_14
.Ltmp36:
	.loc	1 0 23 is_stmt 0
	li	a0, 5
	j	.LBB2_15
.Ltmp37:
.LBB2_6:
	.loc	3 92 13 is_stmt 1
	lui	a0, %hi(.L__unnamed_9)
	addi	a1, a0, %lo(.L__unnamed_9)
	mv	a0, s0
	mv	a2, s1
	call	memcmp@plt
.Ltmp38:
	.loc	1 35 23
	beqz	a0, .LBB2_17
.Ltmp39:
	.loc	3 92 13
	lui	a0, %hi(.L__unnamed_8)
	addi	a1, a0, %lo(.L__unnamed_8)
.Ltmp40:
	mv	a0, s0
	mv	a2, s1
	call	memcmp@plt
.Ltmp41:
	mv	a1, a0
	li	a0, 7
.Ltmp42:
	.loc	1 0 0 is_stmt 0
	bnez	a1, .LBB2_14
	j	.LBB2_15
.Ltmp43:
.LBB2_8:
	.loc	3 92 13
	lui	a0, %hi(.L__unnamed_7)
	addi	a1, a0, %lo(.L__unnamed_7)
	mv	a0, s0
	mv	a2, s1
	call	memcmp@plt
	mv	a1, a0
	li	a0, 6
.Ltmp44:
	.loc	1 35 23 is_stmt 1
	bnez	a1, .LBB2_14
	j	.LBB2_15
.Ltmp45:
.LBB2_9:
	.loc	3 92 13
	lui	a0, %hi(.L__unnamed_5)
	addi	a1, a0, %lo(.L__unnamed_5)
	mv	a0, s0
	mv	a2, s1
	call	memcmp@plt
.Ltmp46:
	.loc	1 35 23
	bnez	a0, .LBB2_14
.Ltmp47:
	.loc	1 0 23 is_stmt 0
	li	a0, 4
	j	.LBB2_15
.Ltmp48:
.LBB2_11:
	.loc	3 92 13 is_stmt 1
	lui	a0, %hi(.L__unnamed_4)
	addi	a1, a0, %lo(.L__unnamed_4)
	li	a2, 11
	mv	a0, s0
	call	memcmp@plt
.Ltmp49:
	.loc	1 35 23
	bnez	a0, .LBB2_14
.Ltmp50:
	.loc	1 0 23 is_stmt 0
	li	a0, 3
	j	.LBB2_15
.Ltmp51:
.LBB2_13:
	.loc	3 92 13 is_stmt 1
	lui	a0, %hi(.L__unnamed_2)
	addi	a1, a0, %lo(.L__unnamed_2)
	li	a2, 15
	mv	a0, s0
	call	memcmp@plt
.Ltmp52:
	.loc	1 35 23
	beqz	a0, .LBB2_18
.Ltmp53:
.LBB2_14:
	.loc	1 0 23 is_stmt 0
	li	a0, 9
.Ltmp54:
.LBB2_15:
	.loc	1 38 14 is_stmt 1
	lw	ra, 12(sp)
	lw	s0, 8(sp)
	lw	s1, 4(sp)
.Ltmp55:
	.loc	1 38 14 epilogue_begin is_stmt 0
	addi	sp, sp, 16
	ret
.Ltmp56:
.LBB2_16:
	.loc	1 0 14
	li	a0, 2
	j	.LBB2_15
.Ltmp57:
.LBB2_17:
	li	a0, 8
	j	.LBB2_15
.Ltmp58:
.LBB2_18:
	li	a0, 1
	j	.LBB2_15
.Lfunc_end2:
	.size	_ZN76_$LT$powdr_riscv_syscalls..Syscall$u20$as$u20$core..str..traits..FromStr$GT$8from_str17h27673a40b24fea4aE, .Lfunc_end2-_ZN76_$LT$powdr_riscv_syscalls..Syscall$u20$as$u20$core..str..traits..FromStr$GT$8from_str17h27673a40b24fea4aE
	.cfi_endproc
	.section	".rodata._ZN76_$LT$powdr_riscv_syscalls..Syscall$u20$as$u20$core..str..traits..FromStr$GT$8from_str17h27673a40b24fea4aE","a",@progbits
	.p2align	2, 0x0
.LJTI2_0:
	.word	.LBB2_2
	.word	.LBB2_3
	.word	.LBB2_6
	.word	.LBB2_14
	.word	.LBB2_8
	.word	.LBB2_9
	.word	.LBB2_11
	.word	.LBB2_14
	.word	.LBB2_14
	.word	.LBB2_14
	.word	.LBB2_13
	.file	4 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src" "cmp.rs"
	.file	5 "/Users/steve/.rustup/toolchains/nightly-2024-02-01-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/str" "traits.rs"

	.type	.L__unnamed_11,@object
	.section	.rodata..L__unnamed_11,"a",@progbits
	.p2align	2, 0x0
.L__unnamed_11:
	.size	.L__unnamed_11, 0

	.type	.L__unnamed_12,@object
	.section	.rodata..L__unnamed_12,"a",@progbits
.L__unnamed_12:
	.ascii	"x10"
	.size	.L__unnamed_12, 3

	.type	.L__unnamed_13,@object
	.section	.rodata..L__unnamed_13,"a",@progbits
.L__unnamed_13:
	.ascii	"x11"
	.size	.L__unnamed_13, 3

	.type	.L__unnamed_14,@object
	.section	.rodata..L__unnamed_14,"a",@progbits
.L__unnamed_14:
	.ascii	"x12"
	.size	.L__unnamed_14, 3

	.type	.L__unnamed_15,@object
	.section	.rodata..L__unnamed_15,"a",@progbits
.L__unnamed_15:
	.ascii	"x13"
	.size	.L__unnamed_15, 3

	.type	.L__unnamed_16,@object
	.section	.rodata..L__unnamed_16,"a",@progbits
.L__unnamed_16:
	.ascii	"x14"
	.size	.L__unnamed_16, 3

	.type	.L__unnamed_17,@object
	.section	.rodata..L__unnamed_17,"a",@progbits
.L__unnamed_17:
	.ascii	"x15"
	.size	.L__unnamed_17, 3

	.type	.L__unnamed_18,@object
	.section	.rodata..L__unnamed_18,"a",@progbits
.L__unnamed_18:
	.ascii	"x16"
	.size	.L__unnamed_18, 3

	.type	.L__unnamed_19,@object
	.section	.rodata..L__unnamed_19,"a",@progbits
.L__unnamed_19:
	.ascii	"x17"
	.size	.L__unnamed_19, 3

	.type	.L__unnamed_20,@object
	.section	.rodata..L__unnamed_20,"a",@progbits
.L__unnamed_20:
	.ascii	"x6"
	.size	.L__unnamed_20, 2

	.type	.L__unnamed_21,@object
	.section	.rodata..L__unnamed_21,"a",@progbits
.L__unnamed_21:
	.ascii	"x7"
	.size	.L__unnamed_21, 2

	.type	.L__unnamed_22,@object
	.section	.rodata..L__unnamed_22,"a",@progbits
.L__unnamed_22:
	.ascii	"x28"
	.size	.L__unnamed_22, 3

	.type	.L__unnamed_23,@object
	.section	.rodata..L__unnamed_23,"a",@progbits
.L__unnamed_23:
	.ascii	"x29"
	.size	.L__unnamed_23, 3

	.type	.L__unnamed_24,@object
	.section	.rodata..L__unnamed_24,"a",@progbits
.L__unnamed_24:
	.ascii	"x30"
	.size	.L__unnamed_24, 3

	.type	.L__unnamed_25,@object
	.section	.rodata..L__unnamed_25,"a",@progbits
.L__unnamed_25:
	.ascii	"x31"
	.size	.L__unnamed_25, 3

	.type	_ZN20powdr_riscv_syscalls17SYSCALL_REGISTERS17h69cc336b2d9de90cE,@object
	.section	.rodata._ZN20powdr_riscv_syscalls17SYSCALL_REGISTERS17h69cc336b2d9de90cE,"a",@progbits
	.globl	_ZN20powdr_riscv_syscalls17SYSCALL_REGISTERS17h69cc336b2d9de90cE
	.p2align	2, 0x0
_ZN20powdr_riscv_syscalls17SYSCALL_REGISTERS17h69cc336b2d9de90cE:
	.word	.L__unnamed_12
	.asciz	"\003\000\000"
	.word	.L__unnamed_13
	.asciz	"\003\000\000"
	.word	.L__unnamed_14
	.asciz	"\003\000\000"
	.word	.L__unnamed_15
	.asciz	"\003\000\000"
	.word	.L__unnamed_16
	.asciz	"\003\000\000"
	.word	.L__unnamed_17
	.asciz	"\003\000\000"
	.word	.L__unnamed_18
	.asciz	"\003\000\000"
	.word	.L__unnamed_19
	.asciz	"\003\000\000"
	.word	.L__unnamed_20
	.asciz	"\002\000\000"
	.word	.L__unnamed_21
	.asciz	"\002\000\000"
	.word	.L__unnamed_22
	.asciz	"\003\000\000"
	.word	.L__unnamed_23
	.asciz	"\003\000\000"
	.word	.L__unnamed_24
	.asciz	"\003\000\000"
	.word	.L__unnamed_25
	.asciz	"\003\000\000"
	.size	_ZN20powdr_riscv_syscalls17SYSCALL_REGISTERS17h69cc336b2d9de90cE, 112

	.type	.L__unnamed_10,@object
	.section	.rodata..L__unnamed_10,"a",@progbits
	.p2align	2, 0x0
.L__unnamed_10:
	.word	.L__unnamed_11
	.zero	4
	.size	.L__unnamed_10, 8

	.type	.L__unnamed_1,@object
	.section	.rodata..L__unnamed_1,"a",@progbits
.L__unnamed_1:
	.ascii	"input"
	.size	.L__unnamed_1, 5

	.type	.L__unnamed_2,@object
	.section	.rodata..L__unnamed_2,"a",@progbits
.L__unnamed_2:
	.ascii	"data_identifier"
	.size	.L__unnamed_2, 15

	.type	.L__unnamed_3,@object
	.section	.rodata..L__unnamed_3,"a",@progbits
.L__unnamed_3:
	.ascii	"output"
	.size	.L__unnamed_3, 6

	.type	.L__unnamed_4,@object
	.section	.rodata..L__unnamed_4,"a",@progbits
.L__unnamed_4:
	.ascii	"poseidon_gl"
	.size	.L__unnamed_4, 11

	.type	.L__unnamed_5,@object
	.section	.rodata..L__unnamed_5,"a",@progbits
.L__unnamed_5:
	.ascii	"affine_256"
	.size	.L__unnamed_5, 10

	.type	.L__unnamed_6,@object
	.section	.rodata..L__unnamed_6,"a",@progbits
.L__unnamed_6:
	.ascii	"ec_add"
	.size	.L__unnamed_6, 6

	.type	.L__unnamed_7,@object
	.section	.rodata..L__unnamed_7,"a",@progbits
.L__unnamed_7:
	.ascii	"ec_double"
	.size	.L__unnamed_7, 9

	.type	.L__unnamed_9,@object
	.section	.rodata..L__unnamed_9,"a",@progbits
.L__unnamed_9:
	.ascii	"mod_256"
	.size	.L__unnamed_9, 7

	.type	.L__unnamed_8,@object
	.section	.rodata..L__unnamed_8,"a",@progbits
.L__unnamed_8:
	.ascii	"keccakf"
	.size	.L__unnamed_8, 7

	.section	.debug_loc,"",@progbits
.Ldebug_loc0:
	.word	-1
	.word	.Lfunc_begin0
	.word	.Lfunc_begin0-.Lfunc_begin0
	.word	.Ltmp1-.Lfunc_begin0
	.half	1
	.byte	90
	.word	0
	.word	0
.Ldebug_loc1:
	.word	-1
	.word	.Lfunc_begin0
	.word	.Lfunc_begin0-.Lfunc_begin0
	.word	.Ltmp0-.Lfunc_begin0
	.half	1
	.byte	91
	.word	.Ltmp0-.Lfunc_begin0
	.word	.Ltmp2-.Lfunc_begin0
	.half	1
	.byte	94
	.word	.Ltmp2-.Lfunc_begin0
	.word	.Lfunc_end0-.Lfunc_begin0
	.half	1
	.byte	92
	.word	0
	.word	0
.Ldebug_loc2:
	.word	-1
	.word	.Lfunc_begin1
	.word	.Lfunc_begin1-.Lfunc_begin1
	.word	.Ltmp5-.Lfunc_begin1
	.half	1
	.byte	90
	.word	0
	.word	0
.Ldebug_loc3:
	.word	-1
	.word	.Lfunc_begin1
	.word	.Lfunc_begin1-.Lfunc_begin1
	.word	.Ltmp6-.Lfunc_begin1
	.half	1
	.byte	91
	.word	.Ltmp6-.Lfunc_begin1
	.word	.Ltmp23-.Lfunc_begin1
	.half	1
	.byte	90
	.word	0
	.word	0
.Ldebug_loc4:
	.word	-1
	.word	.Lfunc_begin1
	.word	.Ltmp19-.Lfunc_begin1
	.word	.Lfunc_end1-.Lfunc_begin1
	.half	9
	.byte	114
	.byte	28
	.byte	159
	.byte	147
	.byte	4
	.byte	49
	.byte	159
	.byte	147
	.byte	4
	.word	0
	.word	0
.Ldebug_loc5:
	.word	-1
	.word	.Lfunc_begin1
	.word	.Ltmp19-.Lfunc_begin1
	.word	.Ltmp20-.Lfunc_begin1
	.half	6
	.byte	147
	.byte	4
	.byte	49
	.byte	159
	.byte	147
	.byte	4
	.word	.Ltmp20-.Lfunc_begin1
	.word	.Ltmp21-.Lfunc_begin1
	.half	7
	.byte	91
	.byte	147
	.byte	4
	.byte	49
	.byte	159
	.byte	147
	.byte	4
	.word	.Ltmp21-.Lfunc_begin1
	.word	.Lfunc_end1-.Lfunc_begin1
	.half	6
	.byte	147
	.byte	4
	.byte	49
	.byte	159
	.byte	147
	.byte	4
	.word	0
	.word	0
.Ldebug_loc6:
	.word	-1
	.word	.Lfunc_begin2
	.word	.Lfunc_begin2-.Lfunc_begin2
	.word	.Ltmp25-.Lfunc_begin2
	.half	6
	.byte	90
	.byte	147
	.byte	4
	.byte	91
	.byte	147
	.byte	4
	.word	.Ltmp25-.Lfunc_begin2
	.word	.Ltmp28-.Lfunc_begin2
	.half	6
	.byte	90
	.byte	147
	.byte	4
	.byte	89
	.byte	147
	.byte	4
	.word	.Ltmp28-.Lfunc_begin2
	.word	.Ltmp53-.Lfunc_begin2
	.half	6
	.byte	88
	.byte	147
	.byte	4
	.byte	89
	.byte	147
	.byte	4
	.word	.Ltmp53-.Lfunc_begin2
	.word	.Ltmp55-.Lfunc_begin2
	.half	5
	.byte	147
	.byte	4
	.byte	89
	.byte	147
	.byte	4
	.word	.Ltmp56-.Lfunc_begin2
	.word	.Lfunc_end2-.Lfunc_begin2
	.half	6
	.byte	88
	.byte	147
	.byte	4
	.byte	89
	.byte	147
	.byte	4
	.word	0
	.word	0
.Ldebug_loc7:
	.word	-1
	.word	.Lfunc_begin2
	.word	.Lfunc_begin2-.Lfunc_begin2
	.word	.Ltmp25-.Lfunc_begin2
	.half	3
	.byte	90
	.byte	147
	.byte	4
	.word	.Ltmp25-.Lfunc_begin2
	.word	.Ltmp28-.Lfunc_begin2
	.half	6
	.byte	90
	.byte	147
	.byte	4
	.byte	89
	.byte	147
	.byte	4
	.word	.Ltmp28-.Lfunc_begin2
	.word	.Ltmp31-.Lfunc_begin2
	.half	6
	.byte	88
	.byte	147
	.byte	4
	.byte	89
	.byte	147
	.byte	4
	.word	0
	.word	0
.Ldebug_loc8:
	.word	-1
	.word	.Lfunc_begin2
	.word	.Ltmp51-.Lfunc_begin2
	.word	.Ltmp53-.Lfunc_begin2
	.half	6
	.byte	88
	.byte	147
	.byte	4
	.byte	89
	.byte	147
	.byte	4
	.word	0
	.word	0
.Ldebug_loc9:
	.word	-1
	.word	.Lfunc_begin2
	.word	.Ltmp31-.Lfunc_begin2
	.word	.Ltmp33-.Lfunc_begin2
	.half	6
	.byte	88
	.byte	147
	.byte	4
	.byte	89
	.byte	147
	.byte	4
	.word	0
	.word	0
.Ldebug_loc10:
	.word	-1
	.word	.Lfunc_begin2
	.word	.Ltmp48-.Lfunc_begin2
	.word	.Ltmp50-.Lfunc_begin2
	.half	6
	.byte	88
	.byte	147
	.byte	4
	.byte	89
	.byte	147
	.byte	4
	.word	0
	.word	0
.Ldebug_loc11:
	.word	-1
	.word	.Lfunc_begin2
	.word	.Ltmp45-.Lfunc_begin2
	.word	.Ltmp47-.Lfunc_begin2
	.half	6
	.byte	88
	.byte	147
	.byte	4
	.byte	89
	.byte	147
	.byte	4
	.word	0
	.word	0
.Ldebug_loc12:
	.word	-1
	.word	.Lfunc_begin2
	.word	.Ltmp43-.Lfunc_begin2
	.word	.Ltmp45-.Lfunc_begin2
	.half	6
	.byte	88
	.byte	147
	.byte	4
	.byte	89
	.byte	147
	.byte	4
	.word	0
	.word	0
.Ldebug_loc13:
	.word	-1
	.word	.Lfunc_begin2
	.word	.Ltmp37-.Lfunc_begin2
	.word	.Ltmp39-.Lfunc_begin2
	.half	6
	.byte	88
	.byte	147
	.byte	4
	.byte	89
	.byte	147
	.byte	4
	.word	0
	.word	0
.Ldebug_loc14:
	.word	-1
	.word	.Lfunc_begin2
	.word	.Lfunc_begin2-.Lfunc_begin2
	.word	.Ltmp25-.Lfunc_begin2
	.half	3
	.byte	90
	.byte	147
	.byte	4
	.word	.Ltmp25-.Lfunc_begin2
	.word	.Ltmp28-.Lfunc_begin2
	.half	6
	.byte	90
	.byte	147
	.byte	4
	.byte	89
	.byte	147
	.byte	4
	.word	.Ltmp28-.Lfunc_begin2
	.word	.Ltmp31-.Lfunc_begin2
	.half	6
	.byte	88
	.byte	147
	.byte	4
	.byte	89
	.byte	147
	.byte	4
	.word	0
	.word	0
.Ldebug_loc15:
	.word	-1
	.word	.Lfunc_begin2
	.word	.Lfunc_begin2-.Lfunc_begin2
	.word	.Ltmp25-.Lfunc_begin2
	.half	3
	.byte	90
	.byte	147
	.byte	4
	.word	.Ltmp25-.Lfunc_begin2
	.word	.Ltmp28-.Lfunc_begin2
	.half	6
	.byte	90
	.byte	147
	.byte	4
	.byte	89
	.byte	147
	.byte	4
	.word	.Ltmp28-.Lfunc_begin2
	.word	.Ltmp31-.Lfunc_begin2
	.half	6
	.byte	88
	.byte	147
	.byte	4
	.byte	89
	.byte	147
	.byte	4
	.word	0
	.word	0
.Ldebug_loc16:
	.word	-1
	.word	.Lfunc_begin2
	.word	.Ltmp26-.Lfunc_begin2
	.word	.Ltmp31-.Lfunc_begin2
	.half	6
	.byte	147
	.byte	4
	.byte	53
	.byte	159
	.byte	147
	.byte	4
	.word	0
	.word	0
.Ldebug_loc17:
	.word	-1
	.word	.Lfunc_begin2
	.word	.Ltmp26-.Lfunc_begin2
	.word	.Ltmp31-.Lfunc_begin2
	.half	6
	.byte	147
	.byte	4
	.byte	53
	.byte	159
	.byte	147
	.byte	4
	.word	0
	.word	0
.Ldebug_loc18:
	.word	-1
	.word	.Lfunc_begin2
	.word	.Ltmp26-.Lfunc_begin2
	.word	.Ltmp31-.Lfunc_begin2
	.half	6
	.byte	147
	.byte	4
	.byte	53
	.byte	159
	.byte	147
	.byte	4
	.word	0
	.word	0
.Ldebug_loc19:
	.word	-1
	.word	.Lfunc_begin2
	.word	.Ltmp33-.Lfunc_begin2
	.word	.Ltmp36-.Lfunc_begin2
	.half	3
	.byte	88
	.byte	147
	.byte	4
	.word	0
	.word	0
.Ldebug_loc20:
	.word	-1
	.word	.Lfunc_begin2
	.word	.Ltmp33-.Lfunc_begin2
	.word	.Ltmp34-.Lfunc_begin2
	.half	6
	.byte	147
	.byte	4
	.byte	54
	.byte	159
	.byte	147
	.byte	4
	.word	.Ltmp34-.Lfunc_begin2
	.word	.Ltmp35-.Lfunc_begin2
	.half	7
	.byte	91
	.byte	147
	.byte	4
	.byte	54
	.byte	159
	.byte	147
	.byte	4
	.word	.Ltmp35-.Lfunc_begin2
	.word	.Ltmp36-.Lfunc_begin2
	.half	6
	.byte	147
	.byte	4
	.byte	54
	.byte	159
	.byte	147
	.byte	4
	.word	0
	.word	0
.Ldebug_loc21:
	.word	-1
	.word	.Lfunc_begin2
	.word	.Ltmp33-.Lfunc_begin2
	.word	.Ltmp36-.Lfunc_begin2
	.half	3
	.byte	88
	.byte	147
	.byte	4
	.word	0
	.word	0
.Ldebug_loc22:
	.word	-1
	.word	.Lfunc_begin2
	.word	.Ltmp33-.Lfunc_begin2
	.word	.Ltmp34-.Lfunc_begin2
	.half	6
	.byte	147
	.byte	4
	.byte	54
	.byte	159
	.byte	147
	.byte	4
	.word	.Ltmp34-.Lfunc_begin2
	.word	.Ltmp35-.Lfunc_begin2
	.half	7
	.byte	91
	.byte	147
	.byte	4
	.byte	54
	.byte	159
	.byte	147
	.byte	4
	.word	.Ltmp35-.Lfunc_begin2
	.word	.Ltmp36-.Lfunc_begin2
	.half	6
	.byte	147
	.byte	4
	.byte	54
	.byte	159
	.byte	147
	.byte	4
	.word	0
	.word	0
.Ldebug_loc23:
	.word	-1
	.word	.Lfunc_begin2
	.word	.Ltmp39-.Lfunc_begin2
	.word	.Ltmp43-.Lfunc_begin2
	.half	6
	.byte	88
	.byte	147
	.byte	4
	.byte	89
	.byte	147
	.byte	4
	.word	0
	.word	0
.Ldebug_loc24:
	.word	-1
	.word	.Lfunc_begin2
	.word	.Ltmp39-.Lfunc_begin2
	.word	.Ltmp40-.Lfunc_begin2
	.half	6
	.byte	147
	.byte	4
	.byte	55
	.byte	159
	.byte	147
	.byte	4
	.word	.Ltmp40-.Lfunc_begin2
	.word	.Ltmp41-.Lfunc_begin2
	.half	7
	.byte	91
	.byte	147
	.byte	4
	.byte	55
	.byte	159
	.byte	147
	.byte	4
	.word	.Ltmp41-.Lfunc_begin2
	.word	.Ltmp43-.Lfunc_begin2
	.half	6
	.byte	147
	.byte	4
	.byte	55
	.byte	159
	.byte	147
	.byte	4
	.word	0
	.word	0
.Ldebug_loc25:
	.word	-1
	.word	.Lfunc_begin2
	.word	.Ltmp39-.Lfunc_begin2
	.word	.Ltmp43-.Lfunc_begin2
	.half	6
	.byte	88
	.byte	147
	.byte	4
	.byte	89
	.byte	147
	.byte	4
	.word	0
	.word	0
.Ldebug_loc26:
	.word	-1
	.word	.Lfunc_begin2
	.word	.Ltmp39-.Lfunc_begin2
	.word	.Ltmp40-.Lfunc_begin2
	.half	6
	.byte	147
	.byte	4
	.byte	55
	.byte	159
	.byte	147
	.byte	4
	.word	.Ltmp40-.Lfunc_begin2
	.word	.Ltmp41-.Lfunc_begin2
	.half	7
	.byte	91
	.byte	147
	.byte	4
	.byte	55
	.byte	159
	.byte	147
	.byte	4
	.word	.Ltmp41-.Lfunc_begin2
	.word	.Ltmp43-.Lfunc_begin2
	.half	6
	.byte	147
	.byte	4
	.byte	55
	.byte	159
	.byte	147
	.byte	4
	.word	0
	.word	0
	.section	.debug_abbrev,"",@progbits
	.byte	1
	.byte	17
	.byte	1
	.byte	37
	.byte	14
	.byte	19
	.byte	5
	.byte	3
	.byte	14
	.byte	16
	.byte	23
	.byte	27
	.byte	14
	.byte	17
	.byte	1
	.byte	85
	.byte	23
	.byte	0
	.byte	0
	.byte	2
	.byte	57
	.byte	1
	.byte	3
	.byte	14
	.byte	0
	.byte	0
	.byte	3
	.byte	52
	.byte	0
	.byte	3
	.byte	14
	.byte	73
	.byte	19
	.byte	63
	.byte	25
	.byte	58
	.byte	11
	.byte	59
	.byte	11
	.ascii	"\210\001"
	.byte	15
	.byte	2
	.byte	24
	.byte	110
	.byte	14
	.byte	0
	.byte	0
	.byte	4
	.byte	4
	.byte	1
	.byte	73
	.byte	19
	.byte	109
	.byte	25
	.byte	3
	.byte	14
	.byte	11
	.byte	11
	.ascii	"\210\001"
	.byte	15
	.byte	0
	.byte	0
	.byte	5
	.byte	40
	.byte	0
	.byte	3
	.byte	14
	.byte	28
	.byte	15
	.byte	0
	.byte	0
	.byte	6
	.byte	46
	.byte	1
	.byte	17
	.byte	1
	.byte	18
	.byte	6
	.byte	64
	.byte	24
	.byte	110
	.byte	14
	.byte	3
	.byte	14
	.byte	58
	.byte	11
	.byte	59
	.byte	11
	.byte	73
	.byte	19
	.byte	63
	.byte	25
	.byte	0
	.byte	0
	.byte	7
	.byte	5
	.byte	0
	.byte	2
	.byte	23
	.byte	3
	.byte	14
	.byte	58
	.byte	11
	.byte	59
	.byte	11
	.byte	73
	.byte	19
	.byte	0
	.byte	0
	.byte	8
	.byte	29
	.byte	1
	.byte	49
	.byte	19
	.byte	17
	.byte	1
	.byte	18
	.byte	6
	.byte	88
	.byte	11
	.byte	89
	.byte	11
	.byte	87
	.byte	11
	.byte	0
	.byte	0
	.byte	9
	.byte	5
	.byte	0
	.byte	2
	.byte	23
	.byte	49
	.byte	19
	.byte	0
	.byte	0
	.byte	10
	.byte	29
	.byte	1
	.byte	49
	.byte	19
	.byte	85
	.byte	23
	.byte	88
	.byte	11
	.byte	89
	.byte	11
	.byte	87
	.byte	11
	.byte	0
	.byte	0
	.byte	11
	.byte	11
	.byte	1
	.byte	17
	.byte	1
	.byte	18
	.byte	6
	.byte	0
	.byte	0
	.byte	12
	.byte	52
	.byte	0
	.byte	2
	.byte	23
	.byte	49
	.byte	19
	.byte	0
	.byte	0
	.byte	13
	.byte	29
	.byte	1
	.byte	49
	.byte	19
	.byte	17
	.byte	1
	.byte	18
	.byte	6
	.byte	88
	.byte	11
	.byte	89
	.byte	5
	.byte	87
	.byte	11
	.byte	0
	.byte	0
	.byte	14
	.byte	52
	.byte	0
	.byte	2
	.byte	24
	.byte	49
	.byte	19
	.byte	0
	.byte	0
	.byte	15
	.byte	29
	.byte	0
	.byte	49
	.byte	19
	.byte	17
	.byte	1
	.byte	18
	.byte	6
	.byte	88
	.byte	11
	.byte	89
	.byte	11
	.byte	87
	.byte	11
	.byte	0
	.byte	0
	.byte	16
	.byte	1
	.byte	1
	.byte	73
	.byte	19
	.byte	0
	.byte	0
	.byte	17
	.byte	33
	.byte	0
	.byte	73
	.byte	19
	.byte	34
	.byte	13
	.byte	55
	.byte	11
	.byte	0
	.byte	0
	.byte	18
	.byte	19
	.byte	1
	.byte	3
	.byte	14
	.byte	11
	.byte	11
	.ascii	"\210\001"
	.byte	15
	.byte	0
	.byte	0
	.byte	19
	.byte	13
	.byte	0
	.byte	3
	.byte	14
	.byte	73
	.byte	19
	.ascii	"\210\001"
	.byte	15
	.byte	56
	.byte	11
	.byte	0
	.byte	0
	.byte	20
	.byte	15
	.byte	0
	.byte	73
	.byte	19
	.byte	51
	.byte	6
	.byte	0
	.byte	0
	.byte	21
	.byte	36
	.byte	0
	.byte	3
	.byte	14
	.byte	62
	.byte	11
	.byte	11
	.byte	11
	.byte	0
	.byte	0
	.byte	22
	.byte	36
	.byte	0
	.byte	3
	.byte	14
	.byte	11
	.byte	11
	.byte	62
	.byte	11
	.byte	0
	.byte	0
	.byte	23
	.byte	19
	.byte	1
	.byte	3
	.byte	14
	.byte	11
	.byte	11
	.byte	50
	.byte	11
	.ascii	"\210\001"
	.byte	15
	.byte	0
	.byte	0
	.byte	24
	.byte	13
	.byte	0
	.byte	3
	.byte	14
	.byte	73
	.byte	19
	.ascii	"\210\001"
	.byte	15
	.byte	56
	.byte	11
	.byte	50
	.byte	11
	.byte	0
	.byte	0
	.byte	25
	.byte	51
	.byte	1
	.byte	21
	.byte	19
	.byte	0
	.byte	0
	.byte	26
	.byte	13
	.byte	0
	.byte	73
	.byte	19
	.ascii	"\210\001"
	.byte	15
	.byte	56
	.byte	11
	.byte	52
	.byte	25
	.byte	0
	.byte	0
	.byte	27
	.byte	25
	.byte	1
	.byte	22
	.byte	11
	.byte	0
	.byte	0
	.byte	28
	.byte	19
	.byte	0
	.byte	3
	.byte	14
	.byte	11
	.byte	11
	.byte	50
	.byte	11
	.ascii	"\210\001"
	.byte	15
	.byte	0
	.byte	0
	.byte	29
	.byte	19
	.byte	0
	.byte	3
	.byte	14
	.byte	11
	.byte	11
	.ascii	"\210\001"
	.byte	15
	.byte	0
	.byte	0
	.byte	30
	.byte	46
	.byte	1
	.byte	17
	.byte	1
	.byte	18
	.byte	6
	.byte	64
	.byte	24
	.byte	110
	.byte	14
	.byte	3
	.byte	14
	.byte	58
	.byte	11
	.byte	59
	.byte	5
	.byte	73
	.byte	19
	.byte	0
	.byte	0
	.byte	31
	.byte	5
	.byte	0
	.byte	2
	.byte	23
	.byte	3
	.byte	14
	.byte	58
	.byte	11
	.byte	59
	.byte	5
	.byte	73
	.byte	19
	.byte	0
	.byte	0
	.byte	32
	.byte	47
	.byte	0
	.byte	73
	.byte	19
	.byte	3
	.byte	14
	.byte	0
	.byte	0
	.byte	33
	.byte	46
	.byte	1
	.byte	110
	.byte	14
	.byte	3
	.byte	14
	.byte	58
	.byte	11
	.byte	59
	.byte	5
	.byte	73
	.byte	19
	.byte	60
	.byte	25
	.byte	0
	.byte	0
	.byte	34
	.byte	5
	.byte	0
	.byte	73
	.byte	19
	.byte	0
	.byte	0
	.byte	35
	.byte	25
	.byte	1
	.byte	0
	.byte	0
	.byte	36
	.byte	46
	.byte	1
	.byte	110
	.byte	14
	.byte	3
	.byte	14
	.byte	58
	.byte	11
	.byte	59
	.byte	11
	.byte	73
	.byte	19
	.byte	32
	.byte	11
	.byte	0
	.byte	0
	.byte	37
	.byte	5
	.byte	0
	.byte	3
	.byte	14
	.byte	58
	.byte	11
	.byte	59
	.byte	11
	.byte	73
	.byte	19
	.byte	0
	.byte	0
	.byte	38
	.byte	11
	.byte	1
	.byte	0
	.byte	0
	.byte	39
	.byte	52
	.byte	0
	.byte	3
	.byte	14
	.byte	58
	.byte	11
	.byte	59
	.byte	11
	.byte	73
	.byte	19
	.byte	0
	.byte	0
	.byte	40
	.byte	46
	.byte	1
	.byte	110
	.byte	14
	.byte	3
	.byte	14
	.byte	58
	.byte	11
	.byte	59
	.byte	5
	.byte	73
	.byte	19
	.byte	32
	.byte	11
	.byte	0
	.byte	0
	.byte	41
	.byte	5
	.byte	0
	.byte	3
	.byte	14
	.byte	58
	.byte	11
	.byte	59
	.byte	5
	.byte	73
	.byte	19
	.byte	0
	.byte	0
	.byte	42
	.byte	15
	.byte	0
	.byte	73
	.byte	19
	.byte	3
	.byte	14
	.byte	51
	.byte	6
	.byte	0
	.byte	0
	.byte	43
	.byte	21
	.byte	1
	.byte	73
	.byte	19
	.byte	0
	.byte	0
	.byte	44
	.byte	46
	.byte	1
	.byte	71
	.byte	19
	.byte	32
	.byte	11
	.byte	0
	.byte	0
	.byte	0
	.section	.debug_info,"",@progbits
.Lcu_begin0:
	.word	.Ldebug_info_end0-.Ldebug_info_start0
.Ldebug_info_start0:
	.half	4
	.word	.debug_abbrev
	.byte	4
	.byte	1
	.word	.Linfo_string0
	.half	28
	.word	.Linfo_string1
	.word	.Lline_table_start0
	.word	.Linfo_string2
	.word	0
	.word	.Ldebug_ranges1
	.byte	2
	.word	.Linfo_string3
	.byte	3
	.word	.Linfo_string4
	.word	1174

	.byte	1
	.byte	11
	.byte	4
	.byte	5
	.byte	3
	.word	_ZN20powdr_riscv_syscalls17SYSCALL_REGISTERS17h69cc336b2d9de90cE
	.word	.Linfo_string11
	.byte	4
	.word	3123

	.word	.Linfo_string30
	.byte	4
	.byte	4
	.byte	5
	.word	.Linfo_string21
	.byte	0
	.byte	5
	.word	.Linfo_string22
	.byte	1
	.byte	5
	.word	.Linfo_string23
	.byte	2
	.byte	5
	.word	.Linfo_string24
	.byte	3
	.byte	5
	.word	.Linfo_string25
	.byte	4
	.byte	5
	.word	.Linfo_string26
	.byte	5
	.byte	5
	.word	.Linfo_string27
	.byte	6
	.byte	5
	.word	.Linfo_string28
	.byte	8
	.byte	5
	.word	.Linfo_string29
	.byte	7
	.byte	0
	.byte	2
	.word	.Linfo_string81
	.byte	6
	.word	.Lfunc_begin1
	.word	.Lfunc_end1-.Lfunc_begin1
	.byte	1
	.byte	82
	.word	.Linfo_string108
	.word	.Linfo_string13
	.byte	1
	.byte	24
	.word	1982

	.byte	7
	.word	.Ldebug_loc2
	.word	.Linfo_string90
	.byte	1
	.byte	24
	.word	3482
	.byte	7
	.word	.Ldebug_loc3
	.word	.Linfo_string113
	.byte	1
	.byte	24
	.word	3303
	.byte	8
	.word	3388
	.word	.Ltmp19
	.word	.Ltmp22-.Ltmp19
	.byte	1
	.byte	25
	.byte	17
	.byte	9
	.word	.Ldebug_loc5
	.word	3394
	.byte	9
	.word	.Ldebug_loc4
	.word	3406
	.byte	0
	.byte	0
	.byte	0
	.byte	2
	.word	.Linfo_string103
	.byte	6
	.word	.Lfunc_begin2
	.word	.Lfunc_end2-.Lfunc_begin2
	.byte	1
	.byte	82
	.word	.Linfo_string109
	.word	.Linfo_string110
	.byte	1
	.byte	33
	.word	2110

	.byte	7
	.word	.Ldebug_loc6
	.word	.Linfo_string115
	.byte	1
	.byte	33
	.word	1187
	.byte	10
	.word	2886
	.word	.Ldebug_ranges0
	.byte	1
	.byte	35
	.byte	23
	.byte	11
	.word	.Ltmp25
	.word	.Ltmp30-.Ltmp25
	.byte	9
	.word	.Ldebug_loc7
	.word	2903
	.byte	12
	.word	.Ldebug_loc18
	.word	2914
	.byte	8
	.word	2598
	.word	.Ltmp25
	.word	.Ltmp30-.Ltmp25
	.byte	5
	.byte	30
	.byte	9
	.byte	13
	.word	2329
	.word	.Ltmp25
	.word	.Ltmp30-.Ltmp25
	.byte	4
	.half	1611
	.byte	13
	.byte	11
	.word	.Ltmp25
	.word	.Ltmp30-.Ltmp25
	.byte	9
	.word	.Ldebug_loc14
	.word	2364
	.byte	9
	.word	.Ldebug_loc17
	.word	2375
	.byte	8
	.word	2253
	.word	.Ltmp25
	.word	.Ltmp30-.Ltmp25
	.byte	3
	.byte	16
	.byte	9
	.byte	9
	.word	.Ldebug_loc15
	.word	2287
	.byte	9
	.word	.Ldebug_loc16
	.word	2298
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	11
	.word	.Ltmp31
	.word	.Ltmp32-.Ltmp31
	.byte	9
	.word	.Ldebug_loc9
	.word	2927
	.byte	14
	.byte	6
	.byte	147
	.byte	4
	.byte	54
	.byte	159
	.byte	147
	.byte	4
	.word	2938
	.byte	8
	.word	2598
	.word	.Ltmp31
	.word	.Ltmp32-.Ltmp31
	.byte	5
	.byte	30
	.byte	9
	.byte	13
	.word	2329
	.word	.Ltmp31
	.word	.Ltmp32-.Ltmp31
	.byte	4
	.half	1611
	.byte	13
	.byte	15
	.word	2253
	.word	.Ltmp31
	.word	.Ltmp32-.Ltmp31
	.byte	3
	.byte	16
	.byte	9
	.byte	0
	.byte	0
	.byte	0
	.byte	8
	.word	2598
	.word	.Ltmp33
	.word	.Ltmp35-.Ltmp33
	.byte	5
	.byte	30
	.byte	9
	.byte	13
	.word	2329
	.word	.Ltmp33
	.word	.Ltmp35-.Ltmp33
	.byte	4
	.half	1611
	.byte	13
	.byte	11
	.word	.Ltmp33
	.word	.Ltmp35-.Ltmp33
	.byte	9
	.word	.Ldebug_loc19
	.word	2412
	.byte	9
	.word	.Ldebug_loc20
	.word	2423
	.byte	8
	.word	2253
	.word	.Ltmp33
	.word	.Ltmp35-.Ltmp33
	.byte	3
	.byte	16
	.byte	9
	.byte	9
	.word	.Ldebug_loc21
	.word	2287
	.byte	9
	.word	.Ldebug_loc22
	.word	2298
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	11
	.word	.Ltmp37
	.word	.Ltmp38-.Ltmp37
	.byte	9
	.word	.Ldebug_loc13
	.word	2975
	.byte	14
	.byte	6
	.byte	147
	.byte	4
	.byte	55
	.byte	159
	.byte	147
	.byte	4
	.word	2986
	.byte	8
	.word	2598
	.word	.Ltmp37
	.word	.Ltmp38-.Ltmp37
	.byte	5
	.byte	30
	.byte	9
	.byte	13
	.word	2329
	.word	.Ltmp37
	.word	.Ltmp38-.Ltmp37
	.byte	4
	.half	1611
	.byte	13
	.byte	8
	.word	2253
	.word	.Ltmp37
	.word	.Ltmp38-.Ltmp37
	.byte	3
	.byte	16
	.byte	9
	.byte	11
	.word	.Ltmp37
	.word	.Ltmp38-.Ltmp37
	.byte	14
	.byte	1
	.byte	89
	.word	2310
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	8
	.word	2598
	.word	.Ltmp39
	.word	.Ltmp42-.Ltmp39
	.byte	5
	.byte	30
	.byte	9
	.byte	13
	.word	2329
	.word	.Ltmp39
	.word	.Ltmp42-.Ltmp39
	.byte	4
	.half	1611
	.byte	13
	.byte	11
	.word	.Ltmp39
	.word	.Ltmp42-.Ltmp39
	.byte	9
	.word	.Ldebug_loc23
	.word	2460
	.byte	9
	.word	.Ldebug_loc24
	.word	2471
	.byte	8
	.word	2253
	.word	.Ltmp39
	.word	.Ltmp42-.Ltmp39
	.byte	3
	.byte	16
	.byte	9
	.byte	9
	.word	.Ldebug_loc25
	.word	2287
	.byte	9
	.word	.Ldebug_loc26
	.word	2298
	.byte	11
	.word	.Ltmp39
	.word	.Ltmp42-.Ltmp39
	.byte	14
	.byte	1
	.byte	89
	.word	2310
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	11
	.word	.Ltmp43
	.word	.Ltmp44-.Ltmp43
	.byte	9
	.word	.Ldebug_loc12
	.word	3023
	.byte	14
	.byte	6
	.byte	147
	.byte	4
	.byte	57
	.byte	159
	.byte	147
	.byte	4
	.word	3034
	.byte	8
	.word	2598
	.word	.Ltmp43
	.word	.Ltmp44-.Ltmp43
	.byte	5
	.byte	30
	.byte	9
	.byte	13
	.word	2329
	.word	.Ltmp43
	.word	.Ltmp44-.Ltmp43
	.byte	4
	.half	1611
	.byte	13
	.byte	8
	.word	2253
	.word	.Ltmp43
	.word	.Ltmp44-.Ltmp43
	.byte	3
	.byte	16
	.byte	9
	.byte	11
	.word	.Ltmp43
	.word	.Ltmp44-.Ltmp43
	.byte	14
	.byte	1
	.byte	89
	.word	2310
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	11
	.word	.Ltmp45
	.word	.Ltmp46-.Ltmp45
	.byte	9
	.word	.Ldebug_loc11
	.word	3047
	.byte	14
	.byte	6
	.byte	147
	.byte	4
	.byte	58
	.byte	159
	.byte	147
	.byte	4
	.word	3058
	.byte	8
	.word	2598
	.word	.Ltmp45
	.word	.Ltmp46-.Ltmp45
	.byte	5
	.byte	30
	.byte	9
	.byte	13
	.word	2329
	.word	.Ltmp45
	.word	.Ltmp46-.Ltmp45
	.byte	4
	.half	1611
	.byte	13
	.byte	8
	.word	2253
	.word	.Ltmp45
	.word	.Ltmp46-.Ltmp45
	.byte	3
	.byte	16
	.byte	9
	.byte	11
	.word	.Ltmp45
	.word	.Ltmp46-.Ltmp45
	.byte	14
	.byte	1
	.byte	89
	.word	2310
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	11
	.word	.Ltmp48
	.word	.Ltmp49-.Ltmp48
	.byte	9
	.word	.Ldebug_loc10
	.word	3071
	.byte	14
	.byte	6
	.byte	147
	.byte	4
	.byte	59
	.byte	159
	.byte	147
	.byte	4
	.word	3082
	.byte	8
	.word	2598
	.word	.Ltmp48
	.word	.Ltmp49-.Ltmp48
	.byte	5
	.byte	30
	.byte	9
	.byte	13
	.word	2329
	.word	.Ltmp48
	.word	.Ltmp49-.Ltmp48
	.byte	4
	.half	1611
	.byte	13
	.byte	15
	.word	2253
	.word	.Ltmp48
	.word	.Ltmp49-.Ltmp48
	.byte	3
	.byte	16
	.byte	9
	.byte	0
	.byte	0
	.byte	0
	.byte	11
	.word	.Ltmp51
	.word	.Ltmp52-.Ltmp51
	.byte	9
	.word	.Ldebug_loc8
	.word	3095
	.byte	14
	.byte	6
	.byte	147
	.byte	4
	.byte	63
	.byte	159
	.byte	147
	.byte	4
	.word	3106
	.byte	8
	.word	2598
	.word	.Ltmp51
	.word	.Ltmp52-.Ltmp51
	.byte	5
	.byte	30
	.byte	9
	.byte	13
	.word	2329
	.word	.Ltmp51
	.word	.Ltmp52-.Ltmp51
	.byte	4
	.half	1611
	.byte	13
	.byte	15
	.word	2253
	.word	.Ltmp51
	.word	.Ltmp52-.Ltmp51
	.byte	3
	.byte	16
	.byte	9
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	16
	.word	1187
	.byte	17
	.word	1240
	.byte	0
	.byte	14
	.byte	0
	.byte	18
	.word	.Linfo_string9
	.byte	8
	.byte	4
	.byte	19
	.word	.Linfo_string5
	.word	1217
	.byte	4
	.byte	0
	.byte	19
	.word	.Linfo_string7
	.word	1233
	.byte	4
	.byte	4
	.byte	0
	.byte	20
	.word	1226
	.word	0
	.byte	21
	.word	.Linfo_string6
	.byte	7
	.byte	1
	.byte	21
	.word	.Linfo_string8
	.byte	7
	.byte	4
	.byte	22
	.word	.Linfo_string10
	.byte	8
	.byte	7
	.byte	2
	.word	.Linfo_string12
	.byte	2
	.word	.Linfo_string13
	.byte	2
	.word	.Linfo_string14
	.byte	4
	.word	1226

	.word	.Linfo_string19
	.byte	1
	.byte	1
	.byte	5
	.word	.Linfo_string15
	.byte	0
	.byte	5
	.word	.Linfo_string16
	.byte	1
	.byte	5
	.word	.Linfo_string17
	.byte	2
	.byte	5
	.word	.Linfo_string18
	.byte	3
	.byte	0
	.byte	23
	.word	.Linfo_string48
	.byte	32
	.byte	1
	.byte	4
	.byte	24
	.word	.Linfo_string36
	.word	1233
	.byte	4
	.byte	20
	.byte	1
	.byte	24
	.word	.Linfo_string37
	.word	3208
	.byte	4
	.byte	16
	.byte	1
	.byte	24
	.word	.Linfo_string39
	.word	1262
	.byte	1
	.byte	28
	.byte	1
	.byte	24
	.word	.Linfo_string40
	.word	3123
	.byte	4
	.byte	24
	.byte	1
	.byte	24
	.word	.Linfo_string41
	.word	1379
	.byte	4
	.byte	0
	.byte	1
	.byte	24
	.word	.Linfo_string47
	.word	1379
	.byte	4
	.byte	8
	.byte	1
	.byte	0
	.byte	23
	.word	.Linfo_string46
	.byte	8
	.byte	1
	.byte	4
	.byte	25
	.word	1392
	.byte	26
	.word	3123
	.byte	4
	.byte	0

	.byte	27
	.byte	0
	.byte	19
	.word	.Linfo_string42
	.word	1442
	.byte	4
	.byte	0
	.byte	0
	.byte	27
	.byte	1
	.byte	19
	.word	.Linfo_string44
	.word	1463
	.byte	4
	.byte	0
	.byte	0
	.byte	27
	.byte	2
	.byte	19
	.word	.Linfo_string45
	.word	1484
	.byte	4
	.byte	0
	.byte	0
	.byte	0
	.byte	23
	.word	.Linfo_string42
	.byte	8
	.byte	1
	.byte	4
	.byte	24
	.word	.Linfo_string43
	.word	1233
	.byte	4
	.byte	4
	.byte	1
	.byte	0
	.byte	23
	.word	.Linfo_string44
	.byte	8
	.byte	1
	.byte	4
	.byte	24
	.word	.Linfo_string43
	.word	1233
	.byte	4
	.byte	4
	.byte	1
	.byte	0
	.byte	28
	.word	.Linfo_string45
	.byte	8
	.byte	1
	.byte	4
	.byte	0
	.byte	23
	.word	.Linfo_string76
	.byte	8
	.byte	1
	.byte	4
	.byte	24
	.word	.Linfo_string54
	.word	3254
	.byte	4
	.byte	0
	.byte	3
	.byte	24
	.word	.Linfo_string58
	.word	3267
	.byte	4
	.byte	4
	.byte	3
	.byte	0
	.byte	2
	.word	.Linfo_string55
	.byte	29
	.word	.Linfo_string56
	.byte	0
	.byte	1
	.byte	0
	.byte	0
	.byte	2
	.word	.Linfo_string31
	.byte	30
	.word	.Lfunc_begin0
	.word	.Lfunc_end0-.Lfunc_begin0
	.byte	1
	.byte	82
	.word	.Linfo_string106
	.word	.Linfo_string107
	.byte	2
	.half	2294
	.word	1982
	.byte	31
	.word	.Ldebug_loc0
	.word	.Linfo_string90
	.byte	2
	.half	2294
	.word	3469
	.byte	31
	.word	.Ldebug_loc1
	.word	.Linfo_string113
	.byte	2
	.half	2294
	.word	3303
	.byte	32
	.word	1226
	.word	.Linfo_string50
	.byte	0
	.byte	0
	.byte	23
	.word	.Linfo_string78
	.byte	24
	.byte	1
	.byte	4
	.byte	24
	.word	.Linfo_string32
	.word	3130
	.byte	4
	.byte	0
	.byte	3
	.byte	24
	.word	.Linfo_string13
	.word	1781
	.byte	4
	.byte	16
	.byte	3
	.byte	24
	.word	.Linfo_string53
	.word	3215
	.byte	4
	.byte	8
	.byte	3
	.byte	33
	.word	.Linfo_string79
	.word	.Linfo_string80
	.byte	2
	.half	331
	.word	1614

	.byte	34
	.word	3130
	.byte	34
	.word	3215
	.byte	0
	.byte	0
	.byte	28
	.word	.Linfo_string62
	.byte	0
	.byte	1
	.byte	1
	.byte	23
	.word	.Linfo_string73
	.byte	36
	.byte	1
	.byte	4
	.byte	24
	.word	.Linfo_string40
	.word	3123
	.byte	4
	.byte	28
	.byte	3
	.byte	24
	.word	.Linfo_string37
	.word	3208
	.byte	4
	.byte	16
	.byte	3
	.byte	24
	.word	.Linfo_string39
	.word	1262
	.byte	1
	.byte	32
	.byte	3
	.byte	24
	.word	.Linfo_string47
	.word	1878
	.byte	4
	.byte	0
	.byte	3
	.byte	24
	.word	.Linfo_string41
	.word	1878
	.byte	4
	.byte	8
	.byte	3
	.byte	24
	.word	.Linfo_string67
	.word	3316
	.byte	4
	.byte	20
	.byte	3
	.byte	0
	.byte	0
	.byte	2
	.word	.Linfo_string34
	.byte	23
	.word	.Linfo_string52
	.byte	8
	.byte	1
	.byte	4
	.byte	25
	.word	1794
	.byte	26
	.word	3123
	.byte	4
	.byte	0

	.byte	27
	.byte	0
	.byte	19
	.word	.Linfo_string35
	.word	1829
	.byte	4
	.byte	0
	.byte	0
	.byte	35
	.byte	19
	.word	.Linfo_string51
	.word	1847
	.byte	4
	.byte	0
	.byte	0
	.byte	0
	.byte	23
	.word	.Linfo_string35
	.byte	8
	.byte	1
	.byte	4
	.byte	32
	.word	3169
	.word	.Linfo_string50
	.byte	0
	.byte	23
	.word	.Linfo_string51
	.byte	8
	.byte	1
	.byte	4
	.byte	32
	.word	3169
	.word	.Linfo_string50
	.byte	24
	.word	.Linfo_string43
	.word	3169
	.byte	4
	.byte	0
	.byte	1
	.byte	0
	.byte	0
	.byte	23
	.word	.Linfo_string66
	.byte	8
	.byte	1
	.byte	4
	.byte	25
	.word	1891
	.byte	26
	.word	3123
	.byte	4
	.byte	0

	.byte	27
	.byte	0
	.byte	19
	.word	.Linfo_string35
	.word	1927
	.byte	4
	.byte	0
	.byte	0
	.byte	27
	.byte	1
	.byte	19
	.word	.Linfo_string51
	.word	1945
	.byte	4
	.byte	0
	.byte	0
	.byte	0
	.byte	23
	.word	.Linfo_string35
	.byte	8
	.byte	1
	.byte	4
	.byte	32
	.word	1233
	.word	.Linfo_string50
	.byte	0
	.byte	23
	.word	.Linfo_string51
	.byte	8
	.byte	1
	.byte	4
	.byte	32
	.word	1233
	.word	.Linfo_string50
	.byte	24
	.word	.Linfo_string43
	.word	1233
	.byte	4
	.byte	4
	.byte	1
	.byte	0
	.byte	0
	.byte	0
	.byte	2
	.word	.Linfo_string59
	.byte	23
	.word	.Linfo_string65
	.byte	1
	.byte	1
	.byte	1
	.byte	25
	.word	1995
	.byte	26
	.word	1226
	.byte	1
	.byte	0

	.byte	27
	.byte	0
	.byte	19
	.word	.Linfo_string60
	.word	2031
	.byte	1
	.byte	0
	.byte	0
	.byte	27
	.byte	1
	.byte	19
	.word	.Linfo_string64
	.word	2070
	.byte	1
	.byte	0
	.byte	0
	.byte	0
	.byte	23
	.word	.Linfo_string60
	.byte	1
	.byte	1
	.byte	1
	.byte	32
	.word	3296
	.word	.Linfo_string50
	.byte	32
	.word	1686
	.word	.Linfo_string63
	.byte	24
	.word	.Linfo_string43
	.word	3296
	.byte	1
	.byte	1
	.byte	1
	.byte	0
	.byte	23
	.word	.Linfo_string64
	.byte	1
	.byte	1
	.byte	1
	.byte	32
	.word	3296
	.word	.Linfo_string50
	.byte	32
	.word	1686
	.word	.Linfo_string63
	.byte	24
	.word	.Linfo_string43
	.word	1686
	.byte	1
	.byte	1
	.byte	1
	.byte	0
	.byte	0
	.byte	23
	.word	.Linfo_string111
	.byte	4
	.byte	1
	.byte	4
	.byte	25
	.word	2123
	.byte	26
	.word	3123
	.byte	4
	.byte	0

	.byte	35
	.byte	19
	.word	.Linfo_string60
	.word	2158
	.byte	4
	.byte	0
	.byte	0
	.byte	27
	.byte	9
	.byte	19
	.word	.Linfo_string64
	.word	2197
	.byte	4
	.byte	0
	.byte	0
	.byte	0
	.byte	23
	.word	.Linfo_string60
	.byte	4
	.byte	1
	.byte	4
	.byte	32
	.word	65
	.word	.Linfo_string50
	.byte	32
	.word	3296
	.word	.Linfo_string63
	.byte	24
	.word	.Linfo_string43
	.word	65
	.byte	4
	.byte	0
	.byte	1
	.byte	0
	.byte	23
	.word	.Linfo_string64
	.byte	4
	.byte	1
	.byte	4
	.byte	32
	.word	65
	.word	.Linfo_string50
	.byte	32
	.word	3296
	.word	.Linfo_string63
	.byte	24
	.word	.Linfo_string43
	.word	3296
	.byte	1
	.byte	0
	.byte	1
	.byte	0
	.byte	0
	.byte	0
	.byte	2
	.word	.Linfo_string82
	.byte	2
	.word	.Linfo_string83
	.byte	2
	.word	.Linfo_string84
	.byte	36
	.word	.Linfo_string87
	.word	.Linfo_string88
	.byte	3
	.byte	83
	.word	3419
	.byte	1
	.byte	32
	.word	1226
	.word	.Linfo_string85
	.byte	32
	.word	1226
	.word	.Linfo_string86
	.byte	37
	.word	.Linfo_string90
	.byte	3
	.byte	83
	.word	3426
	.byte	37
	.word	.Linfo_string92
	.byte	3
	.byte	83
	.word	3426
	.byte	38
	.byte	39
	.word	.Linfo_string93
	.byte	3
	.byte	91
	.word	1233
	.byte	0
	.byte	0
	.byte	0
	.byte	2
	.word	.Linfo_string81
	.byte	36
	.word	.Linfo_string94
	.word	.Linfo_string95
	.byte	3
	.byte	15
	.word	3419
	.byte	1
	.byte	32
	.word	1226
	.word	.Linfo_string85
	.byte	32
	.word	1226
	.word	.Linfo_string86
	.byte	38
	.byte	37
	.word	.Linfo_string90
	.byte	3
	.byte	15
	.word	3426
	.byte	37
	.word	.Linfo_string92
	.byte	3
	.byte	15
	.word	3426
	.byte	0
	.byte	38
	.byte	37
	.word	.Linfo_string90
	.byte	3
	.byte	15
	.word	3426
	.byte	37
	.word	.Linfo_string92
	.byte	3
	.byte	15
	.word	3426
	.byte	0
	.byte	38
	.byte	37
	.word	.Linfo_string90
	.byte	3
	.byte	15
	.word	3426
	.byte	37
	.word	.Linfo_string92
	.byte	3
	.byte	15
	.word	3426
	.byte	0
	.byte	38
	.byte	37
	.word	.Linfo_string90
	.byte	3
	.byte	15
	.word	3426
	.byte	37
	.word	.Linfo_string92
	.byte	3
	.byte	15
	.word	3426
	.byte	0
	.byte	38
	.byte	37
	.word	.Linfo_string90
	.byte	3
	.byte	15
	.word	3426
	.byte	37
	.word	.Linfo_string92
	.byte	3
	.byte	15
	.word	3426
	.byte	0
	.byte	38
	.byte	37
	.word	.Linfo_string90
	.byte	3
	.byte	15
	.word	3426
	.byte	37
	.word	.Linfo_string92
	.byte	3
	.byte	15
	.word	3426
	.byte	0
	.byte	38
	.byte	37
	.word	.Linfo_string90
	.byte	3
	.byte	15
	.word	3426
	.byte	37
	.word	.Linfo_string92
	.byte	3
	.byte	15
	.word	3426
	.byte	0
	.byte	38
	.byte	37
	.word	.Linfo_string90
	.byte	3
	.byte	15
	.word	3426
	.byte	37
	.word	.Linfo_string92
	.byte	3
	.byte	15
	.word	3426
	.byte	0
	.byte	38
	.byte	37
	.word	.Linfo_string90
	.byte	3
	.byte	15
	.word	3426
	.byte	37
	.word	.Linfo_string92
	.byte	3
	.byte	15
	.word	3426
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	2
	.word	.Linfo_string83
	.byte	2
	.word	.Linfo_string96
	.byte	2
	.word	.Linfo_string97
	.byte	40
	.word	.Linfo_string98
	.word	.Linfo_string99
	.byte	4
	.half	1610
	.word	3419
	.byte	1
	.byte	32
	.word	1226
	.word	.Linfo_string85
	.byte	32
	.word	1226
	.word	.Linfo_string86
	.byte	38
	.byte	41
	.word	.Linfo_string90
	.byte	4
	.half	1610
	.word	3456
	.byte	41
	.word	.Linfo_string92
	.byte	4
	.half	1610
	.word	3456
	.byte	0
	.byte	38
	.byte	41
	.word	.Linfo_string90
	.byte	4
	.half	1610
	.word	3456
	.byte	41
	.word	.Linfo_string92
	.byte	4
	.half	1610
	.word	3456
	.byte	0
	.byte	38
	.byte	41
	.word	.Linfo_string90
	.byte	4
	.half	1610
	.word	3456
	.byte	41
	.word	.Linfo_string92
	.byte	4
	.half	1610
	.word	3456
	.byte	0
	.byte	38
	.byte	41
	.word	.Linfo_string90
	.byte	4
	.half	1610
	.word	3456
	.byte	41
	.word	.Linfo_string92
	.byte	4
	.half	1610
	.word	3456
	.byte	0
	.byte	38
	.byte	41
	.word	.Linfo_string90
	.byte	4
	.half	1610
	.word	3456
	.byte	41
	.word	.Linfo_string92
	.byte	4
	.half	1610
	.word	3456
	.byte	0
	.byte	38
	.byte	41
	.word	.Linfo_string90
	.byte	4
	.half	1610
	.word	3456
	.byte	41
	.word	.Linfo_string92
	.byte	4
	.half	1610
	.word	3456
	.byte	0
	.byte	38
	.byte	41
	.word	.Linfo_string90
	.byte	4
	.half	1610
	.word	3456
	.byte	41
	.word	.Linfo_string92
	.byte	4
	.half	1610
	.word	3456
	.byte	0
	.byte	38
	.byte	41
	.word	.Linfo_string90
	.byte	4
	.half	1610
	.word	3456
	.byte	41
	.word	.Linfo_string92
	.byte	4
	.half	1610
	.word	3456
	.byte	0
	.byte	38
	.byte	41
	.word	.Linfo_string90
	.byte	4
	.half	1610
	.word	3456
	.byte	41
	.word	.Linfo_string92
	.byte	4
	.half	1610
	.word	3456
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	2
	.word	.Linfo_string101
	.byte	2
	.word	.Linfo_string102
	.byte	2
	.word	.Linfo_string103
	.byte	36
	.word	.Linfo_string104
	.word	.Linfo_string105
	.byte	5
	.byte	29
	.word	3419
	.byte	1
	.byte	38
	.byte	37
	.word	.Linfo_string90
	.byte	5
	.byte	29
	.word	1187
	.byte	39
	.word	.Linfo_string92
	.byte	5
	.byte	29
	.word	1187
	.byte	0
	.byte	38
	.byte	37
	.word	.Linfo_string90
	.byte	5
	.byte	29
	.word	1187
	.byte	39
	.word	.Linfo_string92
	.byte	5
	.byte	29
	.word	1187
	.byte	0
	.byte	38
	.byte	37
	.word	.Linfo_string90
	.byte	5
	.byte	29
	.word	1187
	.byte	39
	.word	.Linfo_string92
	.byte	5
	.byte	29
	.word	1187
	.byte	0
	.byte	38
	.byte	37
	.word	.Linfo_string90
	.byte	5
	.byte	29
	.word	1187
	.byte	39
	.word	.Linfo_string92
	.byte	5
	.byte	29
	.word	1187
	.byte	0
	.byte	38
	.byte	37
	.word	.Linfo_string90
	.byte	5
	.byte	29
	.word	1187
	.byte	39
	.word	.Linfo_string92
	.byte	5
	.byte	29
	.word	1187
	.byte	0
	.byte	38
	.byte	37
	.word	.Linfo_string90
	.byte	5
	.byte	29
	.word	1187
	.byte	39
	.word	.Linfo_string92
	.byte	5
	.byte	29
	.word	1187
	.byte	0
	.byte	38
	.byte	37
	.word	.Linfo_string90
	.byte	5
	.byte	29
	.word	1187
	.byte	39
	.word	.Linfo_string92
	.byte	5
	.byte	29
	.word	1187
	.byte	0
	.byte	38
	.byte	37
	.word	.Linfo_string90
	.byte	5
	.byte	29
	.word	1187
	.byte	39
	.word	.Linfo_string92
	.byte	5
	.byte	29
	.word	1187
	.byte	0
	.byte	38
	.byte	37
	.word	.Linfo_string90
	.byte	5
	.byte	29
	.word	1187
	.byte	39
	.word	.Linfo_string92
	.byte	5
	.byte	29
	.word	1187
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	21
	.word	.Linfo_string20
	.byte	7
	.byte	4
	.byte	18
	.word	.Linfo_string33
	.byte	8
	.byte	4
	.byte	19
	.word	.Linfo_string5
	.word	3160
	.byte	4
	.byte	0
	.byte	19
	.word	.Linfo_string7
	.word	1233
	.byte	4
	.byte	4
	.byte	0
	.byte	20
	.word	1187
	.word	0
	.byte	18
	.word	.Linfo_string49
	.byte	8
	.byte	4
	.byte	19
	.word	.Linfo_string5
	.word	3199
	.byte	4
	.byte	0
	.byte	19
	.word	.Linfo_string7
	.word	1233
	.byte	4
	.byte	4
	.byte	0
	.byte	20
	.word	1298
	.word	0
	.byte	21
	.word	.Linfo_string38
	.byte	16
	.byte	4
	.byte	18
	.word	.Linfo_string77
	.byte	8
	.byte	4
	.byte	19
	.word	.Linfo_string5
	.word	3245
	.byte	4
	.byte	0
	.byte	19
	.word	.Linfo_string7
	.word	1233
	.byte	4
	.byte	4
	.byte	0
	.byte	20
	.word	1493
	.word	0
	.byte	42
	.word	1531
	.word	.Linfo_string57
	.word	0
	.byte	42
	.word	3280
	.word	.Linfo_string75
	.word	0
	.byte	43
	.word	1982
	.byte	34
	.word	3254
	.byte	34
	.word	3303
	.byte	0
	.byte	21
	.word	.Linfo_string61
	.byte	7
	.byte	0
	.byte	42
	.word	1694
	.word	.Linfo_string74
	.word	0
	.byte	18
	.word	.Linfo_string72
	.byte	8
	.byte	4
	.byte	19
	.word	.Linfo_string68
	.word	3346
	.byte	4
	.byte	0
	.byte	19
	.word	.Linfo_string70
	.word	3362
	.byte	4
	.byte	4
	.byte	0
	.byte	20
	.word	3355
	.word	0
	.byte	29
	.word	.Linfo_string69
	.byte	0
	.byte	1
	.byte	42
	.word	3375
	.word	.Linfo_string71
	.word	0
	.byte	16
	.word	1233
	.byte	17
	.word	1240
	.byte	0
	.byte	3
	.byte	0
	.byte	44
	.word	1658
	.byte	1
	.byte	41
	.word	.Linfo_string32
	.byte	2
	.half	331
	.word	3130
	.byte	41
	.word	.Linfo_string53
	.byte	2
	.half	331
	.word	3215
	.byte	0
	.byte	21
	.word	.Linfo_string89
	.byte	2
	.byte	1
	.byte	18
	.word	.Linfo_string91
	.byte	8
	.byte	4
	.byte	19
	.word	.Linfo_string5
	.word	1217
	.byte	4
	.byte	0
	.byte	19
	.word	.Linfo_string7
	.word	1233
	.byte	4
	.byte	4
	.byte	0
	.byte	42
	.word	3426
	.word	.Linfo_string100
	.word	0
	.byte	42
	.word	1187
	.word	.Linfo_string112
	.word	0
	.byte	42
	.word	65
	.word	.Linfo_string114
	.word	0
	.byte	0
.Ldebug_info_end0:
	.section	.rodata._ZN20powdr_riscv_syscalls17SYSCALL_REGISTERS17h69cc336b2d9de90cE,"a",@progbits
.Lsec_end0:
	.section	".text._ZN44_$LT$$RF$T$u20$as$u20$core..fmt..Display$GT$3fmt17h7370650b9d1da708E","ax",@progbits
.Lsec_end1:
	.section	".text._ZN68_$LT$powdr_riscv_syscalls..Syscall$u20$as$u20$core..fmt..Display$GT$3fmt17hdf8850d239dac7f5E","ax",@progbits
.Lsec_end2:
	.section	".text._ZN76_$LT$powdr_riscv_syscalls..Syscall$u20$as$u20$core..str..traits..FromStr$GT$8from_str17h27673a40b24fea4aE","ax",@progbits
.Lsec_end3:
	.section	.debug_aranges,"",@progbits
	.word	52
	.half	2
	.word	.Lcu_begin0
	.byte	4
	.byte	0
	.zero	4,255
	.word	_ZN20powdr_riscv_syscalls17SYSCALL_REGISTERS17h69cc336b2d9de90cE
	.word	.Lsec_end0-_ZN20powdr_riscv_syscalls17SYSCALL_REGISTERS17h69cc336b2d9de90cE
	.word	.Lfunc_begin0
	.word	.Lsec_end1-.Lfunc_begin0
	.word	.Lfunc_begin1
	.word	.Lsec_end2-.Lfunc_begin1
	.word	.Lfunc_begin2
	.word	.Lsec_end3-.Lfunc_begin2
	.word	0
	.word	0
	.section	.debug_ranges,"",@progbits
.Ldebug_ranges0:
	.word	.Ltmp25
	.word	.Ltmp30
	.word	.Ltmp31
	.word	.Ltmp32
	.word	.Ltmp33
	.word	.Ltmp35
	.word	.Ltmp37
	.word	.Ltmp38
	.word	.Ltmp39
	.word	.Ltmp42
	.word	.Ltmp43
	.word	.Ltmp44
	.word	.Ltmp45
	.word	.Ltmp46
	.word	.Ltmp48
	.word	.Ltmp49
	.word	.Ltmp51
	.word	.Ltmp52
	.word	0
	.word	0
.Ldebug_ranges1:
	.word	.Lfunc_begin0
	.word	.Lfunc_end0
	.word	.Lfunc_begin1
	.word	.Lfunc_end1
	.word	.Lfunc_begin2
	.word	.Lfunc_end2
	.word	0
	.word	0
	.section	.debug_str,"MS",@progbits,1
.Linfo_string0:
	.asciz	"clang LLVM (rustc version 1.77.0-nightly (11f32b73e 2024-01-31))"
.Linfo_string1:
	.asciz	"/Users/steve/Documents/repo/powdr-5_6_24/powdr/riscv-syscalls/src/lib.rs/@/powdr_riscv_syscalls.b0de478eb457f70-cgu.0"
.Linfo_string2:
	.asciz	"/Users/steve/Documents/repo/powdr-5_6_24/powdr/riscv-syscalls"
.Linfo_string3:
	.asciz	"powdr_riscv_syscalls"
.Linfo_string4:
	.asciz	"SYSCALL_REGISTERS"
.Linfo_string5:
	.asciz	"data_ptr"
.Linfo_string6:
	.asciz	"u8"
.Linfo_string7:
	.asciz	"length"
.Linfo_string8:
	.asciz	"usize"
.Linfo_string9:
	.asciz	"&str"
.Linfo_string10:
	.asciz	"__ARRAY_SIZE_TYPE__"
.Linfo_string11:
	.asciz	"_ZN20powdr_riscv_syscalls17SYSCALL_REGISTERS17h69cc336b2d9de90cE"
.Linfo_string12:
	.asciz	"core"
.Linfo_string13:
	.asciz	"fmt"
.Linfo_string14:
	.asciz	"rt"
.Linfo_string15:
	.asciz	"Left"
.Linfo_string16:
	.asciz	"Right"
.Linfo_string17:
	.asciz	"Center"
.Linfo_string18:
	.asciz	"Unknown"
.Linfo_string19:
	.asciz	"Alignment"
.Linfo_string20:
	.asciz	"u32"
.Linfo_string21:
	.asciz	"Input"
.Linfo_string22:
	.asciz	"DataIdentifier"
.Linfo_string23:
	.asciz	"Output"
.Linfo_string24:
	.asciz	"PoseidonGL"
.Linfo_string25:
	.asciz	"Affine256"
.Linfo_string26:
	.asciz	"EcAdd"
.Linfo_string27:
	.asciz	"EcDouble"
.Linfo_string28:
	.asciz	"Mod256"
.Linfo_string29:
	.asciz	"KeccakF"
.Linfo_string30:
	.asciz	"Syscall"
.Linfo_string31:
	.asciz	"{impl#53}"
.Linfo_string32:
	.asciz	"pieces"
.Linfo_string33:
	.asciz	"&[&str]"
.Linfo_string34:
	.asciz	"option"
.Linfo_string35:
	.asciz	"None"
.Linfo_string36:
	.asciz	"position"
.Linfo_string37:
	.asciz	"fill"
.Linfo_string38:
	.asciz	"char"
.Linfo_string39:
	.asciz	"align"
.Linfo_string40:
	.asciz	"flags"
.Linfo_string41:
	.asciz	"precision"
.Linfo_string42:
	.asciz	"Is"
.Linfo_string43:
	.asciz	"__0"
.Linfo_string44:
	.asciz	"Param"
.Linfo_string45:
	.asciz	"Implied"
.Linfo_string46:
	.asciz	"Count"
.Linfo_string47:
	.asciz	"width"
.Linfo_string48:
	.asciz	"Placeholder"
.Linfo_string49:
	.asciz	"&[core::fmt::rt::Placeholder]"
.Linfo_string50:
	.asciz	"T"
.Linfo_string51:
	.asciz	"Some"
.Linfo_string52:
	.asciz	"Option<&[core::fmt::rt::Placeholder]>"
.Linfo_string53:
	.asciz	"args"
.Linfo_string54:
	.asciz	"value"
.Linfo_string55:
	.asciz	"{extern#0}"
.Linfo_string56:
	.asciz	"Opaque"
.Linfo_string57:
	.asciz	"&core::fmt::rt::{extern#0}::Opaque"
.Linfo_string58:
	.asciz	"formatter"
.Linfo_string59:
	.asciz	"result"
.Linfo_string60:
	.asciz	"Ok"
.Linfo_string61:
	.asciz	"()"
.Linfo_string62:
	.asciz	"Error"
.Linfo_string63:
	.asciz	"E"
.Linfo_string64:
	.asciz	"Err"
.Linfo_string65:
	.asciz	"Result<(), core::fmt::Error>"
.Linfo_string66:
	.asciz	"Option<usize>"
.Linfo_string67:
	.asciz	"buf"
.Linfo_string68:
	.asciz	"pointer"
.Linfo_string69:
	.asciz	"dyn core::fmt::Write"
.Linfo_string70:
	.asciz	"vtable"
.Linfo_string71:
	.asciz	"&[usize; 3]"
.Linfo_string72:
	.asciz	"&mut dyn core::fmt::Write"
.Linfo_string73:
	.asciz	"Formatter"
.Linfo_string74:
	.asciz	"&mut core::fmt::Formatter"
.Linfo_string75:
	.asciz	"fn(&core::fmt::rt::{extern#0}::Opaque, &mut core::fmt::Formatter) -> core::result::Result<(), core::fmt::Error>"
.Linfo_string76:
	.asciz	"Argument"
.Linfo_string77:
	.asciz	"&[core::fmt::rt::Argument]"
.Linfo_string78:
	.asciz	"Arguments"
.Linfo_string79:
	.asciz	"_ZN4core3fmt9Arguments6new_v117h9deb289621d58613E"
.Linfo_string80:
	.asciz	"new_v1"
.Linfo_string81:
	.asciz	"{impl#0}"
.Linfo_string82:
	.asciz	"slice"
.Linfo_string83:
	.asciz	"cmp"
.Linfo_string84:
	.asciz	"{impl#5}"
.Linfo_string85:
	.asciz	"A"
.Linfo_string86:
	.asciz	"B"
.Linfo_string87:
	.asciz	"_ZN73_$LT$$u5b$A$u5d$$u20$as$u20$core..slice..cmp..SlicePartialEq$LT$B$GT$$GT$5equal17hedeaaa22bb02df73E"
.Linfo_string88:
	.asciz	"equal<u8, u8>"
.Linfo_string89:
	.asciz	"bool"
.Linfo_string90:
	.asciz	"self"
.Linfo_string91:
	.asciz	"&[u8]"
.Linfo_string92:
	.asciz	"other"
.Linfo_string93:
	.asciz	"size"
.Linfo_string94:
	.asciz	"_ZN4core5slice3cmp81_$LT$impl$u20$core..cmp..PartialEq$LT$$u5b$B$u5d$$GT$$u20$for$u20$$u5b$A$u5d$$GT$2eq17h2c3074623720f922E"
.Linfo_string95:
	.asciz	"eq<u8, u8>"
.Linfo_string96:
	.asciz	"impls"
.Linfo_string97:
	.asciz	"{impl#9}"
.Linfo_string98:
	.asciz	"_ZN4core3cmp5impls69_$LT$impl$u20$core..cmp..PartialEq$LT$$RF$B$GT$$u20$for$u20$$RF$A$GT$2eq17hcce82d5b01baeb7fE"
.Linfo_string99:
	.asciz	"eq<[u8], [u8]>"
.Linfo_string100:
	.asciz	"&&[u8]"
.Linfo_string101:
	.asciz	"str"
.Linfo_string102:
	.asciz	"traits"
.Linfo_string103:
	.asciz	"{impl#1}"
.Linfo_string104:
	.asciz	"_ZN4core3str6traits54_$LT$impl$u20$core..cmp..PartialEq$u20$for$u20$str$GT$2eq17h264187d218304216E"
.Linfo_string105:
	.asciz	"eq"
.Linfo_string106:
	.asciz	"_ZN44_$LT$$RF$T$u20$as$u20$core..fmt..Display$GT$3fmt17h7370650b9d1da708E"
.Linfo_string107:
	.asciz	"fmt<str>"
.Linfo_string108:
	.asciz	"_ZN68_$LT$powdr_riscv_syscalls..Syscall$u20$as$u20$core..fmt..Display$GT$3fmt17hdf8850d239dac7f5E"
.Linfo_string109:
	.asciz	"_ZN76_$LT$powdr_riscv_syscalls..Syscall$u20$as$u20$core..str..traits..FromStr$GT$8from_str17h27673a40b24fea4aE"
.Linfo_string110:
	.asciz	"from_str"
.Linfo_string111:
	.asciz	"Result<powdr_riscv_syscalls::Syscall, ()>"
.Linfo_string112:
	.asciz	"&&str"
.Linfo_string113:
	.asciz	"f"
.Linfo_string114:
	.asciz	"&powdr_riscv_syscalls::Syscall"
.Linfo_string115:
	.asciz	"input"
	.ident	"rustc version 1.77.0-nightly (11f32b73e 2024-01-31)"
	.section	".note.GNU-stack","",@progbits
	.section	.debug_line,"",@progbits
.Lline_table_start0:
