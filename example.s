	.text
	_lin_fib:
	sw    $ra, 0($sp)		# PUSH $ra
	subu  $sp, $sp, 4
	sw    $fp, 0($sp)		# PUSH $fp
	subu  $sp, $sp, 4
	addu  $fp, $sp, 8
	subu  $sp, $sp, 12		# allocating space for locals
	li    $t0, 0
	sw    $t0, 0($sp)		# PUSH $t0
	subu  $sp, $sp, 4
	la    $t0, -8($fp)		# 
	sw    $t0, 0($sp)		# PUSH $t0
	subu  $sp, $sp, 4
	lw    $t0, 4($sp)		# POP to $t0
	addu  $sp, $sp, 4
	lw    $t1, 4($sp)		# POP to $t1
	addu  $sp, $sp, 4
	sw    $t1, 0($t0)		# 
	sw    $t1, 0($sp)		# PUSH $t1
	subu  $sp, $sp, 4
	lw    $t0, 4($sp)		# POP to $t0
	addu  $sp, $sp, 4
	li    $t0, 1
	sw    $t0, 0($sp)		# PUSH $t0
	subu  $sp, $sp, 4
	la    $t0, -12($fp)		# 
	sw    $t0, 0($sp)		# PUSH $t0
	subu  $sp, $sp, 4
	lw    $t0, 4($sp)		# POP to $t0
	addu  $sp, $sp, 4
	lw    $t1, 4($sp)		# POP to $t1
	addu  $sp, $sp, 4
	sw    $t1, 0($t0)		# 
	sw    $t1, 0($sp)		# PUSH $t1
	subu  $sp, $sp, 4
	lw    $t0, 4($sp)		# POP to $t0
	addu  $sp, $sp, 4
.L1:		# while loop start
	lw    $t0, 4($fp)		# load value of n
	sw    $t0, 0($sp)		# PUSH $t0
	subu  $sp, $sp, 4
	li    $t0, 0
	sw    $t0, 0($sp)		# PUSH $t0
	subu  $sp, $sp, 4
	lw    $t1, 4($sp)		# POP to $t1
	addu  $sp, $sp, 4
	lw    $t0, 4($sp)		# POP to $t0
	addu  $sp, $sp, 4
	sgt   $t0, $t0, $t1
	sw    $t0, 0($sp)		# PUSH $t0
	subu  $sp, $sp, 4
	lw    $t0, 4($sp)		# POP to $t0
	addu  $sp, $sp, 4
	li    $t1, 0
	beq   $t0, $t1, .L2
	lw    $t0, -8($fp)		# load value of a
	sw    $t0, 0($sp)		# PUSH $t0
	subu  $sp, $sp, 4
	lw    $a0, 4($sp)		# POP to $a0
	addu  $sp, $sp, 4
	li    $v0, 1
	syscall
	.data
.L3:	.asciiz "\n"
	.text
	la    $t0, .L3		# load address of string literal
	sw    $t0, 0($sp)		# PUSH $t0
	subu  $sp, $sp, 4
	lw    $a0, 4($sp)		# POP to $a0
	addu  $sp, $sp, 4
	li    $v0, 4
	syscall
	lw    $t0, -12($fp)		# load value of b
	sw    $t0, 0($sp)		# PUSH $t0
	subu  $sp, $sp, 4
	la    $t0, -16($fp)		# 
	sw    $t0, 0($sp)		# PUSH $t0
	subu  $sp, $sp, 4
	lw    $t0, 4($sp)		# POP to $t0
	addu  $sp, $sp, 4
	lw    $t1, 4($sp)		# POP to $t1
	addu  $sp, $sp, 4
	sw    $t1, 0($t0)		# 
	sw    $t1, 0($sp)		# PUSH $t1
	subu  $sp, $sp, 4
	lw    $t0, 4($sp)		# POP to $t0
	addu  $sp, $sp, 4
	lw    $t0, -8($fp)		# load value of a
	sw    $t0, 0($sp)		# PUSH $t0
	subu  $sp, $sp, 4
	lw    $t0, -12($fp)		# load value of b
	sw    $t0, 0($sp)		# PUSH $t0
	subu  $sp, $sp, 4
	lw    $t1, 4($sp)		# POP to $t1
	addu  $sp, $sp, 4
	lw    $t0, 4($sp)		# POP to $t0
	addu  $sp, $sp, 4
	add   $t0, $t0, $t1
	sw    $t0, 0($sp)		# PUSH $t0
	subu  $sp, $sp, 4
	la    $t0, -12($fp)		# 
	sw    $t0, 0($sp)		# PUSH $t0
	subu  $sp, $sp, 4
	lw    $t0, 4($sp)		# POP to $t0
	addu  $sp, $sp, 4
	lw    $t1, 4($sp)		# POP to $t1
	addu  $sp, $sp, 4
	sw    $t1, 0($t0)		# 
	sw    $t1, 0($sp)		# PUSH $t1
	subu  $sp, $sp, 4
	lw    $t0, 4($sp)		# POP to $t0
	addu  $sp, $sp, 4
	lw    $t0, -16($fp)		# load value of temp
	sw    $t0, 0($sp)		# PUSH $t0
	subu  $sp, $sp, 4
	la    $t0, -8($fp)		# 
	sw    $t0, 0($sp)		# PUSH $t0
	subu  $sp, $sp, 4
	lw    $t0, 4($sp)		# POP to $t0
	addu  $sp, $sp, 4
	lw    $t1, 4($sp)		# POP to $t1
	addu  $sp, $sp, 4
	sw    $t1, 0($t0)		# 
	sw    $t1, 0($sp)		# PUSH $t1
	subu  $sp, $sp, 4
	lw    $t0, 4($sp)		# POP to $t0
	addu  $sp, $sp, 4
	la    $t0, 4($fp)		# 
	sw    $t0, 0($sp)		# PUSH $t0
	subu  $sp, $sp, 4
	lw    $t0, 4($sp)		# POP to $t0
	addu  $sp, $sp, 4
	lw    $t1, 0($t0)		# 
	addi  $t1, $t1, -1
	sw    $t1, 0($t0)		# 
	j     .L1
.L2:		# while loop end label
lin_fib_Exit:		
	lw    $ra, 0($fp)		# load return addr for lin_fib
	move  $t0, $fp		# old sp
	lw    $fp, -4($fp)		# restore old fp for lin_fib
	move  $sp, $t0		# restore sp
	jr    $ra		# return
	.text
	_fact:
	sw    $ra, 0($sp)		# PUSH $ra
	subu  $sp, $sp, 4
	sw    $fp, 0($sp)		# PUSH $fp
	subu  $sp, $sp, 4
	addu  $fp, $sp, 8
	lw    $t0, 4($fp)		# load value of n
	sw    $t0, 0($sp)		# PUSH $t0
	subu  $sp, $sp, 4
	li    $t0, 1
	sw    $t0, 0($sp)		# PUSH $t0
	subu  $sp, $sp, 4
	lw    $t1, 4($sp)		# POP to $t1
	addu  $sp, $sp, 4
	lw    $t0, 4($sp)		# POP to $t0
	addu  $sp, $sp, 4
	sle   $t0, $t0, $t1
	sw    $t0, 0($sp)		# PUSH $t0
	subu  $sp, $sp, 4
	lw    $t0, 4($sp)		# POP to $t0
	addu  $sp, $sp, 4
	li    $t1, 0
	beq   $t0, $t1, .L4		# if jump
	lw    $t0, 4($fp)		# load value of n
	sw    $t0, 0($sp)		# PUSH $t0
	subu  $sp, $sp, 4
	lw    $v0, 4($sp)		# POP to $v0
	addu  $sp, $sp, 4
	j     fact_Exit
	j     .L5
.L4:		
	lw    $t0, 4($fp)		# load value of n
	sw    $t0, 0($sp)		# PUSH $t0
	subu  $sp, $sp, 4
	lw    $t0, 4($fp)		# load value of n
	sw    $t0, 0($sp)		# PUSH $t0
	subu  $sp, $sp, 4
	li    $t0, 1
	sw    $t0, 0($sp)		# PUSH $t0
	subu  $sp, $sp, 4
	lw    $t1, 4($sp)		# POP to $t1
	addu  $sp, $sp, 4
	lw    $t0, 4($sp)		# POP to $t0
	addu  $sp, $sp, 4
	sub   $t0, $t0, $t1
	sw    $t0, 0($sp)		# PUSH $t0
	subu  $sp, $sp, 4
	jal   _fact
	add   $sp, $sp, 4
	sw    $v0, 0($sp)		# PUSH $v0
	subu  $sp, $sp, 4
	lw    $t1, 4($sp)		# POP to $t1
	addu  $sp, $sp, 4
	lw    $t0, 4($sp)		# POP to $t0
	addu  $sp, $sp, 4
	mult  $t0, $t1
	mflo  $t0
	sw    $t0, 0($sp)		# PUSH $t0
	subu  $sp, $sp, 4
	lw    $v0, 4($sp)		# POP to $v0
	addu  $sp, $sp, 4
	j     fact_Exit
.L5:		
fact_Exit:		
	lw    $ra, 0($fp)		# load return addr for fact
	move  $t0, $fp		# old sp
	lw    $fp, -4($fp)		# restore old fp for fact
	move  $sp, $t0		# restore sp
	jr    $ra		# return
	.text
	.globl main
main:
	sw    $ra, 0($sp)		# PUSH $ra
	subu  $sp, $sp, 4
	sw    $fp, 0($sp)		# PUSH $fp
	subu  $sp, $sp, 4
	addu  $fp, $sp, 8
	subu  $sp, $sp, 4		# allocating space for locals
	.data
.L6:	.asciiz "enter a number: "
	.text
	la    $t0, .L6		# load address of string literal
	sw    $t0, 0($sp)		# PUSH $t0
	subu  $sp, $sp, 4
	lw    $a0, 4($sp)		# POP to $a0
	addu  $sp, $sp, 4
	li    $v0, 4
	syscall
	li    $v0, 5
	syscall
	la    $t0, -8($fp)		# 
	sw    $t0, 0($sp)		# PUSH $t0
	subu  $sp, $sp, 4
	lw    $t0, 4($sp)		# POP to $t0
	addu  $sp, $sp, 4
	sw    $v0, 0($t0)		# 
	lw    $t0, -8($fp)		# load value of num
	sw    $t0, 0($sp)		# PUSH $t0
	subu  $sp, $sp, 4
	jal   _lin_fib
	add   $sp, $sp, 4
	sw    $v0, 0($sp)		# PUSH $v0
	subu  $sp, $sp, 4
	lw    $t0, 4($sp)		# POP to $t0
	addu  $sp, $sp, 4
	la    $t0, .L3		# load address of string literal
	sw    $t0, 0($sp)		# PUSH $t0
	subu  $sp, $sp, 4
	lw    $a0, 4($sp)		# POP to $a0
	addu  $sp, $sp, 4
	li    $v0, 4
	syscall
	.data
.L7:	.asciiz "enter another number: "
	.text
	la    $t0, .L7		# load address of string literal
	sw    $t0, 0($sp)		# PUSH $t0
	subu  $sp, $sp, 4
	lw    $a0, 4($sp)		# POP to $a0
	addu  $sp, $sp, 4
	li    $v0, 4
	syscall
	li    $v0, 5
	syscall
	la    $t0, -8($fp)		# 
	sw    $t0, 0($sp)		# PUSH $t0
	subu  $sp, $sp, 4
	lw    $t0, 4($sp)		# POP to $t0
	addu  $sp, $sp, 4
	sw    $v0, 0($t0)		# 
	lw    $t0, -8($fp)		# load value of num
	sw    $t0, 0($sp)		# PUSH $t0
	subu  $sp, $sp, 4
	lw    $a0, 4($sp)		# POP to $a0
	addu  $sp, $sp, 4
	li    $v0, 1
	syscall
	.data
.L8:	.asciiz "! = "
	.text
	la    $t0, .L8		# load address of string literal
	sw    $t0, 0($sp)		# PUSH $t0
	subu  $sp, $sp, 4
	lw    $a0, 4($sp)		# POP to $a0
	addu  $sp, $sp, 4
	li    $v0, 4
	syscall
	lw    $t0, -8($fp)		# load value of num
	sw    $t0, 0($sp)		# PUSH $t0
	subu  $sp, $sp, 4
	jal   _fact
	add   $sp, $sp, 4
	sw    $v0, 0($sp)		# PUSH $v0
	subu  $sp, $sp, 4
	lw    $a0, 4($sp)		# POP to $a0
	addu  $sp, $sp, 4
	li    $v0, 1
	syscall
main_Exit:		
	lw    $ra, 0($fp)		# load return addr for main
	move  $t0, $fp		# old sp
	lw    $fp, -4($fp)		# restore old fp for main
	move  $sp, $t0		# restore sp
	li    $v0, 10		# exit
	syscall		# exit