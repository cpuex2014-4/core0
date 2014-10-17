	addiu	$fp, $zero, 16384
	addiu	$sp, $zero, 16384
start:
	rrb	$t0
	sll	$a0, $t0, 24
	rrb	$t0
	sll	$t0, $t0, 16
	or	$a0, $a0, $t0
	rrb	$t0
	sll	$t0, $t0, 8
	or	$a0, $a0, $t0
	rrb	$t0
	or	$a0, $a0, $t0
	jal	fib
	srl	$t0, $v0, 24
	rsb	$t0
	srl	$t0, $v0, 16
	rsb	$t0
	srl	$t0, $v0, 8
	rsb	$t0
	rsb	$v0
	j	start
fib:
	addiu	$sp, $sp, -12
	sw	$ra, 8($sp)
	sw	$a0, 4($sp)
	addu	$t1, $a0, $zero
	addiu	$t2, $zero, 1
	slt	$t0, $t2, $a0
	beq	$t0, $zero, exit
	addiu	$a0, $a0, -1
	jal	fib
	sw	$v0, 0($sp)
	lw	$a0, 4($sp)
	addiu	$a0, $a0, -2
	jal	fib
	lw	$t1, 0($sp)
	addu	$t1, $t1, $v0
	lw	$ra, 8($sp)
exit:
	addiu	$sp, $sp, 12
	addu	$v0, $t1, $zero
	jr	$ra
