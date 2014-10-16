loopback:
	rrb $t1
	sw $t1, 4($0)
	lw $t3, 4($0)
	rsb $t3
	j loopback
