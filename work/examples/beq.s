beqtest:
	rrb $t1
	beq $t1, $0, beqtest
	rsb $t1
	j beqtest
