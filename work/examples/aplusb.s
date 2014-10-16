aplusb:
	rrb $t1
	rrb $t2
	addu $t3, $t1, $t2
	rsb $t3
	j aplusb
