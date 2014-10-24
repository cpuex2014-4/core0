mandelbrot:
	rrb	$t0 # Receive any character
	li	$t0, 0x50 # 'P'
	rsb	$t0
	li	$t0, 0x36 # '6'
	rsb	$t0
	li	$t0, 0x20 # ' '
	rsb	$t0
	li	$t0, 0x39 # '9'
	rsb	$t0
	li	$t0, 0x36 # '6'
	rsb	$t0
	li	$t0, 0x20 # ' '
	rsb	$t0
	li	$t0, 0x36 # '6'
	rsb	$t0
	li	$t0, 0x34 # '4'
	rsb	$t0
	li	$t0, 0x20 # ' '
	rsb	$t0
	li	$t0, 0x32 # '2'
	rsb	$t0
	li	$t0, 0x35 # '5'
	rsb	$t0
	li	$t0, 0x35 # '5'
	rsb	$t0
	li	$t0, 0x0A # '\n'
	rsb	$t0
	li	$s4, 0x3D000000 # 1.0 / 2^5
	mtc1	$s4, $f4
	#li	$s2, 0x40000000 # 2.0f
	#mtc1	$s2, $f2
	li	$s3, 0x3F800000 # 1.0f
	mtc1	$s3, $f3
	li	$s1, 0xBF800000 # y <= -1.0f
	mtc1	$s1, $f1
mandelbrot_loop_y:
	li	$s0, 0xC0000000 # x <= -2.0f
	mtc1	$s0, $f0
mandelbrot_loop_x:
	c.olt.s	$f0, $f3
	bc1f	mandelbrot_end_x
	jal	mandelbrot_at
	rsb	$v0
	rsb	$v0
	rsb	$v0
	add.s	$f0, $f0, $f4
	j	mandelbrot_loop_x
mandelbrot_end_x:
	c.olt.s	$f1, $f3
	bc1f	mandelbrot_end_y
	add.s	$f1, $f1, $f4
	j	mandelbrot_loop_y
mandelbrot_end_y:
	j	mandelbrot
# mandelbrot_at
# argument $f0 : real part of C
# argument $f1 : imaginary part of C
# return value : $v0 : 0 or 255
# Uses $t0, $t1, $t3, $t4, $v0
# Uses $f8 - $f19
mandelbrot_at:
	li	$t3, 0x40000000 # 2.0f
	mtc1	$t3, $f8
	li	$t4, 0x40800000 # 4.0f
	mtc1	$t4, $f9
	mtc1	$zero, $f10 # real(z) = 0.0
	mtc1	$zero, $f11 # imag(z) = 0.0
	li	$t0, 200 # Number of repeat
	li	$v0, 0 # Result
mandelbrot_at_loop:
	slt	$t1, $zero, $t0 # 0 < count ?
	beq	$t1, $zero, mandelbrot_at_end1 # if not, exit loop
	mul.s	$f12, $f10, $f10 # real(z) * real(z)
	mul.s	$f13, $f11, $f11 # imag(z) * imag(z)
	add.s	$f14, $f12, $f13 # |z|^2 = real(z)*real(z) + imag(z)*imag(z)
	c.olt.s	$f14, $f9 # |z|^2 < 4.0 ?
	bc1f	mandelbrot_at_end0 # if not, exit loop
	sub.s	$f15, $f12, $f13 # real(z^2) = real(z)*real(z)-imag(z)*imag(z)
	add.s	$f16, $f15, $f0 # real(z^2) + real(C)
	mul.s	$f17, $f8, $f10 # 2.0 * real(z)
	mul.s	$f18, $f17, $f11 # imag(z^2) = 2.0 * real(z) * imag(z)
	add.s	$f19, $f18, $f1 # imag(z^2) + imag(C)
	mov.s	$f10, $f16 # real(z) <= real(z^2) + real(C)
	mov.s	$f11, $f19 # imag(z) <= imag(z^2) + imag(C)
	addiu	$t0, $t0, -1 # --count;
	j	mandelbrot_at_loop
mandelbrot_at_end1:
	addiu	$v0, $zero, 255 # Result <= 255
mandelbrot_at_end0:
	jr $ra # END
