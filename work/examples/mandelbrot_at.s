mandelbrot:
	rrb	$t0 # Receive any character
	li	$t0, 0x3F000000 # 0.5
	mtc1	$t0, $f0
	mtc1	$zero, $f1 # +0.0
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
	rsb	$v0
	j	mandelbrot

