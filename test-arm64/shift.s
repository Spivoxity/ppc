// picoPascal compiler output
	.global pmain

	.section .text
pmain:
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   x := 2;
	mov w20, #2
	ldr x0, =_x
	str w20, [x0]
//   print_num(lsl(lsl(3, x), lsl(x, x))+(y-z)+(p-q)); newline();
	mov w0, #3
	lsl w0, w0, w20
	lsl w1, w20, w20
	lsl w0, w0, w1
	ldr x1, =_y
	ldr w1, [x1]
	ldr x2, =_z
	ldr w2, [x2]
	sub w1, w1, w2
	add w0, w0, w1
	ldr x1, =_p
	ldr w1, [x1]
	ldr x2, =_q
	ldr w2, [x2]
	sub w1, w1, w2
	add w0, w0, w1
	bl print_num
	bl newline
//   print_num(lsl(lsl(3, 2), lsl(2, 2))); newline()
	mov w0, #3072
	bl print_num
	bl newline
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ret
	.pool

	.comm _x, 4, 4
	.comm _y, 4, 4
	.comm _z, 4, 4
	.comm _p, 4, 4
	.comm _q, 4, 4
// End
