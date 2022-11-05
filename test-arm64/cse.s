// picoPascal compiler output
	.global pmain

// proc p(x, y: integer);
	.section .text
_p:
	stp x0, x1, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   print_num((x-y)*(x-y)+x); newline()
	ldr w20, [fp, #48]
	ldr w0, [fp, #56]
	sub w21, w20, w0
	mul w0, w21, w21
	add w0, w0, w20
	bl print_num
	bl newline
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	add sp, sp, #16
	ret
	.pool

pmain:
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   p(9, 5)
	mov w1, #5
	mov w0, #9
	bl _p
	ldp fp, lr, [sp], #16
	ret
	.pool

// End
