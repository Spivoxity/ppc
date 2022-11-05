// picoPascal compiler output
	.global pmain

// proc f(x: integer): integer;
	.section .text
_f:
	stp x0, x1, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   return x
	ldr w0, [fp, #16]
	ldp fp, lr, [sp], #16
	add sp, sp, #16
	ret
	.pool

pmain:
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   print_num(f(3)+f(4)); newline()
	mov w0, #3
	bl _f
	mov x20, x0
	mov w0, #4
	bl _f
	mov x21, x0
	add w0, w20, w21
	bl print_num
	bl newline
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ret
	.pool

// End
