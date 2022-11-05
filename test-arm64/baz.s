// picoPascal compiler output
	.global pmain

// proc baz(u: integer): integer;
	.section .text
_baz:
	stp x0, x1, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   x := u;
	ldr w20, [fp, #32]
	ldr x0, =_x
	str w20, [x0]
//   return x
	mov w0, w20
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	add sp, sp, #16
	ret
	.pool

pmain:
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   print_num(baz(37)); newline();
	mov w0, #37
	bl _baz
	bl print_num
	bl newline
//   print_num(x); newline()
	ldr x0, =_x
	ldr w0, [x0]
	bl print_num
	bl newline
	ldp fp, lr, [sp], #16
	ret
	.pool

	.comm _x, 4, 4
// End
