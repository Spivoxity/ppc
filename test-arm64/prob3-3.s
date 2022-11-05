// picoPascal compiler output
	.global pmain

// proc double(x: integer): integer;
	.section .text
_double:
	stp x0, x1, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   return x + x
	ldr w20, [fp, #32]
	add w0, w20, w20
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	add sp, sp, #16
	ret
	.pool

// proc apply3(proc f(x:integer): integer): integer;
_apply3:
	stp x0, x1, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   return f(3)
	mov w0, #3
	ldr x19, [fp, #40]
	ldr x1, [fp, #32]
	blr x1
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	add sp, sp, #16
	ret
	.pool

pmain:
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   print_num(apply3(double));
	mov x1, xzr
	ldr x0, =_double
	bl _apply3
	bl print_num
//   newline()
	bl newline
	ldp fp, lr, [sp], #16
	ret
	.pool

// End
