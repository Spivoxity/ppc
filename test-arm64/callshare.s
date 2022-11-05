// picoPascal compiler output
	.global pmain

// proc f(n: integer): integer;
	.section .text
_f:
	stp x0, x1, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
// begin x := x + n; return x end;
	ldr x20, =_x
	ldr w0, [x20]
	ldr w1, [fp, #48]
	add w21, w0, w1
	str w21, [x20]
	mov w0, w21
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	add sp, sp, #16
	ret
	.pool

pmain:
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   x := 2;
	ldr x20, =_x
	mov w0, #2
	str w0, [x20]
//   y := f(3) + 1;
	mov w0, #3
	bl _f
	ldr x21, =_y
	add w0, w0, #1
	str w0, [x21]
//   y := f(3) + 1;
	mov w0, #3
	bl _f
	add w0, w0, #1
	str w0, [x21]
//   print_num(x); newline()
	ldr w0, [x20]
	bl print_num
	bl newline
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ret
	.pool

	.comm _x, 4, 4
	.comm _y, 4, 4
// End
