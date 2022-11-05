// picoPascal compiler output
	.global pmain

// proc f(var x: integer);
	.section .text
_f:
	stp x0, x1, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   x := 37
	mov w0, #37
	ldr x1, [fp, #16]
	str w0, [x1]
	ldp fp, lr, [sp], #16
	add sp, sp, #16
	ret
	.pool

pmain:
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   i := 3;
	mov w20, #3
	ldr x0, =_i
	str w20, [x0]
//   f(a[i]);
	ldr x21, =_a
	add x0, x21, w20, SXTW 2
	bl _f
//   print_num(a[3]); newline()
	ldr w0, [x21, #12]
	bl print_num
	bl newline
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ret
	.pool

	.comm _i, 4, 4
	.comm _a, 40, 4
// End
