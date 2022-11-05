// picoPascal compiler output
	.global pmain

// proc foo(var a: array 10000 of integer);
	.section .text
_foo:
	stp x0, x1, [sp, -16]!
	stp x19, x20, [sp, -16]!
	ldr ip0, =40000
	sub sp, sp, ip0
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   x := 5000;
	ldr w20, =5000
//   c[5000] := 4;
	mov w0, #4
	ldr x1, =20016
	str w0, [fp, w1, SXTW]
//   a[5000] := c[x]+3 
	add x0, fp, #16
	ldr w0, [x0, w20, SXTW #2]
	add w0, w0, #3
	ldr x1, =40032
	ldr x1, [fp, w1, SXTW]
	ldr w2, =20000
	str w0, [x1, w2, SXTW]
	ldp fp, lr, [sp], #16
	ldr ip0, =40000
	add sp, sp, ip0
	ldp x19, x20, [sp], #16
	add sp, sp, #16
	ret
	.pool

pmain:
	stp fp, lr, [sp, -16]!
	mov fp, sp
// begin foo(b) end.
	ldr x0, =_b
	bl _foo
	ldp fp, lr, [sp], #16
	ret
	.pool

	.comm _b, 40000, 4
// End
