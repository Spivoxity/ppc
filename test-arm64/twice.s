// picoPascal compiler output
	.global pmain

// proc square(x: int): int; begin return x * x end;
	.section .text
_square:
	stp x0, x1, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
// proc square(x: int): int; begin return x * x end;
	ldr w20, [fp, #32]
	mul w0, w20, w20
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	add sp, sp, #16
	ret
	.pool

// proc twice(proc f(y: int): int; x: int): int;
_twice:
	stp x2, x3, [sp, -16]!
	stp x0, x1, [sp, -16]!
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
// begin return f(f(x)) end;
	ldr x20, [fp, #48]
	ldr x21, [fp, #56]
	ldr w0, [fp, #64]
	mov x19, x21
	blr x20
	mov x19, x21
	blr x20
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	add sp, sp, #32
	ret
	.pool

// proc ap_to_sq(proc ff(proc f(x: int): int; x: int): int; x: int): int;
_ap_to_sq:
	stp x2, x3, [sp, -16]!
	stp x0, x1, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
// begin return ff(square, x) end;
	ldr w2, [fp, #48]
	mov x1, xzr
	ldr x0, =_square
	ldr x19, [fp, #40]
	ldr x3, [fp, #32]
	blr x3
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	add sp, sp, #32
	ret
	.pool

pmain:
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   print_num(ap_to_sq(twice, 3));
	mov w2, #3
	mov x1, xzr
	ldr x0, =_twice
	bl _ap_to_sq
	bl print_num
//   newline()
	bl newline
	ldp fp, lr, [sp], #16
	ret
	.pool

// End
