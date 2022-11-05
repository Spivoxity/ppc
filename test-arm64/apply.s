// picoPascal compiler output
	.global pmain

// proc apply(proc f(x: integer));
	.section .text
_apply:
	stp x0, x1, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   f(111)
	mov w0, #111
	ldr x19, [fp, #40]
	ldr x1, [fp, #32]
	blr x1
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	add sp, sp, #16
	ret
	.pool

// proc beta(y: integer);
_beta:
	stp x0, x1, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   apply(f);
	add x20, fp, #16
	mov x1, x20
	ldr x0, =_f
	bl _apply
//   apply(g)
	mov x1, x20
	ldr x0, =_g
	bl _apply
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	add sp, sp, #16
	ret
	.pool

//   proc f(x: integer);
_f:
	stp x0, x1, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//     print_num(x);
	ldr w0, [fp, #32]
	bl print_num
//     newline();
	bl newline
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	add sp, sp, #16
	ret
	.pool

//   proc g(x:integer);
_g:
	stp x0, x1, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//     print_num(y);
	ldr x0, [fp, #16]
	ldr w0, [x0, #16]
	bl print_num
//     newline();
	bl newline
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	add sp, sp, #16
	ret
	.pool

pmain:
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   beta(222)
	mov w0, #222
	bl _beta
	ldp fp, lr, [sp], #16
	ret
	.pool

// End
