// picoPascal compiler output
	.global pmain

// proc f(x: integer): integer; begin return 2*x end;
	.section .text
_f:
	stp x0, x1, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
// proc f(x: integer): integer; begin return 2*x end;
	ldr w0, [fp, #16]
	lsl w0, w0, #1
	ldp fp, lr, [sp], #16
	add sp, sp, #16
	ret
	.pool

pmain:
	stp fp, lr, [sp, -16]!
	mov fp, sp
// begin print_num(f(f(3))); newline() end.
	mov w0, #3
	bl _f
	bl _f
	bl print_num
	bl newline
	ldp fp, lr, [sp], #16
	ret
	.pool

// End
