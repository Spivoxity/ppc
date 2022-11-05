// picoPascal compiler output
	.global pmain

// proc g(var c: char); begin
	.section .text
_g:
	stp x0, x1, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   c := chr(ord(c)+1)
	ldr x20, [fp, #32]
	ldrb w0, [x20]
	add w0, w0, #1
	strb w0, [x20]
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	add sp, sp, #16
	ret
	.pool

// proc f(x: char): char;
_f:
	stp x0, x1, [sp, -16]!
	sub sp, sp, #16
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   c := x;
	ldrb w0, [fp, #32]
	strb w0, [fp, #31]
//   g(c);
	add x0, fp, #31
	bl _g
//   return c
	ldrb w0, [fp, #31]
	ldp fp, lr, [sp], #16
	add sp, sp, #16
	add sp, sp, #16
	ret
	.pool

pmain:
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   print_char(f('A')); newline()
	mov w0, #65
	bl _f
	bl print_char
	bl newline
	ldp fp, lr, [sp], #16
	ret
	.pool

// End
