// picoPascal compiler output
	.global pmain

// proc f(): integer;
	.section .text
_f:
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   x := 3;
	mov w20, #3
//   y := x + 1;
	add w21, w20, #1
//   g();
	bl _g
//   return x + 1
	add w0, w20, #1
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ret
	.pool

// proc g(); begin end;
_g:
	stp fp, lr, [sp, -16]!
	mov fp, sp
	ldp fp, lr, [sp], #16
	ret
	.pool

pmain:
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   print_num(f()); newline()
	bl _f
	bl print_num
	bl newline
	ldp fp, lr, [sp], #16
	ret
	.pool

// End
