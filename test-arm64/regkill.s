// picoPascal compiler output
	.global pmain

// proc p();
	.section .text
_p:
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   y := 1;
	mov w20, #1
//   z := y + 1;
	add w21, w20, #1
//   z := 3;
	mov w21, #3
//   z := y + 1;
	add w21, w20, #1
//   x := z
	ldr x0, =_x
	str w21, [x0]
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ret
	.pool

pmain:
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   p();
	bl _p
//   print_num(x);
	ldr x0, =_x
	ldr w0, [x0]
	bl print_num
//   newline()
	bl newline
	ldp fp, lr, [sp], #16
	ret
	.pool

	.comm _x, 4, 4
// End
