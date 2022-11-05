// picoPascal compiler output
	.global pmain

	.section .text
pmain:
	stp x21, x22, [sp, -16]!
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   x := 314159265;
	ldr w20, =314159265
	ldr x21, =_x
	str w20, [x21]
//   x := bitnot(x);
	mvn w20, w20
	str w20, [x21]
//   print_num(x); newline()
	mov x0, x20
	bl print_num
	bl newline
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ldp x21, x22, [sp], #16
	ret
	.pool

	.comm _x, 4, 4
// End
