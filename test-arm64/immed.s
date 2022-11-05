// picoPascal compiler output
	.global pmain

	.section .text
pmain:
	stp x19, x20, [sp, -16]!
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   k := 100;
	ldr x20, =_k
	mov w0, #100
	str w0, [x20]
//   print_num(516); newline();
	mov w0, #516
	bl print_num
	bl newline
//   print_num(517); newline();
	mov w0, #517
	bl print_num
	bl newline
//   print_num(k + -50); newline();
	ldr w0, [x20]
	sub w0, w0, #50
	bl print_num
	bl newline
//   print_num(k + -1023); newline();
	ldr w0, [x20]
	sub w0, w0, #1023
	bl print_num
	bl newline
//   print_num(k + -1024); newline()
	ldr w0, [x20]
	sub w0, w0, #1024
	bl print_num
	bl newline
	ldp fp, lr, [sp], #16
	ldp x19, x20, [sp], #16
	ret
	.pool

	.comm _k, 4, 4
// End
