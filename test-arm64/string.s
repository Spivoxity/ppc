// picoPascal compiler output
	.global pmain

	.section .text
pmain:
	stp fp, lr, [sp, -16]!
	mov fp, sp
//   print_string("Hello world!");
	mov w1, #13
	ldr x0, =g1
	bl print_string
//   newline()
	bl newline
	ldp fp, lr, [sp], #16
	ret
	.pool

	.section .rodata
g1:
	.byte 72, 101, 108, 108, 111, 32, 119, 111, 114, 108
	.byte 100, 33
	.byte 0
// End
